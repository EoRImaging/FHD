FUNCTION holo_mapfn_convert,map_fn,psf_dim=psf_dim,dimension=dimension,elements=elements,$
    n_vis=n_vis,threshold=threshold,error=error,timing=timing
t0=Systime(1)

IF N_Elements(elements) EQ 0 THEN elements=dimension
IF N_Elements(threshold) EQ 0 THEN threshold=0.
IF N_Elements(n_vis) EQ 0 THEN norm=1. ELSE norm=n_vis

;convert pointer array holographic map function to a sparse matrix

;Result = SPRSIN(Columns, Rows, Values, N [, /DOUBLE] [, THRESHOLD=value])
;Result = LINBCG( A, B, X [, /DOUBLE] [, ITOL={4 | 5 | 6 | 7}] [, TOL=value] [, ITER=variable] [, ITMAX=value] )
;A: A row-indexed sparse array created by the SPRSIN function.
;B: An n-element vector containing the right-hand side of the linear system Ax=b.
;X: An n-element vector containing the initial solution of the linear system.

;NOW MODIFIED: all entries for the same column are grouped in their own vector, referenced by a pointer. 
;    Diagonal entries are no longer treated seperately

psf_dim2=2*psf_dim
psf_n=psf_dim2^2.
psf_i=Indgen(psf_n)
sub_xv=meshgrid(psf_dim2,1)-psf_dim
sub_yv=meshgrid(psf_dim2,2)-psf_dim

n=dimension*elements

n_arr=fltarr(dimension,elements)
;diag_vals=fltarr(dimension,elements)
FOR xi=1,dimension-2 DO BEGIN
    FOR yi=1,elements-2 DO BEGIN
        IF not Ptr_valid(map_fn[xi,yi]) THEN CONTINUE
        temp_arr=*map_fn[xi,yi]
;        diag_vals[xi,yi]=temp_arr[psf_dim,psf_dim]
        i1=where(Abs(temp_arr) GT threshold,n1)
        n_arr[xi,yi]=n1
    ENDFOR
ENDFOR

i_use=where(n_arr,n_use)
IF n_use EQ 0 THEN BEGIN
    error=1
    RETURN,0
ENDIF

i_use_hist=histogram(i_use,min=0,bin=1,reverse_ind=ri_use)
sa=Ptrarr(n_use,/allocate)
ija=Ptrarr(n_use,/allocate)

FOR i0=0.,n_use-1 DO BEGIN
    i=i_use[i0]
    xi=Long(i mod dimension)
    yi=Long(Floor(i/dimension))
    map_fn_sub=*map_fn[xi,yi]
    *map_fn[xi,yi]=0 ;free memory as soon as it's read
    j_use=where(Abs(map_fn_sub) GT threshold,n_use)
    
    xii_arr=sub_xv[j_use]+xi
    yii_arr=sub_yv[j_use]+yi    
    *sa[i0]=map_fn_sub[j_use]
    *ija[i0]=ri_use[ri_use[xii_arr+yii_arr*dimension]]
ENDFOR

Ptr_free,map_fn
heap_gc
map_fn={ija:ija,sa:sa,i_use:i_use,norm:norm,indexed:1}

timing=Systime(1)-t0
RETURN,map_fn
END