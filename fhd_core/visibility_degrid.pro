FUNCTION visibility_degrid,image_uv,flag_ptr,obs,psf,params,$
    timing=timing,polarization=polarization,silent=silent,$
    complex=complex,double=double,_Extra=extra
t0=Systime(1)
heap_gc

pol_names=['xx','yy','xy','yx']
IF tag_exist(psf,'complex_flag') THEN complex=psf.complex_flag ELSE IF N_Elements(complex) EQ 0 THEN complex=1

;extract information from the structures
dimension=Float(obs.dimension)
elements=Float(obs.elements)
kbinsize=obs.kpix
kx_span=kbinsize*dimension ;Units are # of wavelengths
ky_span=kx_span
min_baseline=obs.min_baseline
max_baseline=obs.max_baseline

IF Tag_exist(obs,'fbin_i') THEN freq_bin_i=obs.fbin_i ELSE freq_bin_i=(*obs.baseline_info).fbin_i
nfreq_bin=Max(freq_bin_i)+1
bin_offset=(*obs.baseline_info).bin_offset
IF Tag_exist(obs,'freq') THEN frequency_array=obs.freq ELSE frequency_array=(*obs.baseline_info).freq

psf_base=psf.base
psf_dim=Sqrt((Size(*psf_base[0],/dimension))[0])
psf_resolution=(Size(psf_base,/dimension))[2]

flag_switch=Keyword_Set(flag_ptr)
kx_arr=params.uu/kbinsize
ky_arr=params.vv/kbinsize
baseline_i=params.baseline_arr
nbaselines=bin_offset[1]
n_samples=N_Elements(bin_offset)
n_frequencies=N_Elements(frequency_array)
n_freq_bin=N_Elements(freq_bin_i)
psf_dim2=2*psf_dim

vis_dimension=Float(nbaselines*n_samples)
IF Keyword_Set(double) THEN visibility_array=DComplexarr(n_frequencies,vis_dimension) $
    ELSE visibility_array=Complexarr(n_frequencies,vis_dimension) 

xcen=frequency_array#kx_arr
ycen=frequency_array#ky_arr

conj_i=where(ky_arr GT 0,n_conj)
IF n_conj GT 0 THEN BEGIN
    xcen[*,conj_i]=-xcen[*,conj_i]
    ycen[*,conj_i]=-ycen[*,conj_i]
ENDIF

x_offset=Floor((xcen-Floor(xcen))*psf_resolution) mod psf_resolution    
y_offset=Floor((ycen-Floor(ycen))*psf_resolution) mod psf_resolution 
xmin=Long(Floor(xcen)+dimension/2.-(psf_dim/2.-1))
ymin=Long(Floor(ycen)+elements/2.-(psf_dim/2.-1))
xmax=xmin+psf_dim-1
ymax=ymin+psf_dim-1

range_test_x_i=where((xmin LE 0) OR (xmax GE dimension-1),n_test_x)
range_test_y_i=where((ymin LE 0) OR (ymax GE elements-1),n_test_y)
xmax=(ymax=0)
IF n_test_x GT 0 THEN xmin[range_test_x_i]=(ymin[range_test_x_i]=-1)
IF n_test_y GT 0 THEN xmin[range_test_y_i]=(ymin[range_test_y_i]=-1)

dist_test=Sqrt((xcen)^2.+(ycen)^2.)*kbinsize
flag_dist_i=where((dist_test LT min_baseline) OR (dist_test GT max_baseline),n_dist_flag)
IF n_dist_flag GT 0 THEN BEGIN
    xmin[flag_dist_i]=-1
    ymin[flag_dist_i]=-1
ENDIF

IF Keyword_Set(flag_ptr) THEN BEGIN
    flag_i=where(*flag_ptr LE 0,n_flag)
    IF n_flag GT 0 THEN BEGIN
        xmin[flag_i]=-1
        ymin[flag_i]=-1
    ENDIF
ENDIF

;match all visibilities that map from and to exactly the same pixels
bin_n=histogram(xmin+ymin*dimension,binsize=1,reverse_indices=ri,min=0) ;should miss any (xmin,ymin)=(-1,-1) from flags
bin_i=where(bin_n,n_bin_use);+bin_min

;initialize ONLY those elements of the map_fn array that will receive data
index_arr=Lindgen(dimension,elements)
CASE 1 OF
    Keyword_Set(complex) AND Keyword_Set(double): init_arr=Dcomplexarr(psf_dim2,psf_dim2)
    Keyword_Set(double): init_arr=Dblarr(psf_dim2,psf_dim2)
    Keyword_Set(complex): init_arr=Complexarr(psf_dim2,psf_dim2)
    ELSE: init_arr=Fltarr(psf_dim2,psf_dim2)
ENDCASE
arr_type=Size(init_arr,/type)

time_check_interval=Ceil(n_bin_use/10.)
t1=0
t2=0
t3=0
t4=0
t5=0
t6=0
image_uv_use=image_uv
FOR bi=0L,n_bin_use-1 DO BEGIN
    t1_0=Systime(1)
    ;MUST use double precision!
;    box_arr=Make_array(psf_dim*psf_dim,psf_dim*psf_dim,type=arr_type)
    inds=ri[ri[bin_i[bi]]:ri[bin_i[bi]+1]-1]
    ind0=inds[0]
    
    x_off=x_offset[inds]
    y_off=y_offset[inds]
        
    xmin_use=xmin[ind0] ;should all be the same, but don't want an array
    ymin_use=ymin[ind0] ;should all be the same, but don't want an array

;    bt_i=Floor(inds/n_frequencies)
;    base_i=baseline_i[bt_i]
    freq_i=(inds mod n_frequencies)
    fbin=freq_bin_i[freq_i]
    
    vis_n=bin_n[bin_i[bi]]
    box_matrix=Make_array(vis_n,psf_dim*psf_dim,type=arr_type)
    
    box_arr=Reform(image_uv_use[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1],psf_dim*psf_dim)
    t3_0=Systime(1)
    t2+=t3_0-t1_0
    FOR ii=0L,vis_n-1 DO BEGIN
        box_matrix[ii,*]=*psf_base[polarization,fbin[ii],x_off[ii],y_off[ii]]     
    ENDFOR

    t4_0=Systime(1)
    t3+=t4_0-t3_0
    vis_box=matrix_multiply(box_arr,box_matrix,/btranspose) ;box_matrix#box_arr
    t5_0=Systime(1)
    t4+=t5_0-t4_0
    
    visibility_array[inds]+=vis_box
    
    t5_1=Systime(1)
    t5+=t5_1-t5_0
    t1+=t5_1-t1_0 
ENDFOR
IF n_conj GT 0 THEN BEGIN
    visibility_array[*,conj_i]=Conj(visibility_array[*,conj_i])
ENDIF

IF not Keyword_Set(silent) THEN print,t1,t2,t3,t4,t5
timing=Systime(1)-t0
RETURN,Ptr_new(visibility_array)
END