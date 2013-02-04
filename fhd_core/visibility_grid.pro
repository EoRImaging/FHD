;+
; :Description:
;    Grids visibilities to the uv plane using a high-resolution beam model. Written using the efficient notation of holo_mapfn_generate.pro
;
; :Params:
;    visibility_array - single polarization visibility data
;    
;    flag_arr - 
;    
;    obs - structure containing details of the observation
;    
;    params - structure containing u and v coordinates of the baselines, in meters/c.
;    
;    psf - structure containing the high-resolution gridded beam model.
;
; :Keywords:
;    weights - if set, contains the weights generated from gridding visibilities with value 1
;    
;    timing
;    
;    polarization - polarization index used to pick the correct beam
;
; :Author: Ian Sullivan
;-
FUNCTION visibility_grid,visibility_array,flag_arr,obs,psf,params,file_path_fhd,weights=weights,$
    timing=timing,polarization=polarization,mapfn_recalculate=mapfn_recalculate,silent=silent,$
    GPU_enable=GPU_enable,complex=complex,double=double,_Extra=extra
t0=Systime(1)
heap_gc

pol_names=['xx','yy','xy','yx']

;extract information from the structures
dimension=Float(obs.dimension)
elements=Float(obs.elements)
kbinsize=obs.kpix
kx_span=kbinsize*dimension ;Units are # of wavelengths
ky_span=kx_span
min_baseline=obs.min_baseline
max_baseline=obs.max_baseline

freq_bin_i=obs.fbin_i
nfreq_bin=Max(freq_bin_i)+1
bin_offset=(*obs.baseline_info).bin_offset
frequency_array=obs.freq

psf_base=psf.base
psf_dim=(Size(*psf_base[0],/dimension))[0]
psf_resolution=(Size(psf_base,/dimension))[2]

flag_switch=Keyword_Set(flag_arr)
kx_arr=params.uu/kbinsize
ky_arr=params.vv/kbinsize
baseline_i=params.baseline_arr
nbaselines=bin_offset[1]
n_samples=N_Elements(bin_offset)
n_frequencies=N_Elements(frequency_array)
n_freq_bin=N_Elements(freq_bin_i)
psf_dim2=2*psf_dim

vis_dimension=Float(nbaselines*n_samples)

image_uv=Complexarr(dimension,elements)
weights=fltarr(dimension,elements)

IF Keyword_Set(mapfn_recalculate) THEN BEGIN
    map_flag=1
    map_fn=Ptrarr(dimension,elements)
ENDIF ELSE map_flag=0

xcen=frequency_array#kx_arr
ycen=frequency_array#ky_arr
x_offset=Round((Ceil(xcen)-xcen)*psf_resolution) mod psf_resolution    
y_offset=Round((Ceil(ycen)-ycen)*psf_resolution) mod psf_resolution
xmin=Floor(Round(xcen+x_offset/psf_resolution+dimension/2.)-psf_dim/2.) 
ymin=Floor(Round(ycen+y_offset/psf_resolution+elements/2.)-psf_dim/2.) 
xmax=xmin+psf_dim-1
ymax=ymin+psf_dim-1

range_test_x_i=where((xmin LT 0) OR (xmax GE dimension),n_test_x)
range_test_y_i=where((ymin LT 0) OR (ymax GE elements),n_test_y)
IF n_test_x GT 0 THEN xmin[range_test_x_i]=(ymin[range_test_x_i]=-1)
IF n_test_y GT 0 THEN xmin[range_test_y_i]=(ymin[range_test_y_i]=-1)

;dist_test=Sqrt((xcen-dimension/2.)^2.+(ycen-elements/2.)^2.)
dist_test=Sqrt((xcen)^2.+(ycen)^2.)*kbinsize
flag_dist_i=where((dist_test LT min_baseline) OR (dist_test GT max_baseline),n_dist_flag)
IF n_dist_flag GT 0 THEN BEGIN
    xmin[flag_dist_i]=-1
    ymin[flag_dist_i]=-1
ENDIF

IF Keyword_Set(flag_arr) THEN BEGIN
    flag_i=where(flag_arr LE 0,n_flag)
    IF n_flag GT 0 THEN BEGIN
        xmin[flag_i]=-1
        ymin[flag_i]=-1
    ENDIF
ENDIF

;match all visibilities that map from and to exactly the same pixels
bin_n=histogram(xmin+ymin*dimension,binsize=1,reverse_indices=ri,min=0) ;should miss any (xmin,ymin)=(-1,-1) from flags
bin_i=where(bin_n,n_bin_use);+bin_min

vis_density=Float(Total(bin_n))/(dimension*elements)

;initialize ONLY those elements of the map_fn array that will receive data
index_arr=Lindgen(dimension,elements)
CASE 1 OF
    Keyword_Set(complex) AND Keyword_Set(double): init_arr=Dcomplexarr(psf_dim2,psf_dim2)
    Keyword_Set(double): init_arr=Dblarr(psf_dim2,psf_dim2)
    Keyword_Set(complex): init_arr=Complexarr(psf_dim2,psf_dim2)
    ELSE: init_arr=Fltarr(psf_dim2,psf_dim2)
ENDCASE
arr_type=Size(init_arr,/type)

IF map_flag THEN BEGIN
    FOR bi=0L,n_bin_use-1 DO BEGIN
        xmin1=xmin[ri[ri[bin_i[bi]]]]
        ymin1=ymin[ri[ri[bin_i[bi]]]]
        ptr_test=Ptr_valid(map_fn[xmin1:xmin1+psf_dim-1,ymin1:ymin1+psf_dim-1])
        inds_init=where(ptr_test EQ 0,nzero)
        IF nzero EQ 0 THEN CONTINUE
        inds_init=(index_arr[xmin1:xmin1+psf_dim-1,ymin1:ymin1+psf_dim-1])[inds_init]
        FOR ii=0,nzero-1 DO map_fn[inds_init[ii]]=Ptr_new(init_arr)
    ENDFOR
ENDIF

time_check_interval=Ceil(n_bin_use/10.)
t1=0
t2=0
t3=0
t4=0
t5=0
t6=0
FOR bi=0L,n_bin_use-1 DO BEGIN
    t1_0=Systime(1)
;    ;MUST use double precision!
;    box_arr=Make_array(psf_dim*psf_dim,psf_dim*psf_dim,type=arr_type)
    inds=ri[ri[bin_i[bi]]:ri[bin_i[bi]+1]-1]
    
    x_off1=x_offset[inds]
    y_off1=y_offset[inds]
    vis_box=visibility_array[inds]
        
    xmin_use=Min(xmin[inds]) ;should all be the same, but don't want an array
    ymin_use=Min(ymin[inds]) ;should all be the same, but don't want an array

    bt_i=Floor(inds/n_frequencies)
    base_i=baseline_i[bt_i]
    freq_i=(inds mod n_frequencies)
    fbin=freq_bin_i[freq_i]
    
    ;match all visibilities that use exactly the same beam model
;    bl_fr_i=fbin+base_i*nfreq_bin
;    bl_fr_n=histogram(bl_fr_i,binsize=1,reverse_indices=rbfi,omin=bl_fr_min)
;    bl_fr_ii=where(bl_fr_n,n_bl_fr_ii)
    
    vis_n=bin_n[bin_i[bi]]
    box_matrix=Make_array(vis_n,psf_dim*psf_dim,type=arr_type)
    
    t3_0=Systime(1)
    t2+=t3_0-t1_0
    FOR ii=0L,vis_n-1 DO BEGIN
        psf_use=*psf_base[polarization,fbin[ii],x_off1[ii],y_off1[ii]]
;        psf_use=Abs(*psf_base[polarization,fbin[ii],x_off1[ii],y_off1[ii]]) ;temporary addition while I transition to complex beams!
        box_matrix[ii,*]=Reform(psf_use,psf_dim*psf_dim,/overwrite)        
    ENDFOR

    t4_0=Systime(1)
    t3+=t4_0-t3_0
    box_arr=vis_box#box_matrix/vis_density
    IF Arg_present(weights) THEN weights[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=$
        Replicate(1./vis_density,vis_n)#box_matrix
    t5_0=Systime(1)
    t4+=t5_0-t4_0
    
    image_uv[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=box_arr
    
    t6_0=Systime(1)
    t5+=t6_0-t5_0
    IF map_flag THEN BEGIN
        box_arr_map=matrix_multiply(box_matrix,box_matrix,/atranspose)/vis_density
;        IF Arg_present(weights) THEN weights[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=$
;            matrix_multiply(Replicate(2.,psf_dim*psf_dim),Abs(box_arr_map))
;        IF map_flag THEN BEGIN
            FOR i=0,psf_dim-1 DO FOR j=0,psf_dim-1 DO BEGIN
                ij=i+j*psf_dim
                (*map_fn[xmin_use+i,ymin_use+j])[psf_dim-i:2*psf_dim-i-1,psf_dim-j:2*psf_dim-j-1]+=Reform(box_arr_map[*,ij],psf_dim,psf_dim)
            ENDFOR
;        ENDIF
    ENDIF
    t6_1=Systime(1)
    t6+=t6_1-t6_0
    t1+=t6_1-t1_0 
ENDFOR

IF map_flag THEN BEGIN
    map_fn=holo_mapfn_convert(map_fn,psf_dim=psf_dim,dimension=dimension)
    save,map_fn,filename=file_path_fhd+'_mapfn_'+pol_names[polarization]+'.sav'
ENDIF

image_uv_conj=Shift(Reverse(reverse(Conj(image_uv),1),2),1,1)
image_uv=(image_uv+image_uv_conj)/2.

IF Arg_present(weights) THEN BEGIN
    weights=(weights+Shift(Reverse(reverse(weights,1),2),1,1));/2.
ENDIF
;normalization=dimension*elements
;image_uv*=normalization ;account for FFT convention

IF not Keyword_Set(silent) THEN print,t1,t2,t3,t4,t5,t6
timing=Systime(1)-t0
RETURN,image_uv
END