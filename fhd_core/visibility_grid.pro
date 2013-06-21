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
FUNCTION visibility_grid,visibility_ptr,flag_ptr,obs,psf,params,file_path_fhd,weights=weights,variance=variance,$
    timing=timing,polarization=polarization,mapfn_recalculate=mapfn_recalculate,silent=silent,$
    GPU_enable=GPU_enable,complex=complex,double=double,time_arr=time_arr,fi_use=fi_use,bi_use=bi_use,$
    visibility_list=visibility_list,image_list=image_list,n_vis=n_vis,no_conjugate=no_conjugate,$
    return_mapfn=return_mapfn,mask_mirror_indices=mask_mirror_indices,no_save=no_save,$
    model_ptr=model_ptr,model_return=model_return,preserve_visibilities=preserve_visibilities,$
    phase_threshold=phase_threshold,error=error,_Extra=extra
t0_0=Systime(1)
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

IF Tag_exist(obs,'fbin_i') THEN freq_bin_i=obs.fbin_i ELSE freq_bin_i=(*obs.baseline_info).fbin_i
n_freq=Long(obs.n_freq)
IF N_Elements(fi_use) EQ 0 THEN fi_use=where((*obs.baseline_info).freq_use)
freq_bin_i=freq_bin_i[fi_use]

IF Keyword_Set(flag_ptr) THEN BEGIN
    IF Keyword_Set(preserve_visibilities) THEN flag_arr=*flag_ptr ELSE flag_arr=Temporary(*flag_ptr)
ENDIF

IF N_Elements(bi_use) EQ 0 THEN BEGIN
    IF Keyword_Set(flag_arr) THEN BEGIN
        flag_test=Total(flag_arr>0,1)
        bi_use=where(flag_test)
    ENDIF ELSE BEGIN
        b_info=*(obs.baseline_info)
        tile_use=(b_info.tile_names)[b_info.tile_use]
        
        bi_use=where((b_info.tile_A EQ tile_use) OR (b_info.tile_B EQ tile_use))
    ENDELSE
ENDIF
n_b_use=N_Elements(bi_use)
n_f_use=N_Elements(fi_use)

vis_inds_use=matrix_multiply(fi_use,replicate(1L,n_b_use))+matrix_multiply(replicate(1L,n_f_use),bi_use)*n_freq
IF Keyword_Set(flag_arr) THEN flag_arr=flag_arr[vis_inds_use]
IF Keyword_Set(preserve_visibilities) THEN vis_arr_use=(*visibility_ptr)[vis_inds_use] ELSE vis_arr_use=(Temporary(*visibility_ptr))[vis_inds_use] 
model_flag=0
IF Keyword_Set(model_ptr) THEN BEGIN
    IF Arg_present(model_return) THEN BEGIN
        IF Keyword_Set(preserve_visibilities) THEN model_use=(*model_ptr)[vis_inds_use] $
        ELSE model_use=(Temporary(*model_ptr))[vis_inds_use]
        model_return=Complexarr(dimension,elements)
        model_flag=1
    ENDIF ELSE BEGIN
        IF Keyword_Set(preserve_visibilities) THEN vis_arr_use-=(*model_ptr)[vis_inds_use] $
        ELSE vis_arr_use-=(Temporary(*model_ptr))[vis_inds_use]
    ENDELSE
ENDIF
IF Tag_exist(obs,'freq') THEN frequency_array=obs.freq ELSE frequency_array=(*obs.baseline_info).freq
frequency_array=frequency_array[fi_use]

IF tag_exist(psf,'complex_flag') THEN complex=psf.complex_flag ELSE IF N_Elements(complex) EQ 0 THEN complex=1
psf_base=psf.base
psf_dim=Sqrt((Size(*psf_base[0],/dimension))[0])
psf_resolution=(Size(psf_base,/dimension))[2]

flag_switch=Keyword_Set(flag_ptr)
weights_flag=Keyword_Set(weights)
variance_flag=Keyword_Set(variance)
kx_arr=params.uu[bi_use]/kbinsize
ky_arr=params.vv[bi_use]/kbinsize

n_freq1=N_Elements(frequency_array)
psf_dim2=2*psf_dim

image_uv=Complexarr(dimension,elements)
weights=Complexarr(dimension,elements)
variance=Fltarr(dimension,elements)

IF Keyword_Set(mapfn_recalculate) THEN BEGIN
    map_flag=1
    map_fn=Ptrarr(dimension,elements)
ENDIF ELSE map_flag=0

xcen=frequency_array#kx_arr
ycen=frequency_array#ky_arr

conj_i=where(ky_arr GT 0,n_conj)
IF n_conj GT 0 THEN BEGIN
    xcen[*,conj_i]=-xcen[*,conj_i]
    ycen[*,conj_i]=-ycen[*,conj_i]
    vis_arr_use[*,conj_i]=Conj(vis_arr_use[*,conj_i])
    IF model_flag THEN model_use[*,conj_i]=Conj(model_use[*,conj_i])
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

IF Keyword_Set(flag_arr) THEN BEGIN
    flag_i=where(flag_arr LE 0,n_flag,ncomplement=n_unflag)
    flag_arr=0
    IF n_flag GT 0 THEN BEGIN
        xmin[flag_i]=-1
        ymin[flag_i]=-1
    ENDIF
ENDIF

IF Keyword_Set(phase_threshold) THEN BEGIN
    phase_cut=where(Abs(Atan(vis_arr_use,/phase)) GT phase_threshold,n_phase_cut)
    IF n_phase_cut GT 0 THEN BEGIN
        xmin[phase_cut]=-1
        ymin[phase_cut]=-1
    ENDIF
ENDIF

IF Keyword_Set(mask_mirror_indices) THEN BEGIN
    IF n_conj GT 0 THEN BEGIN
        xmin[*,conj_i]=-1
        ymin[*,conj_i]=-1
    ENDIF
ENDIF

xcen=(ycen=(dist_test=0)) ;free memory

IF max(xmin)<max(ymin) LT 0 THEN BEGIN
    print,'All data flagged or cut! Returning'
    timing=Systime(1)-t0_0
    image_uv=Complexarr(dimension,elements)
    n_vis=0.
    error=1
    RETURN,image_uv
ENDIF

;match all visibilities that map from and to exactly the same pixels
bin_n=histogram(xmin+ymin*dimension,binsize=1,reverse_indices=ri,min=0) ;should miss any (xmin,ymin)=(-1,-1) from flags
bin_i=where(bin_n,n_bin_use)

n_vis=Float(Total(bin_n))

index_arr=Lindgen(dimension,elements)
n_psf_dim=N_Elements(psf_base)
CASE 1 OF
    Keyword_Set(complex) AND Keyword_Set(double): BEGIN
        init_arr=Dcomplexarr(psf_dim2,psf_dim2)
;        FOR i=0.,n_psf_dim-1 DO *psf_base[i]=Dcomplex(*psf_base[i])
    END
    Keyword_Set(double): BEGIN
        init_arr=Dblarr(psf_dim2,psf_dim2)
;        FOR i=0.,n_psf_dim-1 DO *psf_base[i]=Double(Abs(*psf_base[i]))
    END
    Keyword_Set(complex): BEGIN
        init_arr=Complexarr(psf_dim2,psf_dim2)
;        FOR i=0.,n_psf_dim-1 DO *psf_base[i]=Complex(*psf_base[i])
    END
    ELSE: BEGIN
        init_arr=Fltarr(psf_dim2,psf_dim2)
;        FOR i=0.,n_psf_dim-1 DO *psf_base[i]=Float(real_part(*psf_base[i]))
    ENDELSE
ENDCASE
arr_type=Size(init_arr,/type)

;initialize ONLY those elements of the map_fn array that will receive data
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

t0=Systime(1)-t0_0
time_check_interval=Ceil(n_bin_use/10.)
t1=0
t2=0
t3=0
t4=0
t5=0
t6=0
t6a=0
t6b=0
IF map_flag THEN BEGIN
    map_fn_inds=Ptrarr(psf_dim,psf_dim,/allocate)
    psf2_inds=indgen(psf_dim2,psf_dim2)
    FOR i=0,psf_dim-1 DO FOR j=0,psf_dim-1 DO $  
        *map_fn_inds[i,j]=psf2_inds[psf_dim-i:2*psf_dim-i-1,psf_dim-j:2*psf_dim-j-1]
ENDIF
FOR bi=0L,n_bin_use-1 DO BEGIN
    t1_0=Systime(1)
    inds=ri[ri[bin_i[bi]]:ri[bin_i[bi]+1]-1]
    ind0=inds[0]
    
    x_off=x_offset[inds]
    y_off=y_offset[inds]
        
    xmin_use=xmin[ind0] ;should all be the same, but don't want an array
    ymin_use=ymin[ind0] ;should all be the same, but don't want an array

    freq_i=(inds mod n_freq1)
    fbin=freq_bin_i[freq_i]
    
    vis_n=bin_n[bin_i[bi]]
    vis_n_arr=Replicate(1.,vis_n)
    
    vis_box=vis_arr_use[inds]
    box_matrix=Make_array(vis_n,psf_dim*psf_dim,type=arr_type)
    t3_0=Systime(1)
    t2+=t3_0-t1_0
    FOR ii=0L,vis_n-1 DO box_matrix[ii,*]=*psf_base[polarization,fbin[ii],x_off[ii],y_off[ii]]
    IF Keyword_Set(complex) THEN box_matrix_dag=Conj(box_matrix) ELSE box_matrix_dag=box_matrix 
    
    t4_0=Systime(1)
    t3+=t4_0-t3_0   
    IF Keyword_Set(model_flag) THEN BEGIN
        model_box=model_use[inds]
        box_arr=matrix_multiply(Temporary(model_box)/n_vis,box_matrix_dag,/atranspose)
        model_return[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=Temporary(box_arr) 
    ENDIF
    box_arr=matrix_multiply(vis_box/n_vis,box_matrix_dag,/atranspose)
    image_uv[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=Temporary(box_arr) 
;    image_uv[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=Reform((box_arr),psf_dim,psf_dim)
;    image_uv[xmin_use,ymin_use]+=Reform(box_arr,psf_dim,psf_dim)
    t5_0=Systime(1)
    t4+=t5_0-t4_0

    IF weights_flag THEN BEGIN
        wts_box=matrix_multiply(vis_n_arr/n_vis,box_matrix_dag,/atranspose)
        weights[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=Temporary(wts_box)
;        weights[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=Reform((wts_box),psf_dim,psf_dim)
;        weights[xmin_use,ymin_use]+=Reform(Temporary(wts_box),psf_dim,psf_dim)
    ENDIF
    IF variance_flag THEN BEGIN
        var_box=matrix_multiply(vis_n_arr/n_vis,Abs(box_matrix_dag)^2.,/atranspose)
        variance[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=Temporary(var_box)
;        variance[xmin_use,ymin_use]+=Reform(Temporary(var_box),psf_dim,psf_dim)
    ENDIF
    
    t6_0=Systime(1)
    t5+=t6_0-t5_0
    IF map_flag THEN BEGIN
        t6a_0=Systime(1)
        box_arr_map=matrix_multiply(Temporary(box_matrix),Temporary(box_matrix_dag),/atranspose)
        t6b_0=Systime(1)
        t6a+=t6b_0-t6a_0
        FOR i=0,psf_dim-1 DO FOR j=0,psf_dim-1 DO BEGIN
            ij=i+j*psf_dim
;            (*map_fn[xmin_use+i,ymin_use+j])[psf_dim-i:2*psf_dim-i-1,psf_dim-j:2*psf_dim-j-1]+=Reform(box_arr_map[*,ij],psf_dim,psf_dim)
;            (*map_fn[xmin_use+i,ymin_use+j])[psf_dim-i:2*psf_dim-i-1,psf_dim-j:2*psf_dim-j-1]+=box_arr_map[*,ij]
            (*map_fn[xmin_use+i,ymin_use+j])[*map_fn_inds[i,j]]+=box_arr_map[*,ij]
;            (*map_fn[xmin_use+i,ymin_use+j])[psf_dim-i,psf_dim-j]+=Reform(box_arr_map[*,ij],psf_dim,psf_dim)
            dummy_ref=-1
        ENDFOR
        t6b+=Systime(1)-t6b_0
    ENDIF
    t6_1=Systime(1)
    t6+=t6_1-t6_0
    t1+=t6_1-t1_0 
ENDFOR

;free memory
vis_arr_use=0
xmin=(ymin=(ri=(inds=(x_offset=(y_offset=(bin_i=(bin_n=0)))))))

t7_0=Systime(1)
IF map_flag THEN BEGIN
    map_fn=holo_mapfn_convert(map_fn,psf_dim=psf_dim,dimension=dimension,n_vis=n_vis,error=error)
    IF Keyword_Set(error) THEN BEGIN
        print,'All data flagged, cut, or has a null beam model! Returning'
        timing=Systime(1)-t0_0
        image_uv=Complexarr(dimension,elements)
        n_vis=0.
        error=1
        RETURN,image_uv
    ENDIF
    IF ~Keyword_Set(no_save) THEN save,map_fn,filename=file_path_fhd+'_mapfn_'+pol_names[polarization]+'.sav'
    IF Arg_present(return_mapfn) THEN return_mapfn=map_fn
ENDIF
t7=Systime(1)-t7_0

IF ~Keyword_Set(no_conjugate) THEN BEGIN
    image_uv_conj=Shift(Reverse(reverse(Conj(image_uv),1),2),1,1)
    image_uv=(image_uv+image_uv_conj)/2.
    IF weights_flag THEN BEGIN
        weights_conj=Shift(Reverse(reverse(Conj(weights),1),2),1,1)
        weights=(weights+weights_conj)/2.
    ENDIF
    IF variance_flag THEN BEGIN
        variance_mirror=Shift(Reverse(reverse(variance,1),2),1,1)
        variance=(variance+variance_mirror)/2.
    ENDIF
ENDIF
;normalization=dimension*elements
;image_uv*=normalization ;account for FFT convention

IF ~Keyword_Set(silent) THEN print,t0,t1,t2,t3,t4,t5,t6,t7
IF ~Keyword_Set(silent) THEN print,t6a,t6b
time_arr=[t0,t1,t2,t3,t4,t5,t6,t7]
timing=Systime(1)-t0_0
RETURN,image_uv
END