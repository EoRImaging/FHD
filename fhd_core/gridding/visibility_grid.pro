FUNCTION visibility_grid,visibility_ptr,vis_weight_ptr,obs,status_str,psf,params,file_path_fhd=file_path_fhd,weights=weights,variance=variance,$
    timing=timing,polarization=polarization,mapfn_recalculate=mapfn_recalculate,silent=silent,uniform_filter=uniform_filter,$
    GPU_enable=GPU_enable,complex_flag=complex_flag,fi_use=fi_use,bi_use=bi_use,$
    visibility_list=visibility_list,image_list=image_list,n_vis=n_vis,no_conjugate=no_conjugate,$
    return_mapfn=return_mapfn,mask_mirror_indices=mask_mirror_indices,no_save=no_save,$
    model_ptr=model_ptr,model_return=model_return,preserve_visibilities=preserve_visibilities,$
    error=error,grid_uniform=grid_uniform,interpolate_grid_kernel=interpolate_grid_kernel,$
    grid_spectral=grid_spectral,spectral_uv=spectral_uv,spectral_model_uv=spectral_model_uv,_Extra=extra
t0_0=Systime(1)
heap_gc

pol_names=obs.pol_names

;extract information from the structures
dimension=Float(obs.dimension)
elements=Float(obs.elements)
kbinsize=obs.kpix
kx_span=kbinsize*dimension ;Units are # of wavelengths
ky_span=kx_span
min_baseline=obs.min_baseline
max_baseline=obs.max_baseline
double_precision=0
IF Tag_Exist(obs, 'double_precision') THEN double_precision=obs.double_precision
IF N_Elements(silent) EQ 0 THEN verbose=0 ELSE verbose=0>Round(1-silent)<1
IF Tag_exist(psf,'interpolate_kernel') THEN interp_flag=psf.interpolate_kernel ELSE interp_flag=0

IF Tag_exist(obs,'alpha') THEN alpha=obs.alpha ELSE alpha=0.
freq_bin_i=(*obs.baseline_info).fbin_i
n_freq=Long(obs.n_freq)
IF N_Elements(fi_use) EQ 0 THEN fi_use=where((*obs.baseline_info).freq_use)
freq_bin_i=freq_bin_i[fi_use]
n_vis_arr=obs.nf_vis

vis_weight_switch=Ptr_valid(vis_weight_ptr)
IF vis_weight_switch THEN BEGIN
    IF Keyword_Set(preserve_visibilities) THEN vis_weights=*vis_weight_ptr ELSE BEGIN
        vis_weights=Temporary(*vis_weight_ptr)
        Ptr_free,vis_weight_ptr
    ENDELSE
ENDIF

IF N_Elements(bi_use) EQ 0 THEN BEGIN
    IF vis_weight_switch THEN BEGIN
        flag_test=Total(vis_weights>0,1)
        bi_use=where((flag_test GT 0))
    ENDIF ELSE BEGIN
        b_info=(*obs.baseline_info)
        tile_use=where(b_info.tile_use)+1
        bi_use=array_match(b_info.tile_A,b_info.tile_B,value_match=tile_use)
    ENDELSE
ENDIF
n_b_use=N_Elements(bi_use)
n_f_use=N_Elements(fi_use)

vis_inds_use=matrix_multiply(fi_use,replicate(1L,n_b_use))+matrix_multiply(replicate(1L,n_f_use),bi_use)*n_freq
IF vis_weight_switch THEN vis_weights=vis_weights[vis_inds_use]
IF Keyword_Set(preserve_visibilities) THEN vis_arr_use=(*visibility_ptr)[vis_inds_use] ELSE BEGIN
    vis_arr_use=(Temporary(*visibility_ptr))[vis_inds_use] 
    Ptr_free,visibility_ptr
ENDELSE
model_flag=0
IF Ptr_valid(model_ptr) THEN BEGIN
    IF Keyword_Set(model_return) THEN BEGIN
        IF Keyword_Set(preserve_visibilities) THEN model_use=(*model_ptr)[vis_inds_use] ELSE BEGIN
            model_use=(Temporary(*model_ptr))[vis_inds_use]
            ptr_free,model_ptr
        ENDELSE
        IF Keyword_Set(double_precision) THEN model_return=DComplexarr(dimension,elements) ELSE model_return=Complexarr(dimension,elements)
        model_flag=1
    ENDIF ELSE BEGIN
;        IF Keyword_Set(preserve_visibilities) THEN vis_arr_use-=(*model_ptr)[vis_inds_use] $
;            ELSE vis_arr_use-=(Temporary(*model_ptr))[vis_inds_use]
    ENDELSE
ENDIF
frequency_array=(*obs.baseline_info).freq
frequency_array=frequency_array[fi_use]

complex_flag=psf.complex_flag
psf_dim=psf.dim
psf_resolution=psf.resolution
group_arr=reform(psf.id[polarization,freq_bin_i[fi_use],bi_use])
beam_arr=*psf.beam_ptr

weights_flag=Keyword_Set(weights)
variance_flag=Keyword_Set(variance)
uniform_flag=Keyword_Set(uniform_filter)
kx_arr=params.uu[bi_use]/kbinsize
ky_arr=params.vv[bi_use]/kbinsize

nbaselines=obs.nbaselines
n_samples=obs.n_time
n_freq_use=N_Elements(frequency_array)
psf_dim2=2*psf_dim
psf_dim3=psf_dim*psf_dim
bi_use_reduced=bi_use mod nbaselines

IF Keyword_Set(double_precision) THEN image_uv=DComplexarr(dimension,elements) ELSE image_uv=Complexarr(dimension,elements)
IF Keyword_Set(double_precision) THEN weights=DComplexarr(dimension,elements) ELSE weights=Complexarr(dimension,elements)
IF Keyword_Set(double_precision) THEN variance=Dblarr(dimension,elements) ELSE variance=Fltarr(dimension,elements)
IF Keyword_Set(double_precision) THEN uniform_filter=Dblarr(dimension,elements) ELSE uniform_filter=Fltarr(dimension,elements)
IF Keyword_Set(grid_uniform) THEN BEGIN
    mapfn_recalculate=0 ;mapfn is incompatible with uniformly gridded images!
    uniform_flag=1
ENDIF

IF Keyword_Set(mapfn_recalculate) THEN BEGIN
    map_flag=1
    map_fn=Ptrarr(dimension,elements)
ENDIF ELSE map_flag=0

dist_test=Sqrt((kx_arr)^2.+(ky_arr)^2.)*kbinsize
dist_test=Float(frequency_array#dist_test)
flag_dist_i=where((dist_test LT min_baseline) OR (dist_test GT max_baseline),n_dist_flag)
dist_test=0

conj_i=where(ky_arr GT 0,n_conj)
conj_flag=intarr(N_Elements(ky_arr)) 
IF n_conj GT 0 THEN BEGIN
    conj_flag[conj_i]=1
    kx_arr[conj_i]=-kx_arr[conj_i]
    ky_arr[conj_i]=-ky_arr[conj_i]
    vis_arr_use[*,conj_i]=Conj(vis_arr_use[*,conj_i])
    IF model_flag THEN model_use[*,conj_i]=Conj(model_use[*,conj_i])
ENDIF
xcen=Float(frequency_array#Temporary(kx_arr))
ycen=Float(frequency_array#Temporary(ky_arr))
 
x_offset=Fix(Floor((xcen-Floor(xcen))*psf_resolution) mod psf_resolution, type=12) ; type=12 is unsigned int
y_offset=Fix(Floor((ycen-Floor(ycen))*psf_resolution) mod psf_resolution, type=12) ; type=12 is unsigned int
dx_arr = (xcen-Floor(xcen))*psf_resolution - Floor((xcen-Floor(xcen))*psf_resolution)
dy_arr = (ycen-Floor(ycen))*psf_resolution - Floor((ycen-Floor(ycen))*psf_resolution)
dx0dy0_arr = (1-dx_arr)*(1-dy_arr)
dx0dy1_arr = (1-dx_arr)*dy_arr
dx1dy0_arr = dx_arr*(1-dy_arr)
dx1dy1_arr = Temporary(dx_arr) * Temporary(dy_arr)
xmin=Long(Floor(Temporary(xcen))+dimension/2.-(psf_dim/2.-1))
ymin=Long(Floor(Temporary(ycen))+elements/2.-(psf_dim/2.-1))

range_test_x_i=where((xmin LE 0) OR ((xmin+psf_dim-1) GE dimension-1),n_test_x)
range_test_y_i=where((ymin LE 0) OR ((ymin+psf_dim-1) GE elements-1),n_test_y)

IF n_test_x GT 0 THEN xmin[range_test_x_i]=(ymin[range_test_x_i]=-1)
IF n_test_y GT 0 THEN xmin[range_test_y_i]=(ymin[range_test_y_i]=-1)

IF n_dist_flag GT 0 THEN BEGIN
    xmin[flag_dist_i]=-1
    ymin[flag_dist_i]=-1
ENDIF

IF vis_weight_switch THEN BEGIN
    flag_i=where(vis_weights LE 0,n_flag)
    vis_weights=0
    IF n_flag GT 0 THEN BEGIN
        xmin[flag_i]=-1
        ymin[flag_i]=-1
    ENDIF
    flag_i=0
ENDIF

IF Keyword_Set(mask_mirror_indices) THEN BEGIN
    IF n_conj GT 0 THEN BEGIN
        xmin[*,conj_i]=-1
        ymin[*,conj_i]=-1
    ENDIF
ENDIF

IF max(xmin)<max(ymin) LT 0 THEN BEGIN
    print,'All data flagged or cut! Returning'
    timing=Systime(1)-t0_0
    image_uv=Complexarr(dimension,elements)
    n_vis=0.
    error=1
    RETURN,image_uv
ENDIF

;match all visibilities that map from and to exactly the same pixels
bin_n=histogram(xmin+ymin*dimension,binsize=1,reverse_indices=ri,min=0) ;should miss any (xmin,ymin)=(-1,-1) from weights
bin_i=where(bin_n,n_bin_use)

ind_ref=indgen(max(bin_n))
n_vis=Float(Total(bin_n))
FOR fi=0L,n_f_use-1 DO n_vis_arr[fi_use[fi]]=Total(Long(xmin[fi,*] GT 0))
obs.nf_vis=n_vis_arr

index_arr=Lindgen(dimension,elements)
n_psf_dim=N_Elements(psf_base)
CASE 1 OF
    complex_flag AND Keyword_Set(double_precision): BEGIN
        init_arr=Dcomplexarr(psf_dim2,psf_dim2)
    END
    Keyword_Set(double_precision): BEGIN
        init_arr=Dblarr(psf_dim2,psf_dim2)
    END
    complex_flag: BEGIN
        init_arr=Complexarr(psf_dim2,psf_dim2)
    END
    ELSE: BEGIN
        init_arr=Fltarr(psf_dim2,psf_dim2)
    ENDELSE
ENDCASE
arr_type=Size(init_arr,/type)
IF Keyword_Set(grid_spectral) THEN BEGIN
    spectral_A=Complexarr(dimension,elements)
    spectral_B=Fltarr(dimension,elements)
    spectral_D=Fltarr(dimension,elements)
    IF model_flag THEN BEGIN
        spectral_model_A=Complexarr(dimension,elements)
    ENDIF
ENDIF

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
tspec=0
IF map_flag THEN BEGIN
    map_fn_inds=Ptrarr(psf_dim,psf_dim,/allocate)
    psf2_inds=indgen(psf_dim2,psf_dim2)
    FOR i=0,psf_dim-1 DO FOR j=0,psf_dim-1 DO $  
        *map_fn_inds[i,j]=psf2_inds[psf_dim-i:2*psf_dim-i-1,psf_dim-j:2*psf_dim-j-1]
ENDIF

;pdim=size(psf_base,/dimension)
;psf_base_dag=Ptrarr(pdim,/allocate)
;FOR pdim_i=0L,Product(pdim)-1 DO *psf_base_dag[pdim_i]=Conj(*psf_base[pdim_i])

FOR bi=0L,n_bin_use-1 DO BEGIN
    IF verbose THEN t1_0=Systime(1)
    inds=ri[ri[bin_i[bi]]:ri[bin_i[bi]+1]-1]
    ind0=inds[0]
    
    x_off=x_offset[inds]
    y_off=y_offset[inds]
    dx1dy1 = dx1dy1_arr[inds]
    dx1dy0 = dx1dy0_arr[inds]
    dx0dy1 = dx0dy1_arr[inds]
    dx0dy0 = dx0dy0_arr[inds]
        
    xmin_use=xmin[ind0] ;should all be the same, but don't want an array
    ymin_use=ymin[ind0] ;should all be the same, but don't want an array

    freq_i=(inds mod n_freq_use)
    fbin=freq_bin_i[freq_i]
    
    vis_n=bin_n[bin_i[bi]]
    baseline_inds=bi_use_reduced[Floor(inds/n_f_use) mod nbaselines]
    group_id=group_arr[inds]
    group_max=Max(group_id)+1
    
;    IF Keyword_Set(grid_spectral) THEN n_xyf_bin=vis_n
    IF interp_flag THEN n_xyf_bin=vis_n ELSE BEGIN
        xyf_i=(x_off+y_off*psf_resolution+fbin*psf_resolution^2.)*group_max+group_id
        
        xyf_si=Sort(xyf_i)
        xyf_i=xyf_i[xyf_si]
        xyf_ui=[Uniq(xyf_i)]
        n_xyf_bin=N_Elements(xyf_ui)
    ENDELSE
    
    IF vis_n GT 1.1*n_xyf_bin THEN BEGIN ;there might be a better selection criteria to determine which is most efficient
        rep_flag=1
        inds=inds[xyf_si]
        inds_use=xyf_si[xyf_ui]
        freq_i=freq_i[inds_use]
        
        x_off=x_off[inds_use] 
        y_off=y_off[inds_use]
        fbin=fbin[inds_use]
        baseline_inds=baseline_inds[inds_use]
;        psf_conj_flag=psf_conj_flag[inds_use]
        IF n_xyf_bin GT 1 THEN xyf_ui0=[0,xyf_ui[0:n_xyf_bin-2]+1] ELSE xyf_ui0=0
        psf_weight=xyf_ui-xyf_ui0+1
         
        vis_box1=vis_arr_use[inds]
        vis_box=vis_box1[xyf_ui]
        IF model_flag THEN BEGIN
            model_box1=model_use[inds]
            model_box=model_box1[xyf_ui]
        ENDIF
        
        repeat_i=where(psf_weight GT 1,n_rep,complement=single_i,ncom=n_single)
        
        xyf_ui=xyf_ui[repeat_i]
        xyf_ui0=xyf_ui0[repeat_i]
        FOR rep_ii=0,n_rep-1 DO $
            vis_box[repeat_i[rep_ii]]=Total(vis_box1[xyf_ui0[rep_ii]:xyf_ui[rep_ii]])
        
        IF model_flag THEN FOR rep_ii=0,n_rep-1 DO $
            model_box[repeat_i[rep_ii]]=Total(model_box1[xyf_ui0[rep_ii]:xyf_ui[rep_ii]])
        
        vis_n=n_xyf_bin
    ENDIF ELSE BEGIN
        rep_flag=0
        IF model_flag THEN model_box=model_use[inds]
        vis_box=vis_arr_use[inds]
        psf_weight=Replicate(1.,vis_n)
    ENDELSE
    
    box_matrix=Make_array(psf_dim3,vis_n,type=arr_type)
    IF verbose THEN BEGIN
        t3_0=Systime(1)
        t2+=t3_0-t1_0
    ENDIF
    
    IF interp_flag THEN $
        FOR ii=0L,vis_n-1 DO box_matrix[psf_dim3*ii]=$
            interpolate_kernel(*beam_arr[polarization,fbin[ii],baseline_inds[ii]],x_offset=x_off[ii], y_offset=y_off[ii], dx0dy0=dx0dy0[ii], dx1dy0=dx1dy0[ii], dx0dy1=dx0dy1[ii], dx1dy1=dx1dy1[ii]) $
        ELSE FOR ii=0L,vis_n-1 DO box_matrix[psf_dim3*ii]=*(*beam_arr[polarization,fbin[ii],baseline_inds[ii]])[x_off[ii],y_off[ii]] ;more efficient array subscript notation
;;    FOR ii=0L,vis_n-1 DO box_matrix[psf_dim3*ii]=*psf_base[polarization,fbin[ii],x_off[ii],y_off[ii]]
;    FOR ii=0L,vis_n-1 DO box_matrix[psf_dim3*ii]=*(*beam_arr[polarization,fbin[ii],baseline_inds[ii]])[x_off[ii],y_off[ii]] ;more efficient array subscript notation
;;    FOR ii=0L,vis_n-1 DO box_matrix[psf_dim3*ii]=psf_conj_flag[ii] ? $
;;        *psf_base_dag[polarization,fbin[ii],x_off[ii],y_off[ii]]:*psf_base[polarization,fbin[ii],x_off[ii],y_off[ii]]
    
    IF map_flag THEN BEGIN
        IF complex_flag THEN box_matrix_dag=Conj(box_matrix) ELSE box_matrix_dag=real_part(box_matrix) 
        IF rep_flag THEN box_matrix*=Rebin(Transpose(psf_weight),psf_dim3,vis_n)
    ENDIF ELSE BEGIN
        IF complex_flag THEN box_matrix_dag=Conj(Temporary(box_matrix)) ELSE box_matrix_dag=Real_part(Temporary(box_matrix))
    ENDELSE
    IF verbose THEN BEGIN
        t4_0=Systime(1)
        t3+=t4_0-t3_0   
    ENDIF
    
    IF Keyword_Set(grid_spectral) THEN BEGIN
        tspec0=Systime(1)
        ;slope = (sum(A) - N*sum(B)*sum(C)) / (sum(D) -N*sum(B)^2.)
        ;sum(C) is ordinary gridded visibilities, so is not calculated here
        term_A_box=matrix_multiply(freq_i*vis_box/n_vis,box_matrix_dag,/atranspose,/btranspose)
        term_B_box=matrix_multiply(freq_i/n_vis,box_matrix_dag,/atranspose,/btranspose) 
        term_D_box=matrix_multiply(freq_i^2./n_vis,box_matrix_dag,/atranspose,/btranspose) 
        
        spectral_A[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=Temporary(term_A_box) 
        spectral_B[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=Temporary(term_B_box)
        spectral_D[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=Temporary(term_D_box)
        IF model_flag THEN BEGIN
            term_Am_box=matrix_multiply(freq_i*model_box/n_vis,box_matrix_dag,/atranspose,/btranspose)
            spectral_model_A[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=Temporary(term_Am_box)
        ENDIF
        tspec+=Systime(1)-tspec0
    ENDIF
    IF model_flag THEN BEGIN
        box_arr=matrix_multiply(Temporary(model_box)/n_vis,box_matrix_dag,/atranspose,/btranspose)
        model_return[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=Temporary(box_arr) 
    ENDIF
    box_arr=matrix_multiply(Temporary(vis_box)/n_vis,box_matrix_dag,/atranspose,/btranspose)
    image_uv[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=Temporary(box_arr) 
    
    IF verbose THEN BEGIN
        t5_0=Systime(1)
        t4+=t5_0-t4_0
    ENDIF
    
    IF weights_flag THEN BEGIN
        wts_box=matrix_multiply(psf_weight/n_vis,box_matrix_dag,/atranspose,/btranspose)
        weights[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=Temporary(wts_box)
    ENDIF
    IF variance_flag THEN BEGIN
        var_box=matrix_multiply(psf_weight/n_vis,Abs(box_matrix_dag)^2.,/atranspose,/btranspose)
        variance[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=Temporary(var_box)
    ENDIF
    IF uniform_flag THEN uniform_filter[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=bin_n[bin_i[bi]]
    
    IF verbose THEN BEGIN
        t6_0=Systime(1)
        t5+=t6_0-t5_0
    ENDIF
    IF map_flag THEN BEGIN
        box_arr_map=matrix_multiply(Temporary(box_matrix),Temporary(box_matrix_dag),/btranspose,TPOOL_MIN_ELTS=20000.)
        FOR i=0,psf_dim-1 DO FOR j=0,psf_dim-1 DO BEGIN
            ij=i+j*psf_dim
            (*map_fn[xmin_use+i,ymin_use+j])[*map_fn_inds[i,j]]+=box_arr_map[*,ij]
            dummy_ref=-1
        ENDFOR
    ENDIF
    IF verbose THEN BEGIN
        t6_1=Systime(1)
        t6+=t6_1-t6_0
        t1+=t6_1-t1_0 
    ENDIF
ENDFOR

;free memory
vis_arr_use=(model_use=0)
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
    fhd_save_io,status_str,map_fn,var='map_fn',file_path_fhd=file_path_fhd,pol_i=polarization,no_save=no_save,obs=obs,_Extra=extra
    IF Keyword_Set(return_mapfn) THEN return_mapfn=map_fn ELSE undefine_fhd,map_fn
ENDIF
t7=Systime(1)-t7_0

IF Keyword_Set(grid_spectral) THEN BEGIN
    spectral_uv=(spectral_A-n_vis*spectral_B*image_uv)*weight_invert(spectral_D-spectral_B^2.)
    IF model_flag THEN spectral_model_uv=(spectral_model_A-n_vis*spectral_B*model_return)*weight_invert(spectral_D-spectral_B^2.)
    IF ~Keyword_Set(no_conjugate) THEN BEGIN
        spectral_uv=(spectral_uv+conjugate_mirror(spectral_uv))/2.
        IF model_flag THEN spectral_model_uv=(spectral_model_uv+conjugate_mirror(spectral_model_uv))/2.
    ENDIF
ENDIF

IF Keyword_Set(grid_uniform) THEN BEGIN
    filter_use=weight_invert(uniform_filter,1.) 
    wts_i=where(filter_use,n_wts)
    IF n_wts GT 0 THEN filter_use/=Mean(filter_use[wts_i]) ELSE filter_use/=Mean(filter_use)
    image_uv*=weight_invert(filter_use)
    IF weights_flag THEN weights*=weight_invert(filter_use)
    IF variance_flag THEN variance*=weight_invert(filter_use)
    IF model_flag THEN model_return*=weight_invert(filter_use)
ENDIF

IF ~Keyword_Set(no_conjugate) THEN BEGIN
    image_uv=(image_uv+conjugate_mirror(image_uv))/2.
    IF weights_flag THEN weights=(weights+conjugate_mirror(weights))/2.        
    IF variance_flag THEN variance=(variance+conjugate_mirror(variance))/4. ;2?
    IF model_flag THEN model_return=(model_return+conjugate_mirror(model_return))/2.
    IF uniform_flag THEN uniform_filter=(uniform_filter+conjugate_mirror(uniform_filter))/2.
ENDIF

IF verbose THEN print,t0,t1,t2,t3,t4,t5,t6,t7,tspec
timing=Systime(1)-t0_0
RETURN,image_uv
END