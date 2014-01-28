FUNCTION visibility_grid,visibility_ptr,flag_ptr,obs,psf,params,file_path_fhd,weights=weights,variance=variance,$
    timing=timing,polarization=polarization,mapfn_recalculate=mapfn_recalculate,silent=silent,$
    GPU_enable=GPU_enable,complex_flag=complex_flag,double=double,fi_use=fi_use,bi_use=bi_use,$
    visibility_list=visibility_list,image_list=image_list,n_vis=n_vis,no_conjugate=no_conjugate,$
    return_mapfn=return_mapfn,mask_mirror_indices=mask_mirror_indices,no_save=no_save,$
    model_ptr=model_ptr,model_return=model_return,preserve_visibilities=preserve_visibilities,$
    phase_threshold=phase_threshold,grid_uniform_weight=grid_uniform_weight,error=error,_Extra=extra
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
IF N_Elements(silent) EQ 0 THEN verbose=0 ELSE verbose=0>Round(1-silent)<1

IF Tag_exist(obs,'alpha') THEN alpha=obs.alpha ELSE alpha=0.
IF Tag_exist(obs,'fbin_i') THEN freq_bin_i=obs.fbin_i ELSE freq_bin_i=(*obs.baseline_info).fbin_i
n_freq=Long(obs.n_freq)
IF N_Elements(fi_use) EQ 0 THEN fi_use=where((*obs.baseline_info).freq_use)
freq_bin_i=freq_bin_i[fi_use]
IF Tag_exist(obs,'nf_vis') THEN n_vis_arr=obs.nf_vis ELSE n_vis_arr=Lonarr(n_freq)

IF Keyword_Set(flag_ptr) THEN BEGIN
    IF Keyword_Set(preserve_visibilities) THEN flag_arr=*flag_ptr ELSE flag_arr=Temporary(*flag_ptr)
ENDIF

IF N_Elements(bi_use) EQ 0 THEN BEGIN
    IF Keyword_Set(flag_ptr) THEN BEGIN
        flag_test=Total(flag_arr>0,1)
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
IF Keyword_Set(flag_ptr) THEN flag_arr=flag_arr[vis_inds_use]
IF Keyword_Set(preserve_visibilities) THEN vis_arr_use=(*visibility_ptr)[vis_inds_use] ELSE vis_arr_use=(Temporary(*visibility_ptr))[vis_inds_use] 
model_flag=0
IF Ptr_valid(model_ptr) THEN BEGIN
    IF Keyword_Set(model_return) THEN BEGIN
        IF Keyword_Set(preserve_visibilities) THEN model_use=(*model_ptr)[vis_inds_use] $
            ELSE model_use=(Temporary(*model_ptr))[vis_inds_use]
        model_return=Complexarr(dimension,elements)
        model_flag=1
    ENDIF ELSE BEGIN
;        IF Keyword_Set(preserve_visibilities) THEN vis_arr_use-=(*model_ptr)[vis_inds_use] $
;            ELSE vis_arr_use-=(Temporary(*model_ptr))[vis_inds_use]
    ENDELSE
ENDIF
IF Tag_exist(obs,'freq') THEN frequency_array=obs.freq ELSE frequency_array=(*obs.baseline_info).freq
freq_norm=frequency_array^(-alpha)
;freq_norm/=Sqrt(Mean(freq_norm^2.))
freq_norm/=Mean(freq_norm) 
freq_norm=Float(freq_norm[fi_use])
frequency_array=frequency_array[fi_use]

IF tag_exist(psf,'complex_flag') THEN complex_flag=psf.complex_flag ELSE IF N_Elements(complex_flag) EQ 0 THEN complex_flag=1
psf_base=psf.base
psf_dim=psf.dim
psf_resolution=psf.resolution

flag_switch=Keyword_Set(flag_ptr)
weights_flag=Keyword_Set(weights)
variance_flag=Keyword_Set(variance)
kx_arr=params.uu[bi_use]/kbinsize
ky_arr=params.vv[bi_use]/kbinsize

n_freq1=N_Elements(frequency_array)
psf_dim2=2*psf_dim
psf_dim3=psf_dim*psf_dim

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
ky_arr=(kx_arr=0)
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

IF Keyword_Set(flag_ptr) THEN BEGIN
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

ind_ref=indgen(max(bin_n))
n_vis=Float(Total(bin_n))
FOR fi=0L,n_f_use-1 DO n_vis_arr[fi_use[fi]]=Total(Long(xmin[fi_use[fi],*] GT 0))
IF Tag_exist(obs,'nf_vis') THEN obs.nf_vis=n_vis_arr

;IF Keyword_Set(grid_uniform_weight) THEN n_vis=n_bin_use

index_arr=Lindgen(dimension,elements)
n_psf_dim=N_Elements(psf_base)
CASE 1 OF
    complex_flag AND Keyword_Set(double): BEGIN
        init_arr=Dcomplexarr(psf_dim2,psf_dim2)
;        FOR i=0.,n_psf_dim-1 DO *psf_base[i]=Dcomplex(*psf_base[i])
    END
    Keyword_Set(double): BEGIN
        init_arr=Dblarr(psf_dim2,psf_dim2)
;        FOR i=0.,n_psf_dim-1 DO *psf_base[i]=Double(Abs(*psf_base[i]))
    END
    complex_flag: BEGIN
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
        
    xmin_use=xmin[ind0] ;should all be the same, but don't want an array
    ymin_use=ymin[ind0] ;should all be the same, but don't want an array

    freq_i=(inds mod n_freq1)
    fbin=freq_bin_i[freq_i]
    
    vis_n=bin_n[bin_i[bi]]
    
    xyf_i=x_off+y_off*psf_resolution+fbin*psf_resolution^2.
    
    xyf_si=Sort(xyf_i)
    xyf_i=xyf_i[xyf_si]
    xyf_ui=[Uniq(xyf_i)]
    n_xyf_bin=N_Elements(xyf_ui)
    
    IF vis_n GT 1.1*n_xyf_bin THEN BEGIN ;there might be a better selection criteria to determine which is most efficient
        rep_flag=1
        inds=inds[xyf_si]
        freq_i=freq_i[xyf_si]
        inds_use=xyf_si[xyf_ui]
        
        x_off=x_off[inds_use] 
        y_off=y_off[inds_use]
        fbin=fbin[inds_use]
        IF n_xyf_bin GT 1 THEN xyf_ui0=[0,xyf_ui[0:n_xyf_bin-2]+1] ELSE xyf_ui0=0
        psf_weight=xyf_ui-xyf_ui0+1
         
        vis_box1=vis_arr_use[inds];*freq_norm[freq_i]
        vis_box=vis_box1[xyf_ui]
        IF model_flag THEN BEGIN
            model_box1=model_use[inds];*freq_norm[freq_i]
            model_box=model_box1[xyf_ui]
        ENDIF
        
        repeat_i=where(psf_weight GT 1,n_rep,complement=single_i,ncom=n_single)
        
        xyf_ui=xyf_ui[repeat_i]
;        xyf_ui0=xyf_ui0[repeat_i]+1 ;add 1, since first value already inserted
        xyf_ui0=xyf_ui0[repeat_i]
        FOR rep_ii=0,n_rep-1 DO BEGIN
;            vis_box[repeat_i[rep_ii]]+=Total(vis_box1[xyf_ui0[rep_ii]:xyf_ui[rep_ii]]) ; use += since first value already inserted
            vis_box[repeat_i[rep_ii]]=Total(vis_box1[xyf_ui0[rep_ii]:xyf_ui[rep_ii]])
        ENDFOR
        
        IF model_flag THEN FOR rep_ii=0,n_rep-1 DO model_box[repeat_i[rep_ii]]=Total(model_box1[xyf_ui0[rep_ii]:xyf_ui[rep_ii]])
        
        vis_n=n_xyf_bin
    ENDIF ELSE BEGIN
        rep_flag=0
        IF model_flag THEN model_box=model_use[inds];*freq_norm[freq_i]
        vis_box=vis_arr_use[inds];*freq_norm[freq_i]
        psf_weight=Replicate(1.,vis_n)
;        IF Keyword_Set(grid_uniform_weight) THEN vis_box/=vis_n
    ENDELSE
    
    box_matrix=Make_array(psf_dim3,vis_n,type=arr_type)
    IF verbose THEN BEGIN
        t3_0=Systime(1)
        t2+=t3_0-t1_0
    ENDIF
    
    FOR ii=0L,vis_n-1 DO box_matrix[psf_dim3*ii]=*psf_base[polarization,fbin[ii],x_off[ii],y_off[ii]]
    
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
    IF model_flag THEN BEGIN
        box_arr=matrix_multiply(Temporary(model_box)/n_vis,box_matrix_dag,/atranspose,/btranspose)
        model_return[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=Temporary(box_arr) 
    ENDIF
    box_arr=matrix_multiply(vis_box/n_vis,box_matrix_dag,/atranspose,/btranspose)
    image_uv[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=Temporary(box_arr) 
    
    IF verbose THEN BEGIN
        t5_0=Systime(1)
        t4+=t5_0-t4_0
    ENDIF
    
    IF weights_flag THEN BEGIN
        wts_box=matrix_multiply(psf_weight/n_vis,box_matrix_dag,/atranspose,/btranspose)
;        IF Keyword_Set(grid_uniform_weight) THEN wts_box/=vis_n
        weights[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=Temporary(wts_box)
    ENDIF
    IF variance_flag THEN BEGIN
        var_box=matrix_multiply(psf_weight/n_vis,Abs(box_matrix_dag)^2.,/atranspose,/btranspose)
;        IF Keyword_Set(grid_uniform_weight) THEN wts_box/=vis_n
        variance[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=Temporary(var_box)
    ENDIF
    
    IF verbose THEN BEGIN
        t6_0=Systime(1)
        t5+=t6_0-t5_0
    ENDIF
    IF map_flag THEN BEGIN
        box_arr_map=matrix_multiply(Temporary(box_matrix),Temporary(box_matrix_dag),/btranspose,TPOOL_MIN_ELTS=20000.)
;        IF Keyword_Set(grid_uniform_weight) THEN box_arr_map/=vis_n
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
        variance=(variance+variance_mirror)/4. ;2?
    ENDIF
ENDIF
;normalization=dimension*elements
;image_uv*=normalization ;account for FFT convention

IF verbose THEN print,t0,t1,t2,t3,t4,t5,t6,t7
timing=Systime(1)-t0_0
RETURN,image_uv
END