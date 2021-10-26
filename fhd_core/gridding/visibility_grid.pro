FUNCTION visibility_grid,visibility_ptr,vis_weight_ptr,obs,status_str,psf,params,file_path_fhd=file_path_fhd,weights=weights,variance=variance,$
    timing=timing,polarization=polarization,mapfn_recalculate=mapfn_recalculate,silent=silent,uniform_filter=uniform_filter,$
    GPU_enable=GPU_enable,complex_flag=complex_flag,fi_use=fi_use,bi_use=bi_use,$
    visibility_list=visibility_list,image_list=image_list,n_vis=n_vis,no_conjugate=no_conjugate,$
    return_mapfn=return_mapfn,mask_mirror_indices=mask_mirror_indices,no_save=no_save,$
    model_ptr=model_ptr,model_return=model_return,preserve_visibilities=preserve_visibilities,$
    error=error,grid_uniform=grid_uniform,$
    grid_spectral=grid_spectral,spectral_uv=spectral_uv,spectral_model_uv=spectral_model_uv,$
    beam_per_baseline=beam_per_baseline,uv_grid_phase_only=uv_grid_phase_only,_Extra=extra
t0_0=Systime(1)
heap_gc

; Extract information from the structures
dimension=Long(obs.dimension)
elements=Long(obs.elements)
double_precision=obs.double_precision
interp_flag=psf.interpolate_kernel
IF N_Elements(silent) EQ 0 THEN verbose=0 ELSE verbose=0>Round(1-silent)<1

alpha=obs.alpha
freq_bin_i=(*obs.baseline_info).fbin_i
n_freq=Long(obs.n_freq)
IF N_Elements(fi_use) EQ 0 THEN fi_use=where((*obs.baseline_info).freq_use)
n_f_use=N_Elements(fi_use)
freq_bin_i=freq_bin_i[fi_use]
n_vis_arr=obs.nf_vis


; For each unflagged baseline, get the minimum contributing pixel number for gridding 
; and the 2D derivatives for bilinear interpolation
bin_n = baseline_grid_locations(obs,psf,params,n_bin_use=n_bin_use,bin_i=bin_i,ri=ri,$
  xmin=xmin,ymin=ymin,vis_weight_ptr=vis_weight_ptr,$
  bi_use=bi_use,fi_use=fi_use,vis_inds_use=vis_inds_use,interp_flag=interp_flag,$
  dx0dy0_arr=dx0dy0_arr,dx0dy1_arr=dx0dy1_arr,dx1dy0_arr=dx1dy0_arr,dx1dy1_arr=dx1dy1_arr,$
  x_offset=x_offset,y_offset=y_offset,preserve_visibilities=preserve_visibilities,$
  mask_mirror_indices=mask_mirror_indices)

; Use indices of visibilities to grid during this call (i.e. specific freqs, time sets)
; to initialize output arrays
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

nbaselines=obs.nbaselines
n_samples=obs.n_time
n_freq_use=N_Elements(frequency_array)
psf_dim2=2*psf_dim
psf_dim3=LONG64(psf_dim*psf_dim)
bi_use_reduced=bi_use mod nbaselines

if keyword_set(beam_per_baseline) then begin
    ; Initialization for gridding operation via a low-res beam kernel, calculated per
    ; baseline using offsets from image-space delays
    uu=params.uu[bi_use]
    vv=params.vv[bi_use]
    ww=params.ww[bi_use]
    x = (FINDGEN(dimension) - dimension/2.)*obs.kpix
    y = (FINDGEN(dimension) - dimension/2.)*obs.kpix
    uv_grid_phase_only=1 ;w-terms have not been tested, thus they've been turned off for now
    psf_intermediate_res=(Ceil(Sqrt(psf_resolution)/2)*2.)<psf_resolution
    psf_image_dim=(*psf.image_info).psf_image_dim
    image_bot=-(psf_dim/2)*psf_intermediate_res+psf_image_dim/2
    image_top=(psf_dim*psf_resolution-1)-(psf_dim/2)*psf_intermediate_res+psf_image_dim/2

    n_tracked = l_m_n(obs, psf, l_mode=l_mode, m_mode=m_mode)

    if keyword_set(uv_grid_phase_only) then n_tracked[*]=0
endif

; Initialize uv-arrays based on double or floating point percision
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

conj_i=where(params.vv[bi_use] GT 0,n_conj)
IF n_conj GT 0 THEN BEGIN
    if keyword_set(beam_per_baseline) then begin
        uu[conj_i]=-uu[conj_i]
        vv[conj_i]=-vv[conj_i]
        ww[conj_i]=-ww[conj_i]
    endif
    vis_arr_use[*,conj_i]=Conj(vis_arr_use[*,conj_i])
    IF model_flag THEN model_use[*,conj_i]=Conj(model_use[*,conj_i])
ENDIF

IF n_bin_use EQ 0 THEN BEGIN
    ; Return if all baselines have been flagged
    print,'WARNING: Returning an empty grid since all data are flagged or cut'
    timing=Systime(1)-t0_0
    n_vis=0.
    error=1
    RETURN,image_uv
ENDIF

ind_ref=indgen(max(bin_n))
n_vis=(Total(double(bin_n)))
FOR fi=0L,n_f_use-1 DO n_vis_arr[fi_use[fi]]=Total(Long(xmin[fi,*] GT 0))
obs.nf_vis=n_vis_arr

; Initialization based on double or float precision
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

IF map_flag THEN BEGIN
    ; Initialize ONLY those elements of the map_fn array that will receive data to remain sparse
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

IF map_flag THEN BEGIN
    map_fn_inds=Ptrarr(psf_dim,psf_dim,/allocate)
    psf2_inds=indgen(psf_dim2,psf_dim2)
    FOR i=0,psf_dim-1 DO FOR j=0,psf_dim-1 DO $  
        *map_fn_inds[i,j]=psf2_inds[psf_dim-i:2*psf_dim-i-1,psf_dim-j:2*psf_dim-j-1]
ENDIF


FOR bi=0L,n_bin_use-1 DO BEGIN
    ; Cycle through sets of visibilities which contribute to the same data/model uv-plane pixels, and perform
    ; the gridding operation per set using each visibilities' hyperresolved kernel

    ; Select the indices of the visibilities which contribute to the same data/model uv-plane pixels
    inds=ri[ri[bin_i[bi]]:ri[bin_i[bi]+1]-1]
    ind0=inds[0]
    
    ; Select the pixel offsets of the hyperresolution uv-kernel of the selected visibilities 
    x_off=x_offset[inds]
    y_off=y_offset[inds]
        
    ; Since all selected visibilities have the same minimum x,y pixel they contribute to,
    ; reduce the array
    xmin_use=xmin[ind0]
    ymin_use=ymin[ind0]

    ; Find the frequency group per index
    freq_i=(inds mod n_freq_use)
    fbin=freq_bin_i[freq_i]
    
    ; Calculate the number of selected visibilities and their baseline index
    vis_n=bin_n[bin_i[bi]]
    baseline_inds=bi_use_reduced[(inds/n_f_use) mod nbaselines]

    IF interp_flag THEN BEGIN
        ; Calculate the interpolated kernel on the uv-grid given the derivatives to baseline locations
        ; and the hyperresolved pre-calculated beam kernel

        ; Select the 2D derivatives to baseline locations
        dx1dy1 = dx1dy1_arr[inds]
        dx1dy0 = dx1dy0_arr[inds]
        dx0dy1 = dx0dy1_arr[inds]
        dx0dy0 = dx0dy0_arr[inds]

        ; Select the model/data visibility values of the set, each with a weight of 1
        rep_flag=0
        IF model_flag THEN model_box=model_use[inds]
        vis_box=vis_arr_use[inds]
        psf_weight=Replicate(1.,vis_n)

        box_matrix=Make_array(psf_dim3,vis_n,type=arr_type)
        FOR ii=0L,vis_n-1 DO BEGIN
            ; For each visibility, calculate the kernel values on the static uv-grid given the
            ; hyperresolved kernel and an interpolation involving the derivatives
            box_matrix[psf_dim3*ii]=$
              interpolate_kernel(*beam_arr[polarization,fbin[ii],baseline_inds[ii]],x_offset=x_off[ii], $
              y_offset=y_off[ii], dx0dy0=dx0dy0[ii], dx1dy0=dx1dy0[ii], dx0dy1=dx0dy1[ii], dx1dy1=dx1dy1[ii]) 
        ENDFOR

    ENDIF ELSE BEGIN
        ; Calculate the beam kernel at each baseline location given the hyperresolved pre-calculated
        ; beam kernel

        ; Calculate a unique index for each kernel location and kernel type in order to reduce 
        ; operations if there are repeats
        group_id=group_arr[inds]
        group_max=Max(group_id)+1
        xyf_i=(x_off+y_off*psf_resolution+fbin*psf_resolution^2.)*group_max+group_id
        
        ; Calculate the unique number of kernel locations/types
        xyf_si=Sort(xyf_i)
        xyf_i=xyf_i[xyf_si]
        xyf_ui=[Uniq(xyf_i)]
        n_xyf_bin=N_Elements(xyf_ui)

        ; There might be a better selection criteria to determine which is most efficient
        IF vis_n GT 1.1*n_xyf_bin AND ~keyword_set(beam_per_baseline) THEN BEGIN
            ; If there are any baselines which use the same beam kernel and the same discretized location
            ; given the hyperresolution, then reduce the number of gridding operations to only 
            ; non-repeated baselines
            rep_flag=1
            inds=inds[xyf_si]
            inds_use=xyf_si[xyf_ui]
            freq_i=freq_i[inds_use]

            x_off=x_off[inds_use]
            y_off=y_off[inds_use]
            fbin=fbin[inds_use]
            baseline_inds=baseline_inds[inds_use]
            IF n_xyf_bin GT 1 THEN xyf_ui0=[0,xyf_ui[0:n_xyf_bin-2]+1] ELSE xyf_ui0=0
            psf_weight=xyf_ui-xyf_ui0+1

            vis_box1=vis_arr_use[inds]
            vis_box=vis_box1[xyf_ui]
            IF model_flag THEN BEGIN
                model_box1=model_use[inds]
                model_box=model_box1[xyf_ui]
            ENDIF

            ; For the baselines which map to the same pixels and use the same beam,
            ; add the underlying data/model pixels such that the gridding operation
            ; only needs to be performed once for the set
            repeat_i=where(psf_weight GT 1,n_rep,complement=single_i,ncom=n_single)

            xyf_ui=xyf_ui[repeat_i]
            xyf_ui0=xyf_ui0[repeat_i]
            FOR rep_ii=0,n_rep-1 DO $
                vis_box[repeat_i[rep_ii]]=Total(vis_box1[xyf_ui0[rep_ii]:xyf_ui[rep_ii]])

            IF model_flag THEN FOR rep_ii=0,n_rep-1 DO $
                model_box[repeat_i[rep_ii]]=Total(model_box1[xyf_ui0[rep_ii]:xyf_ui[rep_ii]])

            vis_n=n_xyf_bin
        ENDIF ELSE BEGIN
            ; If there are not enough baselines which use the same beam kernel and discretized
            ; location to warrent reduction, then perform the gridding operation per baseline
            rep_flag=0
            IF model_flag THEN model_box=model_use[inds]
            vis_box=vis_arr_use[inds]
            psf_weight=Replicate(1.,vis_n)
            bt_index = inds / n_freq_use
        ENDELSE

        box_matrix=Make_array(psf_dim3,vis_n,type=arr_type)    
        if keyword_set(beam_per_baseline) then begin
            ; Make the beams on the fly with corrective phases given the baseline location for each visibility
            ; to the static uv-grid
            box_matrix = grid_beam_per_baseline(psf, uu, vv, ww, l_mode, m_mode, n_tracked, frequency_array, x, y,$
              xmin_use, ymin_use, freq_i, bt_index, polarization, fbin, image_bot, image_top, psf_dim3,$
              box_matrix, vis_n, _Extra=extra)
        endif else begin
            FOR ii=0L,vis_n-1 DO BEGIN
                ; For each visibility, calculate the kernel values on the static uv-grid given the
                ; hyperresolved kernel
                box_matrix[psf_dim3*ii]=*(*beam_arr[polarization,fbin[ii],baseline_inds[ii]])[x_off[ii],y_off[ii]]
            ENDFOR
        endelse

    ENDELSE

    ; Calculate the conjugate transpose (dagger) of the uv-pixels that the current beam kernel contributes to
    IF map_flag THEN BEGIN
        IF complex_flag THEN box_matrix_dag=Conj(box_matrix) ELSE box_matrix_dag=real_part(box_matrix) 
        IF rep_flag THEN box_matrix*=Rebin(Transpose(psf_weight),psf_dim3,vis_n)
    ENDIF ELSE BEGIN
        IF complex_flag THEN box_matrix_dag=Conj(Temporary(box_matrix)) ELSE box_matrix_dag=Real_part(Temporary(box_matrix))
    ENDELSE
    
    IF Keyword_Set(grid_spectral) THEN BEGIN
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
    ENDIF
    IF model_flag THEN BEGIN
        ; If model visibilities are being gridded, calculate the product of the model vis and the beam kernel
        ; for all vis which contribute to the same static uv-pixels, and add to the static uv-plane
        box_arr=matrix_multiply(Temporary(model_box)/n_vis,box_matrix_dag,/atranspose,/btranspose)
        model_return[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=Temporary(box_arr) 
    ENDIF
    ; Calculate the product of the data vis and the beam kernel
    ; for all vis which contribute to the same static uv-pixels, and add to the static uv-plane
    box_arr=matrix_multiply(Temporary(vis_box)/n_vis,box_matrix_dag,/atranspose,/btranspose)
    image_uv[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=Temporary(box_arr) 
    
    IF weights_flag THEN BEGIN
        ; If weight visibilities are being gridded, calculate the product the weight (1 per vis) and the beam kernel
        ; for all vis which contribute to the same static uv-pixels, and add to the static uv-plane
        wts_box=matrix_multiply(psf_weight/n_vis,box_matrix_dag,/atranspose,/btranspose)
        weights[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=Temporary(wts_box)
    ENDIF
    IF variance_flag THEN BEGIN
        ; If variance visibilities are being gridded, calculate the product the weight (1 per vis) and the square
        ; of the beam kernel for all vis which contribute to the same static uv-pixels, and add to the static uv-plane
        var_box=matrix_multiply(psf_weight/n_vis,Abs(box_matrix_dag)^2.,/atranspose,/btranspose)
        variance[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=Temporary(var_box)
    ENDIF
    IF uniform_flag THEN uniform_filter[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=bin_n[bin_i[bi]]
    
    IF map_flag THEN BEGIN
        ; If the mapping function is being calculated, then calculate the beam mapping for the current
        ; set of uv-pixels and add to the full mapping function
        box_arr_map=matrix_multiply(Temporary(box_matrix),Temporary(box_matrix_dag),/btranspose,TPOOL_MIN_ELTS=20000.)
        FOR i=0,psf_dim-1 DO FOR j=0,psf_dim-1 DO BEGIN
            ij=i+j*psf_dim
            (*map_fn[xmin_use+i,ymin_use+j])[*map_fn_inds[i,j]]+=box_arr_map[*,ij]
            dummy_ref=-1
        ENDFOR
    ENDIF
ENDFOR

; free memory
vis_arr_use=(model_use=0)
xmin=(ymin=(ri=(inds=(x_offset=(y_offset=(bin_i=(bin_n=0)))))))

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

IF Keyword_Set(grid_spectral) THEN BEGIN
    ; Option to use spectral index information to scale the uv-plane 
    spectral_uv=(spectral_A-n_vis*spectral_B*image_uv)*weight_invert(spectral_D-spectral_B^2.)
    IF model_flag THEN spectral_model_uv=(spectral_model_A-n_vis*spectral_B*model_return)*weight_invert(spectral_D-spectral_B^2.)
    IF ~Keyword_Set(no_conjugate) THEN BEGIN
        spectral_uv=(spectral_uv+conjugate_mirror(spectral_uv))/2.
        IF model_flag THEN spectral_model_uv=(spectral_model_uv+conjugate_mirror(spectral_model_uv))/2.
    ENDIF
ENDIF

IF Keyword_Set(grid_uniform) THEN BEGIN
    ; Option to apply a uniform weighted filter to all uv-planes
    filter_use=weight_invert(uniform_filter,1.) 
    wts_i=where(filter_use,n_wts)
    IF n_wts GT 0 THEN filter_use/=Mean(filter_use[wts_i]) ELSE filter_use/=Mean(filter_use)
    image_uv*=weight_invert(filter_use)
    IF weights_flag THEN weights*=weight_invert(filter_use)
    IF variance_flag THEN variance*=weight_invert(filter_use)
    IF model_flag THEN model_return*=weight_invert(filter_use)
ENDIF

IF ~Keyword_Set(no_conjugate) THEN BEGIN
    ; The uv-plane is its own conjugate mirror about the x-axis, so fill in the rest of the uv-plane
    ; using simple maths instead of extra gridding
    image_uv=(image_uv+conjugate_mirror(image_uv))/2.
    IF weights_flag THEN weights=(weights+conjugate_mirror(weights))/2.        
    IF variance_flag THEN variance=(variance+conjugate_mirror(variance))/4.
    IF model_flag THEN model_return=(model_return+conjugate_mirror(model_return))/2.
    IF uniform_flag THEN uniform_filter=(uniform_filter+conjugate_mirror(uniform_filter))/2.
ENDIF

timing=Systime(1)-t0_0
RETURN,image_uv
END
