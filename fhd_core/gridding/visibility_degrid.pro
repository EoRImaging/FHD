FUNCTION visibility_degrid,image_uv,vis_weight_ptr,obs,psf,params,$
    timing=timing,polarization=polarization,silent=silent,$
    complex=complex,fill_model_visibilities=fill_model_visibilities,$
    vis_input_ptr=vis_input_ptr,spectral_model_uv_arr=spectral_model_uv_arr,$
    beam_mask_threshold=beam_mask_threshold,beam_per_baseline=beam_per_baseline,$
    uv_grid_phase_only=uv_grid_phase_only,conserve_memory=conserve_memory,_Extra=extra
    t0=Systime(1)
    heap_gc

    complex=psf.complex_flag
    n_spectral=obs.degrid_spectral_terms
    double_precision=obs.double_precision
    interp_flag=psf.interpolate_kernel
    IF keyword_set(conserve_memory) then begin
        IF conserve_memory GT 1E6 THEN mem_thresh=conserve_memory ELSE mem_thresh=1E8 ;in bytes
    ENDIF

    ; For each unflagged baseline, get the minimum contributing pixel number for gridding 
    ; and the 2D derivatives for bilinear interpolation
    bin_n=baseline_grid_locations(obs,psf,params,n_bin_use=n_bin_use,bin_i=bin_i,ri=ri,$
      xmin=xmin,ymin=ymin,vis_weight_ptr=vis_weight_ptr,$
      fill_model_visibilities=fill_model_visibilities,interp_flag=interp_flag,$
      dx0dy0_arr=dx0dy0_arr,dx0dy1_arr=dx0dy1_arr,dx1dy0_arr=dx1dy0_arr,dx1dy1_arr=dx1dy1_arr,$
      x_offset=x_offset,y_offset=y_offset)

    ;extract information from the structures
    dimension=Long(obs.dimension)
    elements=Long(obs.elements)
    kbinsize=obs.kpix

    freq_bin_i=(*obs.baseline_info).fbin_i
    frequency_array=(*obs.baseline_info).freq
    freq_delta=(frequency_array-obs.freq_center)/obs.freq_center

    psf_dim=psf.dim
    psf_resolution=Long(psf.resolution)
    psf_dim3=LONG64(psf_dim*psf_dim)

    nbaselines=obs.nbaselines
    n_samples=obs.n_time
    n_freq_use=N_Elements(frequency_array)
    n_freq=Long(obs.n_freq)
    psf_dim2=2*psf_dim
    group_arr=reform(psf.id[polarization,freq_bin_i,*])
    beam_arr=*psf.beam_ptr

    if keyword_set(beam_per_baseline) then begin
        ; Degrid with a unique beam per baseline
        uu=params.uu
        vv=params.vv
        ww=params.ww
        uv_grid_phase_only=1 ;w-terms have not been tested, thus they've been turned off for now
        psf_image_dim=(*psf.image_info).psf_image_dim
        psf_intermediate_res=(Ceil(Sqrt(psf_resolution)/2)*2.)<psf_resolution
        image_bot=-(psf_dim/2)*psf_intermediate_res+psf_image_dim/2
        image_top=(psf_dim*psf_resolution-1)-(psf_dim/2)*psf_intermediate_res+psf_image_dim/2

        n_tracked = l_m_n(obs, psf, l_mode=l_mode, m_mode=m_mode)
        if keyword_set(uv_grid_phase_only) then n_tracked[*]=0

        beam_int=FLTARR(n_freq)
        beam2_int=FLTARR(n_freq)
        n_grp_use=FLTARR(n_freq)
        primary_beam_area=ptr_new(Fltarr(n_freq))
        primary_beam_sq_area=ptr_new(Fltarr(n_freq))
    
        x = (FINDGEN(dimension) - dimension/2.)*obs.kpix
        y = (FINDGEN(dimension) - dimension/2.)*obs.kpix
    endif

    conj_i=where(params.vv GT 0,n_conj)
    IF n_conj GT 0 THEN BEGIN
        if keyword_set(beam_per_baseline) then begin
            uu[conj_i]=-uu[conj_i]
            vv[conj_i]=-vv[conj_i]
            ww[conj_i]=-ww[conj_i]
        endif
    ENDIF

    ; Create the correct size visibility array
    vis_dimension=nbaselines*n_samples
    IF Keyword_Set(double_precision) THEN visibility_array=DComplexarr(n_freq,vis_dimension) $
    ELSE visibility_array=Complexarr(n_freq,vis_dimension)

    ind_ref=Lindgen(max(bin_n))

    ; Create a template array of the correct type
    CASE 1 OF
        Keyword_Set(complex) AND Keyword_Set(double_precision): init_arr=Dcomplexarr(psf_dim2,psf_dim2)
        Keyword_Set(double_precision): init_arr=Dblarr(psf_dim2,psf_dim2)
        Keyword_Set(complex): init_arr=Complexarr(psf_dim2,psf_dim2)
        ELSE: init_arr=Fltarr(psf_dim2,psf_dim2)
    ENDCASE
    arr_type=Size(init_arr,/type)

    IF Keyword_Set(n_spectral) THEN BEGIN
        prefactor=Ptrarr(n_spectral)
        FOR s_i=0,n_spectral-1 DO prefactor[s_i]=Ptr_new(deriv_coefficients(s_i+1,/divide_factorial))
        box_arr_ptr=Ptrarr(n_spectral)
    ENDIF

    FOR bi=0L,n_bin_use-1 DO BEGIN
        vis_n=bin_n[bin_i[bi]]
        inds=ri[ri[bin_i[bi]]:ri[bin_i[bi]+1]-1]

        ;if constraining memory usage, then est number of loops needed
        if keyword_set(conserve_memory) then begin
            required_bytes = 8.*vis_n*psf_dim3
            mem_iter = ceil(required_bytes/mem_thresh)
            if mem_iter GT 1 then begin
                vis_n_full = vis_n
                inds_full = inds
                vis_n_per_iter = ceil(double(vis_n_full)/mem_iter)
            endif
        endif else mem_iter=1

        ;loop over chunks of visibilities to grid to conserve memory
        for mem_i=0L,mem_iter-1 do begin

            if mem_iter GT 1 then begin
                ;calculate the indices of this visibility chunk if split into multiple chunks
                if (vis_n_per_iter*(mem_i+1)) GT vis_n_full then max_ind = vis_n_full else max_ind = (vis_n_per_iter*(mem_i+1))
                inds = inds_full[vis_n_per_iter*(mem_i):max_ind-1]
                vis_n = max_ind - (vis_n_per_iter*(mem_i))
            endif

            ind0=inds[0]

            x_off=x_offset[inds]
            y_off=y_offset[inds]

            xmin_use=xmin[ind0] ;should all be the same, but don't want an array
            ymin_use=ymin[ind0] ;should all be the same, but don't want an array

            freq_i=(inds mod n_freq_use)
            fbin=freq_bin_i[freq_i]
            baseline_inds=(inds/n_freq_use) mod nbaselines

            box_matrix=Make_array(psf_dim3,vis_n,type=arr_type)
            box_arr=Reform(image_uv[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1],psf_dim3)

            IF interp_flag THEN BEGIN
                dx1dy1 = dx1dy1_arr[inds]
                dx1dy0 = dx1dy0_arr[inds]
                dx0dy1 = dx0dy1_arr[inds]
                dx0dy0 = dx0dy0_arr[inds]

                ind_remap_flag=0
                bt_index = inds / n_freq_use

                FOR ii=0L,vis_n-1 DO BEGIN
                    box_matrix[psf_dim3*ii]=$
                        interpolate_kernel(*beam_arr[polarization,fbin[ii],baseline_inds[ii]],x_offset=x_off[ii], $
                        y_offset=y_off[ii],dx0dy0=dx0dy0[ii], dx1dy0=dx1dy0[ii], dx0dy1=dx0dy1[ii], dx1dy1=dx1dy1[ii])
                ENDFOR

            ENDIF ELSE BEGIN
                group_id=group_arr[inds]
                group_max=Max(group_id)+1
                xyf_i=(x_off+y_off*psf_resolution+fbin*psf_resolution^2.)*group_max+group_id
                xyf_si=Sort(xyf_i)
                xyf_i=xyf_i[xyf_si]
                xyf_ui=Uniq(xyf_i)
                n_xyf_bin=N_Elements(xyf_ui)

                ;there might be a better selection criteria to determine which is most efficient
                IF (vis_n GT Ceil(1.1*n_xyf_bin)) AND ~keyword_set(beam_per_baseline) THEN BEGIN
                    ind_remap_flag=1

                    inds=inds[xyf_si]
                    inds_use=[xyf_si[xyf_ui]]

                    freq_i=freq_i[inds_use]
                    x_off=x_off[inds_use]
                    y_off=y_off[inds_use]
                    fbin=fbin[inds_use]
                    baseline_inds=baseline_inds[inds_use]
                    ;psf_conj_flag=psf_conj_flag[inds_use]

                    IF n_xyf_bin EQ 1 THEN ind_remap=intarr(vis_n) ELSE BEGIN
                        hist_inds_u=histogram(xyf_ui,/binsize,min=0,reverse_ind=ri_xyf)
                        ind_remap=ind_ref[ri_xyf[0:n_elements(hist_inds_u)-1]-ri_xyf[0]]
                    ENDELSE

                    vis_n=Long64(n_xyf_bin)
                ENDIF ELSE BEGIN
                    ind_remap_flag=0
                    bt_index = inds / n_freq_use
                ENDELSE

                if keyword_set(beam_per_baseline) then begin
                    box_matrix = grid_beam_per_baseline(psf, uu, vv, ww, l_mode, m_mode, n_tracked, frequency_array, x, y,$
                        xmin_use, ymin_use, freq_i, bt_index, polarization, fbin, image_bot, image_top, psf_dim3,$
                        box_matrix, vis_n, beam_int=beam_int, beam2_int=beam2_int, n_grp_use=n_grp_use,degrid_flag=1,_Extra=extra)
                endif else begin
                    FOR ii=0L,vis_n-1 DO BEGIN
                        ;more efficient array subscript notation
                        box_matrix[psf_dim3*ii]=*(*beam_arr[polarization,fbin[ii],baseline_inds[ii]])[x_off[ii],y_off[ii]]
                    ENDFOR
                endelse

            ENDELSE

            IF Keyword_Set(n_spectral) THEN BEGIN
                vis_box=matrix_multiply(box_matrix,Temporary(box_arr),/atranspose)
                freq_term_arr=Rebin(transpose(freq_delta[freq_i]),psf_dim3,vis_n,/sample)
                FOR s_i=0,n_spectral-1 DO BEGIN
                    ;s_i loop is over terms of the Taylor expansion, starting from the lowest-order term
                    prefactor_use=*prefactor[s_i]
                    box_matrix*=freq_term_arr
                    box_arr_ptr[s_i]=Ptr_new(Reform((*spectral_model_uv_arr[s_i])[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1],psf_dim3))

                    FOR s_i_i=0,s_i DO BEGIN
                        ;s_i_i loop is over powers of the model x alpha^n, n=s_i_i+1
                        degree=n_spectral
                        box_arr=prefactor_use[s_i_i]*(*box_arr_ptr[s_i_i])
                        vis_box+=matrix_multiply(box_matrix,Temporary(box_arr),/atranspose)
                    ENDFOR
                ENDFOR
                ptr_free,box_arr_ptr
            ENDIF ELSE vis_box=matrix_multiply(Temporary(box_matrix),Temporary(box_arr),/atranspose) ;box_matrix#box_arr
            IF ind_remap_flag THEN vis_box=vis_box[ind_remap]
            visibility_array[inds]=vis_box
        ENDFOR
    ENDFOR


    if keyword_set(beam_per_baseline) then begin
        beam2_int*=weight_invert(n_grp_use)/kbinsize^2. ;factor of kbinsize^2 is FFT units normalization
        beam_int*=weight_invert(n_grp_use)/kbinsize^2.
        (*primary_beam_sq_area)=Float(beam2_int)
        (*primary_beam_area)=Float(beam_int)
        obs.primary_beam_area[polarization]=primary_beam_area
        obs.primary_beam_sq_area[polarization]=primary_beam_sq_area
    endif

    x_offset=(y_offset=0)
    xmin=(ymin=0)
    bin_n=0
    IF n_conj GT 0 THEN BEGIN
        visibility_array[*,conj_i]=Conj(visibility_array[*,conj_i])
    ENDIF

    IF Ptr_valid(vis_input_ptr) THEN IF N_Elements(*vis_input_ptr) EQ N_Elements(visibility_array) THEN BEGIN
        vis_return=vis_input_ptr
        *vis_return+=Temporary(visibility_array)
    ENDIF
    IF ~Ptr_valid(vis_return) THEN vis_return=Ptr_new(visibility_array,/no_copy)

    timing=Systime(1)-t0

    RETURN,vis_return
END
