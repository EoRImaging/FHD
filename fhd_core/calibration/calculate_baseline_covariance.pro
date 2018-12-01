FUNCTION calculate_baseline_covariance,obs, psf, cal, freq_i, pol_i, baseline_inds=baseline_inds
    ; Calculate the fraction of power shared between every baseline pair
    n_pol = cal.n_pol
    frequency = (*obs.baseline_info).freq[freq_i]

    psf_dim=psf.dim
    psf_resolution=psf.resolution
    beam_arr=*psf.beam_ptr
    freq_bin_i=(*obs.baseline_info).fbin_i
    fbin_use=freq_bin_i[freq_i]
    kbinsize=obs.kpix
    kx_arr=cal.uu[baseline_inds]/kbinsize
    ky_arr=cal.vv[baseline_inds]/kbinsize
    ; As with the rest of calibration, append the complex conjugate at the mirror location
    kx_arr = [kx_arr, -kx_arr]
    ky_arr = [ky_arr, -ky_arr]
    baseline_inds2 = [baseline_inds, baseline_inds]
    xcen=frequency*Temporary(kx_arr)
    ycen=frequency*Temporary(ky_arr)
    n_baselines = N_Elements(baseline_inds)
    n_baselines2 = N_Elements(baseline_inds2)

    psf_dim2 = 2*psf_dim + 1
    init_arr=Complexarr(psf_dim2,psf_dim2)
    init_arr[Ceil(psf_dim/2):Ceil(psf_dim/2)+psf_dim-1, Ceil(psf_dim/2):Ceil(psf_dim/2)+psf_dim-1] = 1
    psf_base_inds = where(init_arr)
    arr_type=Size(init_arr,/type)
    psf_dim3 = psf_dim2*psf_dim2
    ; Sparse matrix format, see holo_mapfn_convert.pro
    sa=Ptrarr(n_baselines)
    ija=Ptrarr(n_baselines)
    overlap_test = Fltarr(psf_dim)
    FOR pix=0,psf_dim-1 DO BEGIN
        covariance_box = Make_array(psf_dim3, type=arr_type)
        covariance_box[psf_base_inds] = *(*beam_arr[pol_i,fbin_use,0])[0,0]
        ind_offset = pix +pix*psf_dim2
        covariance_box2 = Make_array(psf_dim3, type=arr_type)
        covariance_box2[psf_base_inds + ind_offset] = Conj(*(*beam_arr[pol_i,fbin_use,0])[0,0])
        overlap_test[pix] = Total(Abs(covariance_box*covariance_box2))
    ENDFOR
    overlap_i = where(overlap_test GE cal.covariance_threshold, covariant_dim)

    FOR b_ii=0L,n_baselines-1 DO BEGIN
        b_i = baseline_inds[b_ii]
        covariance_flag = (Abs(xcen[b_i] - xcen) LE covariant_dim) and (Abs(ycen[b_i] - ycen) LE covariant_dim)
        ; Guaranteed at least one match, since the baseline being matched is itself in the set
        bi_covariant_i = where(covariance_flag, n_covariant)
        covariances = Complexarr(n_covariant)

        covariance_box = Make_array(psf_dim3, type=arr_type)
        ; Use the zero-offset beam model as the reference
        covariance_box[psf_base_inds] = *(*beam_arr[pol_i,fbin_use,b_i mod obs.nbaselines])[0,0]
        dx = xcen[bi_covariant_i] - xcen[b_i]
        dy = ycen[bi_covariant_i] - ycen[b_i]
        x0 = Floor(dx)
        y0 = Floor(dy)
        ; Use the same indexing notation as visibility_grid.pro
        ind_offset = x0 + y0*psf_dim2
        x_offset=Fix(Floor((dx-x0)*psf_resolution) mod psf_resolution, type=12) ; type=12 is unsigned int
        y_offset=Fix(Floor((dy-y0)*psf_resolution) mod psf_resolution, type=12)

        FOR covariant_i=0L,n_covariant-1 DO BEGIN
            b_i2 = bi_covariant_i[covariant_i]
            covariance_box2 = Make_array(psf_dim3, type=arr_type)
            ; Note: this should be the complex conjugate, but I combined that and the next complex conjugate to save computations
            covariance_box2[psf_base_inds + ind_offset[covariant_i]] = $
                *(*beam_arr[pol_i,fbin_use,baseline_inds2[b_i2] mod obs.nbaselines])[x_offset[covariant_i], y_offset[covariant_i]]
            ; use the complex conjugate for the second half of the baselines, that have been mirrored
            ; Note that I have combined this with the above complex conjugate, so only apply Conj() to the first half
            IF b_i2 LT n_baselines THEN covariance_box = Conj(covariance_box)

            ; covariances[covariant_i] = Abs(Total(covariance_box*covariance_box2))^2./(Total(Abs(covariance_box)^2.)*Total(Abs(covariance_box2)^2.))
            covariances[covariant_i] = 2*Total(covariance_box*covariance_box2)/(Total(Abs(covariance_box)^2.) + Total(Abs(covariance_box2)^2.))

        ENDFOR
        ija[b_ii] = Ptr_new(bi_covariant_i)
        sa[b_ii] = Ptr_new(covariances)
    ENDFOR

    ; Save the covariances and baseline indices as a sparse matrix, in the same format as the holographic mapping function
    covariance_map_fn = {ija:ija,sa:sa,i_use:baseline_inds,norm:1.,indexed:0}
    RETURN, covariance_map_fn
END
