FUNCTION calculate_baseline_covariance,obs, psf, params, freq_i
    ; Calculate the fraction of power shared between every baseline pair
    n_pol = obs.n_pol < 2
    b_info=(*obs.baseline_info)
    tile_use=where(b_info.tile_use)+1
    bi_use=array_match(b_info.tile_A,b_info.tile_B,value_match=tile_use, n_match=n_baselines_use)
    frequency = (*obs.baseline_info).freq[freq_i]

    complex_flag=psf.complex_flag
    psf_dim=psf.dim
    psf_resolution=psf.resolution
    beam_arr=*psf.beam_ptr
    freq_bin_i=(*obs.baseline_info).fbin_i
    fbin_use=freq_bin_i[freq_i]
    kbinsize=obs.kpix
    kx_arr=params.uu[bi_use]/kbinsize
    ky_arr=params.vv[bi_use]/kbinsize
    xcen=frequency*Temporary(kx_arr)
    ycen=frequency*Temporary(ky_arr)

    psf_dim2 = 2*psf_dim + 1
    IF complex_flag THEN init_arr=Complexarr(psf_dim2,psf_dim2) ELSE init_arr=Fltarr(psf_dim2,psf_dim2)
    init_arr[Ceil(psf_dim/2):Ceil(psf_dim/2)+psf_dim-1, Ceil(psf_dim/2):Ceil(psf_dim/2)+psf_dim-1] = 1
    psf_base_inds = where(init_arr)
    arr_type=Size(init_arr,/type)
    psf_dim3 = psf_dim2*psf_dim2
    ; Sparse matrix format, see holo_mapfn_convert.pro
    sa=Ptrarr(n_baselines_use,/allocate)
    ija=Ptrarr(n_baselines_use,/allocate)

    FOR b_ii=0L,n_baselines_use-1 DO BEGIN
        b_i = bi_use[b_ii]
        covariance_flag = (Abs(xcen[b_i] - xcen) LE psf_dim) & (Abs(ycen[b_i] - ycen) LE psf_dim)
        ; Guaranteed at least one match, since the baseline being matched is itself in the set
        bi_covariant_i = where(covariance_flag, n_covariant)
        IF complex_flag THEN covariances = Complexarr(n_covariant) ELSE covariances = Fltarr(n_covariant)

        covariance_box = Make_array(psf_dim3, type=arr_type)
        ; Use the zero-offset beam model as the reference
        FOR pol_i=0,n_pol-1 DO $
            covariance_box[psf_base_inds] += *(*beam_arr[pol_i,fbin_use,bi_use[b_i]])[0,0]
        dx = xcen[bi_covariant_i] - xcen[b_i]
        dy = ycen[bi_covariant_i] - ycen[b_i]
        x0 = Floor(dx)
        y0 = Floor(dy)
        ind_offset = x0 + y0*psf_dim2
        x_offset=Fix(Floor((dx-x0)*psf_resolution) mod psf_resolution, type=12) ; type=12 is unsigned int
        y_offset=Fix(Floor((dy-y0)*psf_resolution) mod psf_resolution, type=12)

        FOR covariant_i=0L,n_covariant-1 DO BEGIN
            b_i2 = bi_covariant_i[covariant_i]
            covariance_box2 = Make_array(psf_dim3, type=arr_type)
            FOR pol_i=0,n_pol-1 DO $
                covariance_box2[psf_base_inds + ind_offset[covariant_i]] += $
                    *(*beam_arr[pol_i,fbin_use,bi_use[b_i2]])[x_offset[covariant_i], y_offset[covariant_i]]
            IF complex_flag THEN covariance_box2 = Conj(Temporary(covariance_box2))

            covariances[covariant_i] = Abs(Total(covariance_box*covariance_box2))^2./(Total(Abs(covariance_box)^2.)*Total(Abs(covariance_box2)^2.))

        ENDFOR
        *ija[b_ii] = bi_covariant_i
        *sa[b_ii] = covariances
    ENDFOR

    ; Save the covariances and baseline indices as a sparse matrix, in the same format as the holographic mapping function
    covariance_map_fn = {ija:ija,sa:sa,i_use:bi_use,norm:1.,indexed:1}
    RETURN, covariance_map_fn
END
