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
    conj_i=where(ky_arr GT 0,n_conj)
    conj_flag=intarr(N_Elements(ky_arr)) 
    IF n_conj GT 0 THEN BEGIN
        conj_flag[conj_i]=1
        kx_arr[conj_i]=-kx_arr[conj_i]
        ky_arr[conj_i]=-ky_arr[conj_i]
    ENDIF
;    ; As with the rest of calibration, append the complex conjugate at the mirror location
;    kx_arr = [kx_arr, -kx_arr]
;    ky_arr = [ky_arr, -ky_arr]
;    baseline_inds2 = [baseline_inds, baseline_inds]
    xcen=frequency*Temporary(kx_arr)
    ycen=frequency*Temporary(ky_arr)
    n_baselines = N_Elements(baseline_inds)
;    n_baselines2 = N_Elements(baseline_inds2)

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
    covariance_box = Make_array(psf_dim3, type=arr_type)
    covariance_box[psf_base_inds] = *(*beam_arr[pol_i,fbin_use,0])[0,0]
    covariance_box /= Total(Abs(covariance_box)^2.)
    FOR pix=0,psf_dim-1 DO BEGIN
        ind_offset = pix +pix*psf_dim2
        covariance_box2 = Make_array(psf_dim3, type=arr_type)
        covariance_box2[psf_base_inds + ind_offset] = Conj(*(*beam_arr[pol_i,fbin_use,0])[0,0])
        overlap_test[pix] = Total(Abs(covariance_box*covariance_box2))
    ENDFOR
    overlap_i = where(overlap_test GE cal.covariance_threshold, n_overlap)
    covariant_bin = n_overlap - 1
    ; Pad the coordinates so that there is always a buffer of one bin around the edges
    x_min = Min(xcen) - covariant_bin
    y_min = Min(ycen) - covariant_bin
    x_max = Max(xcen) + covariant_bin
    y_max = Max(ycen) + covariant_bin
    xsize = Ceil(x_max - x_min)
    ysize = Ceil(y_max - y_min)
    xsize = covariant_bin*Ceil(xsize/covariant_bin)
    ysize = covariant_bin*Ceil(ysize/covariant_bin)
    lookup_dim = xsize/covariant_bin
    xy_ind = Floor(xcen - x_min)/covariant_bin + lookup_dim*Floor(ycen - y_min)/covariant_bin
    ind_hist = histogram(xy_ind, min=0, max=Max(xy_ind) + 1 + lookup_dim, binsize=1, reverse_indices=ri)
    n_offsets = 5
    x_offsets = [0, -1, 0, 1, 0]
    y_offsets = [-1, 0, 0, 0, 1]
    ind_offsets = x_offsets + y_offsets*lookup_dim
    
;    xmin = Floor(Min(xcen))
;    ymin = Floor(Min(ycen))
;    x_hist = histogram(xcen, binsize=covariant_bin+1, min=xmin, reverse_indices=rix)
;    y_hist = histogram(ycen, binsize=covariant_bin+1, min=ymin, reverse_indices=riy)

    FOR b_ii=0L,n_baselines-1 DO BEGIN
        b_i = baseline_inds[b_ii]
        bi_covariant_i = ri[ri[xy_ind[b_ii]]:ri[xy_ind[b_ii]+1]-1]
        n_covariant = Total(ind_hist[xy_ind[b_ii] + ind_offsets])
        n = 0
        bi_covariant_i = Lonarr(n_covariant)
        FOR offset_i=0,n_offsets-1 DO BEGIN
            off_i = xy_ind[b_ii] + ind_offsets[offset_i]
            off_n = ind_hist[off_i]
            IF off_n GT 0 THEN BEGIN
                bi_covariant_i[n] = ri[ri[off_i]:ri[off_i+1]-1]
                n += off_n
            ENDIF
        ENDFOR
;        ; Guaranteed at least one match, since the baseline being matched is itself in the set
;        bi_covariant_i = where(covariance_flag, n_covariant)
        covariances = Complexarr(n_covariant)

        covariance_box = Make_array(psf_dim3, type=arr_type)
        ; Use the zero-offset beam model as the reference
        IF conj_flag[b_ii] THEN $
            covariance_box[psf_base_inds] = Conj(*(*beam_arr[pol_i,fbin_use,b_i mod obs.nbaselines])[0,0]) ELSE $
            covariance_box[psf_base_inds] = *(*beam_arr[pol_i,fbin_use,b_i mod obs.nbaselines])[0,0]
        dx = xcen[bi_covariant_i] - xcen[b_ii]
        dy = ycen[bi_covariant_i] - ycen[b_ii]
        x0 = Floor(dx)
        y0 = Floor(dy)
        ; Use the same indexing notation as visibility_grid.pro
        ind_offset = x0 + y0*psf_dim2
        x_offset=Fix(Floor((dx-x0)*psf_resolution) mod psf_resolution, type=12) ; type=12 is unsigned int
        y_offset=Fix(Floor((dy-y0)*psf_resolution) mod psf_resolution, type=12)
        
        covariance_box2 =  Make_array(psf_dim3, n_covariant, type=arr_type)

        FOR covariant_i=0L,n_covariant-1 DO BEGIN
            b_i2 = bi_covariant_i[covariant_i]
            beam_use = *(*beam_arr[pol_i,fbin_use,baseline_inds[b_i2] mod obs.nbaselines])[x_offset[covariant_i], y_offset[covariant_i]]
            IF ~conj_flag[b_i2] THEN beam_use = Conj(beam_use)
            ; use the complex conjugate for the second half of the baselines, that have been mirrored
            ; Note that I have combined this with the below complex conjugate, so only apply Conj() to the first half
            ; Note: this should be the complex conjugate, but I combined that and the previous complex conjugate to save computations
            covariance_box2[covariant_i*psf_dim3 + psf_base_inds + ind_offset[covariant_i]] = beam_use

            ; covariances[covariant_i] = Abs(Total(covariance_box*covariance_box2))^2./(Total(Abs(covariance_box)^2.)*Total(Abs(covariance_box2)^2.))
            ;covariances[covariant_i] = 2*Total(covariance_box*covariance_box2)/(Total(Abs(covariance_box)^2.) + Total(Abs(covariance_box2)^2.))

        ENDFOR
        covariances=matrix_multiply((covariance_box2)/n_covariant,(covariance_box),/atranspose)
        ija[b_ii] = Ptr_new(bi_covariant_i)
        sa[b_ii] = Ptr_new(covariances)
    ENDFOR

    ; Save the covariances and baseline indices as a sparse matrix, in the same format as the holographic mapping function
    covariance_map_fn = {ija:ija,sa:sa,i_use:baseline_inds,norm:obs.nbaselines,indexed:0}
    RETURN, covariance_map_fn
END
