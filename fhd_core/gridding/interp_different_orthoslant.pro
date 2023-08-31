function generate_interp_uv, old_obs, new_obs, uv_arr, x_ref_old_pts, y_ref_old_pts, $
    cubic=cubic

    uv_arr_dims = size(uv_arr, /dimension)
    n_pol = uv_arr_dims[0]
    n_freq = uv_arr_dims[1]

    new_uv_arr = Ptrarr(n_pol, n_freq)
    for p_ind = 0, n_pol - 1 do begin
        for f_ind = 0, n_freq - 1 do begin
            orthoslant_sky = Real_part(fft_shift(FFT(fft_shift(*uv_arr[p_ind, f_ind]), /double)))
            new_orthoslant_sky = interpolate(orthoslant_sky, x_ref_old_pts, y_ref_old_pts, cubic=cubic, /double)

            new_uv_arr[p_ind, f_ind] = Ptr_new(fft_shift(FFT(fft_shift(new_orthoslant_sky), /double, /inverse)))
        endfor
    endfor

    return, new_uv_arr

end

pro interp_different_orthoslant, folder_name, obs_id, ref_obs_id, cubic=cubic

    if size(obs_id, /type) ne 7 then begin
        obs_id = number_formatter(obs_id)
    endif

    uvf_even_file_list = file_search(folder_name + path_sep() + obs_id + '_even*_uvf.sav', $
        count = n_even)
    uvf_odd_file_list = file_search(folder_name + path_sep() + obs_id + '_odd*_uvf.sav', $
        count = n_odd)
    uvf_file_list = [uvf_even_file_list, uvf_odd_file_list]
    if n_even ne 1 or n_odd ne 1 then begin
        message, "Did not find exactly one even and one odd file for this obsid"
    endif

    if size(ref_obs_id, /type) ne 7 then begin
        ref_obs_id = number_formatter(ref_obs_id)
    endif

    ref_even_file_list = file_search(folder_name + path_sep() + ref_obs_id + '_even*_uvf.sav', $
        count = n_even)
    ref_odd_file_list = file_search(folder_name + path_sep() + ref_obs_id + '_odd*_uvf.sav', $
        count = n_odd)
    ref_file_list = [ref_even_file_list, ref_odd_file_list]
    if n_even ne 1 or n_odd ne 1 then begin
        message, "Did not find exactly one even and one odd file for ref_obs_id"
    endif

    for file_id = 0, 1 do begin

        void = getvar_savefile(ref_file_list[file_id], names=varnames)
        if n_elements (varnames) eq 0 then begin
            message, "No variables could be extracted from file: " + ref_file_list[file_id]
        endif
        whobs = where(stregex(varnames, "obs", /fold_case) eq 0, nobs)
        if nobs eq 1 then begin
            ref_obs = getvar_savefile(ref_file_list[file_id], varnames[whobs[0]])
        endif else begin
            if nobs eq 0 then begin
                message, "File " + ref_file_list[file_id] + " did not contain a variable with 'obs' in the name."
            endif else begin
                message, "File " + ref_file_list[file_id] + " contained multiple variables with 'obs' in the name."
            endelse
        endelse

        void = getvar_savefile(uvf_file_list[file_id], names=varnames)
        if n_elements (varnames) eq 0 then begin
            message, "No variables could be extracted from file: " + uvf_file_list[file_id]
        endif

        whobs = where(stregex(varnames, "obs", /fold_case) eq 0, nobs)
        if nobs eq 1 then begin
            old_obs = getvar_savefile(uvf_file_list[file_id], varnames[whobs[0]])
        endif else begin
            if nobs eq 0 then begin
                message, "File " + uvf_file_list[file_id] + " did not contain a variable with 'obs' in the name."
            endif else begin
                message, "File " + uvf_file_list[file_id] + " contained multiple variables with 'obs' in the name."
            endelse
        endelse

        apply_astrometry, old_obs, x_arr=meshgrid(old_obs.dimension, old_obs.elements, 1), $
            y_arr=meshgrid(old_obs.dimension, old_obs.elements, 2), ra_arr=old_ra_arr, $
            dec_arr=old_dec_arr, /xy2ad

        apply_astrometry, ref_obs, ra_arr=old_ra_arr, dec_arr=old_dec_arr, $
            x_arr=x_ref_old_pts, y_arr=y_ref_old_pts, /ad2xy

        obs_out = ref_obs

        old_dirty_uv_arr = getvar_savefile(uvf_file_list[file_id], "dirty_uv_arr")
        dirty_uv_arr = generate_interp_uv(old_obs, ref_obs, old_dirty_uv_arr, $
            x_ref_old_pts, y_ref_old_pts, cubic=cubic)

        old_model_uv_arr = getvar_savefile(uvf_file_list[file_id], "model_uv_arr")
        model_uv_arr = generate_interp_uv(old_obs, ref_obs, old_model_uv_arr, $
            x_ref_old_pts, y_ref_old_pts, cubic=cubic)

        old_weights_uv_arr = getvar_savefile(uvf_file_list[file_id], "weights_uv_arr")
        weights_uv_arr = generate_interp_uv(old_obs, ref_obs, old_weights_uv_arr, $
            x_ref_old_pts, y_ref_old_pts, cubic=cubic)

        old_variance_uv_arr = getvar_savefile(uvf_file_list[file_id], "variance_uv_arr")
        variance_uv_arr = generate_interp_uv(old_obs, ref_obs, old_variance_uv_arr, $
            x_ref_old_pts, y_ref_old_pts, cubic=cubic)

        file_base = cgRootName(uvf_file_list[file_id], directory=filedir, extension=exten)
        ;; want to add '_interp' after the obsid, before the even/odd
        fstr_add = 'interp'
        if n_elements(cubic) then begin
            fstr_add += number_formatter(cubic)
        endif
        eo_types = ['even', 'odd']
        for eo_ind = 0, n_elements(eo_types) - 1 do begin
            eo_pos = strpos(file_base, eo_types[eo_ind])
            if eo_pos gt -1 then begin
                new_file_base = strmid(file_base, 0, eo_pos) + fstr_add + "_" + strmid(file_base, eo_pos)
            endif
        endfor
        output_file = filedir + new_file_base + "." + exten

        save, obs_out, dirty_uv_arr, model_uv_arr, weights_uv_arr, variance_uv_arr, $
            filename=output_file, /compress
    endfor
end
