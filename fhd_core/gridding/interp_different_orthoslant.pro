function generate_interp_uv, old_obs, new_obs, uv_arr, x_new_old_pts, y_new_old_pts

uv_arr_dims = size(uv_arr, /dimension)
n_pol = uv_arr_dims[0]
n_freq = uv_arr_dims[1]

new_uv_arr = Ptrarr(n_pol, n_freq)
for p_ind = 0, n_pol - 1 do begin
    for f_ind = 0, n_freq - 1 do begin
        orthoslant_sky = Real_part(fft_shift(FFT(fft_shift(*uv_arr[p_ind, f_ind]),double=1)))
        new_orthoslant_sky = interpolate(orthoslant_sky, x_new_old_pts, y_new_old_pts)

        new_uv_arr[p_ind, f_ind] = Ptr_new(Fltarr(new_obs.dimension, new_obs.elements))
        *new_uv_arr[p_ind, f_ind] = fft_shift(FFT(fft_shift(new_orthoslant_sky),/inverse))
    endfor
endfor

return, new_uv_arr

end

pro interp_different_orthoslant, input_gridded_uvf_file, new_obs

if size(new_obs, /type) eq 7 then begin
    ;; this is a file with an obs structure in it.
    void = getvar_savefile(new_obs, names=varnames)
    if n_elements (varnames) eq 0 then begin
        message, "No variables could be extracted from file: " + new_obs
    endif
    whobs = where(stregex(varnames, "obs", /fold_case) eq 0, nobs)
    if nobs eq 1 then begin
        new_obs_use = getvar_savefile(new_obs, varnames[whobs[0]])
    endif else begin
        if nobs eq 0 then begin
            message, "File " + new_obs + " did not contain a variable with 'obs' in the name."
        endif else begin
            message, "File " + new_obs + " contained multiple variables with 'obs' in the name."
        endelse
    endelse
endif else begin
    if size(new_obs, /type) ne 8 then begin
        message, "Either a string with a file name containg the new obs structure or an obs structure must be passed as the new_obs variable."
    endif
    new_obs_use = new_obs
endelse

void = getvar_savefile(input_gridded_uvf_file, names=varnames)
if n_elements (varnames) eq 0 then begin
    message, "No variables could be extracted from file: " + input_gridded_uvf_file
endif

whobs = where(stregex(varnames, "obs", /fold_case) eq 0, nobs)
if nobs eq 1 then begin
    old_obs = getvar_savefile(input_gridded_uvf_file, varnames[whobs[0]])
endif else begin
    if nobs eq 0 then begin
        message, "File " + input_gridded_uvf_file + " did not contain a variable with 'obs' in the name."
    endif else begin
        message, "File " + input_gridded_uvf_file + " contained multiple variables with 'obs' in the name."
    endelse
endelse

apply_astrometry, new_obs_use, x_arr=meshgrid(new_obs_use.dimension, new_obs_use.elements, 1), $
    y_arr=meshgrid(new_obs_use.dimension, new_obs_use.elements, 2), ra_arr=new_ra_arr, dec_arr=new_dec_arr, /xy2ad

apply_astrometry, old_obs, x_arr=meshgrid(old_obs.dimension, old_obs.elements, 1), $
    y_arr=meshgrid(old_obs.dimension, old_obs.elements, 2), ra_arr=old_ra_arr, dec_arr=old_dec_arr, /xy2ad

apply_astrometry, new_obs_use, ra_arr=old_ra_arr, dec_arr=old_dec_arr, x_arr=x_new_old_pts, y_arr=y_new_old_pts, /ad2xy

restore, input_gridded_uvf_file
obs_out = new_obs_use

new_dirty_uv_arr = generate_interp_uv(old_obs, new_obs_use, dirty_uv_arr, x_new_old_pts, y_new_old_pts)
dirty_uv_arr = new_dirty_uv_arr
undefine_fhd, new_dirty_uv_arr

new_model_uv_arr = generate_interp_uv(old_obs, new_obs_use, model_uv_arr, x_new_old_pts, y_new_old_pts)
model_uv_arr = new_model_uv_arr
undefine_fhd, new_model_uv_arr

new_weights_uv_arr = generate_interp_uv(old_obs, new_obs_use, weights_uv_arr, x_new_old_pts, y_new_old_pts)
weights_uv_arr = new_weights_uv_arr
undefine_fhd, new_weights_uv_arr

new_variance_uv_arr = generate_interp_uv(old_obs, new_obs_use, variance_uv_arr, x_new_old_pts, y_new_old_pts)
variance_uv_arr = new_variance_uv_arr
undefine_fhd, new_variance_uv_arr

file_base = cgRootName(input_gridded_uvf_file, directory=filedir, extension=exten)
output_file = filedir + file_base + "_interp." + exten

save, obs_out, dirty_uv_arr, model_uv_arr, weights_uv_arr, variance_uv_arr, filename=output_file

end
