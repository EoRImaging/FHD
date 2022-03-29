FUNCTION load_diffuse_healpix_map, model_filepath, nside=nside, hpx_inds=hpx_inds, coord_use=coord_use, diffuse_spectral_index=diffuse_spectral_index

  var_dummy=getvar_savefile(model_filepath,names=var_names)
  ;save file must have one variable called 'hpx_inds', one called 'nside', and at least one other variable. If there are multiple other variables, it must be called 'model_arr'
  hpx_inds=getvar_savefile(model_filepath,'hpx_inds')
  nside=getvar_savefile(model_filepath,'nside')
  var_name_inds=where((StrLowCase(var_names) NE 'hpx_inds') AND (StrLowCase(var_names) NE 'nside'))
  var_names=var_names[var_name_inds]
  var_name_use=var_names[(where(StrLowCase(var_names) EQ 'model_arr',n_match))[0]>0] ;will pick 'model_arr' if present, or the first variable that is not 'hpx_inds' or 'nside'
  model_hpx_arr=getvar_savefile(model_filepath,var_name_use)

  model_spectra_i=where(StrLowCase(var_names) EQ 'model_spectral_arr',n_match)
  IF n_match GE 1 THEN diffuse_spectral_index=getvar_savefile(model_filepath,var_names[model_spectra_i])
  IF n_spectral LE 0 THEN undefine_fhd,diffuse_spectral_index

  coord_use=where(StrLowCase(var_names) EQ 'coord_sys',m_match) ;will pick 'coord_sys' if present
  IF m_match EQ 0 THEN coord_use = 'celestial' ELSE coord_use=getvar_savefile(model_filepath,'coord_sys')    ; Assume celestial if not defined otherwise

  IF size(diffuse_spectral_index,/type) EQ 10 THEN BEGIN ;check if pointer type
      ;need to write this!
      print,"A spectral index is defined in the saved diffuse model, but this is not yet supported!"
  ENDIF ;case of specifying a single scalar to be applied to the entire diffuse model is treated AFTER building the model in instrumental polarization

  RETURN, model_hpx_arr

END