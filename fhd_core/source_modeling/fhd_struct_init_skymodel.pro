FUNCTION fhd_struct_init_skymodel,obs,source_list=source_list,catalog_path=catalog_path,$
    galaxy_model=galaxy_model,galaxy_spectral_index=galaxy_spectral_index,$
    diffuse_model=diffuse_model,diffuse_spectral_index=diffuse_spectral_index,$
    calibration_flag=calibration_flag,_Extra=extra

IF N_Elements(calibration_flag) EQ 0 THEN calibration_flag=0
IF Keyword_Set(galaxy_model) THEN galaxy_flag=1 ELSE galaxy_flag=0
IF Keyword_Set(diffuse_model) THEN diffuse_filepath=diffuse_model ELSE diffuse_filepath=''
IF Keyword_Set(catalog_path) THEN catalog_path_use=file_basename(catalog_path,'.sav',/fold_case) ELSE catalog_path_use=''
n_src=N_Elements(source_list)
IF n_src EQ 0 THEN source_list=source_comp_init(n_sources=0,freq=obs.freq_center)
IF N_Elements(galaxy_spectral_index) EQ 0 THEN galaxy_spectral_index=!Values.F_NAN
IF N_Elements(diffuse_spectral_index) EQ 0 THEN diffuse_spectral_index=!Values.F_NAN

skymodel={include_calibration:calibration_flag,$
    n_sources:n_src,source_list:source_list,catalog_name:catalog_path_use,$
    galaxy_model:galaxy_flag,galaxy_spectral_index:galaxy_spectral_index,$
    diffuse_model:diffuse_filepath,diffuse_spectral_index:diffuse_spectral_index}
RETURN,skymodel
END
