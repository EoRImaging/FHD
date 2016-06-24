FUNCTION fhd_struct_combine_skymodel, obs, skymodel_cal, skymodel_update, calibration_flag=calibration_flag

IF N_Elements(skymodel_cal) EQ 0 THEN RETURN,skymodel_update
IF N_Elements(skymodel_update) EQ 0 THEN RETURN,skymodel_cal
IF N_Elements(calibration_flag) EQ 0 THEN calibration_flag=1 ;set to indicate skymodel_cal really is the calibration model

source_list=source_list_append(obs,skymodel_update.source_list,skymodel_cal.source_list)
IF Keyword_Set(skymodel_update.diffuse_model) THEN BEGIN
    diffuse_model=skymodel_update.diffuse_model
    diffuse_spectral_index=skymodel_update.diffuse_spectral_index
ENDIF ELSE BEGIN
    diffuse_model=skymodel_cal.diffuse_model
    diffuse_spectral_index=skymodel_cal.diffuse_spectral_index
ENDELSE
IF Keyword_Set(skymodel_update.galaxy_model) THEN BEGIN
    galaxy_model=skymodel_update.galaxy_model
    galaxy_spectral_index=skymodel_update.galaxy_spectral_index
ENDIF ELSE BEGIN
    galaxy_model=skymodel_cal.galaxy_model
    galaxy_spectral_index=skymodel_cal.galaxy_spectral_index
ENDELSE
IF Keyword_Set(skymodel_update.catalog_name) THEN BEGIN
    catalog_path=skymodel_update.catalog_name
ENDIF ELSE BEGIN
    catalog_path=skymodel_cal.catalog_name
ENDELSE

skymodel=fhd_struct_init_skymodel(obs,source_list=source_list,$
    return_cal=calibration_flag,catalog_path=catalog_path,$
    galaxy_model=galaxy_model,galaxy_spectral_index=galaxy_spectral_index,$
    diffuse_model=diffuse_model,diffuse_spectral_index=diffuse_spectral_index)

RETURN,skymodel
END