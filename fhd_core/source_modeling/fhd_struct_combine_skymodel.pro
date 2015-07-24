FUNCTION fhd_struct_combine_skymodel, obs, skymodel_cal, skymodel_update, calibration_flag=calibration_flag

IF N_Elements(calibration_flag) EQ 0 THEN calibration_flag=1 ;set to indicate skymodel_cal really is the calibration model

skymodel=fhd_struct_init_skymodel(obs,return_cal=calibration_flag)

RETURN,skymodel
END