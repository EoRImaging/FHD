FUNCTION fhd_setup,file_path_vis,status_str,export_images=export_images,cleanup=cleanup,recalculate_all=recalculate_all,$
    beam_recalculate=beam_recalculate,mapfn_recalculate=mapfn_recalculate,grid_recalculate=grid_recalculate,$
    n_pol=n_pol,flag_visibilities=flag_visibilities,deconvolve=deconvolve,transfer_mapfn=transfer_mapfn,$
    healpix_recalculate=healpix_recalculate,$
    file_path_fhd=file_path_fhd,force_data=force_data,force_no_data=force_no_data,$
    calibrate_visibilities=calibrate_visibilities,transfer_calibration=transfer_calibration,$
    weights_grid=weights_grid,save_visibilities=save_visibilities,$
    snapshot_healpix_export=snapshot_healpix_export,log_store=log_store
    
IF N_Elements(recalculate_all) EQ 0 THEN recalculate_all=1
IF N_Elements(calibrate_visibilities) EQ 0 THEN calibrate_visibilities=0
IF N_Elements(beam_recalculate) EQ 0 THEN beam_recalculate=recalculate_all
IF N_Elements(mapfn_recalculate) EQ 0 THEN mapfn_recalculate=recalculate_all
IF N_Elements(grid_recalculate) EQ 0 THEN grid_recalculate=recalculate_all
IF N_Elements(healpix_recalculate) EQ 0 THEN healpix_recalculate=0
IF N_Elements(flag_visibilities) EQ 0 THEN flag_visibilities=0
IF N_Elements(transfer_mapfn) EQ 0 THEN transfer_mapfn=0
IF N_Elements(save_visibilities) EQ 0 THEN save_visibilities=1

fhd_dir=file_dirname(file_path_fhd)
basename=file_basename(file_path_fhd)
flags_filepath=file_path_fhd+'_flags.sav'
obs_filepath=file_path_fhd+'_obs.sav'
params_filepath=file_path_fhd+'_params.sav'
fhd_filepath=file_path_fhd+'_fhd.sav'
cal_filepath=file_path_fhd+'_cal.sav'
;model_filepath=file_path_fhd+'_vis_cal.sav'
;IF Strpos(file_path_vis,'.sav') EQ -1 THEN file_path_vis_sav=file_path_vis+".sav" ELSE file_path_vis_sav=file_path_vis
IF N_Elements(deconvolve) EQ 0 THEN IF file_test(fhd_filepath) EQ 0 THEN deconvolve=1

pol_names=['XX','YY','XY','YX','I','Q','U','V'] ;HACK for now

IF Keyword_Set(n_pol) THEN n_pol1=n_pol ELSE n_pol1=1
test_mapfn=1 & FOR pol_i=0,n_pol1-1 DO test_mapfn*=file_test(file_path_fhd+'_uv_'+pol_names[pol_i]+'.sav')
IF test_mapfn EQ 0 THEN grid_recalculate=1
test_mapfn=1 & FOR pol_i=0,n_pol1-1 DO test_mapfn*=file_test(file_path_fhd+'_mapfn_'+pol_names[pol_i]+'.sav')
IF Keyword_Set(transfer_mapfn) THEN BEGIN
    IF size(transfer_mapfn,/type) NE 7 THEN transfer_mapfn=basename
    IF basename NE transfer_mapfn THEN BEGIN
        mapfn_recalculate=0
        test_mapfn=1
    ENDIF
ENDIF
IF test_mapfn EQ 0 THEN IF Keyword_Set(deconvolve) THEN mapfn_recalculate=1
IF Keyword_Set(mapfn_recalculate) THEN grid_recalculate=1

data_flag= ~(file_test(flags_filepath) AND file_test(obs_filepath) AND file_test(params_filepath)) ;test if the required data files are NOT already present

vis_file_list=file_search(file_path_fhd+'_vis*',count=vis_file_flag)
IF Keyword_Set(beam_recalculate) OR Keyword_Set(grid_recalculate) OR Keyword_Set(mapfn_recalculate) OR $
   (Keyword_Set(save_visibilities) AND (vis_file_flag EQ 0)) THEN data_flag=1

IF Keyword_Set(force_data) THEN data_flag=1
IF Keyword_Set(force_no_data) THEN data_flag=0
    
RETURN,data_flag
END