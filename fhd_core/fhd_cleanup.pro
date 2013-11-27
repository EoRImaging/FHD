PRO fhd_cleanup,file_path_fhd,cleanup_all=cleanup_all

pol_names=['xx','yy','xy','yx','I','Q','U','V']

hdr_filepath=file_path_fhd+'_hdr.sav' & file_path_list=Strarr(1)+hdr_filepath
autos_filepath=file_path_fhd+'_autos.sav' & file_path_list=[file_path_list,autos_filepath]
cal_hist_filepath=file_path_fhd+'_cal_hist.sav' & file_path_list=[file_path_list,cal_hist_filepath]

FOR pol_i=0,3 DO BEGIN mapfn_filepath=file_path_fhd+'_mapfn_'+pol_names[pol_i]+'.sav' & file_path_list=[file_path_list,mapfn_filepath] & ENDFOR

hpx_filepath=file_path_fhd+'_hpxcnv.sav' & file_path_list=[file_path_list,hpx_filepath]
IF Keyword_Set(cleanup_all) THEN BEGIN
    params_filepath=file_path_fhd+'_params.sav' & file_path_list=[file_path_list,params_filepath]
    beams_filepath=file_path_fhd+'_beams.sav' & file_path_list=[file_path_list,beams_filepath]
    cal_filepath=file_path_fhd+'_cal.sav' & file_path_list=[file_path_list,cal_filepath]
    FOR pol_i=0,3 DO BEGIN vis_filepath=file_path_fhd+'_vis_'+pol_names[pol_i]+'.sav' & file_path_list=[file_path_list,vis_filepath] & ENDFOR
    settings_filepath=file_path_fhd+'_settings.txt' & file_path_list=[file_path_list,settings_filepath]
    flags_filepath=file_path_fhd+'_flags.sav' & file_path_list=[file_path_list,flags_filepath]
    obs_filepath=file_path_fhd+'_obs.sav' & file_path_list=[file_path_list,obs_filepath]
    fhd_filepath=file_path_fhd+'_fhd.sav' & file_path_list=[file_path_list,fhd_filepath] 
    fhd_params_filepath=file_path_fhd+'_fhd_params.sav' & file_path_list=[file_path_list,fhd_params_filepath]
    FOR pol_i=0,3 DO BEGIN uv_filepath=file_path_fhd+'_uv_'+pol_names[pol_i]+'.sav' & file_path_list=[file_path_list,uv_filepath] & ENDFOR
ENDIF

n_paths=N_Elements(file_path_list)
FOR i=0,n_paths-1 DO file_delete,file_path_list[i],/allow_nonexistent
END