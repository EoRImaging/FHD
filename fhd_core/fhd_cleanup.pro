PRO fhd_cleanup,file_path_fhd,all=all

header_filepath=file_path_fhd+'_header.sav' & file_path_list=Strarr(1)+header_filepath
flags_filepath=file_path_fhd+'_flags.sav' & file_path_list=[file_path_list,flags_filepath]
vis_filepath=file_path_fhd+'_vis.sav' & file_path_list=[file_path_list,vis_filepath]
IF Keyword_Set(all) THEN BEGIN obs_filepath=file_path_fhd+'_obs.sav' & file_path_list=[file_path_list,obs_filepath] & ENDIF
params_filepath=file_path_fhd+'_params.sav' & file_path_list=[file_path_list,params_filepath]
hdr_filepath=file_path_fhd+'_hdr.sav' & file_path_list=[file_path_list,hdr_filepath]
beams_filepath=file_path_fhd+'_beams.sav' & file_path_list=[file_path_list,beams_filepath]
hpx_filepath=file_path_fhd+'_hpxcnv.sav' & file_path_list=[file_path_list,hpx_filepath]
fhd_filepath=file_path_fhd+'_fhd.sav' & file_path_list=[file_path_list,fhd_filepath]
IF Keyword_Set(all) THEN BEGIN fhd_params_filepath=file_path_fhd+'_fhd_params.sav' & file_path_list=[file_path_list,fhd_params_filepath] & ENDIF
output_filepath=file_path_fhd+'_output.sav' & file_path_list=[file_path_list,output_filepath]

pol_names=['xx','yy','xy','yx','I','Q','U','V']
FOR pol_i=0,3 DO BEGIN
    IF Keyword_Set(all) THEN BEGIN uv_filepath=file_path_fhd+'_uv_'+pol_names[pol_i]+'.sav' & file_path_list=[file_path_list,uv_filepath] & ENDIF
    dirty_filepath=file_path_fhd+'_dirty_'+pol_names[pol_i]+'.sav' & file_path_list=[file_path_list,dirty_filepath]
    mapfn_filepath=file_path_fhd+'_mapfn_'+pol_names[pol_i]+'.sav' & file_path_list=[file_path_list,mapfn_filepath]
ENDFOR

n_paths=N_Elements(file_path_list)
FOR i=0,n_paths-1 DO file_delete,file_path_list[i],/allow_nonexistent
END