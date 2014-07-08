FUNCTION fhd_save_io,param,file_path_fhd=file_path_fhd,pol_i=pol_i,status_str=status_str,compress=compress,name=name,text=text,restore=restore,obs=obs,reset=reset
IF N_Elements(compress) EQ 0 THEN compress=0
IF Keyword_Set(obs) THEN pol_names=obs.pol_names
status_path=filepath(base_name+'_status',root=base_path,subdir='metadata')
IF Keyword_Set(reset) THEN status_str={hdr:0,params:0,obs:0,beams:0,jones:0,cal:0,flags:0,autos:0,vis:intarr(4),vis_model:intarr(4),$
    uv_grid:intarr(4),uv_model_grid:intarr(4),mapfn:intarr(4),fhd:0,fhd_params:0,healpix_cube:0,hpx_even:0,hpx_odd:0}

IF N_Elements(status_str) EQ 0 THEN status_str=getvar_savefile(status_path+'.sav','status_str')

    
base_name=file_basename(file_path_fhd)
base_path=file_dirname(file_path_fhd)
CASE name OF 
    'hdr':BEGIN status_str.hdr=1 & var_name='hdr' & path_add='_hdr' & subdir='metadata' & obs_flag=0 & END
    'obs':BEGIN status_str.obs=1 &  var_name='obs' & path_add='_obs' & subdir='metadata' & obs_flag=0 & END
    'params':BEGIN status_str.params=1 & var_name='params' & path_add='_params' & subdir='metadata' & obs_flag=0 & END
    'beams':BEGIN status_str.beams=1 & var_name='psf' & path_add='_beams' & subdir='beams' & obs_flag=0 & END
    'jones':BEGIN status_str.jones=1 & var_name='jones' & path_add='_jones' & subdir='beams' & obs_flag=0 & END
    'cal':BEGIN status_str.cal=1 & var_name='cal' & path_add='_cal' & subdir='calibration' & obs_flag=0 & END
    'flags':BEGIN status_str.flags=1 & var_name='flag_arr' & path_add='_flags' & subdir='vis_data' & obs_flag=0 & END
    'autos':BEGIN status_str.autos=1 & var_name='auto_corr' & path_add='_autos' & subdir='vis_data' & obs_flag=1 & END
    'vis':BEGIN status_str.vis[pol_i]=1 & var_name='vis_ptr' & path_add='_vis_'+pol_names[pol_i] & subdir='vis_data' & obs_flag=1 & END
    'vis_model':BEGIN status_str.vis_model[pol_i]=1 & var_name='vis_ptr' & path_add='_vis_model_'+pol_names[pol_i] & subdir='vis_data' & obs_flag=1 & END
    'uv':BEGIN status_str.uv_grid[pol_i]=1 & var_name='grid_uv' & path_add='_uv_'+pol_names[pol_i] & subdir='grid_data' & obs_flag=0 & END
    'uv_model':BEGIN status_str.uv_model_grid[pol_i]=1 & var_name='grid_uv' & path_add='_uv_model_'+pol_names[pol_i] & subdir='grid_data' & obs_flag=0 & END
    'mapfn':BEGIN status_str.mapfn[pol_i]=1 & var_name='mapfn' & path_add='_mapfn_'+pol_names[pol_i] & subdir='mapfn' & obs_flag=0 & END
    'fhd_params':BEGIN status_str.fhd_params=1 & var_name='fhd' & path_add='_fhd_params' & subdir='deconvolution' & obs_flag=0 & END
    'fhd':BEGIN status_str.fhd=1 & path_add='_fhd' & subdir='deconvolution' & END
    'cube':BEGIN status_str.healpix_cube=1 & path_add='_cube' & subdir='healpix_cubes' & END
    'even_cube':BEGIN status_str.hpx_even=1 & path_add='_even_cube' & subdir='healpix_cubes' & END
    'odd_cube':BEGIN status_str.hpx_odd=1 & path_add='_odd_cube' & subdir='healpix_cubes' & END
ENDCASE

path_use=filepath(base_name+path_add+'.sav',root=base_path,subdir=subdir)

IF Keyword_Set(restore) THEN RETURN,getvar_savefile(path_use,var_name)

IF Keyword_Set(param) THEN BEGIN
CASE name OF 
    'hdr':BEGIN hdr=param
    'obs':BEGIN obs=param
    'params':BEGIN params=param
    'beams':BEGIN psf=param
    'jones':BEGIN jones=param
    'cal':BEGIN cal=param
    'flags':BEGIN flag_arr=param
    'autos':BEGIN auto_corr=param
    'vis':BEGIN vis_ptr=param
    'vis_model':BEGIN vis_ptr=param
    'uv':BEGIN grid_uv=param
    'uv_model':BEGIN grid_uv=param
    'mapfn':BEGIN mapfn=param
    'fhd_params':BEGIN fhd=param
    'fhd':
    'cube':
    'even_cube':
    'odd_cube':
ENDCASE
ENDIF

IF Keyword_Set(text) THEN TextFast,structure_to_text(status_str),/write,file_path=status_path $
    ELSE SAVE,status_str,filename=status_path+'.sav'


RETURN,status_str
END