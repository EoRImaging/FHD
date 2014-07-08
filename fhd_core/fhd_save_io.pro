PRO fhd_save_io,status_str,param,file_path_fhd=file_path_fhd,pol_i=pol_i,compress=compress,var_name=var_name,$
    text=text,restore=restore,obs=obs,reset=reset,force_set=force_set,no_save=no_save

IF ~Keyword_Set(file_path_fhd) THEN RETURN

IF N_Elements(compress) EQ 0 THEN compress=0
IF Keyword_Set(obs) THEN pol_names=obs.pol_names ;ELSE pol_names=['XX','YY','XY','YX','I','Q','U','V']

base_name=file_basename(file_path_fhd)
base_path=file_dirname(file_path_fhd)
status_path=filepath(base_name+'_status',root=base_path,subdir='metadata')
IF Keyword_Set(reset) THEN status_str={hdr:0,params:0,obs:0,beams:0,jones:0,cal:0,flags:0,autos:0,vis:intarr(4),vis_model:intarr(4),$
    grid_uv:intarr(4),weights_uv:intarr(4),grid_uv_model:intarr(4),mapfn:intarr(4),fhd:0,fhd_params:0,healpix_cube:intarr(4),hpx_even:intarr(4),hpx_odd:intarr(4)}

IF N_Elements(status_str) EQ 0 THEN status_str=getvar_savefile(status_path+'.sav','status_str')

status_use=status_str    
IF N_Elements(var_name) EQ 0 THEN var_name=''
CASE var_name OF ;listed in order typically generated
;    'hdr':BEGIN status_use.hdr=1 & path_add='_hdr' & subdir='metadata'& END
    'obs':BEGIN status_use.obs=1 & path_add='_obs' & subdir='metadata'& END
    'params':BEGIN status_use.params=1 & path_add='_params' & subdir='metadata'& END
    'psf':BEGIN status_use.beams=1 & path_add='_beams' & subdir='beams'& END
    'jones':BEGIN status_use.jones=1 & path_add='_jones' & subdir='beams'& END
    'cal':BEGIN status_use.cal=1 & path_add='_cal' & subdir='calibration'& END
    'flag_arr':BEGIN status_use.flags=1 & path_add='_flags' & subdir='vis_data'& END
    'auto_corr':BEGIN status_use.autos=1 & path_add='_autos' & subdir='vis_data' & obs_flag=1 & END
    'vis_ptr':BEGIN status_use.vis[pol_i]=1 & path_add='_vis_'+pol_names[pol_i] & subdir='vis_data' & obs_flag=1 & END
    'vis_model_ptr':BEGIN status_use.vis_model[pol_i]=1 & path_add='_vis_model_'+pol_names[pol_i] & subdir='vis_data' & obs_flag=1 & END
    'grid_uv':BEGIN status_use.grid_uv[pol_i]=1 & path_add='_uv_'+pol_names[pol_i] & subdir='grid_data'& END
    'weights_uv':BEGIN status_use.weights_uv[pol_i]=1 & path_add='_uv_weights_'+pol_names[pol_i] & subdir='grid_data'& END
    'grid_uv_model':BEGIN status_use.grid_uv_model[pol_i]=1 & path_add='_uv_model_'+pol_names[pol_i] & subdir='grid_data'& END
    'mapfn':BEGIN status_use.mapfn[pol_i]=1 & path_add='_mapfn_'+pol_names[pol_i] & subdir='mapfn'& END
    'fhd_params':BEGIN status_use.fhd_params=1 & path_add='_fhd_params' & subdir='deconvolution'& END
    'fhd':BEGIN status_use.fhd=1 & path_add='_fhd' & subdir='deconvolution' & END 
    'cube':BEGIN status_use.healpix_cube[pol_i]=1 & path_add='_cube' & subdir='healpix_cubes' & END
    'even_cube':BEGIN status_use.hpx_even[pol_i]=1 & path_add='_even_cube' & subdir='healpix_cubes' & END
    'odd_cube':BEGIN status_use.hpx_odd[pol_i]=1 & path_add='_odd_cube' & subdir='healpix_cubes' & END
    ELSE:name_error=1
ENDCASE

IF ~Keyword_Set(name_error) THEN BEGIN
    path_use=filepath(base_name+path_add+'.sav',root=base_path,subdir=subdir)
    
    IF Keyword_Set(restore) THEN BEGIN
        IF file_test(path_use) THEN param=getvar_savefile(path_use,var_name)
        RETURN
    ENDIF
    
    IF Keyword_Set(param) AND ~Keyword_Set(no_save) THEN BEGIN
        dir_use=file_dirname(path_use)
        IF file_test(dir_use) EQ 0 THEN file_mkdir,dir_use
        status_rename=Execute(var_name+'=param') ;rename the variable to be saved
        fn_string='SAVE,'+var_name
        IF Keyword_Set(obs_flag) THEN fn_string+=',obs'
        fn_string+=',filename="'+path_use+'",compress='+String(compress)
        status_save=Execute(fn_string,1,1)
    ENDIF ELSE status_save=0
    
    IF Keyword_Set(force_set) THEN status_save=1
    IF Keyword_Set(status_save) THEN status_str=status_use
ENDIF

SAVE,status_str,filename=status_path+'.sav'
IF Keyword_Set(text) THEN TextFast,structure_to_text(status_str),/write,file_path=status_path

END