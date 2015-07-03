PRO fhd_save_io,status_str,param,file_path_fhd=file_path_fhd,pol_i=pol_i,compress=compress,var_name=var_name,$
    text=text,restore=restore,obs=obs,reset=reset,force_set=force_set,no_save=no_save,path_use=path_use,$
    transfer_filename=transfer_filename,sub_var_name=sub_var_name, compatibility_mode = compatibility_mode

IF ~Keyword_Set(file_path_fhd) THEN BEGIN
    file_path_fhd=''
    no_save=1
    restore=0
ENDIF

IF N_Elements(compress) EQ 0 THEN compress=0
IF Keyword_Set(obs) THEN pol_names=obs.pol_names ELSE pol_names=['XX','YY','XY','YX','I','Q','U','V']

IF Keyword_Set(transfer_filename) THEN BEGIN
    IF file_test(transfer_filename,/directory) THEN base_path=transfer_filename $
        ELSE base_name=transfer_filename 
ENDIF
IF N_Elements(base_name) EQ 0 THEN base_name=file_basename(file_path_fhd)
IF N_Elements(base_path) EQ 0 THEN base_path=file_dirname(file_path_fhd)

status_path=filepath(base_name+'_status',root=base_path,subdir='metadata')
IF size(status_str,/type) NE 8 THEN BEGIN
    IF ~file_test(status_path+'.sav') THEN BEGIN
        no_save=1
        reset=1
    ENDIF
ENDIF
IF Keyword_Set(restore) THEN no_save=1

IF Keyword_Set(reset) THEN status_str={hdr:0,params:0,obs:0,psf:0,antenna:0,jones:0,cal:0,source_array:0,flag_arr:0,auto_corr:0,$
    vis_ptr:intarr(4),vis_model_ptr:intarr(4),grid_uv:intarr(4),weights_uv:intarr(4),grid_uv_model:intarr(4),vis_count:0,$
    map_fn:intarr(4),fhd:0,fhd_params:0,hpx_cnv:0,healpix_cube:intarr(4),hpx_even:intarr(4),hpx_odd:intarr(4),complete:0}
IF size(status_str,/type) NE 8  THEN status_str=getvar_savefile(status_path+'.sav','status_str', compatibility_mode = compatibility_mode)
status_use=status_str
IF Keyword_Set(text) THEN BEGIN
    dir_use=file_dirname(status_path)
    IF file_test(dir_use) EQ 0 THEN file_mkdir,dir_use
    TextFast,structure_to_text(status_str),/write,file_path=status_path
ENDIF

IF N_Elements(var_name) EQ 0 THEN var_name='' ELSE var_name=StrLowCase(var_name)
IF var_name NE 'status_str' THEN IF not tag_exist(status_use,var_name) THEN RETURN

CASE var_name OF ;listed in order typically generated
    'status_str':BEGIN path_add='_status' & subdir='metadata' & END
    'hdr':BEGIN status_use.hdr=1 & path_add='_hdr' & subdir='metadata'& END
    'obs':BEGIN status_use.obs=1 & path_add='_obs' & subdir='metadata'& END
    'params':BEGIN status_use.params=1 & path_add='_params' & subdir='metadata'& END
    'psf':BEGIN status_use.psf=1 & path_add='_beams' & subdir='beams'& END
    'antenna':BEGIN status_use.antenna=1 & path_add='_antenna' & subdir='beams'& END
    'jones':BEGIN status_use.jones=1 & path_add='_jones' & subdir='beams'& END
    'cal':BEGIN status_use.cal=1 & path_add='_cal' & subdir='calibration'& END
    'source_array':BEGIN status_use.source_array=1 & path_add='_source_array' & subdir='output_data'& END
    'flag_arr':BEGIN status_use.flag_arr=1 & path_add='_flags' & subdir='vis_data'& END
    'auto_corr':BEGIN status_use.auto_corr=1 & path_add='_autos' & subdir='vis_data' & obs_flag=1 & END
    'vis_ptr':BEGIN status_use.vis_ptr[pol_i]=1 & path_add='_vis_'+pol_names[pol_i] & subdir='vis_data' & obs_flag=1 & END
    'vis_model_ptr':BEGIN status_use.vis_model_ptr[pol_i]=1 & path_add='_vis_model_'+pol_names[pol_i] & subdir='vis_data' & obs_flag=1 & END
    'grid_uv':BEGIN status_use.grid_uv[pol_i]=1 & path_add='_uv_'+pol_names[pol_i] & subdir='grid_data'& END
    'weights_uv':BEGIN status_use.weights_uv[pol_i]=1 & path_add='_uv_weights_'+pol_names[pol_i] & subdir='grid_data'& END
    'grid_uv_model':BEGIN status_use.grid_uv_model[pol_i]=1 & path_add='_uv_model_'+pol_names[pol_i] & subdir='grid_data'& END
    'vis_count':BEGIN status_use.vis_count=1 & path_add='_vis_count' & subdir='grid_data'& END
    'map_fn':BEGIN status_use.map_fn[pol_i]=1 & path_add='_mapfn_'+pol_names[pol_i] & subdir='mapfn'& END
    'fhd_params':BEGIN status_use.fhd_params=1 & path_add='_fhd_params' & subdir='deconvolution'& END
    'fhd':BEGIN status_use.fhd=1 & path_add='_fhd' & subdir='deconvolution' & END 
    'hpx_cnv':BEGIN status_use.hpx_cnv=1 & path_add='_hpxcnv' & subdir='Healpix' & END
    'healpix_cube':BEGIN status_use.healpix_cube[pol_i]=1 & path_add='_cube'+pol_names[pol_i] & subdir='Healpix' & END
    'hpx_even':BEGIN status_use.hpx_even[pol_i]=1 & path_add='_even_cube'+pol_names[pol_i] & subdir='Healpix' & END
    'hpx_odd':BEGIN status_use.hpx_odd[pol_i]=1 & path_add='_odd_cube'+pol_names[pol_i] & subdir='Healpix' & END
    ELSE:name_error=1
ENDCASE

var_name_use=var_name
;IF Keyword_Set(compatibility_mode) THEN BEGIN
;    subdir=''
;    status_path=filepath(base_name+'_status',root=base_path,subdir='')
;    CASE var_name OF
;        'fhd_params':var_name_use='fhd'
;        'weights_uv':path_add='_uv_'+pol_names[pol_i]
;        'healpix_cube': path_add='_cube'
;        'hpx_even': path_add='_even_cube'
;        'hpx_odd': path_add='_odd_cube'
;        ELSE:
;    ENDCASE
;ENDIF 
IF ~Keyword_Set(name_error) THEN BEGIN
    path_use=filepath(base_name+path_add,root=base_path,subdir=subdir)
    path_sav=path_use+'.sav'
;    IF Keyword_Set(compatibility_mode) THEN IF file_test(path_sav) THEN $
;        fhd_save_io,status_str,pol_i=pol_i,var_name=var_name,/force_set

    IF Keyword_Set(restore) THEN BEGIN
        IF Keyword_Set(sub_var_name) THEN var_name_use=sub_var_name 
        IF file_test(path_sav) THEN param=getvar_savefile(path_sav,var_name_use, compatibility_mode = compatibility_mode)
        RETURN
    ENDIF
    
    IF Keyword_Set(param) AND ~Keyword_Set(no_save) THEN BEGIN
        dir_use=file_dirname(path_sav)
        IF file_test(dir_use) EQ 0 THEN file_mkdir,dir_use
        status_rename=Execute(var_name_use+'=param') ;rename the variable to be saved
        fn_string='SAVE,'+var_name_use
        IF Keyword_Set(obs_flag) THEN fn_string+=',obs'
        fn_string+=',filename="'+path_sav+'",compress='+String(compress)
        status_save=Execute(fn_string,1,1)
    ENDIF ELSE status_save=0
    
    IF Keyword_Set(force_set) THEN status_save=1
    IF Keyword_Set(status_save) THEN status_str=status_use
ENDIF ELSE status_save=0

IF ~Keyword_Set(file_path_fhd) THEN status_save=0
IF Keyword_Set(status_save) THEN BEGIN
    dir_use=file_dirname(status_path)
    IF file_test(dir_use) EQ 0 THEN file_mkdir,dir_use
    SAVE,status_str,filename=status_path+'.sav'
ENDIF
END