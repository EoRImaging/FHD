
;+
; :Description:
;    uvfits2fhd is the main program for working with uvfits data. 
;    It will read the uvfits file, grid the data, generate the holographic mapping functions, 
;    and run Fast Holographic Deconvolution
;
; Test line!
;
; :Keywords:
;    data_directory - working directory
;    
;    filename - uvfits filename, omitting the .uvfits extension. 
;       If the data is already calibrated, it should end with _cal.uvfits instead of just .uvfits
;    
;    mapfn_recalculate - if not set to 0, will generate Holographic Mapping Functions for each polarization
;    
;    dimension - desired dimension in pixels of the final images
;    
;    kbinsize - pixel size in wavelengths of the uv image. 
;    
;    n_pol - 1: use xx only, 2: use xx and yy, 4: use xx, yy, xy, and yx (Default: as many as are available)
;    
;    flag_visibilities - set to look for anomalous visibility data and update flags 
;    
;    Extra - pass any non-default parameters to fast_holographic_deconvolution through this parameter 
;
; :Author: isullivan 2012
;-
PRO fhd_main,file_path_vis,status_str,export_images=export_images,cleanup=cleanup,recalculate_all=recalculate_all,$
    mapfn_recalculate=mapfn_recalculate,grid_recalculate=grid_recalculate,$
    n_pol=n_pol,flag_visibilities=flag_visibilities,silent=silent,GPU_enable=GPU_enable,deconvolve=deconvolve,transfer_mapfn=transfer_mapfn,$
    healpix_recalculate=healpix_recalculate,tile_flag_list=tile_flag_list,$
    file_path_fhd=file_path_fhd,force_data=force_data,force_no_data=force_no_data,freq_start=freq_start,freq_end=freq_end,$
    calibrate_visibilities=calibrate_visibilities,transfer_calibration=transfer_calibration,error=error,$
    calibration_catalog_file_path=calibration_catalog_file_path,dft_threshold=dft_threshold,$
    calibration_image_subtract=calibration_image_subtract,calibration_visibilities_subtract=calibration_visibilities_subtract,$
    weights_grid=weights_grid,save_visibilities=save_visibilities,return_cal_visibilities=return_cal_visibilities,$
    return_decon_visibilities=return_decon_visibilities,snapshot_healpix_export=snapshot_healpix_export,cmd_args=cmd_args,log_store=log_store,$
    generate_vis_savefile=generate_vis_savefile,model_visibilities=model_visibilities,model_catalog_file_path=model_catalog_file_path,$
    transfer_flags=transfer_flags,flag_calibration=flag_calibration, production=production,_Extra=extra

compile_opt idl2,strictarrsubs    
except=!except
!except=0 ;System variable that controls when math errors are printed. Set to 0 to disable.
error=0
heap_gc 
t0=Systime(1)

print,"Processing "+file_basename(file_path_vis)+" in "+file_dirname(file_path_vis)
print,systime()
print,'Output file_path:',file_path_fhd

log_filepath=file_path_fhd+'_log.txt'
IF Keyword_Set(!Journal) THEN journal

data_flag=fhd_setup(file_path_vis,status_str,export_images=export_images,cleanup=cleanup,recalculate_all=recalculate_all,$
    mapfn_recalculate=mapfn_recalculate,grid_recalculate=grid_recalculate,$
    n_pol=n_pol,flag_visibilities=flag_visibilities,deconvolve=deconvolve,transfer_mapfn=transfer_mapfn,$
    transfer_flags=transfer_flags,healpix_recalculate=healpix_recalculate,$
    file_path_fhd=file_path_fhd,force_data=force_data,force_no_data=force_no_data,$
    calibrate_visibilities=calibrate_visibilities,transfer_calibration=transfer_calibration,$
    weights_grid=weights_grid,save_visibilities=save_visibilities,$
    snapshot_healpix_export=snapshot_healpix_export,log_store=log_store,_Extra=extra)

IF data_flag LE 0 THEN BEGIN
    IF Keyword_Set(log_store) THEN Journal,log_filepath
    fhd_save_io,status_str,file_path_fhd=file_path_fhd,/reset
    
    uvfits_read,hdr,params,vis_arr,flag_arr,file_path_vis=file_path_vis,n_pol=n_pol,silent=silent,error=error,_Extra=extra
    IF Keyword_Set(error) THEN BEGIN
        print,"Error occured while reading uvfits data. Returning."
        RETURN
    ENDIF
    IF Keyword_Set(generate_vis_savefile) THEN BEGIN
        IF Strpos(file_path_vis,'.sav') EQ -1 THEN file_path_vis_sav=file_path_vis+".sav" ELSE file_path_vis_sav=file_path_vis
        SAVE,vis_arr,flag_arr,hdr,params,filename=file_path_vis_sav
        timing=Systime(1)-t0
        IF ~Keyword_Set(silent) THEN print,'Processing time (minutes): ',Strn(Round(timing/60.))
        RETURN
    ENDIF
    obs=fhd_struct_init_obs(file_path_vis,hdr,params,n_pol=n_pol,dft_threshold=dft_threshold,_Extra=extra)
    n_pol=obs.n_pol
    n_freq=obs.n_freq
    fhd_save_io,status_str,obs,var='obs',/compress,file_path_fhd=file_path_fhd,_Extra=extra ;save obs structure right away for debugging. Will be overwritten a few times before the end 
       
    ;Read in or construct a new beam model. Also sets up the structure PSF
    print,'Calculating beam model'
    psf=beam_setup(obs,status_str,antenna,file_path_fhd=file_path_fhd,restore_last=0,silent=silent,timing=t_beam,no_save=no_save,_Extra=extra)
    IF Keyword_Set(t_beam) THEN IF ~Keyword_Set(silent) THEN print,'Beam modeling time: ',t_beam
    jones=fhd_struct_init_jones(obs,status_str,file_path_fhd=file_path_fhd,restore=0,mask=beam_mask,_Extra=extra)
    
    IF Keyword_Set(transfer_flags) THEN BEGIN
        flag_visibilities=0 ;
        transfer_flag_data,flag_arr,obs,status_str,params,file_path_fhd=file_path_fhd,$
            transfer_filename=transfer_flags,error=error,flag_visibilities=flag_visibilities,$
            flag_calibration=flag_calibration,_Extra=extra
        IF Keyword_Set(error) THEN BEGIN
            print,"Error occured while attempting to transfer flags. Returning."
            RETURN
        ENDIF
    ENDIF
    
    flag_arr=vis_flag_basic(flag_arr,obs,params,n_pol=n_pol,n_freq=n_freq,freq_start=freq_start,$
        freq_end=freq_end,tile_flag_list=tile_flag_list,vis_ptr=vis_arr,_Extra=extra)
    vis_flag_update,flag_arr,obs,psf,params,_Extra=extra
    
    IF Keyword_Set(calibrate_visibilities) THEN BEGIN
        IF Keyword_Set(calibration_catalog_file_path) THEN catalog_use=calibration_catalog_file_path
        IF ~Keyword_Set(calibration_source_list) THEN $
            calibration_source_list=generate_source_cal_list(obs,psf,catalog_path=catalog_use,_Extra=extra)        
        cal=fhd_struct_init_cal(obs,params,source_list=calibration_source_list,$
            catalog_path=catalog_use,transfer_calibration=transfer_calibration,_Extra=extra)
    ENDIF
    
    ;print informational messages
    obs_status,obs
    fhd_log_settings,file_path_fhd,obs=obs,psf=psf,cal=cal,antenna=antenna,cmd_args=cmd_args,/overwrite,sub_dir='metadata'  ;write preliminary settings file for debugging, in case later steps crash
    
    IF Keyword_Set(transfer_calibration) THEN BEGIN
        calibrate_visibilities=1
        IF size(transfer_calibration,/type) LT 7 THEN transfer_calibration=file_path_fhd ;this will not modify string or structure types, but will over-write it if set to a numerical type
    ENDIF
    
    IF Keyword_Set(calibrate_visibilities) THEN BEGIN
        print,"Calibrating visibilities"
        IF Keyword_Set(calibration_visibilities_subtract) THEN calibration_image_subtract=0
        IF Keyword_Set(calibration_image_subtract) THEN return_cal_visibilities=1
        vis_arr=vis_calibrate(vis_arr,cal,obs,status_str,psf,params,jones,flag_ptr=flag_arr,file_path_fhd=file_path_fhd,$
             transfer_calibration=transfer_calibration,timing=cal_timing,error=error,model_uv_arr=model_uv_arr,$
             return_cal_visibilities=return_cal_visibilities,vis_model_arr=vis_model_arr,$
             calibration_visibilities_subtract=calibration_visibilities_subtract,silent=silent,$
             flag_calibration=flag_calibration,_Extra=extra)
        IF ~Keyword_Set(silent) THEN print,String(format='("Calibration timing: ",A)',Strn(cal_timing))
        IF Keyword_Set(error) THEN BEGIN
            print,"Error occured during calibration. Returning."
            RETURN
        ENDIF
        fhd_save_io,status_str,cal,var='cal',/compress,file_path_fhd=file_path_fhd,_Extra=extra
        vis_flag_update,flag_arr,obs,psf,params,_Extra=extra
    ENDIF
    
    IF Keyword_Set(flag_visibilities) THEN BEGIN
        print,'Flagging anomalous data'
        vis_flag,vis_arr,flag_arr,obs,psf,params,_Extra=extra
        fhd_save_io,status_str,flag_arr,var='flag_arr',/compress,file_path_fhd=file_path_fhd,_Extra=extra
    ENDIF ELSE $ ;saved flags are needed for some later routines, so save them even if no additional flagging is done
        fhd_save_io,status_str,flag_arr,var='flag_arr',/compress,file_path_fhd=file_path_fhd,_Extra=extra
    
    IF Keyword_Set(model_visibilities) THEN BEGIN
        IF Keyword_Set(model_catalog_file_path) THEN BEGIN
            model_source_list=generate_source_cal_list(obs,psf,catalog_path=model_catalog_file_path,/model_visibilities,_Extra=extra) 
            IF Keyword_Set(return_cal_visibilities) OR Keyword_Set(calibration_visibilities_subtract) THEN BEGIN
                model_source_list=source_list_append(obs,model_source_list,cal.source_list,/exclude)
                source_array=source_list_append(obs,model_source_list,cal.source_list)
            ENDIF ELSE source_array=model_source_list
        ENDIF ELSE IF N_Elements(model_source_list) GT 0 THEN source_array=model_source_list 
        vis_model_arr=vis_source_model(model_source_list,obs,status_str,psf,params,flag_arr,0,jones,model_uv_arr=model_uv_arr2,$
            timing=model_timing,silent=silent,error=error,vis_model_ptr=vis_model_arr,calibration_flag=0,_Extra=extra) 
        IF Min(Ptr_valid(model_uv_arr)) GT 0 THEN FOR pol_i=0,n_pol-1 DO *model_uv_arr[pol_i]+=*model_uv_arr2[pol_i] $
            ELSE model_uv_arr=Pointer_copy(model_uv_arr2) 
        undefine_fhd,model_uv_arr2
    ENDIF ELSE IF Keyword_Set(calibrate_visibilities) THEN source_array=cal.source_list
    IF N_Elements(vis_model_arr) LT n_pol THEN vis_model_arr=Ptrarr(n_pol) ;supply as array of null pointers to allow it to be indexed, but not be used
    model_flag=min(Ptr_valid(vis_model_arr))
    
    IF Keyword_Set(error) THEN BEGIN
        print,"Error occured during source modeling"
        RETURN
    ENDIF
    
    IF N_Elements(source_array) GT 0 THEN BEGIN
        fhd_save_io,status_str,source_array,var='source_array',/compress,file_path_fhd=file_path_fhd,path_use=path_use,_Extra=extra 
        source_array_export,source_array,obs,file_path=path_use
    ENDIF
    
    vis_noise_calc,obs,vis_arr,flag_arr
    IF ~Keyword_Set(silent) THEN flag_status,obs
    fhd_save_io,status_str,obs,var='obs',/compress,file_path_fhd=file_path_fhd,_Extra=extra
    fhd_save_io,status_str,params,var='params',/compress,file_path_fhd=file_path_fhd,_Extra=extra
    fhd_log_settings,file_path_fhd,obs=obs,psf=psf,cal=cal,antenna=antenna,cmd_args=cmd_args,/overwrite,sub_dir='metadata'
    undefine_fhd,antenna
    auto_corr=vis_extract_autocorr(obs,status_str,vis_arr=vis_arr,file_path_fhd=file_path_fhd,_Extra=extra)
    
    IF obs.n_vis EQ 0 THEN BEGIN
        print,"All data flagged! Returning."
        error=1
        IF Keyword_Set(!Journal) THEN Journal ;write and close log file if present
        RETURN
    ENDIF    

    IF Keyword_Set(return_decon_visibilities) THEN save_visibilities=1
    IF Keyword_Set(save_visibilities) THEN BEGIN
        t_save0=Systime(1)
        vis_export,obs,status_str,vis_arr,flag_arr,file_path_fhd=file_path_fhd,/compress
        IF Keyword_Set(model_flag) THEN vis_export,obs,status_str,vis_model_arr,flag_arr,file_path_fhd=file_path_fhd,/compress,/model
        t_save=Systime(1)-t_save0
        IF ~Keyword_Set(silent) THEN print,'Visibility save time: ',t_save
    ENDIF
    
    ;Grid the visibilities
    IF Keyword_Set(grid_recalculate) THEN BEGIN
        image_uv_arr=visibility_grid_wrap(vis_arr,flag_arr,obs,status_str,psf,params,file_path_fhd=file_path_fhd,vis_model_arr=vis_model_arr,$
            deconvolve=deconvolve,model_flag=model_flag,snapshot_healpix_export=snapshot_healpix_export,mapfn_recalculate=mapfn_recalculate,$
            save_visibilities=save_visibilities,error=error,no_save=no_save,weights_arr=weights_arr,model_uv_holo=model_uv_holo,_Extra=extra)
    ENDIF ELSE BEGIN
        print,'Visibilities not re-gridded'
    ENDELSE
    IF ~Keyword_Set(snapshot_healpix_export) THEN Ptr_free,vis_arr,flag_arr,vis_model_arr
    IF Keyword_Set(!Journal) THEN Journal ;write and close log file if present
ENDIF

IF N_Elements(cal) EQ 0 THEN fhd_save_io,status_str,cal,var='cal',/restore,file_path_fhd=file_path_fhd,_Extra=extra
IF N_Elements(obs) EQ 0 THEN fhd_save_io,status_str,obs,var='obs',/restore,file_path_fhd=file_path_fhd,_Extra=extra
;deconvolve point sources using fast holographic deconvolution
IF Keyword_Set(deconvolve) THEN BEGIN
    print,'Deconvolving point sources'
    fhd_wrap,obs,status_str,psf,params,fhd_params,cal,jones,file_path_fhd=file_path_fhd,silent=silent,calibration_image_subtract=calibration_image_subtract,$
        transfer_mapfn=transfer_mapfn,map_fn_arr=map_fn_arr,image_uv_arr=image_uv_arr,weights_arr=weights_arr,$
        vis_model_arr=vis_model_arr,return_decon_visibilities=return_decon_visibilities,model_uv_arr=model_uv_arr,$
        log_store=log_store,flag_arr=flag_arr,_Extra=extra
    IF Keyword_Set(return_decon_visibilities) AND Keyword_Set(save_visibilities) THEN vis_export,obs,status_str,vis_model_arr,flag_arr,file_path_fhd=file_path_fhd,/compress,/model
ENDIF ELSE BEGIN
    print,'Gridded visibilities not deconvolved'
ENDELSE

;Generate fits data files and images
IF Keyword_Set(export_images) THEN BEGIN
    IF status_str.fhd GT 0 THEN BEGIN
        fhd_output,obs,status_str,fhd_params,cal,jones,file_path_fhd=file_path_fhd,map_fn_arr=map_fn_arr,silent=silent,transfer_mapfn=transfer_mapfn,$
            image_uv_arr=image_uv_arr,weights_arr=weights_arr,beam_arr=beam_arr,_Extra=extra 
    ENDIF ELSE BEGIN
        IF (obs.residual GT 0) AND (N_Elements(cal) GT 0) THEN source_array=cal.source_list
        fhd_quickview,obs,status_str,psf,cal,jones,image_uv_arr=image_uv_arr,weights_arr=weights_arr,source_array=source_array,$
            model_uv_holo=model_uv_holo,beam_arr=beam_arr,file_path_fhd=file_path_fhd,silent=silent,_Extra=extra
    ENDELSE
ENDIF

;optionally export frequency-splt Healpix cubes
IF Keyword_Set(snapshot_healpix_export) THEN healpix_snapshot_cube_generate,obs,status_str,psf,cal,params,vis_arr,$
    vis_model_arr=vis_model_arr,file_path_fhd=file_path_fhd,flag_arr=flag_arr,cmd_args=cmd_args,_Extra=extra

;Optionally fill the fhd table on the mwa_qc database located on eor-00 under the mwa username. See the python script 
;for more information about possible queries.
IF Keyword_Set(production) THEN BEGIN
  ;Set up paths and keywords for the database filler, including the complete tag
  IF ~Keyword_Set(error) THEN complete='1'
  descr_file_path=file_path_fhd+'_settings.txt'
  descr_file_path=filepath(file_basename(descr_file_path),root=file_dirname(descr_file_path),sub="metadata")

  ;Spawn a child process to run the database filler, located in MWA_Tools
  SPAWN, 'fhd_database_filler.py -s ' + descr_file_path + ' -o ' + obs.obsname + ' -c ' + obs.code_version + $
      ' -p ' + file_path_fhd + ' -m ' + complete
ENDIF

undefine_fhd,map_fn_arr,image_uv_arr,weights_arr,model_uv_arr,vis_arr,flag_arr,vis_model_arr
undefine_fhd,obs,cal,jones,psf,antenna,fhd_params
;;generate images showing the uv contributions of each tile. Very helpful for debugging!
;print,'Calculating individual tile uv coverage'
timing=Systime(1)-t0
IF ~Keyword_Set(silent) THEN print,'Full pipeline time (minutes): ',Strn(Round(timing/60.))
print,''
!except=except
END
