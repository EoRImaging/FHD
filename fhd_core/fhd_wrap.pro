;+
; :Description:
;    Sets up default values for Fast Holographic Deconvolution, loads previously-gridded visibility data, and calls fast_holographic_deconvolution.pro 
;
; :Params:
;    obs - structure containing details of the observation
;    
;    psf - structure containing the high-resolution gridded beam model.
;
; :Keywords:
;    beam_threshold
;
; :Author: isullivan May 6, 2012
;-
PRO fhd_wrap,obs,status_str,psf,params,fhd_params,cal,jones,file_path_fhd=file_path_fhd,beam_threshold=beam_threshold,quickview=quickview,$
    data_directory=data_directory,filename=filename,version=version,silent=silent,transfer_mapfn=transfer_mapfn,$
    map_fn_arr=map_fn_arr,GPU_enable=GPU_enable,image_uv_arr=image_uv_arr,weights_arr=weights_arr,$
    calibration_image_subtract=calibration_image_subtract,model_uv_arr=model_uv_arr,$
    vis_model_ptr=vis_model_ptr,return_decon_visibilities=return_decon_visibilities,flag_arr=flag_arr,log_store=log_store,_Extra=extra

;snapshot data must have been gridded previously, and the Holo map fns generated
;reads and deconvolves simultaneously on multiple polarizations, time intervals, and frequencies
;multiple time intervals and frequencies are NOT SUPPORTED 
;(mostly just need to generate their UV grid and map_fn files)
;
heap_gc
compile_opt idl2,strictarrsubs  

IF Keyword_Set(!Journal) THEN journal
IF Keyword_Set(log_store) THEN journal,file_path_fhd+'_fhd_log.txt'
IF N_Elements(obs) EQ 0 THEN fhd_save_io,status_str,obs,var='obs',/restore,file_path_fhd=file_path_fhd
IF N_Elements(params) EQ 0 THEN fhd_save_io,status_str,params,var='params',/restore,file_path_fhd=file_path_fhd
IF size(cal,/type) NE 8 THEN $
    IF status_str.cal GT 0 THEN fhd_save_io,status_str,cal,var='cal',/restore,file_path_fhd=file_path_fhd $ 
        ELSE cal=fhd_struct_init_cal(obs,params)

IF N_Elements(jones) EQ 0 THEN jones=fhd_struct_init_jones(obs,file_path_fhd=file_path_fhd,/restore)

fhd_params=fhd_init(obs,cal,calibration_image_subtract=calibration_image_subtract,transfer_mapfn=transfer_mapfn,file_path_fhd=file_path_fhd,_Extra=extra)

n_pol=fhd_params.npol

IF N_Elements(psf) EQ 0 THEN psf=beam_setup(obs,file_path_fhd,/restore_last) 

;set to fit each polarization independantly 
;(only flux is independant, all locations are from Stokes I). 
;Otherwise they are fit to Stokes I and U only 
IF N_Elements(image_uv_arr) EQ 0 THEN BEGIN
    image_uv_arr=Ptrarr(n_pol,/allocate)
    FOR pol_i=0,n_pol-1 DO BEGIN
        fhd_save_io,status_str,grid_uv,var='grid_uv',/restore,file_path_fhd=file_path_fhd,obs=obs,pol_i=pol_i
        *image_uv_arr[pol_i]=grid_uv
    ENDFOR
ENDIF
IF N_Elements(weights_arr) EQ 0 THEN BEGIN
    weights_arr=Ptrarr(n_pol,/allocate)
    FOR pol_i=0,n_pol-1 DO BEGIN
        fhd_save_io,status_str,weights_uv,var='weights_uv',/restore,file_path_fhd=file_path_fhd,obs=obs,pol_i=pol_i
        *weights_arr[pol_i]=weights_uv
    ENDFOR
ENDIF
IF Keyword_Set(calibration_image_subtract) THEN BEGIN
    IF N_Elements(model_uv_arr) EQ 0 THEN BEGIN
        IF Min(status_str.grid_uv_model[0:n_pol-1]) GT 0 THEN BEGIN
            model_uv_arr=Ptrarr(n_pol,/allocate)
            FOR pol_i=0,n_pol-1 DO BEGIN
                fhd_save_io,status_str,grid_uv_model,var='grid_uv_model',/restore,file_path_fhd=file_path_fhd,obs=obs,pol_i=pol_i
                *model_uv_arr[pol_i]=grid_uv_model
            ENDFOR
        ENDIF
    ENDIF
ENDIF

fhd_save_io,status_str,fhd_params,var='fhd_params',/compress,file_path_fhd=file_path_fhd
fhd_log_settings,file_path_fhd,fhd=fhd_params,obs=obs,psf=psf ;DO NOT SUPPLY CAL STRUCTURE HERE!!!

fast_holographic_deconvolution,fhd_params,obs,psf,params,cal,jones,image_uv_arr,source_array,comp_arr,timing=timing,weights_arr=weights_arr,$
    residual_array=residual_array,dirty_array=dirty_array,model_uv_full=model_uv_full,model_uv_holo=model_uv_holo,$
    ra_arr=ra_arr,dec_arr=dec_arr,astr=astr,silent=silent,transfer_mapfn=transfer_mapfn,$
    beam_base=beam_base,beam_correction=beam_correction,model_uv_arr=model_uv_arr,$
    file_path_fhd=file_path_fhd,map_fn_arr=map_fn_arr,_Extra=extra
        
;compression reduces the file size by 50%, but takes 5-30 seconds longer
fhd_save_io,status_str,var='fhd',file_path_fhd=file_path_fhd,path_use=fhd_sav_filepath,/no_save ;call first to obtain the correct path. Will NOT update status structure yet
SAVE,residual_array,dirty_array,image_uv_arr,source_array,comp_arr,model_uv_full,model_uv_holo,weights_arr,$
    beam_base,beam_correction,astr,filename=fhd_sav_filepath,/compress
fhd_save_io,status_str,var='fhd',file_path_fhd=file_path_fhd,/force ;call a second time to update the status structure now that the file has actually been written

IF Keyword_Set(return_decon_visibilities) THEN BEGIN
    IF Arg_Present(vis_model_ptr) THEN BEGIN
        ;could generate model visibilities from just the source list (allows sources to be pruned), or from the final uv model (don't have to redo the DFT) 
        vis_model_ptr=vis_source_model(source_array,obs,status_str,psf,params,flag_arr,$
            timing=model_timing,silent=silent,error=error,file_path_fhd=file_path_fhd)   
;        vis_model_ptr=vis_source_model(source_array,obs,psf,params,model_uv_arr=model_uv_arr,$
;            timing=model_timing,silent=silent,error=error,file_path_fhd=file_path_fhd)      
    ENDIF
ENDIF
IF Keyword_Set(!Journal) THEN journal ;write log file
END