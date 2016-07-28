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
PRO fhd_wrap,obs,status_str,psf,params,fhd_params,cal,jones,skymodel,file_path_fhd=file_path_fhd,$
    data_directory=data_directory,filename=filename,version=version,silent=silent,transfer_mapfn=transfer_mapfn,$
    map_fn_arr=map_fn_arr,GPU_enable=GPU_enable,image_uv_arr=image_uv_arr,weights_arr=weights_arr,$
    model_uv_arr=model_uv_arr, vis_model_arr=vis_model_arr,$
    return_decon_visibilities=return_decon_visibilities,vis_weights=vis_weights,log_store=log_store,_Extra=extra

;snapshot data must have been gridded previously, and the Holo map fns generated
;reads and deconvolves simultaneously on multiple polarizations, time intervals, and frequencies
;multiple time intervals and frequencies are NOT SUPPORTED 
;(mostly just need to generate their UV grid and map_fn files)
;
heap_gc
compile_opt idl2,strictarrsubs  

IF Keyword_Set(!Journal) THEN journal
IF Keyword_Set(log_store) THEN journal,file_path_fhd+'_fhd_log.txt'
IF N_Elements(obs) EQ 0 THEN fhd_save_io,status_str,obs,var='obs',/restore,file_path_fhd=file_path_fhd,_Extra=extra
IF N_Elements(params) EQ 0 THEN fhd_save_io,status_str,params,var='params',/restore,file_path_fhd=file_path_fhd,_Extra=extra
IF size(cal,/type) NE 8 THEN $
    IF status_str.cal GT 0 THEN fhd_save_io,status_str,cal,var='cal',/restore,file_path_fhd=file_path_fhd,_Extra=extra $ 
        ELSE cal=fhd_struct_init_cal(obs,params)

IF N_Elements(jones) EQ 0 THEN fhd_save_io,status_str,jones,var='jones',/restore,file_path_fhd=file_path_fhd,_Extra=extra
IF N_Elements(skymodel) EQ 0 THEN fhd_save_io,status_str,skymodel,var='skymodel',/restore,file_path_fhd=file_path_fhd,_Extra=extra

fhd_params=fhd_init(obs,skymodel,transfer_mapfn=transfer_mapfn,file_path_fhd=file_path_fhd,_Extra=extra)

n_pol=fhd_params.npol

IF N_Elements(psf) EQ 0 THEN fhd_save_io,status_str,psf,var='psf',/restore,file_path_fhd=file_path_fhd,_Extra=extra

;set to fit each polarization independantly 
;(only flux is independant, all locations are from Stokes I). 
;Otherwise they are fit to Stokes I and U only 
IF N_Elements(image_uv_arr) EQ 0 THEN BEGIN
    image_uv_arr=Ptrarr(n_pol)
    FOR pol_i=0,n_pol-1 DO BEGIN
        fhd_save_io,status_str,grid_uv,var='grid_uv',/restore,file_path_fhd=file_path_fhd,obs=obs,pol_i=pol_i,_Extra=extra
        image_uv_arr[pol_i]=Ptr_new(grid_uv,/no_copy)
    ENDFOR
ENDIF
IF N_Elements(weights_arr) EQ 0 THEN BEGIN
    weights_arr=Ptrarr(n_pol)
    FOR pol_i=0,n_pol-1 DO BEGIN
        fhd_save_io,status_str,weights_uv,var='weights_uv',/restore,file_path_fhd=file_path_fhd,obs=obs,pol_i=pol_i,_Extra=extra
        weights_arr[pol_i]=Ptr_new(weights_uv,/no_copy)
    ENDFOR
ENDIF
IF Keyword_Set(transfer_mapfn) THEN print,String(format='("Transferring mapfn from: ",A)',transfer_mapfn)

IF N_Elements(map_fn_arr) EQ 0 THEN map_fn_arr=Ptrarr(n_pol)
FOR pol_i=0,n_pol-1 DO BEGIN
    IF Ptr_valid(map_fn_arr[pol_i]) EQ 0 THEN BEGIN
;        fhd_save_io,status_str,map_fn,var='map_fn',file_path_fhd=file_path_fhd,pol_i=pol_i,/restore,transfer=transfer_mapfn,obs=obs,_Extra=extra
        ;IMPORTANT: this approach of restoring the map_fn uses the least memory
        fhd_save_io,status_str,map_fn,var='map_fn',file_path_fhd=file_path_fhd,pol_i=pol_i,$
            transfer=transfer_mapfn,/no_save,path_use=path_use,obs=obs,_Extra=extra
        RESTORE,path_use+'.sav' ;map_fn
;        print,'Restoring: ' + file_path_mapfn+pol_names[pol_i]+'.sav'
;        restore,file_path_mapfn+pol_names[pol_i]+'.sav' ;map_fn
        map_fn_arr[pol_i]=Ptr_new(map_fn,/no_copy)
    ENDIF
ENDFOR
;IF Keyword_Set(calibration_image_subtract) THEN BEGIN
;    IF N_Elements(model_uv_arr) EQ 0 THEN BEGIN
;        IF Min(status_str.grid_uv_model[0:n_pol-1]) GT 0 THEN BEGIN
;            model_uv_arr=Ptrarr(n_pol)
;            FOR pol_i=0,n_pol-1 DO BEGIN
;                fhd_save_io,status_str,grid_uv_model,var='grid_uv_model',/restore,file_path_fhd=file_path_fhd,obs=obs,pol_i=pol_i,_Extra=extra
;                model_uv_arr[pol_i]=Ptr_new(grid_uv_model,/no_copy)
;            ENDFOR
;        ENDIF
;    ENDIF
;ENDIF

IF ~Keyword_Set(silent) THEN fhd_log_settings,file_path_fhd,fhd=fhd_params,obs=obs,psf=psf,sub_dir='metadata' ;DO NOT SUPPLY CAL STRUCTURE HERE!!!

fast_holographic_deconvolution,fhd_params,obs,psf,params,cal,jones,image_uv_arr,source_array,component_array,timing=timing,weights_arr=weights_arr,$
    residual_array=residual_array,dirty_array=dirty_array,model_uv_full=model_uv_full,model_uv_holo=model_uv_holo,$
    ra_arr=ra_arr,dec_arr=dec_arr,astr=astr,silent=silent,transfer_mapfn=transfer_mapfn,$
    beam_base=beam_base,beam_correction=beam_correction,model_uv_arr=model_uv_arr,$
    source_mask=source_mask,file_path_fhd=file_path_fhd,map_fn_arr=map_fn_arr,_Extra=extra

;Save results

fhd_save_io,status_str,fhd_params,var='fhd_params',/compress,file_path_fhd=file_path_fhd,_Extra=extra
;compression reduces the file size by 50%, but takes 5-30 seconds longer
fhd_save_io,var='fhd',file_path_fhd=file_path_fhd,path_use=fhd_sav_filepath,/no_save,_Extra=extra ;call first to obtain the correct path. Will NOT update status structure yet
SAVE,residual_array,dirty_array,image_uv_arr,source_array,component_array,model_uv_full,model_uv_holo,weights_arr,$
    source_mask,beam_base,beam_correction,astr,filename=fhd_sav_filepath+'.sav',/compress
fhd_save_io,status_str,var='fhd',file_path_fhd=file_path_fhd,/force,_Extra=extra ;call a second time to update the status structure now that the file has actually been written

FOR pol_i=0,n_pol-1 DO fhd_save_io,status_str,*model_uv_holo[pol_i],var='grid_uv_model',/compress,file_path_fhd=file_path_fhd,pol_i=pol_i,obs=obs,_Extra=extra

;save and export deconvolved source list
beam_avg=*beam_base[0]
IF n_pol GT 1 THEN beam_avg=Sqrt(beam_avg^2.+*beam_base[1]^2.)/Sqrt(2.)
stokes_residual_arr=Stokes_cnv(residual_array,jones,obs,beam_arr=beam_arr,_Extra=extra)
fhd_save_io,status_str,source_array,var='source_array',/compress,file_path_fhd=file_path_fhd,path_use=source_array_path_use,_Extra=extra 
source_array_export,source_array,obs,beam=beam_avg,stokes_images=stokes_residual_arr,file_path=source_array_path_use

skymodel_decon=fhd_struct_init_skymodel(obs,source_list=source_array,catalog_path=source_array_path_use,return_cal=calibration_image_subtract)
skymodel=skymodel_decon ;In the future, we might want to include support for different combinations of calibration, firstpass source/diffuse subtraction, and deconvolution
fhd_save_io,status_str,skymodel,var='skymodel',/compress,file_path_fhd=file_path_fhd,_Extra=extra
fhd_log_settings,file_path_fhd,fhd=fhd_params,obs=obs,cal=cal,psf=psf,skymodel=skymodel,sub_dir='metadata' 

undefine_fhd,map_fn_arr
;Optionally degrid and export the visibilities formed from the deconvolution model 
IF Keyword_Set(return_decon_visibilities) THEN BEGIN
    IF Arg_Present(vis_model_arr) THEN BEGIN
        ;could generate model visibilities from just the source list (allows sources to be pruned), or from the final uv model (don't have to redo the DFT) 
        vis_model_arr=vis_source_model(skymodel,obs,status_str,psf,params,vis_weights,$
            timing=model_timing,silent=silent,error=error,file_path_fhd=file_path_fhd,_Extra=extra)   
;        vis_model_arr=vis_source_model(source_array,obs,psf,params,model_uv_arr=model_uv_arr,$
;            timing=model_timing,silent=silent,error=error,file_path_fhd=file_path_fhd)      
    ENDIF
ENDIF
IF Keyword_Set(!Journal) THEN journal ;write log file
END