;+
; :Description:
;    Sets up default values for Fast Holographic Deconvolution, loads previously-gridded visibility data, and calls fast_holographic_deconvolution.pro 
;
; :Params:
;    obs - structure containing details of the observation
;    
;    params - structure containing u and v coordinates of the baselines, in meters/c.
;    
;    psf - structure containing the high-resolution gridded beam model.
;
; :Keywords:
;    beam_threshold
;
; :Author: isullivan May 6, 2012
;-
PRO fhd_wrap,obs,params,psf,fhd,file_path_fhd=file_path_fhd,beam_threshold=beam_threshold,quickview=quickview,$
    data_directory=data_directory,filename=filename,version=version,silent=silent,transfer_mapfn=transfer_mapfn,$
    map_fn_arr=map_fn_arr,GPU_enable=GPU_enable,image_uv_arr=image_uv_arr,weights_arr=weights_arr,_Extra=extra

;snapshot data must have been gridded previously, and the Holo map fns generated
;reads and deconvolves simultaneously on multiple polarizations, time intervals, and frequencies
;multiple time intervals and frequencies are NOT  SUPPORTED 
;(mostly just need to generate their UV grid and map_fn files)
;
heap_gc
compile_opt idl2,strictarrsubs  

IF N_Elements(obs) EQ 0 THEN RESTORE,file_path_fhd+'_obs.sav'
fhd=fhd_init(obs,_Extra=extra)
SAVE,fhd,filename=file_path_fhd+'_fhd_params.sav',/compress
fhd_log_settings,file_path_fhd,fhd=fhd

npol=fhd.npol
dimension=obs.dimension
elements=obs.elements

pol_names=['xx','yy','xy','yx']
ext='.UVFITS'

IF N_Elements(params) EQ 0 THEN RESTORE,filename=file_path_fhd+'_params.sav'
IF N_Elements(psf) EQ 0 THEN psf=beam_setup(obs,file_path_fhd,/restore_last) 

;set to fit each polarization independantly 
;(only flux is independant, all locations are from Stokes I). 
;Otherwise they are fit to Stokes I and U only 
IF N_Elements(image_uv_arr) EQ 0 THEN BEGIN
    image_uv_arr=Ptrarr(npol,/allocate)
    FOR pol_i=0,npol-1 DO *image_uv_arr[pol_i]=getvar_savefile(file_path_fhd+'_uv_'+pol_names[pol_i]+'.sav','dirty_uv');*obs.cal[pol_i]
ENDIF
IF N_Elements(weights_arr) EQ 0 THEN BEGIN
    weights_arr=Ptrarr(npol,/allocate)
    FOR pol_i=0,npol-1 DO *weights_arr[pol_i]=getvar_savefile(file_path_fhd+'_uv_'+pol_names[pol_i]+'.sav','weights_grid')
ENDIF
;IF Keyword_Set(GPU_enable) THEN $    
;    GPU_fast_holographic_deconvolution,fhd,obs,psf,image_uv_arr,source_array,comp_arr,weights_arr=weights_arr,timing=timing,$
;        residual_array=residual_array,dirty_array=dirty_array,model_uv_full=model_uv_full,model_uv_holo=model_uv_holo,$
;        ra_arr=ra_arr,dec_arr=dec_arr,astr=astr,silent=silent,$
;        beam_base=beam_base,beam_correction=beam_correction,normalization=normalization $
;ELSE $
    fast_holographic_deconvolution,fhd,obs,psf,image_uv_arr,source_array,comp_arr,timing=timing,weights_arr=weights_arr,$
        residual_array=residual_array,dirty_array=dirty_array,model_uv_full=model_uv_full,model_uv_holo=model_uv_holo,$
        ra_arr=ra_arr,dec_arr=dec_arr,astr=astr,silent=silent,transfer_mapfn=transfer_mapfn,$
        beam_base=beam_base,beam_correction=beam_correction,normalization=normalization,$
        file_path_fhd=file_path_fhd,map_fn_arr=map_fn_arr,_Extra=extra
        
;compression reduces the file size by 50%, but takes 5-30 seconds longer
SAVE,residual_array,dirty_array,image_uv_arr,source_array,comp_arr,model_uv_full,model_uv_holo,normalization,weights_arr,$
    beam_base,beam_correction,ra_arr,dec_arr,astr,filename=file_path_fhd+'_fhd.sav',/compress

IF N_Elements(quickview) EQ 0 THEN quickview=1
IF Keyword_Set(quickview) THEN fhd_quickview,fhd,obs,image_uv_arr,model_uv_holo,source_array,comp_arr,$
    beam_base,file_path_fhd=file_path_fhd,_Extra=extra

END