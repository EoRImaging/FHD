PRO transfer_weights_data,vis_weights,obs,status_str,params,file_path_fhd=file_path_fhd,$
    transfer_filename=transfer_filename,error=error,flag_visibilities=flag_visibilities,$
    flag_calibration=flag_calibration,_Extra=extra
n_pol=obs.n_pol
n_freq=obs.n_freq
n_tile=obs.n_tile
n_time=obs.n_time
IF file_basename(file_path_fhd) EQ transfer_filename THEN BEGIN 
;    IF Keyword_Set(flag_visibilities) THEN BEGIN
;        print,'Flagging anomalous data'
;        vis_flag,vis_arr,vis_weights,obs,params,_Extra=extra
;    ENDIF
    fhd_save_io,status_str,vis_weights,var='vis_weights',/compress,file_path_fhd=file_path_fhd,_Extra=extra
    RETURN
ENDIF ELSE BEGIN
    fhd_save_io,0,vis_weights_xfer,var='vis_weights',/restore,file_path_fhd=file_path_fhd,$
        transfer=transfer_filename,_Extra=extra 
ENDELSE

n0=N_Elements(*vis_weights_xfer[0])
n1=N_Elements(*vis_weights[0])
IF n1 GT n0 THEN BEGIN
    ;If more data, zero out additional
    nf0=(size(*vis_weights_xfer[0],/dimension))[0]
    nb0=(size(*vis_weights_xfer[0],/dimension))[1]
    FOR pol_i=0,n_pol-1 DO BEGIN
        *vis_weights[pol_i]=fltarr(size(*vis_weights[pol_i],/dimension))
        (*vis_weights[pol_i])[0:nf0-1,0:nb0-1]*=Temporary(*vis_weights_xfer[pol_i])
    ENDFOR
    Ptr_free,vis_weights_xfer
    fhd_save_io,status_str,vis_weights,var='vis_weights',/compress,file_path_fhd=file_path_fhd,_Extra=extra
    RETURN
ENDIF
IF n0 GT n1 THEN BEGIN
    ;If less data, return with an error!
    error=1
    RETURN
ENDIF

; if n0 eq n1
Ptr_free,vis_weights
vis_weights=vis_weights_xfer

flag_calibration=0 ;make sure no flagging is done later
;reset tile/frequency/time flags in obs structure, then update with current values with a call to vis_flag_basic after this
(*obs.baseline_info).tile_use=1L
(*obs.baseline_info).freq_use=1L
;(*obs.baseline_info).time_use=1L ; TEMPORARY
END