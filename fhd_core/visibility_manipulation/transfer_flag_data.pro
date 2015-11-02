PRO transfer_flag_data,flag_arr,obs,status_str,params,file_path_fhd=file_path_fhd,$
    transfer_filename=transfer_filename,error=error,flag_visibilities=flag_visibilities,$
    flag_calibration=flag_calibration,_Extra=extra
n_pol=obs.n_pol
n_freq=obs.n_freq
n_tile=obs.n_tile
n_time=obs.n_time
IF file_basename(file_path_fhd) EQ transfer_filename THEN BEGIN 
;    IF Keyword_Set(flag_visibilities) THEN BEGIN
;        print,'Flagging anomalous data'
;        vis_flag,vis_arr,flag_arr,obs,params,_Extra=extra
;    ENDIF
    fhd_save_io,status_str,flag_arr,var='flag_arr',/compress,file_path_fhd=file_path_fhd,_Extra=extra
    RETURN
ENDIF ELSE BEGIN
    fhd_save_io,0,flag_arr_xfer,var='flag_arr',/restore,file_path_fhd=file_path_fhd,$
        transfer=transfer_filename,_Extra=extra 
ENDELSE

n0=N_Elements(*flag_arr_xfer[0])
n1=N_Elements(*flag_arr[0])
IF n1 GT n0 THEN BEGIN
    ;If more data, zero out additional
    nf0=(size(*flag_arr_xfer[0],/dimension))[0]
    nb0=(size(*flag_arr_xfer[0],/dimension))[1]
    FOR pol_i=0,n_pol-1 DO BEGIN
        *flag_arr[pol_i]=fltarr(size(*flag_arr[pol_i],/dimension))
        (*flag_arr[pol_i])[0:nf0-1,0:nb0-1]*=Temporary(*flag_arr_xfer[pol_i])
    ENDFOR
    Ptr_free,flag_arr_xfer
    fhd_save_io,status_str,flag_arr,var='flag_arr',/compress,file_path_fhd=file_path_fhd,_Extra=extra
    RETURN
ENDIF
IF n0 GT n1 THEN BEGIN
    ;If less data, return with an error!
    error=1
    RETURN
ENDIF

; if n0 eq n1
Ptr_free,flag_arr
flag_arr=flag_arr_xfer

flag_calibration=0 ;make sure no flagging is done later
;reset tile/frequency/time flags in obs structure, then update with current values with a call to vis_flag_basic after this
(*obs.baseline_info).tile_use=1L
(*obs.baseline_info).freq_use=1L
(*obs.baseline_info).time_use=1L
END