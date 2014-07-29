PRO transfer_flag_data,flag_arr,obs,params,file_path_fhd=file_path_fhd,transfer_filename=transfer_filename,error=error,flag_visibilities=flag_visibilities,_Extra=extra
IF file_basename(file_path_fhd) EQ transfer_mapfn THEN BEGIN 
    IF Keyword_Set(flag_visibilities) THEN BEGIN
        print,'Flagging anomalous data'
        vis_flag,vis_arr,flag_arr,obs,params,_Extra=extra
    ENDIF
ENDIF ELSE fhd_save_io,0,flag_arr_xfer,var='flag_arr',/restore,file_path_fhd=file_path_fhd,transfer=transfer_filename,_Extra=extra 
n0=N_Elements(*flag_arr_xfer[0])
n1=N_Elements(*flag_arr[0])
IF n1 GT n0 THEN BEGIN
    ;If more data, zero out additional
    nf0=(size(*flag_arr_xfer[0],/dimension))[0]
    nb0=(size(*flag_arr_xfer[0],/dimension))[1]
    FOR pol_i=0,n_pol-1 DO BEGIN
        *flag_arr[pol_i]=fltarr(size(*flag_arr[pol_i],/dimension))
        (*flag_arr[pol_i])[0:nf0-1,0:nb0-1]*=*flag_arr_xfer[pol_i]
    ENDFOR
ENDIF
IF n0 GT n1 THEN BEGIN
    ;If less data, return with an error!
    error=1
    RETURN
ENDIF
fhd_save_io,status_str,flag_arr,var='flag_arr',/compress,file_path_fhd=file_path_fhd,_Extra=extra

END