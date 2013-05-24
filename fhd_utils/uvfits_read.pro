PRO uvfits_read,data,header,params,file_path=file_path

IF file_test(file_path) EQ 0 THEN RETURN
Fits_open,file_path,fcb,/no_abort
;Fits_read,fcb,0,header,/no_abort,/header_only
Fits_read,fcb,data,header,params,/no_abort,/noscale,EXTEN_NO=0

Fits_close,fcb,/no_abort

END