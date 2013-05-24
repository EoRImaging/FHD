PRO uvfits_read,data,header,file_path=file_path

IF file_test(file_path) EQ 0 THEN RETURN
Fits_open,file_path,fcb,/no_abort
Fits_read,fcb,data,header,_EXTRA=extra,/no_abort
Fits_close,fcb,/no_abort

END