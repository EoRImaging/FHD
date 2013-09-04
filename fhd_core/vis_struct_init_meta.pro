FUNCTION vis_struct_init_meta,file_path_vis,hdr,params
metafits_path=file_basename(file_path_vis,'.uvfits',/fold_case)
metafits_path=file_basename(metafits_path,'_cal',/fold_case) ;sometimes "_cal" is present, sometimes not.
metafits_path+='.metafits'

IF file_test(metafits_path) THEN BEGIN
    hdr0=headfits(metafits_path,exten=0,/silent)
    data=mrdfits(metafits_path,exten=1,hdr1,/silent)
    
    
ENDIF ELSE BEGIN
    ;use hdr and params to guess metadata
    
ENDELSE

RETURN,meta
END