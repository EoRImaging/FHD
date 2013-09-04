FUNCTION vis_struct_init_meta,file_path_vis,hdr=hdr
metafits_path=file_basename(file_path_vis,'.uvfits',/fold_case)
metafits_path=file_basename(metafits_path,'_cal',/fold_case) ;sometimes "_cal" is present, sometimes not.
metafits_path+='.metafits'

IF file_test(metafits_path

RETURN,meta
END