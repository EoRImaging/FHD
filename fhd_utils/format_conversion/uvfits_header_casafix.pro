PRO uvfits_header_casafix,file_path_vis,file_path_fhd=file_path_fhd
;vis_path_default,data_directory,filename,file_path,version=version

;file_path_fhd=fhd_path_setup(file_path_vis,data_directory=data_directory,filename=filename,version=version)
file_path_use=filepath(file_basename(file_path_vis,'_cal.uvfits',/fold_case),root_dir=file_dirname((file_path_vis)))
ext='.uvfits'
data_struct=mrdfits(file_path_use+ext,0,data_header,/silent)
data_struct=0
file_path_hdr=file_path_fhd+'_header.sav'
IF file_test(file_dirname(file_path_fhd)) EQ 0 THEN file_mkdir,file_dirname(file_path_fhd)  
save,data_header,filename=file_path_hdr

END