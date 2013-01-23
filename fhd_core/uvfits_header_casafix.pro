PRO uvfits_header_casafix,file_path_vis,version=version
;vis_path_default,data_directory,filename,file_path,version=version

file_path_fhd=fhd_path_setup(file_path_vis,data_directory=data_directory,filename=filename,version=version)
file_path_use=filepath(filename,root_dir=data_directory)
ext='.uvfits'
data_struct=mrdfits(file_path_use+ext,0,data_header,/silent)
data_struct=0
file_path2=file_path_fhd+'_header.sav'
IF file_test(file_dirname(file_path_fhd)) EQ 0 THEN file_mkdir,file_dirname(file_path_fhd)  
save,data_header,filename=file_path2

END