PRO uvfits_header_casafix,filename,data_directory,version=version
vis_path_default,data_directory,filename,file_path,version=version

ext='.uvfits'
UPNAME=StrUpCase(filename)
pcal=strpos(UPNAME,'_CAL')
IF pcal EQ -1 THEN filename_use=filename ELSE filename_use=StrMid(filename,0,pcal)
data_struct=mrdfits(filepath(filename_use+ext,root_dir=rootdir('mwa'),subdir=data_directory),0,data_header,/silent)
data_struct=0
file_path2=file_path+'_header.sav'
IF file_test(rootdir('mwa')+data_directory) EQ 0 THEN file_mkdir,rootdir('mwa')+data_directory  
save,data_header,filename=file_path2

END