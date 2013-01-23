FUNCTION fhd_path_setup,file_path_vis,data_directory=data_directory,filename=filename,version=version

IF Keyword_Set(file_path_vis) THEN BEGIN
    filename=file_basename(file_path_vis,'_cal.uvfits',/fold_case)
    data_directory=file_dirname(file_path_vis)
ENDIF

IF ~Keyword_Set(version) THEN BEGIN
    dir=filepath('',root_dir=data_directory,sub=['fhd'])
    IF file_test(dir) EQ 0 THEN file_mkdir,dir   
    file_path=filepath(filename,root_dir=dir)
ENDIF ELSE BEGIN
    version=Fix(version)
    version_name='v'+strn(version)
    version_dirname='fhd_'+version_name
    dir=filepath('',root_dir=data_directory,sub=[version_dirname])
    IF file_test(dir) EQ 0 THEN file_mkdir,dir   
    file_path=filepath(filename+version_name,root_dir=dir)
ENDELSE

RETURN,file_path
END