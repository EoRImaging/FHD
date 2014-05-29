FUNCTION fhd_path_setup,file_path_vis,data_directory=data_directory,filename=filename,version=version,$
    output_directory=output_directory,output_filename=output_filename,no_sub_fhd=no_sub_fhd,subdir=subdir,_Extra=extra
;NOTE that any keywords passed through _Extra (i.e. from the command line) will supersede any passed explicitly

IF Keyword_Set(file_path_vis) THEN BEGIN
    filename=file_basename(file_path_vis,'.sav',/fold_case) ;visibilities may be stored in an IDL save file, instead of uvfits
    filename=file_basename(file_path_vis,'.uvfits',/fold_case)
    filename=file_basename(filename,'_cal',/fold_case) ;sometimes "_cal" is present, sometimes not.
    data_directory=file_dirname(file_path_vis)
ENDIF

IF N_Elements(output_directory)+N_Elements(data_directory) EQ 0 THEN message,"Output directory not specified"
IF ~Keyword_Set(output_directory) THEN output_directory=data_directory
IF Keyword_Set(output_filename) THEN filename=output_filename

IF N_Elements(filename) EQ 0 THEN message,"Filename not specified"

IF Keyword_Set(version) THEN IF size(version,/type) EQ 7 THEN version_dirname='fhd_'+version ELSE version_dirname='fhd_v'+strn(Fix(version)) $
    ELSE version_dirname='fhd'

CASE 1 OF 
    Keyword_Set(subdir) AND ~Keyword_Set(no_sub_fhd):subdir_use=[version_dirname,subdir]
    ~Keyword_Set(no_sub_fhd):subdir_use=Strarr(1)+version_dirname
    Keyword_Set(subdir):subdir_use=subdir
    ELSE: subdir_use=''
ENDCASE

n_files=N_Elements(file_path_vis)
IF N_Elements(output_directory) LT n_files THEN output_dir_use=Replicate(output_directory[0],n_files) ELSE output_dir_use=output_directory
IF n_files GT 1 THEN BEGIN
    file_path=Strarr(n_files)
    FOR file_i=0,n_files-1 DO BEGIN
        dir=filepath('',root_dir=output_dir_use[file_i],sub=subdir_use)
        IF file_test(dir) EQ 0 THEN file_mkdir,dir   
        file_path[file_i]=filepath(filename[file_i],root_dir=dir)
    ENDFOR

ENDIF ELSE BEGIN
    dir=filepath('',root_dir=output_dir_use,sub=subdir_use)
    IF file_test(dir) EQ 0 THEN file_mkdir,dir   
    file_path=filepath(filename,root_dir=dir)
ENDELSE

RETURN,file_path
END