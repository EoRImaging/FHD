FUNCTION rootdir,project_name
Rootdirectory='' ;
IF N_Elements(project_name) EQ 0 THEN project_name='mwa' ELSE project_name=StrLowCase(project_name)
len=Strlen(project_name)
;returns the root directory on a variety of machines
os_type=!version.os_family
CASE os_type OF
    'Windows':idlpath_sep=';'  
    'unix': idlpath_sep=':'
ENDCASE
sub_strings=strsplit(StrLOWcase(!Path),path_sep()+idlpath_sep,/extract)
sub_strings_i=strsplit(StrLOWcase(!Path),path_sep()+idlpath_sep)
match_i=where(sub_strings EQ project_name,n_match)
IF n_match EQ 0 THEN BEGIN
    ;handle errors here
ENDIF
final_pos=sub_strings_i[match_i]+len
init_pos=lonarr(n_match)
dir_arr=Strarr(n_match)
FOR i=0,n_match-1 DO BEGIN
    init_pos[i]=Strpos(!Path,idlpath_sep,sub_strings_i[match_i[i]],/REVERSE_SEARCH)+1
    dir_arr[i]=Strmid(!Path,init_pos[i],final_pos[i]-init_pos[i])+path_sep()
ENDFOR
dir_len=strlen(dir_arr)
dir_len_use=Min(dir_len,dir_i)
RootDirectory=dir_arr[dir_i]

IF file_test(Rootdirectory) EQ 0 THEN print,String(format='(A," : folder not found")',Rootdirectory)
RETURN,RootDirectory
END

