FUNCTION string_list_read,file_path,data_directory=data_directory

;data_directory will only be used if the string file_path is not a path to a valid file.
file_path_use=file_path
IF file_test(file_path) EQ 0 THEN $
    IF Keyword_Set(data_directory) THEN file_path_use=filepath(file_path,root=data_directory)

Textfast,string_list,file_path=file_path_use,/read,/string

RETURN,string_list
END