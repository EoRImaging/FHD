FUNCTION rootdir,project_name
Rootdirectory='' ;
IF N_Elements(project_name) EQ 0 THEN project_name='mwa' ELSE project_name=StrLowCase(project_name)
len=Strlen(project_name)
;returns the root directory on a variety of machines
os_type=!version.os_family
CASE os_type OF
   'Windows':BEGIN
      pos=Strpos(StrLOWcase(!Path),project_name+';')
      if pos eq -1 then pos=Strpos(StrLOWcase(!Path),project_name+'\;')
      IF pos EQ -1 THEN BEGIN
        pos=Strpos(StrLOWcase(!Path),project_name)
        IF pos EQ -1 THEN BEGIN
            print,String(format='(A,": folder not found in IDL !Path, using empty string")',project_name)
            RETURN,RootDirectory
        ENDIF
      ENDIF
      final_pos=pos+len
      ;'+1' handles the case of the directory being the first entry, and of stripping off the ';' in all other cases
      init_pos=Strpos(!Path,';',pos,/REVERSE_SEARCH)+1
      RootDirectory=Strmid(!Path,init_pos,final_pos-init_pos)+'\'
   END
     'unix': BEGIN
      pos=Strpos(StrLOWcase(!Path),project_name+':')
      if pos eq -1 then pos=Strpos(StrLOWcase(!Path),project_name+'/:')
      IF pos EQ -1 THEN BEGIN
        pos=Strpos(StrLOWcase(!Path),project_name)
        IF pos EQ -1 THEN BEGIN
            print,String(format='(A,": folder not found in IDL !Path, using empty string")',project_name)
            RETURN,RootDirectory
        ENDIF
      ENDIF
      ;'+1' handles the case of the directory being the first entry, and of stripping off the ':' in all other cases
      init_pos=Strpos(!Path,':',pos,/REVERSE_SEARCH)+1 
      final_pos=pos+len
      RootDirectory=Expand_path(Strmid(!Path,init_pos,final_pos-init_pos))+'/'
   END
ENDCASE
IF file_test(Rootdirectory) EQ 0 THEN print,String(format='(A," : folder not found")',Rootdirectory)
RETURN,RootDirectory
END


