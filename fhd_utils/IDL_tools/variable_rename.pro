FUNCTION variable_rename,varname,keep_original=keep_original
;function that will supply the value of a variable if supplied a string of its name. 
; Useful when restoring a sav file or other instance where the name of a variable is not known programmatically
IF Keyword_Set(keep_original) THEN BEGIN
    RETURN,scope_varfetch(varname,level=-1)
ENDIF ELSE BEGIN
    RETURN,Temporary(scope_varfetch(varname,level=-1))
ENDELSE
END