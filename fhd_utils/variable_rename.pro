FUNCTION variable_rename,varname
;function that will supply the value of a variable if supplied a string of its name. 
; Useful when restoring a sav file or other instance where the name of a variable is not known programmatically
RETURN,scope_varfetch(varname,level=-1)
END