FUNCTION var_bundle,level=level,routine=routine
IF N_Elements(level) EQ 0 THEN level=0
IF size(routine,/type) NE 7 THEN routine=scope_traceback()
IF N_Elements(routine) GT 1 THEN routine=routine[level+1] ELSE routine=routine[level]
routine=Strtrim((Strsplit(routine,'<',/extract))[0],2)

var_names=routine_info(routine,/variables)
n_var=N_Elements(var_names)

IF Max(strmatch(var_names,'EXTRA',/fold_case)) EQ 1 THEN BEGIN
    vi0=where(strmatch(var_names,'EXTRA',/fold_case),complement=vi_use,ncomplement=n_var)
    struct=scope_varfetch(var_names[vi0],level=2+level)
    IF n_var GT 0 THEN var_names=var_names[vi_use]
ENDIF
    
FOR vi=0,n_var-1 DO BEGIN
    IF N_Elements(scope_varfetch(var_names[vi],level=2+level)) EQ 0 THEN CONTINUE
    IF N_Elements(struct) GT 0 THEN IF Max(strmatch(tag_names(struct),var_names[vi],/fold_case)) EQ 1 THEN CONTINUE
    IF N_Elements(struct) EQ 0 THEN struct=create_struct(var_names[vi],scope_varfetch(var_names[vi],level=2+level)) $
        ELSE struct=create_struct(var_names[vi],scope_varfetch(var_names[vi],level=2+level),struct)
ENDFOR
RETURN,struct
END