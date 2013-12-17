PRO undefine_fhd,param,param1,param2,param3,param4,param5,param6,param7,param8,param9,level=level

;program to delete fhd pointers, particularly the mapping functions
;note: this works just fine even if undefined parameters are supplied
IF N_Elements(level) EQ 0 THEN level_use=0 ELSE level_use=level+1
np=N_Params()
SWITCH np OF
    10:undefine_fhd,param9
    9:undefine_fhd,param8
    8:undefine_fhd,param7
    7:undefine_fhd,param6
    6:undefine_fhd,param5
    5:undefine_fhd,param4
    4:undefine_fhd,param3
    3:undefine_fhd,param2
    2:undefine_fhd,param1
    1:BEGIN
        type=size(param,/type)
        IF type EQ 8 THEN BEGIN ;structure type
            FOR t_i=0L,N_tags(param)-1 DO BEGIN
                type1=size(param.(t_i),/type)
                IF (type1 EQ 8) OR (type1 EQ 10) THEN undefine_fhd,param.(t_i),level=level_use
            ENDFOR
        ENDIF
        IF type EQ 10 THEN BEGIN ;pointer type
            ptr_i=where(Ptr_valid(param),n_valid)
            FOR p_i=0L,n_valid-1 DO BEGIN
                type1=size(*param[ptr_i[p_i]],/type)
                IF (type1 EQ 8) OR (type1 EQ 10) THEN undefine_fhd,*param[ptr_i[p_i]],level=level_use
            ENDFOR
            ptr_free,param
        ENDIF
        IF level_use EQ 0 THEN BEGIN
            param=0
            dummy=Temporary(param)
        ENDIF
    END
ENDSWITCH
END