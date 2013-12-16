PRO undefine_fhd,param0,param1,param2,param3,param4,param5,param6,param7,param8,param9

;program to delete fhd pointers, particularly the mapping functions

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
        type=size(param0,/type)
        CASE type OF
            8:BEGIN ;structure type
                
            END
            10:BEGIN ;pointer type
            
            END
        ENDCASE
    END
ENDSWITCH
END