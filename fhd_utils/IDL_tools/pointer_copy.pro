FUNCTION pointer_copy,param

type=size(param,/type)
CASE type OF
    8: BEGIN ;structure type
        param_out = param
        FOR t_i=0L,N_tags(param)-1 DO BEGIN
            type1=size(param.(t_i),/type)
            IF (type1 EQ 8) OR (type1 EQ 10) THEN BEGIN
                param_out.(t_i)=pointer_copy(param.(t_i))
            ENDIF ELSE BEGIN
                param_out.(t_i) = param.(t_i)
            ENDELSE
        ENDFOR
    END
    10: BEGIN ;pointer type
        IF N_Elements(param) EQ 1 THEN param_out=Ptr_new() ELSE param_out=Ptrarr(size(param,/dimension))
        ptr_i=where(Ptr_valid(param),n_valid)
        FOR p_i=0L,n_valid-1 DO BEGIN
            param_out[ptr_i[p_i]] = Ptr_new(pointer_copy(*param[ptr_i[p_i]]))
        ENDFOR
    END
    ELSE: param_out = param
ENDCASE

;IF N_Elements(ptr_in) EQ 1 THEN ptr_out=Ptr_new() ELSE ptr_out=Ptrarr(size(ptr_in,/dimension))
;
;FOR dim_i=0L,N_Elements(ptr_in)-1 DO IF Ptr_valid(ptr_in[dim_i]) THEN ptr_out[dim_i]=Ptr_new(*ptr_in[dim_i]) 
RETURN,param_out
END