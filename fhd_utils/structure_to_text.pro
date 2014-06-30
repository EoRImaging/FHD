FUNCTION structure_to_text,str,delimiter=delimiter,heading=heading,indent=indent,max_len=max_len
IF N_Elements(str) EQ 0 THEN RETURN,''
IF N_Elements(indent) EQ 0 THEN indent=0

IF N_Elements(delimiter) NE 0 THEN delimiter_use=delimiter ELSE delimiter_use=String(9B) ;tab character

IF size(str,/type) EQ 10 THEN BEGIN ;if pointer
    n_tags=N_Elements(str)
    tag_name_arr=Strarr(n_tags) + '->'
ENDIF ELSE BEGIN
    n_tags=n_tags(str)
    tag_name_arr=tag_names(str)
ENDELSE
IF N_Elements(max_len) EQ 0 THEN max_len=128.

IF Keyword_Set(heading) THEN BEGIN
    result=Strarr(2,n_tags+1) 
    result[0,0]=heading
    ti_use=1L
ENDIF ELSE BEGIN
    result=Strarr(2,n_tags)
    ti_use=0L
ENDELSE

FOR ti=0L,n_tags-1 DO BEGIN
    result[0,ti_use]=tag_name_arr[ti]
    IF size(str,/type) EQ 10 THEN BEGIN
        IF Ptr_valid(str[ti]) THEN tag_val=*str[ti] ELSE tag_val=str[ti]
    ENDIF ELSE tag_val=str.(ti)
    len=N_Elements(tag_val)
    
    IF size(tag_val,/type) EQ 8 THEN BEGIN ;if structure
        n_sub_tags=n_tags(tag_val)
        result[1,ti_use]=String(format='("Structure (",A," tags)")',Strn(n_sub_tags))
        IF n_sub_tags EQ 0 THEN BEGIN
            ti_use+=1
            CONTINUE
        ENDIF
        result_insert=structure_to_text(tag_val,delimiter=delimiter_use,/indent,max_len=max_len-1)
        IF size(result_insert,/n_dim) LT 2 THEN BEGIN
            ti_use+=1
            CONTINUE
        ENDIF
        stretch=(size(result_insert,/dimension))[1]
        result=[[result],[Strarr(2,stretch)]]
        result[0,ti_use+1]=result_insert
        ti_use+=stretch+1
        CONTINUE
    ENDIF
    
    IF size(tag_val,/type) EQ 10 THEN BEGIN ;if pointer
        n_dims=size(tag_val,/n_dimension)
        IF n_dims EQ 0 THEN BEGIN
            IF not Ptr_valid(tag_val) THEN BEGIN
                result[1,ti_use]='Null pointer' 
                ti_use+=1
                CONTINUE
            ENDIF
            result[1,ti_use]='Pointer' 
            result_insert=structure_to_text(tag_val,delimiter=delimiter_use,/indent,max_len=max_len-1)
            stretch=(size(result_insert,/dimension))[size(result_insert,/n_dim)-1]
            result=[[result],[Strarr(2,stretch)]]
            result[0,ti_use+1]=result_insert
            ti_use+=stretch+1
            CONTINUE
        ENDIF ELSE BEGIN
            dims=size(tag_val,/dimensions)
            dim_arr=Strarr(n_dims)
            FOR dim_i=0,n_dims-1 DO dim_arr[dim_i]=Strn(dims[dim_i])
            format_code='("Pointer array (",'+Strn(n_dims)+'(A,:," x "))'
            result[1,ti_use]=String(format=format_code,dim_arr)+' elements)'
;            IF n_dims LE 4 THEN BEGIN
;                null_test=Max(Ptr_valid(tag_val))
;                IF null_test EQ 0 THEN BEGIN
;                    ti_use+=1
;                    CONTINUE
;                ENDIF
;                result_insert=structure_to_text(tag_val,delimiter=delimiter_use,/indent,max_len=max_len-1)
;                IF size(result_insert,/n_dim) LT 2 THEN BEGIN
;                    ti_use+=1
;                    CONTINUE
;                ENDIF
;                result=[[result],[Strarr(2,stretch)]]
;                result[0,ti_use+1]=result_insert
;                ti_use+=stretch+1
;                CONTINUE
;            ENDIF
        ENDELSE
        
;        result[1,ti_use]=String(format='("Pointer (",A," elements)")',Strn(len))
        ti_use+=1
        CONTINUE
    ENDIF
    
    IF len GT max_len THEN BEGIN 
        n_dims=size(tag_val,/n_dimension) ;guaranteed greater than one!
        dims=size(tag_val,/dimensions)
        dim_arr=Strarr(n_dims)
        FOR dim_i=0,n_dims-1 DO dim_arr[dim_i]=Strn(dims[dim_i])
        format_code='("'+size_type(tag_val)+' array (",'+Strn(n_dims)+'(A,:," x "))'
        text_val=String(format=format_code,dim_arr)+' elements)'
    ENDIF ELSE BEGIN
        format_code=Strcompress('('+String(format='(A,"(A),:,")',Strn(len)),/remove_all)+'"'+delimiter_use+'"'+')'
        text_val=String(format=format_code,tag_val)
    ENDELSE
    result[1,ti_use]=text_val
    ti_use+=1
ENDFOR

IF Keyword_Set(indent) THEN result[0,*]=delimiter_use+result[0,*]
RETURN,result
END