FUNCTION structure_to_text,str,delimiter=delimiter,heading=heading,indent=indent
IF N_Elements(str) EQ 0 THEN RETURN,''
IF N_Elements(indent) EQ 0 THEN indent=0

IF N_Elements(delimiter) EQ 0 THEN delimiter=String(9B) ;tab character

n_tags=n_tags(str)
tag_name_arr=tag_names(str)
max_len=32

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
    tag_val=str.(ti)
    len=N_Elements(tag_val)
    
    IF size(tag_val,/type) EQ 8 THEN BEGIN ;if structure
        n_sub_tags=n_tags(tag_val)
        stretch=n_sub_tags
        result[1,ti_use]=String(format='("Structure (",A," tags)")',Strn(n_sub_tags))
        IF stretch GT 0 THEN result=[[result],[Strarr(2,stretch)]]
        result[0,ti_use+1]=structure_to_text(tag_val,delimiter=delimiter,/indent)
        ti_use+=stretch+1
        CONTINUE
    ENDIF
    
    IF size(tag_val,/type) EQ 10 THEN BEGIN ;if pointer
        n_dims=size(tag_val,/n_dimension)
        IF n_dims EQ 0 THEN BEGIN
            result[1,ti_use]='Pointer' 
        ENDIF ELSE BEGIN
            dims=size(tag_val,/dimensions)
            dim_arr=Strarr(n_dims)
            FOR dim_i=0,n_dims-1 DO dim_arr[dim_i]=Strn(dims[dim_i])
            format_code='("Pointer array (",'+Strn(n_dims)+'(A,:," x "))'
            result[1,ti_use]=String(format=format_code,dim_arr)+' elements)'
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
        format_code=Strcompress('('+String(format='(A,"(A),:,")',Strn(len)),/remove_all)+'"'+delimiter+'"'+')'
        text_val=String(format=format_code,tag_val)
    ENDELSE
    result[1,ti_use]=text_val
    ti_use+=1
ENDFOR

IF Keyword_Set(indent) THEN result[0,*]=delimiter+result[0,*]
RETURN,result
END