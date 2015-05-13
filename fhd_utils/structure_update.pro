FUNCTION structure_update,str_in,delete_tags=delete_tags,_Extra=extra
;once defined, IDL structures cannot have their tags changed. This program allows new tags to be added (not hard) and to change the type of existing tags.
; If delete_tags is set, this function will instead return a structure with any tag names supplied as keywords removed
;NOTE: This function will always return an anonymous array. Redefining named arrays is not possible within an IDL session
IF N_Elements(extra) EQ 0 THEN RETURN,str_in

tags_in=Tag_names(str_in)
tags_update=Tag_names(extra)
nt_in=N_Elements(tags_in)
nt_up=N_Elements(tags_update)

match_i=intarr(nt_up)-1 ;initialize array with no matches, giving new tags a value -1

FOR ti=0,nt_up-1 DO match_i[ti]=where(tags_update[ti] EQ tags_in)
match_i_i=where(match_i GE 0, n_match,complement=unmatch_i_i,ncomplement=n_unmatch)

IF Keyword_Set(delete_tags) THEN BEGIN
    tags_use=intarr(nt_in)+1
    tags_use[match_i[match_i_i]]=0
    tags_i=where(tags_use,n_use)
    IF n_match EQ 0 THEN RETURN,str_in 
        
    FOR m_i=0,n_match-1 DO BEGIN
        tag_i=match_i[match_i_i[m_i]]
        tempvar=str_in.(tag_i)
        undefine_fhd,tempvar ;this will recursively trace out the variable and delete it. Useful if it is a complicated structure or pointer to avoid a memory leak. 
    ENDFOR
    
    fn_string='str_out=Create_Struct(tags_in[tags_i]'
    FOR ti=0,n_use-1 DO fn_string+=','+'str_in.('+Strn(tags_i[ti])+')'
    fn_string+=')'
    err_code=Execute(fn_string,1,1)
ENDIF ELSE BEGIN
    n_use=nt_in+n_unmatch
    
    tags_combined=Strarr(n_use)
    tags_combined[0:nt_in-1]=tags_in
    IF n_match GT 0 THEN tags_combined[match_i[match_i_i]]=tags_update[match_i_i]
    
    tags_i=indgen(n_use)
    IF n_unmatch GT 0 THEN BEGIN
        tags_i[nt_in:nt_in+n_unmatch-1]=unmatch_i_i
        tags_combined[nt_in:nt_in+n_unmatch-1]=tags_update[unmatch_i_i]
    ENDIF
    
    ;work out ahead of time which of the two structures (str_in or extra) will be used for each final tag 
    tags_switch=intarr(n_use)+1
    tags_switch[0:nt_in-1]=0
    IF n_match GT 0 THEN BEGIN
        tags_switch[match_i[match_i_i]]=1
        tags_i[match_i[match_i_i]]=match_i_i
    ENDIF
    
    fn_string='str_out=Create_Struct(tags_combined'
    FOR ti=0,n_use-1 DO fn_string+=(tags_switch[ti] ? (',extra.('+Strn(tags_i[ti])+')'):(',str_in.('+Strn(tags_i[ti])+')'))
    fn_string+=')'
    err_code=Execute(fn_string,1,1)
ENDELSE

RETURN,str_out
END