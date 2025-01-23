FUNCTION source_list_append,obs,source_list1,source_list2,exclude_duplicate=exclude_duplicate

;identify duplicate entries
n1=N_Elements(source_list1)
n2=N_Elements(source_list2)
IF n1 EQ 0 THEN RETURN, source_list2
IF n2 EQ 0 THEN RETURN, source_list1
dup_flag=lonarr(n1)-1
degpix=obs.degpix
ang_threshold=degpix/100.
FOR si1=0L,n1-1 DO BEGIN
    dup_flag[si1]=Min(where(Abs(angle_difference(source_list2.dec,source_list2.RA,$
        source_list1[si1].dec,source_list1[si1].RA,/degree,/nearest)) LT ang_threshold))
ENDFOR

dup_i=where(dup_flag GE 0,n_dup,complement=uniq_i1,ncomplement=n_uniq1)
IF Keyword_Set(exclude_duplicate) THEN BEGIN
    ;Only use unique sources from first source list.
    ;If no unique sources, return the entire first source list.
    IF n_uniq1 GT 0 THEN source_list=source_list1[uniq_i1] ELSE source_list=source_list1
ENDIF ELSE BEGIN
    source_list=[source_list1,source_list2] 
ENDELSE

RETURN,source_list
END