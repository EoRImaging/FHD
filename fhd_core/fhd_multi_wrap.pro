PRO fhd_multi_wrap,fhd_file_list,obs_arr,N_simultaneous=N_simultaneous,_Extra=extra
except=!except
!except=0
compile_opt idl2,strictarrsubs  

nd=size(fhd_file_list,/n_dim)
IF nd EQ 2 THEN BEGIN
    N_simultaneous=(size(fhd_file_list,/dimension))[1]
    
ENDIF ELSE BEGIN
    
    IF N_Elements(N_simultaneous) EQ 0 THEN N_simultaneous=N_Elements(fhd_file_list)
ENDELSE


f_index=lonarr(nd)/N_simultaneous
n_rep=Max(f_index)+1
FOR j=0L,n_rep-1 DO BEGIN
    fhd_file_list_sub=fhd_file_list[where(f_index EQ j,nsub)]
    fhd_multi,fhd_file_list_sub,obs_arr,_Extra=extra
ENDFOR

END