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

n_rep=Ceil(N_Elements(

fhd_multi,fhd_file_list_sub,obs_arr,_Extra=extra

END