PRO fhd_multi_wrap,fhd_file_list,N_simultaneous=N_simultaneous,_Extra=extra
except=!except
!except=0
compile_opt idl2,strictarrsubs  

nd=size(fhd_file_list,/n_dim)
IF nd EQ 2 THEN BEGIN
    N_simultaneous=(size(fhd_file_list,/dimension))[1]
;    f_index=lonarr(nd)/N_simultaneous
ENDIF ELSE BEGIN
    
    IF N_Elements(N_simultaneous) EQ 0 THEN N_simultaneous=N_Elements(fhd_file_list)
    f_index=Lindgen(N_Elements(fhd_file_list))/N_simultaneous
ENDELSE

n_rep=Max(f_index)+1
FOR j=0L,n_rep-1 DO BEGIN
    fhd_file_list_sub=fhd_file_list[where(f_index EQ j,nsub)]
    fhd_multi,fhd_file_list_sub,source_array,comp_arr,weights_arr=weights_arr,timing=timing,$
        residual_array=residual_array,dirty_uv_arr=dirty_uv_arr,model_uv_full=model_uv_full,model_uv_holo=model_uv_holo,$
        silent=silent,beam_model=beam_model,beam_corr=beam_corr,normalization=normalization,source_mask=source_mask,hpx_inds=hpx_inds,_Extra=extra
ENDFOR

END