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
    fhd_multi,fhd_file_list_sub,source_array,comp_arr,obs_arr=obs_arr,weights_arr=weights_arr2,timing=timing,$
        residual_array=residual_array2,dirty_uv_arr=dirty_uv_arr2,model_uv_full=model_uv_full2,model_uv_holo=model_uv_holo2,$
        silent=silent,beam_model=beam_model2,beam_corr=beam_corr2,normalization=normalization2,source_mask=source_mask2,$
        hpx_inds=hpx_inds2,_Extra=extra
    FOR fi=0,nsub-1 DO BEGIN
        file_path=fhd_file_list_sub[fi]
        obs=obs_arr[fi]
        residual_array=Reform(residual_array[*,fi])
        save,residual_array,dirty_array,image_uv_arr,source_array,comp_arr,model_uv_full,model_uv_holo,normalization,weights_arr,$
            beam_base,beam_correction,ra_arr,dec_arr,astr,filename=file_path_fhd+'_fhd.sav'
    ENDFOR
    hashname=hash_name_gen()
;    save_path=filepath(hashname)
ENDFOR

END