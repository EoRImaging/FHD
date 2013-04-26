PRO fhd_multi_wrap,fhd_file_list,N_simultaneous=N_simultaneous,quickview=quickview,individual_save=individual_save,_Extra=extra
except=!except
!except=0
compile_opt idl2,strictarrsubs  
individual_save=1

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
    fhd_multi,fhd_file_list_sub,source_array2,comp_arr2,fhd=fhd,obs_arr=obs_arr,weights_arr=weights_arr2,timing=timing,$
        residual_array=residual_array2,dirty_uv_arr=dirty_uv_arr2,model_uv_full=model_uv_full2,model_uv_holo=model_uv_holo2,$
        silent=silent,beam_model=beam_model2,beam_corr=beam_corr2,norm_arr=norm_arr2,source_mask=source_mask2,$
        hpx_inds=hpx_inds2,_Extra=extra
    
    IF Keyword_Set(quickview) OR Keyword_Set(individual_save) THEN BEGIN
        FOR fi=0,nsub-1 DO BEGIN
            file_path_fhd=fhd_file_list_sub[fi]
            obs=obs_arr[fi]
            degpix=obs.degpix
            residual_array=Reform(residual_array2[*,fi])
            image_uv_arr=Reform(dirty_uv_arr2[*,fi])
            source_array=*source_array2[fi]
            comp_arr=*comp_arr2[fi]
            model_uv_full=Reform(model_uv_full2[*,fi])
            model_uv_holo=Reform(model_uv_holo2[*,fi])
            beam_base=Reform(beam_model2[*,fi])
            beam_correction=Reform(beam_corr2[*,fi])
            normalization=norm_arr2[fi]
            n_pol=obs.n_pol
            astr=obs.astr
            xy2ad,meshgrid(obs.dimension,obs.elements,1),meshgrid(obs.dimension,obs.elements,2),astr,ra_arr,dec_arr
            dirty_array=Ptrarr(n_pol)
            weights_arr=Reform(weights_arr2[*,fi])
            FOR pol_i=0,n_pol-1 DO BEGIN
                dirty_array[pol_i]=Ptr_new(dirty_image_generate(*image_uv_arr[pol_i],degpix=degpix)*(*beam_correction[pol_i]))
            ENDFOR
            IF N_Elements(quickview) EQ 0 THEN quickview=1
            IF Keyword_Set(quickview) THEN fhd_quickview,fhd,obs,image_uv_arr,model_uv_holo,source_array,comp_arr,$
                beam_base,file_path_fhd=file_path_fhd,_Extra=extra
            IF Keyword_Set(individual_save) THEN $
                save,residual_array,dirty_array,image_uv_arr,source_array,comp_arr,model_uv_full,model_uv_holo,normalization,weights_arr,$
                    beam_base,beam_correction,ra_arr,dec_arr,astr,filename=file_path_fhd+'_fhd.sav'
        ENDFOR
    ENDIF
    hashname=hash_name_gen()
;    save_path=filepath(hashname)
ENDFOR

END