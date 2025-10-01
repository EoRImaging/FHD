PRO fhd_multi_wrap,fhd_file_list,status_arr,N_simultaneous=N_simultaneous,transfer_mapfn=transfer_mapfn,_Extra=extra
except=!except
!except=0
compile_opt idl2,strictarrsubs  
individual_save=1
heap_gc

nd=size(fhd_file_list,/n_dim)
IF nd EQ 2 THEN BEGIN
    N_simultaneous=(size(fhd_file_list,/dimension))[1]
;    f_index=lonarr(nd)/N_simultaneous
ENDIF ELSE BEGIN
    IF N_Elements(N_simultaneous) EQ 0 THEN N_simultaneous=N_Elements(fhd_file_list)
    f_index=Lindgen(N_Elements(fhd_file_list))/N_simultaneous
ENDELSE
IF N_Elements(status_arr) NE N_Elements(fhd_file_list) THEN BEGIN
    fhd_save_io,status_str,var='status_str',/restore,file_path_fhd=fhd_file_list[0],_Extra=extra
    status_arr=Replicate(status_str,size(fhd_file_list,/dimensions))
    FOR fi=1L,N_Elements(fhd_file_list)-1 DO BEGIN
        fhd_save_io,status_str,var='status_str',/restore,file_path_fhd=fhd_file_list[fi],_Extra=extra
        status_arr[fi]=status_str
    ENDFOR
ENDIF

n_rep=Max(f_index)+1
FOR j=0L,n_rep-1 DO BEGIN
    file_i_use=where(f_index EQ j,nsub)
    IF nsub EQ 0 THEN CONTINUE
    fhd_file_list_sub=fhd_file_list[file_i_use]
    status_arr_sub=status_arr[file_i_use]
    
    IF N_Elements(transfer_mapfn) GT 1 THEN transfer_mapfn_use=transfer_mapfn[file_i_use] ELSE IF N_Elements(transfer_mapfn) EQ 1 THEN transfer_mapfn_use=transfer_mapfn 
    fhd_multi,status_arr_sub,fhd_file_list_sub,source_array2,comp_arr2,fhd_params=fhd_params,obs_arr=obs_arr,weights_arr=weights_arr2,timing=timing,$
        residual_array=residual_array2,dirty_uv_arr=dirty_uv_arr2,model_uv_full=model_uv_full2,model_uv_holo=model_uv_holo2,$
        silent=silent,beam_model=beam_model2,beam_corr=beam_corr2,norm_arr=norm_arr2,source_mask=source_mask2,$
        hpx_inds=hpx_inds2,transfer_mapfn=transfer_mapfn_use,_Extra=extra
    
    FOR fi=0L,nsub-1 DO BEGIN
        file_path_fhd=fhd_file_list_sub[fi]
        obs=obs_arr[fi]
        degpix=obs.degpix
        status_str=status_arr_sub[fi]
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
        apply_astrometry, obs, x_arr=meshgrid(obs.dimension,obs.elements,1), y_arr=meshgrid(obs.dimension,obs.elements,2), ra_arr=ra_arr, dec_arr=dec_arr, /xy2ad, /refraction
        dirty_array=Ptrarr(n_pol)
        weights_arr=Reform(weights_arr2[*,fi])
        FOR pol_i=0,n_pol-1 DO BEGIN
            dirty_array[pol_i]=Ptr_new(dirty_image_generate(*image_uv_arr[pol_i],degpix=degpix)*(*beam_correction[pol_i]))
        ENDFOR
        
        fhd_save_io,status_str,fhd_params,var='fhd_params',/compress,file_path_fhd=file_path_fhd,_Extra=extra
        fhd_log_settings,file_path_fhd,fhd=fhd_params,obs=obs,psf=psf,sub_dir='metadata' ;DO NOT SUPPLY CAL STRUCTURE HERE!!!
        ;compression reduces the file size by 50%, but takes 5-30 seconds longer
        fhd_save_io,status_str,var='fhd',file_path_fhd=file_path_fhd,path_use=fhd_sav_filepath,/no_save,_Extra=extra ;call first to obtain the correct path. Will NOT update status structure yet
        SAVE,residual_array,dirty_array,image_uv_arr,source_array,comp_arr,model_uv_full,model_uv_holo,weights_arr,$
            beam_base,beam_correction,astr,filename=fhd_sav_filepath+'.sav',/compress
        fhd_save_io,status_str,var='fhd',file_path_fhd=file_path_fhd,/force,_Extra=extra ;call a second time to update the status structure now that the file has actually been written
        
        undefine_fhd,weights_arr,residual_array,image_uv_arr,model_uv_full,model_uv_holo,beam_base,beam_correction,dirty_array
        status_arr[file_i_use[fi]]=status_str
    ENDFOR
    undefine_fhd,weights_arr2,residual_array2,dirty_uv_arr2,model_uv_full2,model_uv_holo2,beam_model2,beam_corr2,source_mask2,hpx_inds2
ENDFOR

END