PRO combine_obs_healpix,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,smooth_hpx,$
    nside=nside,restore_last=restore_last,version=version,data_directory=data_directory,$
    flux_scale=flux_scale,obs_arr=obs_arr

except=!except
!except=0 
heap_gc

IF N_Elements(flux_scale) EQ 0 THEN flux_scale=1.

IF not Keyword_Set(data_directory) THEN vis_path_default,data_directory

IF N_Elements(version) EQ 0 THEN version=0 ELSE version=Fix(version)
version_name='v'+strn(version)
version_dirname='fhd_'+version_name
output_dir=filepath('',root=data_directory,sub=['Combined_obs',version_dirname])
IF file_test(rootdir('mwa')+output_dir) EQ 0 THEN file_mkdir,rootdir('mwa')+output_dir   
output_path=filepath('Healpix'+'v'+strn(version)+'.sav',root_dir=rootdir('mwa')+output_dir)


IF not Keyword_Set(restore_last) THEN BEGIN
    
    ;color_table=0.1
    dimension=1024.
    elements=dimension
    npol=2
    cal_ref_i=2
    fix_flux=1
    combine_obs_sources,calibration,source_list,filename_list,version=version,/restore_last,data_directory=data_directory
    
    cal_use=calibration
    obs_i_use=where(cal_use,n_obs,complement=fi_cut,ncomp=n_cut)
    cal_use[obs_i_use]=1./cal_use[obs_i_use]
    
    IF n_cut NE 0 THEN BEGIN
        cal_use[fi_cut]=1.
        n_obs=N_Elements(cal_use)
        obs_i_use=lindgen(n_obs)
    ENDIF
    
;    obs_base=vis_struct_init_obs()
;    obs_arr=Replicate(obs_base,n_obs)
;    obs_arr=Ptrarr(n_obs,/allocate)
    hpx_cnv=Ptrarr(n_obs,/allocate) 
;    lon_arr=fltarr(n_obs)
;    lat_arr=fltarr(n_obs)
    
    
    FOR obs_i=0,n_obs-1 DO BEGIN
        vis_path_default,data_directory,filename_list[obs_i_use[obs_i]],file_path,version=version
        restore,file_path+'_obs.sav'
        IF obs_i EQ 0 THEN obs_arr=Replicate(obs,n_obs)
        obs_arr[obs_i]=obs
;        lon_arr[obs_i]=obs.obsra
;        lat_arr[obs_i]=obs.obsdec
        *hpx_cnv[obs_i]=healpix_cnv_generate(obs,nside=nside,/restore_last,/silent)
        IF obs_i EQ 0 THEN nside_check=nside ELSE IF nside NE nside_check THEN $
            message,String(format='("Mismatched HEALPix NSIDE for ",A)',filename_list[obs_i]) 
    ENDFOR
    hpx_ind_map=healpix_combine_inds(hpx_cnv,hpx_inds=hpx_inds)
    n_hpx=N_Elements(hpx_inds)

;    n_obs=N_Elements(cal_use)
;    fi_use=lindgen(n_obs)
;    cal_use[*]=1.
    
    cal_use*=flux_scale 
    
    residual_hpx=Ptrarr(npol,/allocate)
    weights_hpx=Ptrarr(npol,/allocate)
    sources_hpx=Ptrarr(npol,/allocate)
    restored_hpx=Ptrarr(npol,/allocate)
    dirty_hpx=Ptrarr(npol,/allocate)
;    mrc_hpx=Ptrarr(npol,/allocate)
    smooth_hpx=Ptrarr(npol,/allocate)
    FOR pol_i=0,npol-1 DO BEGIN
      *residual_hpx[pol_i]=fltarr(n_hpx)
      *weights_hpx[pol_i]=fltarr(n_hpx)
      *sources_hpx[pol_i]=fltarr(n_hpx)
      *restored_hpx[pol_i]=fltarr(n_hpx)
      *dirty_hpx[pol_i]=fltarr(n_hpx)
      *smooth_hpx[pol_i]=fltarr(n_hpx)
    ENDFOR
    
    FOR obs_i=0,n_obs-1 DO BEGIN
        heap_gc
        obs=obs_arr[obs_i]
        filename=filename_list[obs_i]
        vis_path_default,data_directory,filename,file_path,version=version,obs=obs
        restore,file_path+'_fhd_params.sav'
        ;restores the fhd structure that contains the parameters used in deconvolution 
        restore,file_path+'_fhd.sav'
    ;   save,residual_array,dirty_array,image_uv_arr,source_array,comp_arr,model_uv_full,model_uv_holo,normalization,weights_arr,$
    ;       beam_base,beam_correction,ra_arr,dec_arr,astr,filename=file_path+'_fhd.sav'
;        restore,file_path+'_output.sav'
    ;    save,mrc_cat,mrc_image,beam_mask,beam_avg,instr_images,stokes_images,instr_sources,stokes_sources,$
    ;        beam_est,model_uv_arr,model_holo_arr,calibration,p_map_simple,p_corr_simple,filename=file_path+'_output.sav'
        restore,file_path+'_obs.sav'
                
        astr=(*obs.bin).astr            
        si_use=where(source_array.ston GE fhd.sigma_cut,ns_use)
        source_arr=source_array[si_use]
                
        FOR pol_i=0,npol-1 DO BEGIN
            dirty_single=dirty_image_generate(*image_uv_arr[pol_i])*cal_use[obs_i]
            model_single=dirty_image_generate(*model_uv_holo[pol_i])*cal_use[obs_i]

            sources_single=source_image_generate(source_arr,obs,pol_i=pol_i,resolution=8,dimension=dimension,width=.75)*(*beam_base[pol_i])*cal_use[obs_i]
            
            residual_single=dirty_single-model_single
            
            residual_background=dirty_image_generate(*image_uv_arr[pol_i]-*model_uv_holo[pol_i],/hanning)*cal_use[obs_i]
            residual_smooth=residual_single-residual_background
            weights_single=(*beam_base[pol_i]^2.)
            
            
            (*residual_hpx[pol_i])[*hpx_ind_map[obs_i]]+=healpix_cnv_apply(residual_single,*hpx_cnv[obs_i])
            (*weights_hpx[pol_i])[*hpx_ind_map[obs_i]]+=healpix_cnv_apply(weights_single,*hpx_cnv[obs_i])
            (*sources_hpx[pol_i])[*hpx_ind_map[obs_i]]+=healpix_cnv_apply(sources_single,*hpx_cnv[obs_i])
            (*restored_hpx[pol_i])[*hpx_ind_map[obs_i]]+=healpix_cnv_apply(residual_single+sources_single,*hpx_cnv[obs_i])
            (*dirty_hpx[pol_i])[*hpx_ind_map[obs_i]]+=healpix_cnv_apply(dirty_single,*hpx_cnv[obs_i])
            (*smooth_hpx[pol_i])[*hpx_ind_map[obs_i]]+=healpix_cnv_apply(residual_smooth,*hpx_cnv[obs_i])
            
        ENDFOR
    ENDFOR
    
    save,residual_hpx,weights_hpx,sources_hpx,restored_hpx,dirty_hpx,smooth_hpx,hpx_inds,nside,obs_arr,filename=output_path
ENDIF ELSE restore,output_path

END