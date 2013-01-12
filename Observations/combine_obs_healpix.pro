PRO combine_obs_healpix,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,mrc_hpx,smooth_hpx,$
    nside=nside,restore_last=restore_last,version=version,data_directory=data_directory,$
    lon_arr=lon_arr,lat_arr=lat_arr,flux_scale=flux_scale

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
    fi_use=where(cal_use,n_files,complement=fi_cut,ncomp=n_cut)
    cal_use[fi_use]=1./cal_use[fi_use]
    
    IF n_cut NE 0 THEN BEGIN
        cal_use[fi_cut]=1.
        n_files=N_Elements(cal_use)
        fi_use=lindgen(n_files)
    ENDIF

;    n_files=N_Elements(cal_use)
;    fi_use=lindgen(n_files)
;    cal_use[*]=1.
    
    cal_use*=flux_scale 
    
    residual_hpx=Ptrarr(npol,/allocate)
    weights_hpx=Ptrarr(npol,/allocate)
    sources_hpx=Ptrarr(npol,/allocate)
    restored_hpx=Ptrarr(npol,/allocate)
    dirty_hpx=Ptrarr(npol,/allocate)
    mrc_hpx=Ptrarr(npol,/allocate)
    smooth_hpx=Ptrarr(npol,/allocate)
 
    lon_arr=fltarr(n_files)
    lat_arr=fltarr(n_files)
    
    FOR fi0=0,n_files-1 DO BEGIN
        heap_gc
        fi=fi_use[fi0]
        filename=filename_list[fi]
        vis_path_default,data_directory,filename,file_path,version=version
        restore,file_path+'_fhd.sav'
    ;   save,residual_array,dirty_array,image_uv_arr,source_array,comp_arr,model_uv_full,model_uv_holo,normalization,weights_arr,$
    ;       beam_base,beam_correction,ra_arr,dec_arr,astr,filename=file_path+'_fhd.sav'
        restore,file_path+'_output.sav'
    ;    save,mrc_cat,mrc_image,beam_mask,beam_avg,instr_images,stokes_images,instr_sources,stokes_sources,$
    ;        beam_est,model_uv_arr,model_holo_arr,calibration,p_map_simple,p_corr_simple,filename=file_path+'_output.sav'
        restore,file_path+'_obs.sav'
        
;        print,angle_difference(obs.obsdec,obs.obsra,obs.zendec,obs.zenra,/degree,/nearest)        
;        CONTINUE
        
        lon_arr[fi0]=obs.obsra
        lat_arr[fi0]=obs.obsdec
        astr=obs.astr
        IF not Keyword_Set(nside) THEN BEGIN
            pix_sky=4.*!Pi*!RaDeg^2./Product(astr.cdelt)
            Nside=2.^(Ceil(ALOG(Sqrt(pix_sky/12.))/ALOG(2))) ;=1024. for 0.1119 degrees/pixel
            npix=nside2npix(nside)
            FOR pol_i=0,npol-1 DO BEGIN
                *residual_hpx[pol_i]=Fltarr(npix)
                *weights_hpx[pol_i]=Fltarr(npix)
                *sources_hpx[pol_i]=Fltarr(npix)
                *restored_hpx[pol_i]=Fltarr(npix)
                *dirty_hpx[pol_i]=Fltarr(npix)
                
                *mrc_hpx[pol_i]=Fltarr(npix)
                *smooth_hpx[pol_i]=Fltarr(npix)
                
            ENDFOR
        ENDIF ELSE npix=nside2npix(nside)
        IF fi0 EQ 0 THEN hpx_inds=lon64arr(npix)
        
        FOR pol_i=0,npol-1 DO BEGIN
            model_uv_single=*model_uv_holo[pol_i]
            
            dirty_single=dirty_image_generate(*image_uv_arr[pol_i])*cal_use[fi]
            model_single=dirty_image_generate(model_uv_single)*cal_use[fi]
            sources_single=*instr_sources[pol_i]*(*beam_base[pol_i])*cal_use[fi]
            
            residual_single=dirty_single-model_single
            
            residual_background=dirty_image_generate(*image_uv_arr[pol_i]-*model_uv_holo[pol_i],/hanning)*cal_use[fi]
            residual_smooth=residual_single-residual_background
            weights_single=(*beam_base[pol_i]^2.)*beam_mask;*(*p_map_simple[pol_i])
            mrc_single=mrc_image*weights_single*cal_use[fi]
            
            image_list=Ptrarr(5,/allocate)
            *image_list[0]=sources_single
            *image_list[1]=residual_single+sources_single
            *image_list[2]=dirty_single
            *image_list[3]=mrc_single
            *image_list[4]=residual_smooth
            
            healpix_cnv,residual_single,hpx_inds0,hpx_vals,hpx_weights,mask=beam_mask,astr=astr,weights=weights_single,nside=nside,image_list=image_list
            
            (*residual_hpx[pol_i])[hpx_inds0]+=hpx_vals
            (*weights_hpx[pol_i])[hpx_inds0]+=hpx_weights
            (*sources_hpx[pol_i])[hpx_inds0]+=*image_list[0]
            (*restored_hpx[pol_i])[hpx_inds0]+=*image_list[1]
            (*dirty_hpx[pol_i])[hpx_inds0]+=*image_list[2]
            (*mrc_hpx[pol_i])[hpx_inds0]+=*image_list[3]
            (*smooth_hpx[pol_i])[hpx_inds0]+=*image_list[4]
            
            hpx_inds[hpx_inds0]=1
            Ptr_free,image_list
        ENDFOR
        Ptr_free,residual_array,dirty_array,image_uv_arr,weights_arr,$
           beam_base,beam_correction,instr_images,stokes_images,instr_sources,stokes_sources,$
           model_uv_arr,model_holo_arr;,p_map_simple,p_corr_simple;,map_fn_arr
    ENDFOR
    hpx_inds=where(hpx_inds,n_hpx)
    FOR pol_i=0,npol-1 DO BEGIN
        *residual_hpx[pol_i]=(*residual_hpx[pol_i])[hpx_inds]
        *weights_hpx[pol_i]=(*weights_hpx[pol_i])[hpx_inds]
        *sources_hpx[pol_i]=(*sources_hpx[pol_i])[hpx_inds]
        *restored_hpx[pol_i]=(*restored_hpx[pol_i])[hpx_inds]
        *dirty_hpx[pol_i]=(*dirty_hpx[pol_i])[hpx_inds]
        *mrc_hpx[pol_i]=(*mrc_hpx[pol_i])[hpx_inds]
        *smooth_hpx[pol_i]=(*smooth_hpx[pol_i])[hpx_inds]    
    ENDFOR
    
    save,residual_hpx,weights_hpx,sources_hpx,restored_hpx,dirty_hpx,mrc_hpx,smooth_hpx,hpx_inds,nside,lon_arr,lat_arr,filename=output_path
ENDIF ELSE restore,output_path

END