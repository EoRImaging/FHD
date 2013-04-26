PRO combine_obs_healpix,file_list,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,$
    nside=nside,restore_last=restore_last,version=version,output_path=output_path,$
    flux_scale=flux_scale,obs_arr=obs_arr,image_filter_fn=image_filter_fn,ston_cut=ston_cut,_Extra=extra

except=!except
!except=0 
heap_gc

IF N_Elements(flux_scale) EQ 0 THEN flux_scale=1.
save_path=output_path+'_maps.sav'

IF Keyword_Set(restore_last) THEN BEGIN
    restore,save_path
    RETURN
ENDIF

;color_table=0.1
dimension=1024.
elements=dimension
npol=2
cal_ref_i=2
fix_flux=1
;combine_obs_sources,file_list,calibration,source_list,/restore_last,output_path=output_path

n_obs=N_Elements(file_list)

ftest=intarr(n_obs)
FOR file_i=0,n_obs-1 DO ftest[file_i]=file_test(file_list[file_i]+'_obs.sav')   
file_i_use=where(ftest,n_obs) 
file_list_use=file_list[file_i_use]

;cal_use=calibration[file_i_use]
;
;cal_thresh=[0.5,2.]
;obs_i_use=where((cal_use GT min(cal_thresh)) AND (cal_use LT max(cal_thresh)) AND ftest,$
;    n_obs,complement=fi_cut,ncomp=n_cut)
;cal_use[obs_i_use]=1./cal_use[obs_i_use]
;IF n_cut GT 0 THEN cal_use[fi_cut]=0
;
;cal_use[obs_i_use]=1.
;
;;    IF n_cut NE 0 THEN BEGIN
;;        cal_use[fi_cut]=1.
;;        n_obs=n_obs
;;        obs_i_use=file_i_use
;;    ENDIF
;cal_use*=flux_scale 
cal_use=replicate(flux_scale,n_obs)

FOR obs_i=0,n_obs-1 DO BEGIN
    file_path=file_list_use[obs_i]
    restore,file_path+'_obs.sav'
    IF obs_i EQ 0 THEN obs_arr=Replicate(obs,n_obs)
    obs_arr[obs_i]=obs
    astr=obs.astr
    pix_sky=4.*!Pi*!RaDeg^2./Product(Abs(astr.cdelt))
    Nside_chk=2.^(Ceil(ALOG(Sqrt(pix_sky/12.))/ALOG(2))) ;=1024. for 0.1119 degrees/pixel
    
    IF ~Keyword_Set(nside) THEN nside_use=Nside_chk
    nside_use=nside_use>Nside_chk
ENDFOR
IF Keyword_Set(nside) THEN nside_use=nside ELSE nside=nside_use
n_hpx=nside2npix(nside_use)

residual_hpx=Ptrarr(npol,/allocate)
weights_hpx=Ptrarr(npol,/allocate)
sources_hpx=Ptrarr(npol,/allocate)
restored_hpx=Ptrarr(npol,/allocate)
dirty_hpx=Ptrarr(npol,/allocate)
FOR pol_i=0,npol-1 DO BEGIN
  *residual_hpx[pol_i]=fltarr(n_hpx)
  *weights_hpx[pol_i]=fltarr(n_hpx)
  *sources_hpx[pol_i]=fltarr(n_hpx)
  *restored_hpx[pol_i]=fltarr(n_hpx)
  *dirty_hpx[pol_i]=fltarr(n_hpx)
ENDFOR

FOR obs_i=0,n_obs-1 DO BEGIN
    heap_gc
    obs=obs_arr[obs_i]
    n_vis=obs.n_vis
    file_path=file_list_use[obs_i]
;        restore,file_path+'_fhd_params.sav'
;        restore,file_path+'_fhd.sav'
    IF file_test(file_path+'_fhd_params.sav') EQ 0 THEN fhd=fhd_init() ELSE fhd=getvar_savefile(file_path+'_fhd_params.sav','fhd')
    image_uv_arr=getvar_savefile(file_path+'_fhd.sav','image_uv_arr')
    source_array=getvar_savefile(file_path+'_fhd.sav','source_array')
    model_uv_holo=getvar_savefile(file_path+'_fhd.sav','model_uv_holo')
    beam_base=getvar_savefile(file_path+'_fhd.sav','beam_base')
;   save,residual_array,dirty_array,image_uv_arr,source_array,comp_arr,model_uv_full,model_uv_holo,normalization,weights_arr,$
;       beam_base,beam_correction,ra_arr,dec_arr,astr,filename=file_path+'_fhd.sav'
    
    hpx_cnv=healpix_cnv_generate(file_path_fhd=file_path,nside=nside_chk,/restore_last,/silent)
    IF nside_chk NE nside_use THEN hpx_cnv=healpix_cnv_generate(obs,file_path_fhd=file_path,nside=nside,$
        mask=beam_mask,radius=radius,restore_last=0,_Extra=extra)
    
    astr=obs.astr            
    si_use=where(source_array.ston GE fhd.sigma_cut,ns_use)
    IF ns_use EQ 0 THEN CONTINUE
    source_arr=source_array[si_use]
    
;    IF Keyword_Set(ston_cut) THEN IF max(source_array.ston) LT fhd.ston_cut THEN CONTINUE
    
    restored_beam_width=(!RaDeg/(obs.MAX_BASELINE/obs.KPIX)/obs.degpix)/(2.*Sqrt(2.*Alog(2.)))
    FOR pol_i=0,npol-1 DO BEGIN
        dirty_single=dirty_image_generate(*image_uv_arr[pol_i],image_filter_fn=image_filter_fn,degpix=obs_arr[obs_i].degpix)*cal_use[obs_i]*n_vis
        model_single=dirty_image_generate(*model_uv_holo[pol_i],image_filter_fn=image_filter_fn,degpix=obs_arr[obs_i].degpix)*cal_use[obs_i]*n_vis

        sources_single=source_image_generate(source_arr,obs,pol_i=pol_i,resolution=16,dimension=dimension,width=restored_beam_width)*$
            cal_use[obs_i]*(*beam_base[pol_i])*n_vis ;source_arr is already in instrumental pol (x beam once)
        
        residual_single=dirty_single-model_single
        
        weights_single=(*beam_base[pol_i]^2.)*n_vis
        
        (*residual_hpx[pol_i])[hpx_cnv.inds]+=healpix_cnv_apply(residual_single,hpx_cnv)
        (*weights_hpx[pol_i])[hpx_cnv.inds]+=healpix_cnv_apply(weights_single,hpx_cnv)
        (*sources_hpx[pol_i])[hpx_cnv.inds]+=healpix_cnv_apply(sources_single,hpx_cnv)
        (*restored_hpx[pol_i])[hpx_cnv.inds]+=healpix_cnv_apply(residual_single+sources_single,hpx_cnv)
        (*dirty_hpx[pol_i])[hpx_cnv.inds]+=healpix_cnv_apply(dirty_single,hpx_cnv)
        
    ENDFOR
ENDFOR

save,residual_hpx,weights_hpx,sources_hpx,restored_hpx,dirty_hpx,hpx_inds,nside,obs_arr,filename=save_path

END