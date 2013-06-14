PRO combine_obs_healpix,file_list,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,mrc_hpx=mrc_hpx,$
    nside=nside,restore_last=restore_last,version=version,output_path=output_path,beam_threshold=beam_threshold,$
    flux_scale=flux_scale,obs_arr=obs_arr,image_filter_fn=image_filter_fn,ston_cut=ston_cut,silent=silent,$
    catalog_file_path=catalog_file_path,_Extra=extra

except=!except
!except=0 
heap_gc

IF N_Elements(flux_scale) EQ 0 THEN flux_scale=1.
IF N_Elements(beam_threshold) EQ 0 THEN beam_threshold=0.05
save_path=output_path+'_maps.sav'

IF Keyword_Set(restore_last) THEN BEGIN
    RESTORE,save_path
    RETURN
ENDIF

;color_table=0.1
dimension=1024.
elements=dimension
n_pol=2
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

IF ~Keyword_Set(silent) THEN print,'Initializing HEALPix maps'
FOR obs_i=0,n_obs-1 DO BEGIN
    file_path=file_list_use[obs_i]
    RESTORE,file_path+'_obs.sav'
    IF obs_i EQ 0 THEN obs_arr=Replicate(obs,n_obs)
    obs_arr[obs_i]=obs
    astr=obs.astr
    pix_sky=4.*!Pi*!RaDeg^2./Product(Abs(astr.cdelt))
    Nside_chk=2.^(Ceil(ALOG(Sqrt(pix_sky/12.))/ALOG(2))) ;=1024. for 0.1119 degrees/pixel
    
    IF ~Keyword_Set(nside) THEN nside_use=Nside_chk
    nside_use=nside_use>Nside_chk
ENDFOR
IF Keyword_Set(nside) THEN nside_use=nside ELSE nside=nside_use
hpx_cnv=Ptrarr(n_obs,/allocate)
FOR obs_i=0.,n_obs-1 DO BEGIN
    file_path_fhd=file_list_use[obs_i]
    obs=obs_arr[obs_i]
    dimension=obs.dimension
    elements=obs.elements
    xvals=meshgrid(dimension,elements,1)-dimension/2
    yvals=meshgrid(dimension,elements,2)-elements/2
    
    beam_base=getvar_savefile(file_path_fhd+'_fhd.sav','beam_base')
    beam_mask=fltarr(dimension,elements)+1.
    FOR pol_i=0,(n_pol<2)-1 DO BEGIN
        mask0=fltarr(dimension,elements)
        mask_i=region_grow(*beam_base[pol_i],Floor(obs.obsx)+dimension*Floor(obs.obsy),thresh=[beam_threshold,max(*beam_base[pol_i])])
        mask0[mask_i]=1
        beam_mask*=mask0
    ENDFOR

    ;supply beam_mask in case file is missing and needs to be generated
    *hpx_cnv[obs_i]=healpix_cnv_generate(obs,file_path_fhd=file_path_fhd,nside=nside_chk,mask=beam_mask,radius=radius,restore_last=0) 
    IF N_Elements(nside) EQ 0 THEN nside=nside_chk
    IF nside_chk NE nside THEN *hpx_cnv[obs_i]=healpix_cnv_generate(obs,file_path_fhd=file_path_fhd,nside=nside,mask=beam_mask,radius=radius,restore_last=0)
ENDFOR
hpx_ind_map=healpix_combine_inds(hpx_cnv,hpx_inds=hpx_inds);,reverse_ind=reverse_inds)

n_hpx=N_Elements(hpx_inds)

residual_hpx=Ptrarr(n_pol,/allocate)
weights_hpx=Ptrarr(n_pol,/allocate)
sources_hpx=Ptrarr(n_pol,/allocate)
restored_hpx=Ptrarr(n_pol,/allocate)
dirty_hpx=Ptrarr(n_pol,/allocate)
IF Keyword_Set(catalog_file_path) THEN mrc_hpx=Ptrarr(n_pol,/allocate)
FOR pol_i=0,n_pol-1 DO BEGIN
  *residual_hpx[pol_i]=fltarr(n_hpx)
  *weights_hpx[pol_i]=fltarr(n_hpx)
  *sources_hpx[pol_i]=fltarr(n_hpx)
  *restored_hpx[pol_i]=fltarr(n_hpx)
  *dirty_hpx[pol_i]=fltarr(n_hpx)
  IF Keyword_Set(catalog_file_path) THEN *mrc_hpx[pol_i]=fltarr(n_hpx)
ENDFOR

FOR obs_i=0,n_obs-1 DO BEGIN
    IF ~Keyword_Set(silent) THEN print,StrCompress('Converting snapshot '+Strn(obs_i+1)+' of '+Strn(n_obs)+' to common HEALPix coordinates')
    heap_gc
    obs=obs_arr[obs_i]
    n_vis_rel=obs.n_vis/Mean(obs_arr.n_vis)
    file_path=file_list_use[obs_i]
;        restore,file_path+'_fhd_params.sav'
;        restore,file_path+'_fhd.sav'
    IF file_test(file_path+'_fhd_params.sav') EQ 0 THEN fhd=fhd_init() ELSE fhd=getvar_savefile(file_path+'_fhd_params.sav','fhd')
    image_uv_arr=getvar_savefile(file_path+'_fhd.sav','image_uv_arr')
    weights_uv_arr=getvar_savefile(file_path+'_fhd.sav','weights_arr')
    source_array=getvar_savefile(file_path+'_fhd.sav','source_array')
    model_uv_holo=getvar_savefile(file_path+'_fhd.sav','model_uv_holo')
    beam_base=getvar_savefile(file_path+'_fhd.sav','beam_base')
;   save,residual_array,dirty_array,image_uv_arr,source_array,comp_arr,model_uv_full,model_uv_holo,normalization,weights_arr,$
;       beam_base,beam_correction,ra_arr,dec_arr,astr,filename=file_path+'_fhd.sav'
    
;    hpx_cnv=healpix_cnv_generate(file_path_fhd=file_path,nside=nside_chk,/restore_last,/silent)
;    IF nside_chk NE nside_use THEN hpx_cnv=healpix_cnv_generate(obs,file_path_fhd=file_path,nside=nside_use,$
;        mask=beam_mask,radius=radius,restore_last=0,_Extra=extra)
    
    astr=obs.astr            
    si_use=where(source_array.ston GE fhd.sigma_cut,ns_use)
    IF ns_use EQ 0 THEN CONTINUE
    source_arr=source_array[si_use]
    
;    IF Keyword_Set(ston_cut) THEN IF max(source_array.ston) LT fhd.ston_cut THEN CONTINUE
    
;    n_vis_rel=1.
    restored_beam_width=(!RaDeg/(obs.MAX_BASELINE/obs.KPIX)/obs.degpix)/(2.*Sqrt(2.*Alog(2.)))
    FOR pol_i=0,n_pol-1 DO BEGIN
        dirty_single=dirty_image_generate(*image_uv_arr[pol_i],image_filter_fn=image_filter_fn,degpix=obs_arr[obs_i].degpix,weights=*weights_uv_arr[pol_i])*cal_use[obs_i]*n_vis_rel
        model_single=dirty_image_generate(*model_uv_holo[pol_i],image_filter_fn=image_filter_fn,degpix=obs_arr[obs_i].degpix,weights=*weights_uv_arr[pol_i])*cal_use[obs_i]*n_vis_rel

        sources_single=source_image_generate(source_arr,obs,pol_i=pol_i,resolution=16,dimension=dimension,width=restored_beam_width)*$
            cal_use[obs_i]*(*beam_base[pol_i])*n_vis_rel ;source_arr is already in instrumental pol (x beam once)
        
        IF Keyword_Set(catalog_file_path) AND file_test(catalog_file_path) EQ 1 THEN BEGIN
            mrc_cat=mrc_catalog_read(astr,file_path=catalog_file_path)
            mrc_i_use=where((mrc_cat.x GE 0) AND (mrc_cat.x LE dimension-1) AND (mrc_cat.y GE 0) AND (mrc_cat.y LE elements-1),n_mrc)
            
;            mrc_image*=5./median(mrc_cat.flux.I)
            IF n_mrc GT 2 THEN BEGIN
                mrc_cat=mrc_cat[mrc_i_use]
                mrc_image=source_image_generate(mrc_cat,obs_out,pol_i=4,resolution=16,dimension=dimension,$
                    width=1.2,ring=8.)*(*beam_base[pol_i])^2.*n_vis_rel
;                mrc_image*=2.5^(0.8*400./150) ;very approximate spectral index correction
            ENDIF
        ENDIF ELSE n_mrc=0
        residual_single=dirty_single-model_single
        
        weights_single=(*beam_base[pol_i]^2.)*n_vis_rel
        
        (*residual_hpx[pol_i])[*hpx_ind_map[obs_i]]+=healpix_cnv_apply(residual_single,*hpx_cnv[obs_i])
        (*weights_hpx[pol_i])[*hpx_ind_map[obs_i]]+=healpix_cnv_apply(weights_single,*hpx_cnv[obs_i])
        (*sources_hpx[pol_i])[*hpx_ind_map[obs_i]]+=healpix_cnv_apply(sources_single,*hpx_cnv[obs_i])
        (*restored_hpx[pol_i])[*hpx_ind_map[obs_i]]+=healpix_cnv_apply(residual_single+sources_single,*hpx_cnv[obs_i])
        (*dirty_hpx[pol_i])[*hpx_ind_map[obs_i]]+=healpix_cnv_apply(dirty_single,*hpx_cnv[obs_i])
        IF n_mrc GT 0 THEN (*mrc_hpx[pol_i])[*hpx_ind_map[obs_i]]+=healpix_cnv_apply(mrc_image,*hpx_cnv[obs_i])
    ENDFOR
    dv=1
ENDFOR

SAVE,hpx_inds,nside,obs_arr,residual_hpx,weights_hpx,sources_hpx,restored_hpx,dirty_hpx,mrc_hpx,filename=save_path,/compress

END