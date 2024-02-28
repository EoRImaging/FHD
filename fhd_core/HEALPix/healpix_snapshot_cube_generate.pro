PRO healpix_snapshot_cube_generate,obs_in,status_str,psf_in,cal,params,vis_arr,vis_model_arr=vis_model_arr,$
    file_path_fhd=file_path_fhd,ps_dimension=ps_dimension,ps_fov=ps_fov,ps_degpix=ps_degpix,$
    ps_kbinsize=ps_kbinsize,ps_kspan=ps_kspan,ps_beam_threshold=ps_beam_threshold,ps_nfreq_avg=ps_nfreq_avg,$
    rephase_weights=rephase_weights,n_avg=n_avg,vis_weights=vis_weights,split_ps_export=split_ps_export,$
    restrict_hpx_inds=restrict_hpx_inds,hpx_radius=hpx_radius,cmd_args=cmd_args,save_uvf=save_uvf,save_imagecube=save_imagecube,$
    obs_out=obs_out,psf_out=psf_out,ps_tile_flag_list=ps_tile_flag_list,_Extra=extra
    
  t0=Systime(1)
  
  IF N_Elements(silent) EQ 0 THEN silent=0
  IF N_Elements(status_str) EQ 0 THEN fhd_save_io,status_str,file_path_fhd=file_path_fhd,/no_save
  IF N_Elements(rephase_weights) EQ 0 THEN rephase_weights=1
  
  IF Keyword_Set(split_ps_export) THEN cube_name=['hpx_even','hpx_odd'] $
    ELSE cube_name='healpix_cube'
  
  IF N_Elements(obs_in) EQ 0 THEN fhd_save_io,status_str,obs_in,var='obs',/restore,file_path_fhd=file_path_fhd,_Extra=extra
  n_pol=obs_in.n_pol
  n_freq=obs_in.n_freq
  
  ;whether cubes are recalculated is now set in fhd_setup (or array_simulator) through status_str
  IF Keyword_Set(split_ps_export) THEN cube_test=Min(status_str.hpx_even[0:n_pol-1])<Min(status_str.hpx_odd[0:n_pol-1]) $
    ELSE cube_test=Min(status_str.healpix_cube[0:n_pol-1])
  IF cube_test GT 0 THEN BEGIN
    print,'HEALPix cubes not recalculated'
    RETURN
  ENDIF
  
  IF N_Elements(psf_in) EQ 0 THEN fhd_save_io,status_str,psf_in,var='psf',/restore,file_path_fhd=file_path_fhd,_Extra=extra
  IF N_Elements(params) EQ 0 THEN fhd_save_io,status_str,params,var='params',/restore,file_path_fhd=file_path_fhd,_Extra=extra
  IF N_Elements(cal) EQ 0 THEN IF status_str.cal GT 0 THEN fhd_save_io,status_str,cal,var='cal',/restore,file_path_fhd=file_path_fhd,_Extra=extra
  
  IF ~Keyword_Set(n_avg) THEN n_avg=1 ;default of no averaging
  n_freq_use=Floor(n_freq/n_avg)
  IF N_Elements(ps_beam_threshold) GT 0 THEN beam_threshold=ps_beam_threshold ELSE beam_threshold=0
  
  IF Keyword_Set(ps_kbinsize) THEN kbinsize=ps_kbinsize ELSE $
    IF Keyword_Set(ps_fov) THEN kbinsize=!RaDeg/ps_FoV ELSE kbinsize=obs_in.kpix
  FoV_use=!RaDeg/kbinsize
  
  IF Keyword_Set(ps_kspan) THEN dimension_use=ps_kspan/kbinsize ELSE $
    IF Keyword_Set(ps_dimension) THEN dimension_use=ps_dimension ELSE $
    IF Keyword_Set(ps_degpix) THEN dimension_use=FoV_use/ps_degpix ELSE dimension_use=FoV_use/obs_in.degpix
  
  nfreq_avg_in=Round(n_freq/Max(psf_in.fbin_i+1))
  IF ~Keyword_Set(ps_nfreq_avg) THEN  ps_nfreq_avg=nfreq_avg_in
  
  degpix_use=FoV_use/dimension_use
  pix_sky=4.*!Pi*!RaDeg^2./degpix_use^2.
  Nside_chk=2.^(Ceil(ALOG(Sqrt(pix_sky/12.))/ALOG(2))) ;=1024. for 0.1119 degrees/pixel
  IF ~Keyword_Set(nside) THEN nside_use=Nside_chk
  nside_use=nside_use>Nside_chk
  IF Keyword_Set(nside) THEN nside_use=nside ELSE nside=nside_use
  
  obs_out=fhd_struct_update_obs(obs_in,n_pol=n_pol,beam_nfreq_avg=ps_nfreq_avg,FoV=FoV_use,dimension=dimension_use)
  ps_psf_resolution=Round(psf_in.resolution*obs_out.kpix/obs_in.kpix)
  IF (kbinsize EQ obs_in.kpix) AND Min((*obs_out.baseline_info).fbin_i EQ (*obs_in.baseline_info).fbin_i) THEN BEGIN
    ;If the beam model to be used for making the snapshot cubes is the same as the one used for imaging, then simply copy the existing data and don't recalculate it
    IF N_Elements(antenna) EQ 0 THEN fhd_save_io,status_str,antenna_out,var='antenna',/restore,file_path_fhd=file_path_fhd,_Extra=extra ELSE antenna_out=antenna
    psf_out=psf_in
  ENDIF ELSE  psf_out=beam_setup(obs_out,0,antenna_out,/no_save,psf_resolution=ps_psf_resolution,/silent,_Extra=extra)
  
  beam_arr=beam_image_cube(obs_out,psf_out,n_freq=n_freq_use,beam_mask=beam_mask,/square,beam_threshold=beam_threshold)
  if N_Elements(hpx_radius) EQ 0 then hpx_radius=FoV_use/sqrt(2.)
  hpx_cnv=healpix_cnv_generate(obs_out,file_path_fhd=file_path_fhd,nside=nside_use,restore_last=0,/no_save,$
    mask=beam_mask,hpx_radius=hpx_radius,restrict_hpx_inds=restrict_hpx_inds,_Extra=extra)
  IF Keyword_Set(restrict_hpx_inds) THEN nside=nside_use
  hpx_inds=hpx_cnv.inds
  n_hpx=N_Elements(hpx_inds)
  
  fhd_log_settings,file_path_fhd+'_ps',obs=obs_out,psf=psf_out,antenna=antenna_out,cal=cal,cmd_args=cmd_args,/overwrite,sub_dir='metadata'
  undefine_fhd,antenna_out
  
  IF Min(Ptr_valid(vis_weights)) LT n_pol THEN fhd_save_io,status_str,vis_weights_use,var='vis_weights',/restore,file_path_fhd=file_path_fhd,_Extra=extra $
    ELSE vis_weights_use=Pointer_copy(vis_weights)
  
  IF Keyword_Set(ps_tile_flag_list) THEN BEGIN
    vis_flag_tiles, obs_out, vis_weights_use, tile_flag_list=ps_tile_flag_list
  ENDIF
  vis_weights_update,vis_weights_use,obs_out,psf_out,params,_Extra=extra
  IF Min(Ptr_valid(vis_arr)) EQ 0 THEN vis_arr=Ptrarr(n_pol,/allocate)
  IF N_Elements(*vis_arr[0]) EQ 0 THEN BEGIN
    IF ~Keyword_Set(silent) THEN print,"Restoring saved visibilities (this may take a while)"
    FOR pol_i=0,n_pol-1 DO BEGIN
        fhd_save_io,status_str,vis_ptr,var='vis_ptr',/restore,file_path_fhd=file_path_fhd,obs=obs_out,pol_i=pol_i,path_use=path_use,_Extra=extra
        IF status_str.vis_ptr[pol_i] EQ 0 THEN BEGIN
            error=1
            print,"Error: file not found!: "+path_use
            RETURN
        ENDIF
        vis_arr[pol_i]=vis_ptr
    ENDFOR
    IF ~Keyword_Set(silent) THEN print,"...Done"
  ENDIF
  
  IF Keyword_Set(split_ps_export) THEN BEGIN
    n_iter=2
    vis_weights_use=split_vis_weights(obs_out,vis_weights_use,bi_use=bi_use,_Extra=extra)
    vis_noise_calc,obs_out,vis_arr,vis_weights_use,bi_use=bi_use
    uvf_name = ['even','odd']
    if keyword_set(save_imagecube) then imagecube_filepath = file_path_fhd+['_even','_odd'] + '_gridded_imagecube.sav'
  ENDIF ELSE BEGIN
    n_iter=1
    bi_use=Ptrarr(n_iter,/allocate_heap)
   *bi_use[0]=0
    vis_noise_calc,obs_out,vis_arr,vis_weights_use
    uvf_name = ''
    if keyword_set(save_imagecube) then imagecube_filepath = file_path_fhd+'_gridded_imagecube.sav'
  ENDELSE
  
  residual_flag=obs_out.residual
  model_flag=0
  
  IF Min(Ptr_valid(vis_model_arr)) THEN IF N_Elements(*vis_model_arr[0]) GT 0 THEN model_flag=1
  IF residual_flag EQ 0 THEN IF model_flag EQ 0 THEN BEGIN
    vis_model_arr=Ptrarr(n_pol)
    IF Min(status_str.vis_model_ptr[0:n_pol-1]) GT 0 THEN BEGIN
        model_flag=1
        IF ~Keyword_Set(silent) THEN print,"Restoring saved model visibilities (this may take a while)"
        FOR pol_i=0,n_pol-1 DO BEGIN
            fhd_save_io,status_str,vis_model_ptr,var='vis_model_ptr',/restore,file_path_fhd=file_path_fhd,obs=obs_out,pol_i=pol_i,_Extra=extra
            vis_model_arr[pol_i]=vis_model_ptr
        ENDFOR 
        IF ~Keyword_Set(silent) THEN print,"...Done"
    ENDIF
  ENDIF
  IF model_flag AND ~residual_flag THEN dirty_flag=1 ELSE dirty_flag=0
  
  t_hpx=0.
  t_split=0.
  obs_out_ref=obs_out
  obs_in_ref=obs_in
  FOR iter=0,n_iter-1 DO BEGIN
    obs=obs_out_ref ;will have some values over-written!
    obs_in=obs_in_ref
    psf=psf_out
    
    residual_arr1=vis_model_freq_split(obs_in,status_str,psf_in,params,vis_weights_use,obs_out=obs,psf_out=psf,rephase_weights=rephase_weights,$
      weights_arr=weights_arr1,variance_arr=variance_arr1,model_arr=model_arr1,n_avg=n_avg,timing=t_split1,/fft,$
      file_path_fhd=file_path_fhd,vis_n_arr=vis_n_arr,/preserve_visibilities,vis_data_arr=vis_arr,vis_model_arr=vis_model_arr,$
      save_uvf=save_uvf, uvf_name=uvf_name[iter],bi_use=*bi_use[iter], _Extra=extra)

    t_split+=t_split1
    IF dirty_flag THEN BEGIN
      dirty_arr1=residual_arr1
      residual_flag=0
    ENDIF ELSE residual_flag=1
    nf_vis=obs.nf_vis
    nf_vis_use=Lonarr(2,n_freq_use)
    FOR pol_i=0,n_pol-1 DO BEGIN
      FOR freq_i=0L,n_freq_use-1 DO nf_vis_use[pol_i,freq_i]=Total(nf_vis[pol_i,freq_i*n_avg:(freq_i+1)*n_avg-1])
    ENDFOR
    
    t_hpx0=Systime(1)
    
    beam_squared_cube=fltarr(n_hpx,n_freq_use)
    weights_cube=fltarr(n_hpx,n_freq_use)
    variance_cube=fltarr(n_hpx,n_freq_use)
    IF residual_flag THEN res_cube=fltarr(n_hpx,n_freq_use)
    IF dirty_flag THEN dirty_cube=fltarr(n_hpx,n_freq_use)
    IF model_flag THEN model_cube=fltarr(n_hpx,n_freq_use)

    FOR pol_i=0,n_pol-1 DO BEGIN

        FOR freq_i=Long64(0),n_freq_use-1 DO BEGIN

            beam_squared_cube[n_hpx*freq_i]=healpix_cnv_apply((*beam_arr[pol_i,freq_i])*nf_vis_use[pol_i,freq_i],hpx_cnv)
            weights_cube[n_hpx*freq_i]=healpix_cnv_apply((*weights_arr1[pol_i,freq_i]),hpx_cnv)
            variance_cube[n_hpx*freq_i]=healpix_cnv_apply((*variance_arr1[pol_i,freq_i]),hpx_cnv)

            IF residual_flag THEN BEGIN
                res_cube[n_hpx*freq_i]=healpix_cnv_apply((*residual_arr1[pol_i,freq_i]),hpx_cnv)
            ENDIF
            IF dirty_flag THEN BEGIN
                dirty_cube[n_hpx*freq_i]=healpix_cnv_apply((*dirty_arr1[pol_i,freq_i]),hpx_cnv)
            ENDIF
            IF model_flag THEN BEGIN
                model_cube[n_hpx*freq_i]=healpix_cnv_apply((*model_arr1[pol_i,freq_i]),hpx_cnv)
            ENDIF

        ENDFOR

        ;call fhd_save_io first to obtain the correct path. Will NOT update status structure yet
        fhd_save_io,status_str,file_path_fhd=file_path_fhd,var=cube_name[iter],pol_i=pol_i,path_use=path_use,/no_save,_Extra=extra
        IF file_test(file_dirname(path_use)) EQ 0 THEN file_mkdir,file_dirname(path_use)
        save,filename=path_use+'.sav',/compress,dirty_cube,model_cube,weights_cube,variance_cube,res_cube,beam_squared_cube,$
            obs,nside,hpx_inds,n_avg
        ;call fhd_save_io a second time to update the status structure now that the file has actually been written
        fhd_save_io,status_str,file_path_fhd=file_path_fhd,var=cube_name[iter],pol_i=pol_i,/force,_Extra=extra

    ENDFOR
    
    IF Keyword_Set(save_imagecube) THEN BEGIN
        save, filename = imagecube_filepath[iter], dirty_arr1, residual_arr1, model_arr1, weights_arr1, variance_arr1, beam_arr, nf_vis_use, obs_out, /compress
    ENDIF
    
    undefine_fhd,weights_arr1,variance_arr1,residual_arr1,dirty_arr1,model_arr1 ;free memory for beam_arr later!
    dirty_cube=(model_cube=(res_cube=(weights_cube=(variance_cube=(beam_squared_cube=0)))))
    IF iter EQ n_iter-1 THEN undefine_fhd,beam_arr
    
ENDFOR
obs_out=obs ;for return
Ptr_free,vis_weights_use
timing=Systime(1)-t0
IF ~Keyword_Set(silent) THEN print,'HEALPix cube export timing: ',timing,t_split,t_hpx

END
