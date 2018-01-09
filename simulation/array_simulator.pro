PRO array_simulator,vis_arr,vis_weights,obs,status_str,psf,params,jones,error=error,$
    instrument=instrument,cleanup=cleanup,recalculate_all=recalculate_all,$
    n_pol=n_pol,silent=silent,tile_flag_list=tile_flag_list,$
    file_path_fhd=file_path_fhd,freq_start=freq_start,freq_end=freq_end,$
    eor_sim=eor_sim, include_noise = include_noise,select_radius_multiplier=select_radius_multiplier, $
    include_catalog_sources = include_catalog_sources, source_array=source_array,$
    catalog_file_path=catalog_file_path,snapshot_healpix_export=snapshot_healpix_export, export_images=export_images, $
    snapshot_recalculate=snapshot_recalculate,grid_recalculate=grid_recalculate,eor_uvf_cube_file=eor_uvf_cube_file,_Extra=extra
    
  compile_opt idl2,strictarrsubs
  except=!except
  !except=0 ;System variable that controls when math errors are printed. Set to 0 to disable.
  error=0
  heap_gc
  t0=Systime(1)
  IF N_Elements(recalculate_all) EQ 0 THEN recalculate_all=0
  IF N_Elements(save_visibilities) EQ 0 THEN save_visibilities=1
  IF N_Elements(silent) EQ 0 THEN silent=0
  fhd_save_io,status_str,file_path_fhd=file_path_fhd
  
  print,systime()
  print,'Output file_path:',file_path_fhd
  
  IF Keyword_Set(n_pol) THEN vis_test=Min(status_str.vis_ptr[0:n_pol-1]) ELSE vis_test=status_str.vis_ptr[0]
  metadata_test=status_str.obs<status_str.params<status_str.psf<status_str.jones
  IF Keyword_Set(recalculate_all) THEN vis_test=(metadata_test=0)
  
  IF metadata_test THEN BEGIN
    fhd_save_io,status_str,obs,var='obs',/restore,file_path_fhd=file_path_fhd,_Extra=extra
    fhd_save_io,status_str,params,var='params',/restore,file_path_fhd=file_path_fhd,_Extra=extra
    fhd_save_io,status_str,psf,var='psf',/restore,file_path_fhd=file_path_fhd,_Extra=extra
    fhd_save_io,status_str,jones,var='jones',/restore,file_path_fhd=file_path_fhd,_Extra=extra
    n_pol=obs.n_pol
  ;    vis_arr=Ptrarr(n_pol)
  ;    IF ~silent THEN print,"Restoring saved visibilities (this may take a while)"
  ;    FOR pol_i=0,n_pol-1 DO BEGIN
  ;        fhd_save_io,status_str,vis_ptr,var='vis_ptr',/restore,file_path_fhd=file_path_fhd,obs=obs,pol_i=pol_i,_Extra=extra
  ;        vis_arr[pol_i]=vis_ptr
  ;    ENDFOR
  ;    IF ~silent THEN print,"...Done"
  ;    RETURN
  ENDIF
  
  ;set up the obs and params structure. Will generate arbitary array layout if supplied
  array_simulator_init,obs,params,layout,error=error,instrument=instrument,n_pol=n_pol,_Extra=extra
  n_pol=obs.n_pol
  n_freq=obs.n_freq
  jones=fhd_struct_init_jones(obs,status_str,file_path_fhd=file_path_fhd,restore=0)
  
  ;Read in or construct a new beam model. Also sets up the structure PSF
  IF ~silent THEN print,'Calculating beam model'
  psf=beam_setup(obs,status_str,file_path_fhd=file_path_fhd,restore_last=0,silent=silent,timing=t_beam,no_save=0,_Extra=extra)
  IF Keyword_Set(t_beam) THEN IF ~silent THEN print,'Beam modeling time: ',t_beam

  ;; Get a selection radius based on the primary beam width
  select_radius = primary_beam_radius(obs,psf,beam_threshold=beam_threshold,select_radius_multiplier=select_radius_multiplier,_Extra=extra)
  vis_weights=Ptrarr(n_pol,/allocate)
  n_param=N_Elements(params.uu)
  FOR pol_i=0,n_pol-1 DO BEGIN
    *vis_weights[pol_i]=Replicate(1.,n_freq,n_param)
    IF n_freq<n_param EQ 1 THEN *vis_weights[pol_i]=Reform(*vis_weights[pol_i],n_freq,n_param) ;need to make sure all dimensions are there even if n_freq=1 or n_param=1
  ENDFOR
  
  vis_weights=vis_flag_basic(vis_weights,obs,params,n_pol=n_pol,n_freq=n_freq,freq_start=freq_start,$
    freq_end=freq_end,tile_flag_list=tile_flag_list,instrument=instrument,_Extra=extra)
  vis_weights_update,vis_weights,obs,psf,params
  ;print informational messages
  IF ~silent THEN obs_status,obs
  ;save and output settings here for debugging, though they should be re-saved later in case things change
  fhd_save_io,status_str,obs,var='obs',/compress,file_path_fhd=file_path_fhd,_Extra=extra
  fhd_save_io,status_str,params,var='params',/compress,file_path_fhd=file_path_fhd,_Extra=extra
  fhd_save_io,status_str,layout,var='layout',/compress,file_path_fhd=file_path_fhd,_Extra=extra
  fhd_log_settings,file_path_fhd,obs=obs,layout=layout,psf=psf,cal=cal
      
  IF Size(source_array,/type) EQ 8 THEN source_array=generate_source_cal_list(obs,psf,source_array,_Extra=extra)   
  vis_arr=vis_simulate(obs,status_str,psf,params,jones,skymodel,file_path_fhd=file_path_fhd,vis_weights=vis_weights,$
    recalculate_all=recalculate_all, include_eor = eor_sim, select_radius=select_radius, include_noise = include_noise, noise_sigma_freq = noise_sigma_freq, $
    include_catalog_sources = include_catalog_sources, source_array=source_array, catalog_file_path=catalog_file_path, $
    model_uvf_cube=model_uvf_cube, model_image_cube=model_image_cube,eor_uvf_cube_file=eor_uvf_cube_file,_Extra=extra)
   
  ; This is a persistent problem! Mostly due to misnaming of the array that is to be passed out whenever vis_simulate is edited. So here is a quick test...
  test_vis = max(abs(*vis_arr[0]))
  if test_vis eq 0 then print, "Visiblities are probably identically zero, you should check very carefully!"
  
  vis_noise_calc,obs,vis_arr,vis_weights
  tile_use_i=where((*obs.baseline_info).tile_use,n_tile_use,ncomplement=n_tile_cut)
  freq_use_i=where((*obs.baseline_info).freq_use,n_freq_use,ncomplement=n_freq_cut)
  print,String(format='(A," frequency channels used and ",A," channels flagged")',$
    Strn(n_freq_use),Strn(n_freq_cut))
  print,String(format='(A," tiles used and ",A," tiles flagged")',$
    Strn(n_tile_use),Strn(n_tile_cut))
    
  fhd_save_io,status_str,obs,var='obs',/compress,file_path_fhd=file_path_fhd,_Extra=extra
  fhd_save_io,status_str,skymodel,var='skymodel',/compress,file_path_fhd=file_path_fhd,_Extra=extra
  fhd_save_io,status_str,params,var='params',/compress,file_path_fhd=file_path_fhd,_Extra=extra
  fhd_log_settings,file_path_fhd,obs=obs,layout=layout,psf=psf,cal=cal,skymodel=skymodel
  
  IF obs.n_vis EQ 0 THEN BEGIN
    print,"All data flagged! Returning."
    error=1
    RETURN
  ENDIF

  autocorr_i=where((*obs.baseline_info).tile_A EQ (*obs.baseline_info).tile_B,n_autocorr)
  auto_corr=Ptrarr(n_pol)
  IF n_autocorr GT 0 THEN FOR pol_i=0,n_pol-1 DO BEGIN
    auto_vals=(*vis_arr[pol_i])[*,autocorr_i]
    auto_corr[pol_i]=Ptr_new(auto_vals)
  ENDFOR
  fhd_save_io,status_str,auto_corr,var='auto_corr',/compress,file_path_fhd=file_path_fhd,obs=obs,_Extra=extra
  
  ;; handling simulation with no model
  ; if there is no vis_model_ptr defined, then create a dummy that is not a valid pointer to be passed to visibility_grid().
  ; (visibility_grid and vis_export will not do anything with model_ptr if it is not a valid pointer)
  if ~ptr_valid(vis_model_ptr) then vis_model_ptr = intarr(n_pol)
  
  
  IF Keyword_Set(save_visibilities) THEN BEGIN
    t_save0=Systime(1)
    vis_export,obs,status_str,vis_model_ptr,vis_weights,file_path_fhd=file_path_fhd,/compress,/model
    vis_export,obs,status_str,vis_arr,vis_weights,file_path_fhd=file_path_fhd,/compress
    t_save=Systime(1)-t_save0
    IF ~Keyword_Set(silent) THEN print,'Visibility save time: ',t_save
  ENDIF
  
  t_grid=fltarr(n_pol)
  t_mapfn_gen=fltarr(n_pol)
  
  IF N_Elements(obs) EQ 0 THEN fhd_save_io,status_str,obs,var='obs',/restore,file_path_fhd=file_path_fhd,_Extra=extra
  
  ;Grid the visibilities
  IF Keyword_Set(grid_recalculate) THEN BEGIN
    print,'Gridding visibilities'
    image_uv_arr=Ptrarr(n_pol,/allocate)
    weights_arr=Ptrarr(n_pol,/allocate)
    
    weights_grid=1
    mapfn_recalculate=0
    preserve_visibilities=1
    FOR pol_i=0,n_pol-1 DO BEGIN
      dirty_UV=visibility_grid(vis_arr[pol_i],vis_weights[pol_i],obs,status_str,psf,params,file_path_fhd=file_path_fhd,$
        timing=t_grid0,polarization=pol_i,weights=weights_grid,silent=silent,$
        mapfn_recalculate=mapfn_recalculate,return_mapfn=return_mapfn,error=error,no_save=no_save,$
        model_return=model_return,model_ptr=vis_model_ptr[pol_i],preserve_visibilities=preserve_visibilities,_Extra=extra)
      t_grid[pol_i]=t_grid0
      *image_uv_arr[pol_i]=Temporary(dirty_UV)
      
      IF N_Elements(weights_grid) GT 0 THEN BEGIN
        *weights_arr[pol_i]=Temporary(weights_grid)
        weights_grid=1
      ENDIF
    ENDFOR
    fhd_save_io,status_str,obs,var='obs',/compress,file_path_fhd=file_path_fhd,_Extra=extra ;over-write saved obs structure now that nf_vis has been calculated
    print,'Gridding time:',t_grid
    
    ;Generate fits data files and images
    IF Keyword_Set(export_images) THEN BEGIN
      fhd_quickview,obs,status_str,psf,cal,jones,skymodel,image_uv_arr=image_uv_arr,weights_arr=weights_arr,source_array=source_array,$
        model_uv_holo=model_uv_holo,file_path_fhd=file_path_fhd,silent=silent,_Extra=extra
    ENDIF
  ENDIF
  
  ;optionally export frequency-split Healpix cubes
  IF Keyword_Set(snapshot_healpix_export) THEN begin
    IF ~Keyword_Set(n_avg) THEN n_avg=1
    IF Keyword_Set(snapshot_recalculate) THEN status_str.healpix_cube=(status_str.hpx_even=(status_str.hpx_odd=0))
    IF ~Keyword_Set(restrict_hpx_inds) THEN restrict_hpx_inds=0
    healpix_snapshot_cube_generate,obs,status_str,psf,cal,params,vis_arr,restrict_hpx_inds=restrict_hpx_inds,hpx_radius=select_radius,$
      vis_model_ptr=vis_model_ptr,file_path_fhd=file_path_fhd,vis_weights=vis_weights,n_avg=n_avg,$
      save_uvf=save_uvf,save_imagecube=save_imagecube,obs_out=obs_out,psf_out=psf_out,_Extra=extra
      
    ;; now save beam cubes for the gridded pre-healpix cubes.
    ;; These need to be at full freq resolution NOT averaged as in Healpix cubes
    n_freq=obs_out.n_freq
    beam2_xx_image = fltarr(obs_out.dimension, obs_out.elements, n_freq)
    beam2_yy_image = fltarr(obs_out.dimension, obs_out.elements, n_freq)
    
    nf_vis=obs_out.nf_vis
    beam_arr=beam_image_cube(obs_out,psf_out, n_freq_bin = n_freq,/square)
    for freq_i=0,n_freq-1 do begin
      beam2_xx_image[*,*, freq_i] = Temporary(*beam_arr[0,freq_i])*nf_vis[freq_i]
      beam2_yy_image[*,*, freq_i] = Temporary(*beam_arr[1,freq_i])*nf_vis[freq_i]
    endfor
    ptr_free,beam_arr
    gridded_beam_filepath = file_path_fhd + '_gridded_beam2_image.sav'
    save, file=gridded_beam_filepath, beam2_xx_image, beam2_yy_image, obs_out
  endif
  undefine_fhd,map_fn_arr,cal,obs,fhd,image_uv_arr,weights_arr,model_uv_arr,vis_arr,status_str
  undefine_fhd,vis_model_ptr,beam2_xx_image, beam2_yy_image, obs, obs_out, psf, psf_out,vis_weights
  
  timing=Systime(1)-t0
  print,'Full pipeline time (minutes): ',Strn(Round(timing/60.))
  print,''
  !except=except
  
END
