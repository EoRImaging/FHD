
PRO fhd_sim,file_path_vis,export_images=export_images,cleanup=cleanup,recalculate_all=recalculate_all,$
    beam_recalculate=beam_recalculate,grid_recalculate=grid_recalculate,$
    n_pol=n_pol,silent=silent,$
    healpix_recalculate=healpix_recalculate,tile_flag_list=tile_flag_list,$
    file_path_fhd=file_path_fhd,freq_start=freq_start,freq_end=freq_end,error=error, $
    eor_sim=eor_sim, flat_sigma = flat_sigma, no_distrib = no_distrib, delta_power = delta_power, delta_uv_loc = delta_uv_loc, $
    include_catalog_sources = include_catalog_sources, source_list=source_list, catalog_file_path=catalog_file_path, $
    model_uvf_cube=model_uvf_cube, model_image_cube=model_image_cube, $
    weights_grid=weights_grid,save_visibilities=save_visibilities,save_uvf=save_uvf,save_imagecube=save_imagecube,$
    snapshot_healpix_export=snapshot_healpix_export,n_avg=n_avg,_Extra=extra
    
  compile_opt idl2,strictarrsubs
  except=!except
  !except=0 ;System variable that controls when math errors are printed. Set to 0 to disable.
  error=0
  heap_gc
  t0=Systime(1)
  IF N_Elements(recalculate_all) EQ 0 THEN recalculate_all=0
  IF N_Elements(beam_recalculate) EQ 0 THEN beam_recalculate=recalculate_all
  IF N_Elements(grid_recalculate) EQ 0 THEN grid_recalculate=recalculate_all
  IF N_Elements(healpix_recalculate) EQ 0 THEN healpix_recalculate=recalculate_all
  IF N_Elements(save_visibilities) EQ 0 THEN save_visibilities=1
  
  fhd_save_io,status_str,file_path_fhd=file_path_fhd
  
  print,'Simulating: ',file_path_vis
  print,systime()
  print,'Output file_path:',file_path_fhd
  ext='.uvfits'
  ;  fhd_dir=file_dirname(file_path_fhd)
  ;  basename=file_basename(file_path_fhd)
  ;  header_filepath=file_path_fhd+'_header.sav'
;  flags_filepath=file_path_fhd+'_flags.sav'
  input_model_filepath = file_path_fhd + '_input_model.sav'
  coarse_input_model_filepath = file_path_fhd + '_input_model_coarse.sav'
  init_beam_filepath = file_path_fhd + '_initial_beam2_image.sav'
  gridded_beam_filepath = file_path_fhd + '_gridded_beam2_image.sav'
  ;  vis_filepath=file_path_fhd+'_vis.sav'
;  obs_filepath=file_path_fhd+'_obs.sav'
;  params_filepath=file_path_fhd+'_params.sav'
  ;  hdr_filepath=file_path_fhd+'_hdr.sav'
  ;  fhd_filepath=file_path_fhd+'_fhd.sav'
;  autocorr_filepath=file_path_fhd+'_autos.sav'
  ;  model_filepath=file_path_fhd+'_vis_model.sav'
  
  
  IF file_test(file_path_vis) EQ 0 THEN BEGIN
    print,"File: "+file_path_vis+" not found! Returning"
    error=1
    return
  ENDIF
  uvfits_read,hdr,params,layout,vis_arr,vis_weights,file_path_vis=file_path_vis,n_pol=n_pol,silent=silent,_Extra=extra
  
  obs=fhd_struct_init_obs(file_path_vis,hdr,params,n_pol=n_pol,_Extra=extra)
  n_pol=obs.n_pol
  n_freq=obs.n_freq
  
  ;Read in or construct a new beam model. Also sets up the structure PSF
  print,'Calculating beam model'
  
  psf=beam_setup(obs,status_str,file_path_fhd=file_path_fhd,restore_last=0,silent=silent,timing=t_beam,no_save=0,_Extra=extra)
  IF Keyword_Set(t_beam) THEN print,'Beam modeling time: ',t_beam
  
  vis_weights=vis_flag_basic(vis_weights,obs,params,n_pol=n_pol,n_freq=n_freq,freq_start=freq_start,$
    freq_end=freq_end,tile_flag_list=tile_flag_list,_Extra=extra)
  vis_weights_update,vis_weights,obs,psf,params
  ;print informational messages
  obs_status,obs
  
  vis_model_ptr=vis_simulate(obs,status_str,psf,params,file_path_fhd=file_path_fhd,vis_weights=vis_weights,$
    recalculate_all=recalculate_all,$
    eor_sim=eor_sim, flat_sigma = flat_sigma, no_distrib = no_distrib, delta_power = delta_power, delta_uv_loc = delta_uv_loc, $
    include_catalog_sources = include_catalog_sources, source_list=source_list, catalog_file_path=catalog_file_path, $
    model_uvf_cube=model_uvf_cube, model_image_cube=model_image_cube,_Extra=extra)
  
;  if keyword_set(recalculate_all) then begin
;    if keyword_set(include_catalog_sources) then begin
;      catalog_source_list=generate_source_cal_list(obs,psf,catalog_path=catalog_file_path,_Extra=extra)
;      if n_elements(source_list) gt 0 then source_list = [source_list, catalog_source_list] else source_list = catalog_source_list
;    endif
;    
;    if n_elements(source_list) gt 0 then begin
;      source_model_uv_arr=source_dft_model(obs,0,source_list,t_model=t_model,sigma_threshold=2.,uv_mask=uv_mask)
;      IF ~Keyword_Set(silent) THEN print,"DFT timing: "+strn(t_model)+" (",strn(n_sources)+" sources)"
;    endif
;    
;    beam2_xx_image = fltarr(obs.dimension, obs.elements, n_freq)
;    beam2_yy_image = fltarr(obs.dimension, obs.elements, n_freq)
;    beam_arr=beam_image_cube(obs,psf, n_freq_bin = n_freq,/square)
;    for freq_i=0,n_freq-1 do begin
;      beam2_xx_image[*,*, freq_i] = Temporary(*beam_arr[0,freq_i])
;      beam2_yy_image[*,*, freq_i] = Temporary(*beam_arr[1,freq_i])
;    endfor
;    save, file=init_beam_filepath, beam2_xx_image, beam2_yy_image, obs
;    undefine_fhd, beam2_xx_image, beam2_yy_image,beam_arr
;    
;    if n_elements(model_image_cube) gt 0 or n_elements(model_uvf_cube) gt 0 or keyword_set(eor_sim) then begin
;      model_uvf_arr=Ptrarr(n_pol,/allocate)
;      for pol_i=0,n_pol-1 do *model_uvf_arr[pol_i]=Complexarr(obs.dimension,obs.elements, n_freq)
;      
;      if n_elements(model_uvf_cube) eq 0 and n_elements(model_image_cube) gt 0 then begin
;        ;; convert from Jy/str to Jy/pixel
;        model_image_use = model_image_cube/(degpix*!DtoR)^2. ;; Jy/pixel
;        model_uvf_cube = Complexarr(obs.dimension,obs.elements, n_freq)
;        for i=0, n_freq-1 do model_uvf_cube[*,*,i] = fft_shift(FFT(fft_shift(model_image_use[*,*,1]),/inverse)) * (degpix*!DtoR)^2.
;        undefine, model_image_use
;      endif
;      
;      if keyword_set(eor_sim) then begin
;        print, 'Generating model EoR cube'
;        freq_arr = (*obs.baseline_info).freq
;        delta_uv=obs.kpix
;        uv_arr = (findgen(obs.dimension)-obs.dimension/2)*delta_uv
;        
;        uv_locs = findgen(101)*4.-200.
;        eor_uvf = eor_sim(uv_locs, uv_locs, freq_arr, flat_sigma = flat_sigma, no_distrib = no_distrib, delta_power = delta_power, delta_uv_loc = delta_uv_loc)
;        save,filename=coarse_input_model_filepath, eor_uvf, uv_locs, freq_arr, /compress
;        
;        time0 = systime(1)        
;        eor_uvf_cube = eor_sim(uv_arr, uv_arr, freq_arr, flat_sigma = flat_sigma, no_distrib = no_distrib, delta_power = delta_power, delta_uv_loc = delta_uv_loc)       
;        time1 = systime(1)
;        print, 'time for eor modelling: ' + number_formatter(time1-time0)
;        if n_elements(model_uvf_cube) gt 0 then model_uvf_cube = model_uvf_cube + temporary(eor_uvf_cube) $
;        else model_uvf_cube = temporary(eor_uvf_cube)
;      endif
;      
;      ;; model cube assumed to be Stokes I
;      switch n_pol of
;        4:(*model_uvf_arr[3])[*]=0.
;        3:(*model_uvf_arr[2])[*]=0.
;        2:(*model_uvf_arr[1])[*]=model_uvf_cube/2.
;        1:(*model_uvf_arr[0])[*]=model_uvf_cube/2.
;      endswitch
;      
;      undefine, model_uvf_cube
;    endif
;    
;    if n_elements(source_model_uv_arr) gt 0 then begin
;      if n_elements(model_uvf_arr) gt 0 then begin
;        FOR pol_i=0,n_pol-1 DO *model_uv_arr[pol_i]+=*source_model_uv_arr[pol_i]
;      endif else model_uvf_arr = source_model_uv_arr
;      undefine_fhd, source_model_uv_arr
;    endif
;    
;    if n_elements(model_uvf_arr) eq 0 then begin
;      print, 'No input model (image cube, model_uvf or sources)'
;      error=1
;      RETURN
;    endif
;    
;    model_uvf = *model_uvf_arr[0]
;    save,filename=input_model_filepath, model_uvf, uv_arr, freq_arr, /compress
;    undefine, model_uvf
;    
;    vis_dimension=N_Elements(params)
;    
;    vis_model_ptr = Ptrarr(n_pol,/allocate)
;    for pol_i=0,n_pol-1 do *vis_model_ptr[pol_i]=Complexarr(n_freq,vis_dimension)
;    
;    time0=systime(1)
;    for fi=0, n_freq-1 do begin
;      if max([(*vis_weights[0])[fi,*], (*vis_weights[1])[fi,*]]) lt 1 then continue
;      
;      this_vis_weight_ptr = Ptrarr(n_pol,/allocate)
;      this_model_uv = Ptrarr(n_pol,/allocate)
;      for pol_i=0,n_pol-1 do begin
;        *this_vis_weight_ptr[pol_i]=intarr(n_freq, vis_dimension)
;        (*this_vis_weight_ptr[pol_i])[fi,*] = (*vis_weights[pol_i])[fi,*]
;        
;        *this_model_uv[pol_i] = (*model_uvf_arr[pol_i])[*,*,fi]
;      endfor
;      
;      if max(abs(*this_model_uv[0])) eq 0 and max(abs(*this_model_uv[1])) eq 0 then continue
;      
;      this_model_ptr=vis_source_model(0,obs,status_str,psf,params,this_vis_weight_ptr,model_uv_arr=this_model_uv,$
;        timing=model_timing,silent=silent,error=error,_Extra=extra)
;      print, 'model loop num, timing(s):'+ number_formatter(fi) + ' , ' + number_formatter(model_timing)
;      
;      for pol_i=0,n_pol-1 do (*vis_model_ptr[pol_i])[fi,*] = (*this_model_ptr[pol_i])[fi,*]
;      
;      undefine_fhd, this_vis_weight_ptr, this_model_ptr, this_model_uv
;    endfor
;    undefine_fhd, model_uvf_arr
;    time1=systime(0)
;    print, 'model visibility timing(s):'+ number_formatter(time1-time0)
;    
;    fhd_save_io,status_str,vis_weights,var='vis_weights',/compress,file_path_fhd=file_path_fhd,_Extra=extra
;  endif
  
  vis_noise_calc,obs,vis_arr,vis_weights
  tile_use_i=where((*obs.baseline_info).tile_use,n_tile_use,ncomplement=n_tile_cut)
  freq_use_i=where((*obs.baseline_info).freq_use,n_freq_use,ncomplement=n_freq_cut)
  print,String(format='(A," frequency channels used and ",A," channels flagged")',$
    Strn(n_freq_use),Strn(n_freq_cut))
  print,String(format='(A," tiles used and ",A," tiles flagged")',$
    Strn(n_tile_use),Strn(n_tile_cut))
    
  fhd_save_io,status_str,obs,var='obs',/compress,file_path_fhd=file_path_fhd,_Extra=extra
  fhd_save_io,status_str,params,var='params',/compress,file_path_fhd=file_path_fhd,_Extra=extra
  fhd_save_io,status_str,layout,var='layout',/compress,file_path_fhd=file_path_fhd,_Extra=extra
  fhd_log_settings,file_path_fhd,obs=obs,layout=layout,psf=psf,cal=cal
  
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
      ;            SAVE,dirty_UV,weights_grid,filename=file_path_fhd+'_uv_'+pol_names[pol_i]+'.sav',/compress
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
    
      fhd_quickview,obs,status_str,psf,cal,image_uv_arr=image_uv_arr,weights_arr=weights_arr,source_array=source_array,$
        model_uv_holo=model_uv_holo,file_path_fhd=file_path_fhd,silent=silent,_Extra=extra
    ENDIF
    
  ENDIF
  
  ;optionally export frequency-split Healpix cubes
  IF Keyword_Set(snapshot_healpix_export) THEN begin
    IF ~Keyword_Set(n_avg) THEN n_avg=1
    healpix_snapshot_cube_generate,obs,status_str,psf,cal,params,vis_arr,/restrict_hpx_inds,/snapshot_recalculate, $
      vis_model_arr=vis_model_ptr,file_path_fhd=file_path_fhd,vis_weights=vis_weights,n_avg=n_avg,$
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
    save, file=gridded_beam_filepath, beam2_xx_image, beam2_yy_image, obs_out
  endif
  undefine_fhd,map_fn_arr,cal,obs,fhd,image_uv_arr,weights_arr,model_uv_arr,vis_arr,status_str
  undefine_fhd,vis_model_ptr,beam2_xx_image, beam2_yy_image, obs, obs_out, psf, psf_out,vis_weights
  
  timing=Systime(1)-t0
  print,'Full pipeline time (minutes): ',Strn(Round(timing/60.))
  print,''
  !except=except
END
