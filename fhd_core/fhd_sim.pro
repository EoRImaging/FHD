
PRO fhd_sim,file_path_vis,export_images=export_images,cleanup=cleanup,recalculate_all=recalculate_all,$
    beam_recalculate=beam_recalculate,$
    n_pol=n_pol,silent=silent,$
    healpix_recalculate=healpix_recalculate,tile_flag_list=tile_flag_list,$
    file_path_fhd=file_path_fhd,freq_start=freq_start,freq_end=freq_end,error=error, $
    eor_sim=eor_sim, include_catalog_sources = include_catalog_sources, source_list=source_list, catalog_file_path=catalog_file_path, $
    model_uvf_cube=model_uvf_cube, model_image_cube=model_image_cube, $
    weights_grid=weights_grid,save_visibilities=save_visibilities,$
    snapshot_healpix_export=snapshot_healpix_export,_Extra=extra
    
  compile_opt idl2,strictarrsubs
  except=!except
  !except=0 ;System variable that controls when math errors are printed. Set to 0 to disable.
  error=0
  heap_gc
  t0=Systime(1)
  IF N_Elements(recalculate_all) EQ 0 THEN recalculate_all=1
  IF N_Elements(beam_recalculate) EQ 0 THEN beam_recalculate=recalculate_all
  IF N_Elements(healpix_recalculate) EQ 0 THEN healpix_recalculate=0
  IF N_Elements(save_visibilities) EQ 0 THEN save_visibilities=1
  
  print,'Simulating: ',file_path_vis
  print,systime()
  print,'Output file_path:',file_path_fhd
  ext='.uvfits'
  fhd_dir=file_dirname(file_path_fhd)
  basename=file_basename(file_path_fhd)
  header_filepath=file_path_fhd+'_header.sav'
  flags_filepath=file_path_fhd+'_flags.sav'
  input_model_filepath = file_path_fhd + '_input_model.sav'
  ;vis_filepath=file_path_fhd+'_vis.sav'
  obs_filepath=file_path_fhd+'_obs.sav'
  params_filepath=file_path_fhd+'_params.sav'
  hdr_filepath=file_path_fhd+'_hdr.sav'
  fhd_filepath=file_path_fhd+'_fhd.sav'
  autocorr_filepath=file_path_fhd+'_autos.sav'
  model_filepath=file_path_fhd+'_vis_model.sav'
  
  pol_names=['xx','yy','xy','yx','I','Q','U','V']
  
  IF Keyword_Set(n_pol) THEN n_pol1=n_pol ELSE n_pol1=1
  
  vis_file_list=file_search(file_path_fhd+'_vis*',count=vis_file_flag)
  
  IF file_test(file_path_vis) EQ 0 THEN BEGIN
    print,"File: "+file_path_vis+" not found! Returning"
    error=1
    return
  ENDIF
  
  data_struct=mrdfits(file_path_vis,0,data_header0,/silent)
  hdr=vis_header_extract(data_header0, params = data_struct.params)
  params=vis_param_extract(data_struct.params,hdr)
  data_array=Temporary(data_struct.array[*,0:n_pol-1,*])
  data_struct=0. ;free memory
  
  obs=vis_struct_init_obs(file_path_vis,hdr,params,n_pol=n_pol,_Extra=extra)
  pol_dim=hdr.pol_dim
  freq_dim=hdr.freq_dim
  real_index=hdr.real_index
  imaginary_index=hdr.imaginary_index
  flag_index=hdr.flag_index
  n_pol=obs.n_pol
  n_freq=obs.n_freq
  
  
  vis_arr=Ptrarr(n_pol,/allocate)
  flag_arr=Ptrarr(n_pol,/allocate)
  FOR pol_i=0,n_pol-1 DO BEGIN
    *vis_arr[pol_i]=Complex(reform(data_array[real_index,pol_i,*,*]),Reform(data_array[imaginary_index,pol_i,*,*]))
    *flag_arr[pol_i]=reform(data_array[flag_index,pol_i,*,*])
  ENDFOR
  ;free memory
  data_array=0
  flag_arr0=0
  
  ;Read in or construct a new beam model. Also sets up the structure PSF
  print,'Calculating beam model'
  psf=beam_setup(obs,file_path_fhd,restore_last=(Keyword_Set(beam_recalculate) ? 0:1),silent=silent,timing=t_beam,no_save=no_save,_Extra=extra)
  IF Keyword_Set(t_beam) THEN print,'Beam modeling time: ',t_beam
  beam=Ptrarr(n_pol,/allocate)
  FOR pol_i=0,n_pol-1 DO *beam[pol_i]=beam_image(psf,obs,pol_i=pol_i,/fast)>0.
  
  flag_arr=vis_flag_basic(flag_arr,obs,params,n_pol=n_pol,n_freq=n_freq,freq_start=freq_start,$
    freq_end=freq_end,tile_flag_list=tile_flag_list,_Extra=extra)
  vis_flag_update,flag_arr,obs,psf,params
  ;print informational messages
  obs_status,obs
  
  if keyword_set(include_catalog_sources) then begin
    catalog_source_list=generate_source_cal_list(obs,psf,catalog_path=catalog_file_path,_Extra=extra)
    if n_elements(source_list) gt 0 then source_list = [source_list, catalog_source_list] else source_list = catalog_source_list
  endif
  
  if n_elements(source_list) gt 0 then begin
    source_model_uv_arr=source_dft_model(obs,source_list,t_model=t_model,sigma_threshold=2.,uv_mask=uv_mask)
    IF ~Keyword_Set(silent) THEN print,"DFT timing: "+strn(t_model)+" (",strn(n_sources)+" sources)"
  endif
  
  
  if n_elements(model_image_cube) gt 0 or n_elements(model_uvf_cube) gt 0 or keyword_set(eor_sim) then begin
    model_uvf_arr=Ptrarr(n_pol,/allocate)
    for pol_i=0,n_pol-1 do *model_uvf_arr[pol_i]=Complexarr(obs.dimension,obs.elements, n_freq)
    
    if n_elements(model_uvf_cube) eq 0 and n_elements(model_image_cube) gt 0 then begin
      ;; convert from Jy/str to Jy/pixel
      model_image_use = model_image_cube/(degpix*!DtoR)^2. ;; Jy/pixel
      model_uvf_cube = Complexarr(obs.dimension,obs.elements, n_freq)
      for i=0, n_freq-1 do model_uvf_cube[*,*,i] = fft_shift(FFT(fft_shift(model_image_use[*,*,1]),/inverse)) * (degpix*!DtoR)^2.
      undefine, model_image_use
    endif
    
    if keyword_set(eor_sim) then begin
      freq_arr = (*obs.baseline_info).freq
      delta_uv=obs.kpix
      uv_arr = (findgen(obs.dimension)-obs.dimension/2)*delta_uv
      eor_uvf_cube = eor_sim(uv_arr, uv_arr, freq_arr)
      if n_elements(model_uvf_cube) gt 0 then model_uvf_cube = model_uvf_cube + temporary(eor_uvf_cube) $
      else model_uvf_cube = temporary(eor_uvf_cube)
    endif
    
    ;; model cube assumed to be Stokes I
    switch n_pol of
      4:(*model_uvf_arr[3])[*]=0.
      3:(*model_uvf_arr[2])[*]=0.
      2:(*model_uvf_arr[1])[*]=model_uvf_cube/2.
      1:(*model_uvf_arr[0])[*]=model_uvf_cube/2.
    endswitch
  endif
  
  if n_elements(source_model_uv_arr) gt 0 then begin
    if n_elements(model_uvf_arr) gt 0 then begin
      FOR pol_i=0,n_pol-1 DO *model_uv_arr[pol_i]+=*source_model_uv_arr[pol_i]
    endif else model_uvf_arr = source_model_uv_arr
    undefine_fhd, source_model_uv_arr
  endif
  
  if n_elements(model_uvf_arr) eq 0 then begin
    print, 'No input model (image cube, model_uvf or sources)'
    error=1
    RETURN
  endif
  
  save,filename=input_model_filepath,model_uvf_arr, uv_arr, freq_arr, /compress
  
  bin_offset=(*obs.baseline_info).bin_offset
  nbaselines=bin_offset[1]
  n_samples=N_Elements(bin_offset)
  undefine_fhd, bin_offset
  vis_dimension=Float(nbaselines*n_samples)
  
  vis_model_ptr = Ptrarr(n_pol,/allocate)
  for pol_i=0,n_pol-1 do *vis_model_ptr[pol_i]=Complexarr(n_freq,vis_dimension)
  
  for fi=0, n_freq-1 do begin
    if max([(*flag_arr[0])[fi,*], (*flag_arr[1])[fi,*]]) lt 1 then continue
    
    this_flag_ptr = Ptrarr(n_pol,/allocate)
    for pol_i=0,n_pol-1 do begin
      *this_flag_ptr[pol_i]=intarr(n_freq, vis_dimension)
      (*this_flag_ptr[pol_i])[fi,*] = (*flag_arr[pol_i])[fi,*]
    endfor
    
    this_model_ptr=vis_source_model(0,obs,psf,params,this_flag_ptr,model_uv_arr=(*model_uvf_arr[pol_i])[*,*,fi],$
      timing=model_timing,silent=silent,error=error,_Extra=extra)
    print, 'model loop num, timing(s):', number_formatter(fi), number_formatter(model_timing)
    
    for pol_i=0,n_pol-1 do (*vis_model_ptr[pol_i])[fi,*] = (*this_model_ptr[pol_i])[fi,*]
    
    undefine_fhd, this_flag_ptr, this_model_ptr
  endfor
  
  SAVE,flag_arr,filename=flags_filepath,/compress
  
  vis_noise_calc,obs,vis_arr,flag_arr
  tile_use_i=where((*obs.baseline_info).tile_use,n_tile_use,ncomplement=n_tile_cut)
  freq_use_i=where((*obs.baseline_info).freq_use,n_freq_use,ncomplement=n_freq_cut)
  print,String(format='(A," frequency channels used and ",A," channels flagged")',$
    Strn(n_freq_use),Strn(n_freq_cut))
  print,String(format='(A," tiles used and ",A," tiles flagged")',$
    Strn(n_tile_use),Strn(n_tile_cut))
    
  SAVE,obs,filename=obs_filepath,/compress
  SAVE,params,filename=params_filepath,/compress
  fhd_log_settings,file_path_fhd,obs=obs,psf=psf,cal=cal
  
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
  SAVE,auto_corr,obs,filename=autocorr_filepath,/compress
  
  IF Keyword_Set(save_visibilities) THEN BEGIN
    t_save0=Systime(1)
    vis_export,obs,vis_model_ptr,flag_arr,file_path_fhd=file_path_fhd,/compress,/model
    t_save=Systime(1)-t_save0
    IF ~Keyword_Set(silent) THEN print,'Visibility save time: ',t_save
  ENDIF
  
  t_grid=fltarr(n_pol)
  t_mapfn_gen=fltarr(n_pol)
  
  IF N_Elements(obs) EQ 0 THEN IF file_test(obs_filepath) THEN obs=getvar_savefile(obs_filepath,'obs')
  
  ;Generate fits data files and images
  IF Keyword_Set(export_images) THEN BEGIN
    IF file_test(file_path_fhd+'_fhd.sav') THEN BEGIN
      fhd_output,obs,fhd,cal,file_path_fhd=file_path_fhd,map_fn_arr=map_fn_arr,silent=silent,transfer_mapfn=transfer_mapfn,$
        image_uv_arr=image_uv_arr,weights_arr=weights_arr,beam_arr=beam,_Extra=extra
    ENDIF ELSE BEGIN
      IF obs.residual GT 0 THEN BEGIN
        IF N_Elements(cal) EQ 0 THEN IF file_test(file_path_fhd+'_cal.sav') THEN RESTORE,file_path_fhd+'_cal.sav'
        IF N_Elements(cal) GT 0 THEN source_array=cal.source_list
      ENDIF
      fhd_quickview,obs,psf,cal,image_uv_arr=image_uv_arr,weights_arr=weights_arr,source_array=source_array,$
        model_uv_holo=model_uv_holo,file_path_fhd=file_path_fhd,silent=silent,_Extra=extra
    ENDELSE
  ENDIF
  
  ;optionally export frequency-split Healpix cubes
  IF Keyword_Set(snapshot_healpix_export) THEN healpix_snapshot_cube_generate,obs,psf,params,vis_arr,$
    vis_model_ptr=vis_model_ptr,file_path_fhd=file_path_fhd,flag_arr=flag_arr,_Extra=extra
    
  undefine_fhd,map_fn_arr,cal,obs,fhd,image_uv_arr,weights_arr,model_uv_arr,vis_arr,flag_arr,vis_model_ptr
  
  timing=Systime(1)-t0
  print,'Full pipeline time (minutes): ',Strn(Round(timing/60.))
  print,''
  !except=except
END
