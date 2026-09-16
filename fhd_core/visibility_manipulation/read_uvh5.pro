FUNCTION read_uvh5, filename, partial_read=partial_read, antenna_mod_index=antenna_mod_index,$
  file_path_vis=file_path_vis, file_path_fhd=file_path_fhd

  fid = H5F_OPEN(filename)

  ;--------------------------------------------------
  ; Metadata
  ;--------------------------------------------------

  ant1 = LONG(H5D_READ(H5D_OPEN(fid,'/Header/ant_1_array')))
  ant2 = LONG(H5D_READ(H5D_OPEN(fid,'/Header/ant_2_array')))
  nants_telescope = LONG(H5D_READ(H5D_OPEN(fid,'/Header/Nants_telescope')))

  uvw_array = H5D_READ(H5D_OPEN(fid,'/Header/uvw_array'))
  time_array = H5D_READ(H5D_OPEN(fid,'/Header/time_array'))
  lst_array = H5D_READ(H5D_OPEN(fid,'/Header/lst_array'))
  integration_time = H5D_READ(H5D_OPEN(fid,'/Header/integration_time'))
  freq_array = H5D_READ(H5D_OPEN(fid,'/Header/freq_array'))
  channel_width = H5D_READ(H5D_OPEN(fid,'/Header/channel_width'))

  nblts_total = LONG(H5D_READ(H5D_OPEN(fid,'/Header/Nblts')))
  nfreq_total = LONG(H5D_READ(H5D_OPEN(fid,'/Header/Nfreqs')))
  npol_total = LONG(H5D_READ(H5D_OPEN(fid,'/Header/Npols')))
  nants_data = LONG(H5D_READ(H5D_OPEN(fid,'/Header/Nants_data')))

  date_obs = STRING(H5D_READ(H5D_OPEN(fid,'/Header/rdate')))
  obsra = DOUBLE(H5D_READ(H5D_OPEN(fid,'/Header/phase_center_app_ra')))
  obsdec = DOUBLE(H5D_READ(H5D_OPEN(fid,'/Header/phase_center_app_dec')))
  lon = DOUBLE(H5D_READ(H5D_OPEN(fid,'/Header/longitude')))
  lat = DOUBLE(H5D_READ(H5D_OPEN(fid,'/Header/latitude')))
  alt = DOUBLE(H5D_READ(H5D_OPEN(fid,'/Header/altitude')))

  IF N_ELEMENTS(lon) GT 1 THEN lon = MEAN(lon)
  IF N_ELEMENTS(lat) GT 1 THEN lat = MEAN(lat)
  IF N_ELEMENTS(alt) GT 1 THEN alt = MEAN(alt)

  ;--------------------------------------------------
  ; Resolve selections to do a partial read, create new obsname
  ;--------------------------------------------------

  IF N_ELEMENTS(partial_read) GT 0 THEN BEGIN
      if tag_exist(partial_read, 'freq_channels') then freq_selection = LONG(partial_read.freq_channels)
      if tag_exist(partial_read, 'time_channels') then time_selection = LONG(partial_read.time_channels)
      if tag_exist(partial_read, 'polarisations') then pol_selection = LONG(partial_read.polarisations)
      if tag_exist(partial_read, 'antenna') then antenna_selection = LONG(partial_read.antenna)
  ENDIF

  IF N_ELEMENTS(freq_selection) EQ 0 THEN BEGIN
      freq_indices = LINDGEN(nfreq_total)
      freq_name_suffix = ''
  ENDIF ELSE BEGIN
      freq_indices = freq_selection
      valid = WHERE((freq_indices GE 0) AND (freq_indices LT nfreq_total), nvalid)
      IF nvalid EQ 0 THEN freq_indices = LONARR(0) ELSE freq_indices = freq_indices[valid]

      nsel = N_ELEMENTS(freq_selection)
      IF nsel EQ 1 THEN BEGIN
          freq_name_suffix = '_freq' + STRTRIM(freq_selection[0], 2)
      ENDIF ELSE IF MAX(ABS(freq_selection[1:*] - freq_selection[0:nsel-2])) EQ 1 THEN BEGIN
          freq_name_suffix = '_freq' + $
              STRTRIM(MIN(freq_selection), 2) + '-' + STRTRIM(MAX(freq_selection), 2)
      ENDIF ELSE BEGIN
          freq_name_suffix = '_freq' + STRJOIN(STRTRIM(freq_selection, 2), '_')
      ENDELSE
  ENDELSE

  IF N_ELEMENTS(pol_selection) EQ 0 THEN BEGIN
      pol_indices = LINDGEN(npol_total)
      pol_name_suffix = ''
  ENDIF ELSE BEGIN
      pol_indices = pol_selection
      valid = WHERE((pol_indices GE 0) AND (pol_indices LT npol_total), nvalid)
      IF nvalid EQ 0 THEN pol_indices = LONARR(0) ELSE pol_indices = pol_indices[valid]

      nsel = N_ELEMENTS(pol_selection)
      IF nsel EQ 1 THEN BEGIN
          pol_name_suffix = '_time' + STRTRIM(pol_selection[0], 2)
      ENDIF ELSE IF MAX(ABS(pol_selection[1:*] - pol_selection[0:nsel-2])) EQ 1 THEN BEGIN
          pol_name_suffix = '_time' + $
              STRTRIM(MIN(pol_selection), 2) + '-' + STRTRIM(MAX(pol_selection), 2)
      ENDIF ELSE BEGIN
          pol_name_suffix = '_time' + STRJOIN(STRTRIM(pol_selection, 2), '_')
      ENDELSE
  ENDELSE

  IF N_ELEMENTS(time_selection) EQ 0 THEN BEGIN
      time_indices = LINDGEN(nblts_total)
      time_name_suffix = ''
  ENDIF ELSE BEGIN
      time_indices = time_selection
      valid = WHERE((time_indices GE 0) AND (time_indices LT nblts_total), nvalid)
      IF nvalid EQ 0 THEN time_indices = LONARR(0) ELSE time_indices = time_indices[valid]

      nsel = N_ELEMENTS(time_selection)
      IF nsel EQ 1 THEN BEGIN
          time_name_suffix = '_time' + STRTRIM(time_selection[0], 2)
      ENDIF ELSE IF MAX(ABS(time_selection[1:*] - time_selection[0:nsel-2])) EQ 1 THEN BEGIN
          time_name_suffix = '_time' + $
              STRTRIM(MIN(time_selection), 2) + '-' + STRTRIM(MAX(time_selection), 2)
      ENDIF ELSE BEGIN
          time_name_suffix = '_time' + STRJOIN(STRTRIM(time_selection, 2), '_')
      ENDELSE
  ENDELSE

  IF N_ELEMENTS(antenna_selection) GT 0 THEN BEGIN
      selected_times = time_indices[WHERE($
          (ant1[time_indices] EQ antenna_selection[0]) OR $
          (ant2[time_indices] EQ antenna_selection[0]), nselected_times)]
      IF nselected_times EQ 0 THEN time_indices = LONARR(0) ELSE time_indices = selected_times

      nsel = N_ELEMENTS(antenna_selection)
      IF nsel EQ 1 THEN BEGIN
          antenna_name_suffix = '_time' + STRTRIM(antenna_selection[0], 2)
      ENDIF ELSE IF MAX(ABS(antenna_selection[1:*] - antenna_selection[0:nsel-2])) EQ 1 THEN BEGIN
          antenna_name_suffix = '_time' + $
              STRTRIM(MIN(antenna_selection), 2) + '-' + STRTRIM(MAX(antenna_selection), 2)
      ENDIF ELSE BEGIN
          antenna_name_suffix = '_time' + STRJOIN(STRTRIM(antenna_selection, 2), '_')
      ENDELSE
  ENDIF ELSE antenna_name_suffix = ''

  nfreq = N_ELEMENTS(freq_indices)
  npol = N_ELEMENTS(pol_indices)
  ntime = N_ELEMENTS(time_indices)
  IF nfreq EQ 0 THEN MESSAGE, 'The frequency selection is empty.'
  IF npol EQ 0 THEN MESSAGE, 'The polarisation selection is empty.'
  IF ntime EQ 0 THEN MESSAGE, 'The time/antenna selection is empty.'

  orig_obsname = file_basename(file_path_fhd)

  ; rename the save file names to be representative of the selections runs
  file_path_vis = file_dirname(file_path_vis) + '/' + file_basename(file_basename(file_path_vis, '.h5'), 'uvh5') $
      + freq_name_suffix + time_name_suffix + pol_name_suffix + antenna_name_suffix 
  file_path_fhd = file_dirname(file_path_fhd) + '/' + file_basename(file_path_fhd) $
      + freq_name_suffix + time_name_suffix + pol_name_suffix + antenna_name_suffix

  ;--------------------------------------------------
  ; Read visibilities
  ;--------------------------------------------------

  ; HDF5 hyperslabs are contiguous. Read the smallest bounding block and
  ; then reduce it to the requested (possibly non-contiguous) indices.
  time_start = MIN(time_indices)
  freq_start = MIN(freq_indices)
  pol_start = MIN(pol_indices)
  time_span = MAX(time_indices) - time_start + 1L
  freq_span = MAX(freq_indices) - freq_start + 1L
  pol_span = MAX(pol_indices) - pol_start + 1L
  time_relative = time_indices - time_start
  freq_relative = freq_indices - freq_start
  pol_relative = pol_indices - pol_start

  ; IDL sees UVH5 data as [pol,freq,time].
  offset = ULONG64([pol_start,freq_start,time_start])
  count = ULONG64([pol_span,freq_span,time_span])

  did = H5D_OPEN(fid, '/Data/visdata')
  fspace = H5D_GET_SPACE(did)
  dims = H5S_GET_SIMPLE_EXTENT_DIMS(fspace)
  IF N_ELEMENTS(dims) NE 3 OR dims[0] NE npol_total OR dims[1] NE nfreq_total OR dims[2] NE nblts_total THEN $
    MESSAGE,'Unexpected /Data/visdata dimensions.'
  H5S_SELECT_HYPERSLAB, fspace, offset, count, /reset
  mspace = H5S_CREATE_SIMPLE(count)
  dtype = H5D_GET_TYPE(did)
  vis_span = H5D_READ(did, dtype, FILE_SPACE=fspace, MEMORY_SPACE=mspace)
  vis_tags = STRLOWCASE(TAG_NAMES(vis_span))
  real_index = (WHERE(vis_tags EQ 'r'))[0]
  imaginary_index = (WHERE(vis_tags EQ 'i'))[0]
  weights_index = N_ELEMENTS(vis_tags)
  vis_r = REFORM(vis_span.r,pol_span,freq_span,time_span)
  vis_i = REFORM(vis_span.i,pol_span,freq_span,time_span)

  H5S_CLOSE, fspace
  H5D_CLOSE, did
  H5T_CLOSE, dtype
  H5S_CLOSE, mspace

  did = H5D_OPEN(fid, '/Data/nsamples')
  fspace = H5D_GET_SPACE(did)

  CATCH, weights_error 
  IF weights_error NE 0 THEN BEGIN
      CATCH, /CANCEL
      message, 'Weights could not be read from h5 file. Ensure flags_compression=None,nsample_compression=None is set when making h5 files.'
  ENDIF ELSE BEGIN
      H5S_SELECT_HYPERSLAB, fspace, offset, count, /reset
      mspace = H5S_CREATE_SIMPLE(count)
      weights_dtype = H5D_GET_TYPE(did)
      weights_span = reform(H5D_READ(did, FILE_SPACE=fspace, MEMORY_SPACE=mspace),pol_span,freq_span,time_span)
      CATCH, /CANCEL
  ENDELSE

  H5S_CLOSE, fspace
  H5D_CLOSE, did
  H5S_CLOSE, mspace

  ;--------------------------------------------------
  ; UVW arrays
  ;--------------------------------------------------

  time_subset = DOUBLE(time_array[time_indices])
  lst_subset = DOUBLE(lst_array[time_indices])
  ant1_subset = ant1[time_indices]
  ant2_subset = ant2[time_indices]
  uvw_subset = uvw_array[*, time_indices]
  inttime_subset = integration_time[time_indices]
  freq_subset = freq_array[freq_indices]

  c_m_s = 299792458D

  uu = DOUBLE(uvw_subset[0,*]) / c_m_s
  vv = DOUBLE(uvw_subset[1,*]) / c_m_s
  ww = DOUBLE(uvw_subset[2,*]) / c_m_s

  ;--------------------------------------------------
  ; Parameter extraction
  ;--------------------------------------------------

  jd0 = FLOOR(MIN(time_subset))
  baseline = ant1_subset*nants_telescope + ant2_subset

  param_names = ['uu','vv','ww','date_int','date_frac','baseline','ant1','ant2','inttime','lst']
  params = DBLARR(N_ELEMENTS(param_names), ntime)
  params[(WHERE(param_names EQ 'uu'))[0],*] = uu
  params[(WHERE(param_names EQ 'vv'))[0],*] = vv
  params[(WHERE(param_names EQ 'ww'))[0],*] = ww

  params[(WHERE(param_names EQ 'date_int'))[0],*] = 0D
  params[(WHERE(param_names EQ 'date_frac'))[0],*] = DOUBLE(time_subset) - DOUBLE(jd0)

  params[(WHERE(param_names EQ 'baseline'))[0],*] = baseline
  params[(WHERE(param_names EQ 'ant1'))[0],*] = ant1_subset
  params[(WHERE(param_names EQ 'ant2'))[0],*] = ant2_subset
  params[(WHERE(param_names EQ 'inttime'))[0],*] = inttime_subset
  params[(WHERE(param_names EQ 'lst'))[0],*] = lst_subset

  IF MAX(ABS(lst_subset)) LE 2D*!DPI THEN BEGIN
      params[(WHERE(param_names EQ 'lst'))[0],*] = lst_subset * !RADEG
  ENDIF

  hdr = fhd_struct_init_hdr(n_grp_params=N_ELEMENTS(param_names),nbaselines=ntime,n_tile=nants_data,n_pol=npol,$
    n_freq=nfreq,freq_res=MEAN(channel_width),freq_arr=freq_subset,lon=lon,lat=lat,alt=alt,$
    obsra=obsra[0],obsdec=obsdec[0],uu_i=(WHERE(param_names EQ 'uu'))[0],vv_i=(WHERE(param_names EQ 'vv'))[0],$
    ww_i=(WHERE(param_names EQ 'ww'))[0],baseline_i=(WHERE(param_names EQ 'baseline'))[0],$
    date_i=[(WHERE(param_names EQ 'date_int'))[0],(WHERE(param_names EQ 'date_frac'))[0]],jd0=jd0,date_obs=date_obs,$
    pol_dim=(WHERE(dims EQ npol_total))[0]+1L,freq_dim=(WHERE(dims EQ nfreq_total))[0]+1L,real_index=real_index,$
    imaginary_index=imaginary_index,weights_index=weights_index,ant1_i=(WHERE(param_names EQ 'ant1'))[0],$
    ant2_i=(WHERE(param_names EQ 'ant2'))[0])

  hdr = structure_update(hdr, _Extra={freq_ref:MEAN(freq_array), orig_obsname:orig_obsname})

  ;--------------------------------------------------
  ; Convert to UVFITS layout
  ; array[3,npol,num_chans,nblts]
  ;--------------------------------------------------

  array = FLTARR(3,npol,nfreq,ntime)
  n_values = LONG64(npol)*LONG64(nfreq)*LONG64(ntime)
  output_index = L64INDGEN(n_values)
  i_pol = output_index MOD npol
  i_freq = (output_index/npol) MOD nfreq
  i_time = output_index/(LONG64(npol)*LONG64(nfreq))

  input_index = LONG64(pol_relative[i_pol]) + $
      LONG64(pol_span)*(LONG64(freq_relative[i_freq]) + $
      LONG64(freq_span)*LONG64(time_relative[i_time]))
  output_index = LONG64(3)*output_index

  array[output_index] = vis_r[input_index]
  array[output_index+1L] = vis_i[input_index]
  array[output_index+2L] = weights_span[input_index]

  H5F_CLOSE,fid

  RETURN, {params:params, array:array, hdr:hdr}

END