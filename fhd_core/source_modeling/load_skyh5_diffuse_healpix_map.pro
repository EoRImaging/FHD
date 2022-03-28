FUNCTION load_skyh5_diffuse_healpix_map, model_filepath, freq_center=freq_center, coord_use=coord_use,$
  skyh5_name_hpx_stokes=skyh5_name_hpx_stokes, skyh5_name_hpx_inds=skyh5_name_hpx_inds, skyh5_name_hpx_order=skyh5_name_hpx_order,$
  skyh5_name_hpx_nside=skyh5_name_hpx_nside, skyh5_name_hpx_nfreqs=skyh5_name_hpx_nfreqs, skyh5_name_hpx_npixels=skyh5_name_hpx_npixels,$
  skyh5_name_hpx_freq_arr=skyh5_name_hpx_freq_arr
  ; Restore a Healpix map from a .skyh5 file
  
  coord_use = 'celestial' ;assume celestial coordinate system

  if ~keyword_set(skyh5_name_hpx_stokes) then skyh5_name_hpx_stokes='/Data/stokes'
  if ~keyword_set(skyh5_name_hpx_inds) then skyh5_name_hpx_inds='/Header/hpx_inds'
  if ~keyword_set(skyh5_name_hpx_order) then skyh5_name_hpx_order='/Header/hpx_order'
  if ~keyword_set(skyh5_name_hpx_nside) then skyh5_name_hpx_nside='/Header/nside'
  if ~keyword_set(skyh5_name_hpx_nfreqs) then skyh5_name_hpx_nfreqs='/Header/Nfreqs'
  if ~keyword_set(skyh5_name_hpx_npixels) then skyh5_name_hpx_npixels='/Header/Ncomponents'
  if ~keyword_set(skyh5_name_hpx_freq_arr) then skyh5_name_hpx_freq_arr='/Header/freq_array'

  file_id = H5F_OPEN(model_filepath)

  stokes_id = H5D_OPEN(file_id, skyh5_name_hpx_stokes)
  model_hpx_arr = H5D_READ(stokes_id)
  H5D_CLOSE, stokes_id
  
  hpx_inds_id = H5D_OPEN(file_id, skyh5_name_hpx_inds)
  hpx_inds = H5D_READ(hpx_inds_id)
  H5D_CLOSE, hpx_inds_id
  
  hpx_order_id = H5D_OPEN(file_id, skyh5_name_hpx_order)
  hpx_order = H5D_READ(hpx_order_id)
  H5D_CLOSE, hpx_order_id
  
  nside_id = H5D_OPEN(file_id, skyh5_name_hpx_nside)
  nside = H5D_READ(nside_id)
  H5D_CLOSE, nside_id
  
  nfreqs_id = H5D_OPEN(file_id, skyh5_name_hpx_nfreqs)
  nfreqs = H5D_READ(nfreqs_id)
  H5D_CLOSE, nfreqs_id
  
  npixels_id = H5D_OPEN(file_id, skyh5_name_hpx_npixels)
  npixels = H5D_READ(npixels_id)
  H5D_CLOSE, npixels_id
  
  ; If more than one frequency is present, choose the closest
  if nfreqs eq 1 then model_hpx_arr = reform(model_hpx_arr) else begin
    freq_arr_id = H5D_OPEN(file_id, skyh5_name_hpx_freq_arr)
    freq_arry = H5D_READ(freq_arr_id)
    H5D_CLOSE, freq_arr_id
    null = min(freq_arr-freq_center, min_ind, /absolute)
    model_hpx_arr = reform(model_hpx_arr[*, min_ind, *])
  endelse
  
  H5F_CLOSE, file_id

  if StrUpCase(hpx_order) eq 'NESTED' then begin ;reorder to ring ordering
    ;requires implicit indexing
    data_implicit = make_array(12*nside^2, 4, /float, value=-1.6375e+30)
    for pix=0,npixels-1 do begin
      data_implicit[hpx_inds[pix], *] = model_hpx_arr[pix, *]
    endfor
    data_implicit = reorder(data_implicit, /n2r)
    keep_pixels = where(data_implicit[*, 0] ne -1.6375e+30)
    model_hpx_arr = data_implicit[keep_pixels, *]
    hpx_inds = keep_pixels
  endif
  
  ;Convert to pointer array
  n_stokes = 4 ;assume all 4 Stokes parameters are present
  model_hpx_ptr_array = Ptrarr(n_stokes)
  for stokes_ind=0,n_stokes-1 do model_hpx_ptr_array[stokes_ind]=Ptr_new(reform(model_hpx_arr[*,stokes_ind]))
  model_hpx_arr=model_hpx_ptr_array
  
  RETURN, model_hpx_arr

END