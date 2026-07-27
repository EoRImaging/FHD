; Rephase visibilities to a common phase centre across observations and pointings
pro rephase_uv_data, vis_data_arr, vis_model_arr, vis_weights_use, obs=obs, params=params, $
  w_phasera=w_phasera, w_phasedec=w_phasedec

  n_pol=obs.n_pol
  uu = params.uu
  vv = params.vv
  ww = params.ww

  if ~keyword_set(w_phasera) then w_phasera = obs.orig_phasera
  if ~keyword_set(w_phasedec) then w_phasedec = obs.orig_phasedec
  ; Store for later astrometry / WCS use
  obs = structure_update(obs, _Extra={w_phasera: w_phasera, w_phasedec: w_phasedec})

  ; Calculate the rotation to the common phase centre for the visibilities through
  ; directional cosines to new phase centre expressed in the old tangent frame
  ra0  = obs.phasera  * !dpi / 180d
  dec0 = obs.phasedec * !dpi / 180d
  ra_w  = w_phasera    * !dpi / 180d
  dec_w = w_phasedec   * !dpi / 180d
  dra = ra_w - ra0

  l0 = cos(dec_w) * sin(dra)
  m0 = sin(dec_w) * cos(dec0) - cos(dec_w) * sin(dec0) * cos(dra)
  n0 = sin(dec0) * sin(dec_w) + cos(dec0) * cos(dec_w) * cos(dra)

  freq_arr = (*obs.baseline_info).freq
  ;phase = 2d * !dpi * (freq_arr # (uu*l0 + vv*m0 + ww*(n0 - 1d)))
  phase = 2d * !dpi * (freq_arr # (uu*l0 + vv*m0 - ww*(n0 - 1d)))
  exp_rotate = complex(cos(phase), -sin(phase))

  ; Rephase the visibilities and models (if present) to the new phase centre.
  for pol_i=0, n_pol-1 do begin
    *vis_data_arr[pol_i] = *vis_data_arr[pol_i] * exp_rotate
    if vis_model_arr[pol_i] NE !Null then *vis_model_arr[pol_i] = *vis_model_arr[pol_i] * exp_rotate
  endfor

  ; Now, calculate the rotation to the common phase centre for the uvw coordinates through 
  ; Cartesian vectors and rotation matrices.
  ; Columns are the local tangent basis vectors in celestial Cartesian coords:
  ;   col 0 = east  (u)
  ;   col 1 = north (v)
  ;   col 2 = source direction (w)
  tangent_0 = dblarr(3, 3)
  tangent_0[*,0] = [-sin(ra0), cos(ra0), 0d]
  tangent_0[*,1] = [-sin(dec0)*cos(ra0), -sin(dec0)*sin(ra0), cos(dec0)]
  tangent_0[*,2] = [cos(dec0)*cos(ra0), cos(dec0)*sin(ra0), sin(dec0)]

  tangent_w = dblarr(3, 3)
  tangent_w[*,0] = [-sin(ra_w), cos(ra_w), 0d]
  tangent_w[*,1] = [-sin(dec_w)*cos(ra_w), -sin(dec_w)*sin(ra_w), cos(dec_w)]
  tangent_w[*,2] = [cos(dec_w)*cos(ra_w), cos(dec_w)*sin(ra_w), sin(dec_w)]

  ; Transformation matrix from old tangent frame -> new tangent frame
  T = matrix_multiply(transpose(tangent_w), tangent_0)

  ; Rotate UVW
  uvw_mat = dblarr(3, n_elements(uu))
  uvw_mat[0,*] = uu
  uvw_mat[1,*] = vv
  uvw_mat[2,*] = ww 
  uvw_out = matrix_multiply(T, uvw_mat)

  params.uu = reform(uvw_out[0, *])
  params.vv = reform(uvw_out[1, *])
  params.ww = reform(uvw_out[2, *])

  return

end