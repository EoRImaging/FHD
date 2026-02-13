function rotation_matrix, a=a, b=b, obs=obs
; rotation from a into b

  ; Cross product and dot product
  v = crossp(a, b)
  c = total(a * b)
  s = sqrt(total(v^2))

  if s EQ 0.0 then begin
    if c GT 0.0 then return, identity(3)  ; no rotation
    ; 180-degree rotation: choose orthogonal axis
    if abs(a[0]) LT 0.9 then axis = [1.0, 0.0, 0.0] else axis = [0.0, 1.0, 0.0]
    v = crossp(a, axis)
    v = v / sqrt(total(v^2))
    vx = [[ 0,     -v[2],  v[1]], $
          [ v[2],   0,    -v[0]], $
          [-v[1],  v[0],   0   ]]
    return, identity(3) + 2.0 * matrix_multiply(vx, vx)
  endif

  ; Skew-symmetric matrix of v
  vx = [[ 0,     -v[2],  v[1]], $
        [ v[2],   0,    -v[0]], $
        [-v[1],  v[0],   0   ]]

  ; Compute the rotation matrix
  R = identity(3) + vx + matrix_multiply(vx, vx) * ((1.0 - c) / (s^2))

  return, R
end

; === MAIN: Rephase visibilities to a common phase centre across observations and pointings
; === i.e. point to EoR0 for EoR0 observations.
pro rephase_uv_data, vis_data_arr, vis_model_arr, vis_weights_use, obs, params=params

  n_pol=obs.n_pol
  uu = params.uu
  vv = params.vv
  ww = params.ww

  ra  = obs.phasera  * !dpi / 180.0
  dec = obs.phasedec * !dpi / 180.0
  x = cos(dec) * cos(ra)
  y = cos(dec) * sin(ra)
  z = sin(dec)
  phase_centre_vec = [x, y, z]

  ra  = obs.orig_phasera  * !dpi / 180.0
  dec = obs.orig_phasedec * !dpi / 180.0
  x = cos(dec) * cos(ra)
  y = cos(dec) * sin(ra)
  z = sin(dec)
  orig_phase_centre_vec = [x, y, z]

  ; Get rotation matrix to align the fitted plane with the z-axis
  R = rotation_matrix(a=phase_centre_vec, b=orig_phase_centre_vec)

  ; Apply phase shift to visibilities
  freq_arr = (*obs.baseline_info).freq
  delta_phase_centre =  orig_phase_centre_vec - phase_centre_vec
  phase = - 2.0 * !dpi * (freq_arr#(uu * delta_phase_centre[0])  + freq_arr#(vv * delta_phase_centre[1])  + freq_arr#(ww * delta_phase_centre[2])) 

  ; Rotate UVW
  uvw_out = matrix_multiply(R, transpose([[uu], [vv], [ww]]),/atranspose)

  params.uu = reform(uvw_out[0, *])
  params.vv = reform(uvw_out[1, *])
  params.ww = reform(uvw_out[2, *])

  exp_rotate = complex(cos(phase), sin(phase))

  for pol_i=0, n_pol-1 do begin
    *vis_data_arr[pol_i] = *vis_data_arr[pol_i] * exp_rotate
    if vis_model_arr[pol_i] NE !Null then *vis_model_arr[pol_i] = *vis_model_arr[pol_i] * exp_rotate
  endfor

  return

end
