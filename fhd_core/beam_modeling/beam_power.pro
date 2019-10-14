FUNCTION beam_power,antenna1,antenna2,ant_pol1=ant_pol1,ant_pol2=ant_pol2,freq_i=freq_i,psf_dim=psf_dim,$
  psf_image_dim=psf_image_dim,psf_intermediate_res=psf_intermediate_res,psf_resolution=psf_resolution,$
  beam_mask_electric_field=beam_mask_electric_field,beam_mask_threshold=beam_mask_threshold,$
  xvals_uv_superres=xvals_uv_superres,yvals_uv_superres=yvals_uv_superres,zen_int_x=zen_int_x,zen_int_y=zen_int_y,$
  interpolate_beam_threshold=interpolate_beam_threshold,debug_beam_clip_grow=debug_beam_clip_grow,$
  debug_beam_conjugate=debug_beam_conjugate, beam_clip_floor=beam_clip_floor,$
  debug_clip_beam_mask=debug_clip_beam_mask,beam_per_baseline=beam_per_baseline,$
  image_power_beam=image_power_beam,kernel_window=kernel_window,_Extra=extra

  icomp = Complex(0, 1)
  freq_center=antenna1.freq[freq_i]
  dimension_super=(size(xvals_uv_superres,/dimension))[0]

  Jones1=antenna1.Jones[*,*,freq_i]
  Jones2=antenna2.Jones[*,*,freq_i]

  beam_ant1=DComplex(*(antenna1.response[ant_pol1,freq_i]))
  beam_ant2=DComplex(Conj(*(antenna2.response[ant_pol2,freq_i])))
  beam_norm=1.

  IF ant_pol1 NE ant_pol2 THEN BEGIN
    power_zenith_beam1=abs((*Jones1[0,ant_pol1])*(Conj(*Jones2[0,ant_pol1]))+$
      (*Jones1[1,ant_pol1])*(Conj(*Jones2[1,ant_pol1])))
    power_zenith1=Interpolate(power_zenith_beam1,zen_int_x,zen_int_y,cubic=-0.5)
    power_zenith_beam2=abs((*Jones1[0,ant_pol2])*(Conj(*Jones2[0,ant_pol2]))+$
      (*Jones1[1,ant_pol2])*(Conj(*Jones2[1,ant_pol2])))
    power_zenith2=Interpolate(power_zenith_beam2,zen_int_x,zen_int_y,cubic=-0.5)
    power_zenith=Sqrt(power_zenith1*power_zenith2)

    power_beam1=(*Jones1[0,ant_pol1]*beam_ant1)*(Conj(*Jones2[0,ant_pol1])*beam_ant2)+$
      (*Jones1[1,ant_pol1]*beam_ant1)*(Conj(*Jones2[1,ant_pol1])*beam_ant2)
    power_beam2=(*Jones1[0,ant_pol2]*beam_ant1)*(Conj(*Jones2[0,ant_pol2])*beam_ant2)+$
      (*Jones1[1,ant_pol2]*beam_ant1)*(Conj(*Jones2[1,ant_pol2])*beam_ant2)
    power_beam=Sqrt(power_beam1*power_beam2)
    debug_point=1
  ENDIF ELSE BEGIN
    power_zenith_beam=abs((*Jones1[0,ant_pol1])*(Conj(*Jones2[0,ant_pol2]))+$
      (*Jones1[1,ant_pol1])*(Conj(*Jones2[1,ant_pol2])))
    power_zenith=Interpolate(power_zenith_beam,zen_int_x,zen_int_y,cubic=-0.5)
    ;power_zenith=abs((*Jones1[0,ant_pol1])[zen_int_x,zen_int_y]*(Conj(*Jones2[0,ant_pol2]))[zen_int_x,zen_int_y]+$
    ;             (*Jones1[1,ant_pol1])[zen_int_x,zen_int_y]*(Conj(*Jones2[1,ant_pol2]))[zen_int_x,zen_int_y])
    power_beam=(*Jones1[0,ant_pol1]*beam_ant1)*(Conj(*Jones2[0,ant_pol2])*beam_ant2)+$
      (*Jones1[1,ant_pol1]*beam_ant1)*(Conj(*Jones2[1,ant_pol2])*beam_ant2)
  ENDELSE

  image_power_beam=power_beam/power_zenith
  if keyword_set(kernel_window) then image_power_beam *= *(antenna1.pix_window)

  ;Generate UV beam and interpolate to a super resolution
  psf_base_single=dirty_image_generate(image_power_beam,/no_real)
  psf_base_superres=Interpolate(psf_base_single,xvals_uv_superres,yvals_uv_superres,cubic=-0.5)
  
  s=size(psf_base_superres, /dimensions)
  uv_mask_superres=Fltarr(s[0],s[1]) ;dynamically set size to match psf_base_superres
  psf_mask_threshold_use = Max(Abs(psf_base_superres))/beam_mask_threshold
  IF ant_pol1 NE ant_pol2 THEN BEGIN
    seed_i=where(Abs(psf_base_superres) GE Max(Abs(psf_base_superres))/2.,n_seed)
    beam_i=region_grow(Abs(psf_base_superres),seed_i,$
      thresh=[psf_mask_threshold_use,Max(Abs(psf_base_superres))])
  ENDIF ELSE BEGIN
    beam_i=region_grow(Abs(psf_base_superres),dimension_super*(1.+dimension_super)/2.,$
      thresh=[psf_mask_threshold_use,Max(Abs(psf_base_superres))])
  ENDELSE
  uv_mask_superres[beam_i]=1

  IF Keyword_Set(debug_beam_clip_grow) THEN BEGIN
    mask_dist_test = morph_distance(uv_mask_superres, neighbor=3, /background)
    uv_mask_superres_int=Fltarr(dimension_super,dimension_super)
    uv_mask_superres_int[where(mask_dist_test LT psf_resolution)] = 1
    inds_ring = where(uv_mask_superres_int - uv_mask_superres, n_ring)

    psf_base_real = real_part(psf_base_superres) > 0
    psf_base_imag = imaginary(psf_base_superres)
    IF n_ring GT 0 THEN BEGIN
      inds_cut = where(psf_base_real LE 0, n_cut)
      IF n_cut GT 0 THEN psf_base_imag[inds_cut] = 0
    ENDIF
    psf_base_superres_int = psf_base_real + icomp*psf_base_imag

    psf_base_superres = psf_base_superres_int
    uv_mask_superres = uv_mask_superres_int
  ENDIF

  ;FFT normalization correction in case this changes the total number of pixels
  psf_base_superres*=psf_intermediate_res^2.
  psf_base_superres/=beam_norm
  psf_val_ref=Total(psf_base_superres)

  IF Keyword_Set(debug_clip_beam_mask) THEN BEGIN
    xvals_i=Reform(meshgrid(psf_dim,psf_dim,1)*psf_resolution,psf_dim^2.)
    yvals_i=Reform(meshgrid(psf_dim,psf_dim,2)*psf_resolution,psf_dim^2.)
    beam_mask = Fltarr(size(psf_base_superres[xvals_i,yvals_i], /dimension)) + 1
    FOR i=0,psf_resolution-1 DO FOR j=0,psf_resolution-1 DO beam_mask *= $
      uv_mask_superres[xvals_i+i,yvals_i+j]
    FOR i=0,psf_resolution-1 DO FOR j=0,psf_resolution-1 DO $
      uv_mask_superres[xvals_i+i,yvals_i+j] *= beam_mask
  ENDIF

  IF Keyword_Set(interpolate_beam_threshold) THEN BEGIN
    psf_amp = interpol_2d(abs(psf_base_superres*uv_mask_superres), uv_mask_superres) > 0
    psf_phase = Fltarr(size(psf_base_superres, /dimension))
    psf_phase[beam_i] = Atan(psf_base_superres[beam_i], /phase)
    psf_phase = interpol_2d(psf_phase, uv_mask_superres)
    psf_base_superres = psf_amp*Cos(psf_phase) + icomp*psf_amp*Sin(psf_phase)
  ENDIF ELSE psf_base_superres*=uv_mask_superres

  IF Keyword_Set(beam_clip_floor) THEN BEGIN
    i_use = where(abs(psf_base_superres))
    psf_amp = abs(psf_base_superres)
    psf_phase = Atan(psf_base_superres, /phase)

    psf_floor = psf_mask_threshold_use*(psf_intermediate_res^2.)/beam_norm
    psf_amp[i_use] -= psf_floor
    psf_base_superres = psf_amp*Cos(psf_phase) + icomp*psf_amp*Sin(psf_phase)
  ENDIF

  psf_base_superres*=psf_val_ref/Total(psf_base_superres)
  IF Keyword_Set(debug_beam_conjugate) THEN psf_base_superres=Conj(psf_base_superres)
  RETURN,psf_base_superres
END
