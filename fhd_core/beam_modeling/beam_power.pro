FUNCTION beam_power,antenna1,antenna2,obs=obs,ant_pol1=ant_pol1,ant_pol2=ant_pol2,freq_i=freq_i,psf_dim=psf_dim,$
  psf_intermediate_res=psf_intermediate_res,psf_resolution=psf_resolution,beam_mask_threshold=beam_mask_threshold,$
  xvals_uv_superres=xvals_uv_superres,yvals_uv_superres=yvals_uv_superres,$
  zen_int_x=zen_int_x,zen_int_y=zen_int_y,interpolate_beam_threshold=interpolate_beam_threshold,$
  debug_beam_clip_grow=debug_beam_clip_grow,debug_beam_conjugate=debug_beam_conjugate,$
  debug_clip_beam_mask=debug_clip_beam_mask,beam_clip_floor=beam_clip_floor,$
  image_power_beam=image_power_beam,kernel_window=kernel_window,beam_gaussian_decomp=beam_gaussian_decomp,$
  beam_gaussian_params=beam_gaussian_params,volume_beam=volume_beam,sq_volume_beam=sq_volume_beam, res_super=res_super,$
  beam_gauss_param_transfer=beam_gauss_param_transfer,pol_i=pol_i,psf_superres_dim=psf_superres_dim,_Extra=extra

  icomp = Complex(0, 1)
  dimension_super=psf_superres_dim

  ; Generate UV beam at a super resolution
  ; Note: Beam uses the forward FFT for the sky->UV transformation (note that the image uses the inverse FFT)
  if keyword_set(beam_gaussian_decomp) then begin
    ;
    ; Build an analytic-tranformed uv-plane using gaussian mixture models of the image beam
    ;  at the desired overresolution (instead of interpolating)
    beam_gaussian_decomp,dimension_super,res_super,obs=obs,$
      antenna1=antenna1,antenna2=antenna2,psf_base_superres=psf_base_superres,$
      volume_beam=volume_beam,sq_volume_beam=sq_volume_beam,beam_gaussian_params=beam_gaussian_params,$
      freq_i=freq_i,pol=pol_i,ant_pol1=ant_pol1,ant_pol2=ant_pol2,zen_int_x=zen_int_x,zen_int_y=zen_int_y,$
      beam_gauss_param_transfer=beam_gauss_param_transfer,_Extra=extra

    ; Match the FFT norm expected if one had done an overresolved beam (necessary in FFT instances)
    ;  instead of an exact calculation
    fft_norm_expected = 1/double(res_super)^2
    psf_base_superres *= fft_norm_expected
    sq_volume_beam *= fft_norm_expected*psf_intermediate_res^2.

  endif else begin
    ; 
    ; FFT the beam image and interpolate to the desired overresolution

    image_power_beam = beam_image_hyperresolved(antenna1,antenna2,ant_pol1,ant_pol2,freq_i,zen_int_x,zen_int_y)
    if keyword_set(kernel_window) then image_power_beam *= *(antenna1.pix_window)
    psf_base_single=fft_shift(FFT(fft_shift(image_power_beam)))
    psf_base_superres=Interpolate(psf_base_single,xvals_uv_superres,yvals_uv_superres,cubic=-0.5)

  endelse

  ; Build a mask to create a well-defined finite beam
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

  ;;total of the gaussian decomposition can be calculated analytically, but is an over-estimate 
  ;; of the numerical representation and results in a beam norm of greater than one,
  ;; thus the discrete total is used  
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
    psf_amp = interpol_2d(abs(psf_base_superres*uv_mask_superres), uv_mask_superres, nan_safe=1) > 0
    psf_phase = Fltarr(size(psf_base_superres, /dimension))
    psf_phase[beam_i] = Atan(psf_base_superres[beam_i], /phase)
    psf_phase = interpol_2d(psf_phase, uv_mask_superres, nan_safe=1)
    psf_base_superres = psf_amp*Cos(psf_phase) + icomp*psf_amp*Sin(psf_phase)
  ENDIF ELSE psf_base_superres*=uv_mask_superres

  IF Keyword_Set(beam_clip_floor) THEN BEGIN
    i_use = where(abs(psf_base_superres))
    psf_amp = abs(psf_base_superres)
    psf_phase = Atan(psf_base_superres, /phase)

    psf_floor = psf_mask_threshold_use*(psf_intermediate_res^2.)
    psf_amp[i_use] -= psf_floor
    psf_base_superres = psf_amp*Cos(psf_phase) + icomp*psf_amp*Sin(psf_phase)
  ENDIF

  psf_base_superres*=psf_val_ref/Total(psf_base_superres)
  IF Keyword_Set(debug_beam_conjugate) THEN psf_base_superres=Conj(psf_base_superres)
  RETURN,psf_base_superres
END
