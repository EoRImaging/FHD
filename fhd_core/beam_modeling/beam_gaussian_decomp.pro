;;
;; Decompose the image beam of an instrument using pre-defined gaussians in a 
;;  2D-fitting routine.
;;
;; dimension_super: Required dimension of the super resolved beam kernel (e.g. the kernel lookup table)
;; res_super: Required resolution of the super resolved beam kernel (e.g. the kernel lookup table)
;; obs: Required obs structure for observation info
;; antenna1, antenna2: Required antenna structures for metadata and responses 
;; ant_pol1, ant_pol2: Required polarization of the antennas
;; psf_base_single: Returns the gaussian decomposed uv beam for the two antennas 
;; maxiter: Option to control the maximum number of iterations for the 2D fitter
;; beam_gaussian_params: Returns the gaussian parameters fit with the 2D fitter
;; volume_beam: Returns the analytical calculation of the beam volume
;; sq_volume_beam: Returns the analytical calculation of the squared beam volume
;; freq_i: Required frequency index for the current fit
;; pol: Required polarization index for the current_fit
;; zen_int_x, zen_int_y: Pixel indicies that line up with a common grid
;; beam_gauss_param_transfer: Optionally transfer a psf structure with params instead of fitting
;; silent: Optionally turn off print statements 
;; conserve_memory: Optionally use a max byte limit for heavy load loops

pro beam_gaussian_decomp, dimension_super, res_super, obs=obs, $
  antenna1=antenna1, antenna2=antenna2, ant_pol1=ant_pol1,ant_pol2=ant_pol2,$
  psf_base_superres=psf_base_superres, maxiter=maxiter, beam_gaussian_params=beam_gaussian_params, $
  volume_beam=volume_beam,sq_volume_beam=sq_volume_beam,freq_i=freq_i,pol=pol,$
  zen_int_x=zen_int_x,zen_int_y=zen_int_y,beam_gauss_param_transfer=beam_gauss_param_transfer,$
  silent=silent,conserve_memory=conserve_memory,_Extra=extra

  instrument=obs.instrument
  ;All phases of the MWA use the same beam parameters
  if STRMID(instrument,0,3) eq 'mwa' then begin
    beam_decomp_fn = 'mwa_beam_gaussian_decomp'
  endif else beam_decomp_fn = instrument + '_beam_gaussian_decomp'

  n_freq = antenna1.nfreq_bin
  pix_hor = round(obs.dimension/antenna1.psf_scale) ;number of pixels spanning horizon to horizon
  psf_image_dim = antenna1.psf_image_dim
  ;slight image padding factor to include zeroed super-horizon pixels for fitting purposes, which is also even
  pix_hor_pad = Ceil(pix_hor*1.3/2)*2 
  pixel_vector = FINDGEN(pix_hor_pad)
  range = [psf_image_dim/2-pix_hor_pad/2.,psf_image_dim/2+pix_hor_pad/2.-1]

  ;; Create a functional form of the gaussian beams, with linear least squares fitting at selected frequencies
  ;;  Fitting every frequency would take too long *and* introduce spectral structure due to fitting residuals
  ;;  The YY polarization is just the flip of XX, so no need to refit.
  if (freq_i EQ 0) then begin

    if (pol EQ 0) then begin

      ;optionally transfer pre-fitted gaussian parameters from a psf structure
      if keyword_set(beam_gauss_param_transfer) then begin
        psf_transfer = getvar_savefile(beam_gauss_param_transfer,'psf')
        beam_gaussian_params = *psf_transfer.beam_gaussian_params[0]
      endif else begin

        ;instrument-specific and pointing-specific gaussian parameters (calls mwa_beam_gaussian_decomp for the mwa)
        Call_procedure, beam_decomp_fn, pix_hor_pad/2., pix_hor, obs, parinfo=parinfo, parvalues=p, freq=antenna1.freq[freq_i], $
          gauss_beam_fbin = gauss_beam_fbin, pol=pol

        ;; Least-squares iterator to fit gaussians
        ;;  Loop through a subset of frequencies determined by the instrument in gauss_beam_fbin for speed
        ;;  and spectral smoothness, then fit a polynomial across the params to fill in the rest of the frequencies
        if ~keyword_set(maxiter) then maxiter=100 ;default for MPFIT2DFUN is 200
        if gauss_beam_fbin GT n_freq then gauss_beam_fbin = n_freq
        for i=0, n_freq - 1 do if n_freq mod gauss_beam_fbin NE 0 then gauss_beam_fbin += 1.
        all_p = DBLARR(gauss_beam_fbin,N_elements(p)) ;array for all of the gaussian parameters
        for fbin_i=0, gauss_beam_fbin-1 do begin
          fbin = (n_freq / gauss_beam_fbin) * fbin_i
        
          ;; Create image power beam at specific frequency
          image_power_beam=beam_image_hyperresolved(antenna1,antenna2,ant_pol1,ant_pol2,fbin,zen_int_x,zen_int_y)

          ;; Fit the gaussian decomposition to the instrumental beam image using the 2D fitter
          ;;  p are the input params, parinfo is a structure which details constraints on p, weights are set to one by default,
          ;;  no covariance information is default, and chi_squared and niter detail the statistics of the fit 
          t0=Systime(1)
          fitted_p = MPFIT2DFUN('gaussian_decomp', pixel_vector, pixel_vector, $
            abs(image_power_beam[range[0]:range[1],range[0]:range[1]]), 1 , p, parinfo=parinfo, weights=1d, /quiet, errmsg=errmsg, $
            maxiter=maxiter,nocovar=1,bestnorm=chi_squared,niter=niter)
          timing=Systime(1)-t0
          if ~keyword_set(silent) then print, "Chi-squared of beam gaussian fit is " + strtrim(chi_squared,2) + $
            " in " + strtrim(niter,2) + " total iterations for " + strtrim(timing,2) + 'secs'
          if keyword_set(errmsg) then message, "Gaussian mixture model least-squares fitting return error: " + errmsg

          all_p[fbin_i,*] = fitted_p 

          ;Update estimate of gauss params for next iteration
          p=fitted_p
        endfor

        if ~keyword_set(beam_gaussian_params) then beam_gaussian_params = DBLARR(N_elements(p),n_freq)
      
        ;; Gaussian parameters constrained by selected frequencies. Fill in the rest of the freq with second-order polynomial
        if gauss_beam_fbin LT n_freq then begin
          poly_coeffs=FLTARR(N_elements(p),3)
          for p_i=0,N_elements(p)-1 do begin
            poly_coeffs[p_i,*] = poly_fit(FINDGEN(gauss_beam_fbin)*(n_freq/gauss_beam_fbin), all_p[*,p_i], 2)
            beam_gaussian_params[p_i,*] = $
              poly_coeffs[p_i,0] + poly_coeffs[p_i,1]*FINDGEN(n_freq) + poly_coeffs[p_i,2]*FINDGEN(n_freq)^2.
          endfor
        endif else beam_gaussian_params = transpose(all_p)

      endelse ;end else transfer


    endif else begin
    ;
    ;if YY polarization, then flip the fitted XX rather than refit for speed purposes

      ;Expand the beam_gaussian_params vector to readable names
      var = reform(beam_gaussian_params,5,N_elements(beam_gaussian_params[*,0])/5.,n_freq)
      amp = var[0,*,*]
      ;Flip the x and y offsets/sigmas
      offset_x = var[3,*,*]
      sigma_x = var[4,*,*]
      offset_y = var[1,*,*]
      sigma_y = var[2,*,*]

      ;Reform the beam_gaussian_params vector
      var = [[amp,offset_x,sigma_x,offset_y,sigma_y]]
      beam_gaussian_params = reform(var,N_elements(beam_gaussian_params[*,0]),n_freq)
    endelse

  endif

  ;Build uv-plane of the fitted gaussians
  psf_base_superres = gaussian_decomp(FINDGEN(dimension_super),FINDGEN(dimension_super),beam_gaussian_params[*,freq_i],$
    ftransform=1,model_npix=pix_hor_pad,model_res=res_super*dimension_super/psf_image_dim,volume_beam=volume_beam,$
    sq_volume_beam=sq_volume_beam,conserve_memory=conserve_memory)

end
