;;
;; Decompose the image beam of an instrument using functions
;;
;; dimension_super: Required dimension of the super resolved beam kernel (e.g. the kernel lookup table)
;; res_super: Required resolution of the super resolved beam kernel (e.g. the kernel lookup table)
;; psf_intermediate_res: Required intermediate resolution of the beam given its extent
;; obs: Required obs structure for observation info
;; antenna1, antenna2: Required antenna structures for metadata and responses 
;; ant_pol1, ant_pol2: Required polarization of the antennas
;; psf_base_superres: Returns the gaussian decomposed uv beam for the two antennas 
;; maxiter: Option to control the maximum number of iterations for the 2D fitter
;; freq_i: Required frequency index for the current fit
;; pol: Required polarization index for the current_fit
;; zen_int_x, zen_int_y: Pixel indicies that line up with a common grid
;; beam_param_transfer: Optionally transfer a psf structure with params instead of fitting
;; beam_function_decomp: The functional form to use for the decomposition, currently supports 'gaussian' and 'blackmanharris'
;; conserve_memory: Optionally use a max byte limit for heavy load loops
;; image_power_beam: Optionally return the beam image 
;; beam_decomp_info: Pass back the beam decomposition information structure
;; beam_width_deg: Pass a beam width in degrees centered on zenith to create using the functional form
;; silent: Optionally turn off print statements 

pro beam_decomp, dimension_super, res_super, psf_intermediate_res, obs=obs, $
  antenna1=antenna1, antenna2=antenna2, ant_pol1=ant_pol1,ant_pol2=ant_pol2,$
  psf_base_superres=psf_base_superres, maxiter=maxiter, $
  freq_i=freq_i,pol=pol,zen_int_x=zen_int_x,zen_int_y=zen_int_y,$
  beam_param_transfer=beam_param_transfer,beam_function_decomp=beam_function_decomp,$
  conserve_memory=conserve_memory,image_power_beam=image_power_beam,$
  beam_decomp_info=beam_decomp_info,beam_width_deg=beam_width_deg,silent=silent,_Extra=extra

  ;Current options for decomp type are blackmanharris and gaussian
  IF SIZE(beam_function_decomp, /TYPE) EQ 7 THEN BEGIN
    decomp_type = strlowcase(beam_function_decomp)
    if (decomp_type NE 'gaussian') AND (decomp_type NE 'blackmanharris') THEN BEGIN
      message, 'beam_function_decomp options are "gaussian" or "blackmanharris"'
    ENDIF
  ENDIF ELSE BEGIN
    ; Default to blackman-harris decomposition, pass back default
    beam_function_decomp = 'blackmanharris'
    decomp_type = beam_function_decomp
  ENDELSE

  instrument=obs.instrument
  ;All phases of the MWA use the same beam parameters
  if STRMID(instrument,0,3) eq 'mwa' then begin
    beam_decomp_fn = 'mwa_beam_'+decomp_type+'_decomp'
  endif else beam_decomp_fn = instrument + '_beam_'+decomp_type+'_decomp'

  n_freq = antenna1.nfreq_bin
  pix_hor = round(obs.dimension/antenna1.psf_scale) ;number of pixels spanning horizon to horizon
  psf_image_dim = antenna1.psf_image_dim
  ;slight image padding factor to include zeroed super-horizon pixels for fitting purposes, which is also even
  pix_hor_pad = Ceil(pix_hor*1.3/2)*2 
  pixel_vector = FINDGEN(pix_hor_pad)
  range = [psf_image_dim/2-pix_hor_pad/2.,psf_image_dim/2+pix_hor_pad/2.-1]

  ;; Create a functional form of the beams as a function of chosen frequencies either given a parameter list or, 
  ;;  in the case of Gaussians, letting the in-house fitter calculate the parameters with linear least squares fitting.
  ;;  Fitting every frequency would take too long *and* introduce spectral structure due to fitting residuals
  ;;  The YY polarization is just the flip of XX, so no need to refit.
  if (freq_i EQ 0) then begin

    if (pol EQ 0) then begin

      if keyword_set(beam_width_deg) then begin
        ; If the user has provided a simple beam width in degrees, formulate the input beam paramters

        model_npix = pix_hor_pad
        model_res = (obs.dimension*psf_intermediate_res/psf_image_dim)

        fwhm_use = beam_width_deg
        if N_elements(fwhm_use) EQ 1 then fwhm_use = replicate(double(fwhm_use), n_freq)
        if N_elements(fwhm_use) NE n_freq then begin
          message, 'beam_width_deg must be a scalar or have length n_freq.'
        endif

        if decomp_type EQ 'gaussian' then begin
           print, 'Using beam_width_deg as a FWHM to set gaussian parameters for the beam decomposition.'
          ; Convert FWHM in degrees to sigma in model pixels (assuming 180 deg horizon-to-horizon).
          sigma_deg = double(fwhm_use) / (2.0d * sqrt(2.0d * alog(2.0d)))
          deg_per_pix = 180.0d / double(pix_hor)
          sigma_pix = sigma_deg / deg_per_pix

          beam_params = dblarr(5, n_freq)
          beam_params[0,*] = 1.0d
          beam_params[1,*] = pix_hor_pad/2.
          beam_params[2,*] = sigma_pix
          beam_params[3,*] = pix_hor_pad/2.
          beam_params[4,*] = sigma_pix
        endif else if decomp_type EQ 'blackmanharris' then begin
          print, 'Using beam_width_deg as the window length to set blackman-harris parameters for the beam decomposition.'
          ; Convert width (deg) to width in pixels
          deg_per_pix = 180.0D / DOUBLE(pix_hor)
          fwhm_pix = DOUBLE(fwhm_use) / deg_per_pix

          ; BH window support width conversion
          ; Harris 7-term coefficients (Table II, 1978)
          a0 = 0.2712203606D & a1 = 0.4334446123D & a2 = 0.21800412D
          a3 = 0.0657853433D & a4 = 0.0107618673D & a5 = 0.0007700121D & a6 = 0.00001368088D

          ; window defined on t in [0,1], peak at t=0.5
          lo = 0.5D & hi = 1.0D
          FOR i=0,60 DO BEGIN
            mid = (lo + hi) / 2.0D
            w = a0 - a1*COS(2D*!DPI*mid) + a2*COS(4D*!DPI*mid) $
                - a3*COS(6D*!DPI*mid) + a4*COS(8D*!DPI*mid) $
                - a5*COS(10D*!DPI*mid) + a6*COS(12D*!DPI*mid)
            IF w GT 0.5D THEN lo = mid ELSE hi = mid
          ENDFOR

          ; full-width at half max in normalized units
          fwhm_norm = 2.0D * (mid - 0.5D)
          width_pix = fwhm_pix / fwhm_norm

          beam_params = dblarr(4, n_freq)
          beam_params[0,*] = 1.0D
          beam_params[1,*] = width_pix
          beam_params[2,*] = pix_hor_pad/2.
          beam_params[3,*] = pix_hor_pad/2.
        endif

        if ~keyword_set(beam_params_ptr) then beam_params_ptr = ptrarr(obs.n_pol)
        beam_params_ptr[pol] = ptr_new(beam_params)
        beam_decomp_info = {beam_params:beam_params_ptr, decomp_type:decomp_type, model_res:model_res, model_npix:model_npix}

      endif else if keyword_set(beam_param_transfer) then begin

      ;optionally transfer pre-fitted parameters from a psf structure
        psf_transfer = getvar_savefile(beam_param_transfer,'psf')
        obs_transfer = getvar_savefile(beam_param_transfer,'obs')
        if ~tag_exist(psf_transfer, 'beam_decomp_info') then $
          message, 'No beam_decomp_info found in the provided psf structure for transfer.'
        beam_decomp_info = *psf_transfer.beam_decomp_info
        if decomp_type NE beam_decomp_info.decomp_type then $
          message, 'No beam parameters of type '+decomp_type+' found in the provided psf structure for transfer.'
        beam_params = *beam_decomp_info.beam_params[0]

        ;Get or calculate the extent (original pix_hor_pad) that the model was fit to
        if tag_exist(beam_decomp_info, 'model_npix') then begin
          model_npix = beam_decomp_info.model_npix
        endif else model_npix = ceil(psf_transfer.pix_horizon * 1.3/2)*2

      endif else begin 
        ;;
        ;; Use an iterative least-squares fitter to determine the beam decomposition
        ;; Currently only tested with MWA Gaussian decomposition

        if strlowcase(beam_function_decomp) NE 'gaussian' then begin
          message, 'Beam decomposition iterative fitter currently only implemented for gaussian decomposition.' + $
            ' Use beam_param_transfer to provide pre-fitted parameters for other decompositions fitted in external code.'
        endif

        model_npix = pix_hor_pad
        model_res = (obs.dimension*psf_intermediate_res/psf_image_dim)

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
          fitted_p = MPFIT2DFUN(decomp_type + '_decomp', pixel_vector, pixel_vector, $
            abs(image_power_beam[range[0]:range[1],range[0]:range[1]]), 1 , p, parinfo=parinfo, weights=1d, /quiet, errmsg=errmsg, $
            maxiter=maxiter,nocovar=1,bestnorm=chi_squared,niter=niter)
          timing=Systime(1)-t0
          if ~keyword_set(silent) then print, "Chi-squared of beam gaussian fit is " + strtrim(chi_squared,2) + $
            " in " + strtrim(niter,2) + " total iterations for " + strtrim(timing,2) + 'secs'
          if keyword_set(errmsg) then message, "Gaussian mixture model least-squares fitting return error: " + errmsg

          all_p[fbin_i,*] = fitted_p 
          stop
          ;Update estimate of gauss params for next iteration
          p=fitted_p
        endfor

        if ~keyword_set(beam_params) then beam_params = DBLARR(N_elements(p),n_freq)
      
        ;; Gaussian parameters constrained by selected frequencies. Fill in the rest of the freq with second-order polynomial
        if gauss_beam_fbin LT n_freq then begin
          poly_coeffs=FLTARR(N_elements(p),3)
          for p_i=0,N_elements(p)-1 do begin
            poly_coeffs[p_i,*] = poly_fit(FINDGEN(gauss_beam_fbin)*(n_freq/gauss_beam_fbin), all_p[*,p_i], 2)
            beam_params[p_i,*] = $
              poly_coeffs[p_i,0] + poly_coeffs[p_i,1]*FINDGEN(n_freq) + poly_coeffs[p_i,2]*FINDGEN(n_freq)^2.
          endfor
        endif else beam_params = transpose(all_p)

        if ~keyword_set(beam_params_ptr) then beam_params_ptr = ptrarr(obs.n_pol)
        beam_params_ptr[pol] = ptr_new(beam_params)
        beam_decomp_info = {beam_params:beam_params_ptr, decomp_type:decomp_type, model_res:model_res, model_npix:model_npix}

      endelse ;end else transfer

    endif else begin
    ;
    ;if YY polarization, then flip the fitted XX rather than refit for speed purposes
    beam_params = *beam_decomp_info.beam_params[0] 

      if decomp_type EQ 'gaussian' then begin
        ;Expand the vector to readable names
        var = reform(beam_params,5,N_elements(beam_params[*,0])/5.,n_freq)
        amp = var[0,*,*]
        ;Flip the x and y offsets/sigmas
        offset_x = var[3,*,*]
        sigma_x = var[4,*,*]
        offset_y = var[1,*,*]
        sigma_y = var[2,*,*]

        ;Reform the vector
        var = [[amp,offset_x,sigma_x,offset_y,sigma_y]]
        beam_params = reform(var,N_elements(beam_params[*,0]),n_freq)
      endif
      if decomp_type EQ 'blackmanharris' then begin
        var = REFORM(beam_params, 4, N_ELEMENTS(beam_params)/4)
        amp = var[0,*]
        sigma = var[1,*]
        ;Flip the x and y offsets
        offset_x = var[3,*]
        offset_y = var[2,*]

        ;Reform the vector
        var = [[amp,sigma,offset_x,offset_y]]
        beam_params = reform(var,N_elements(beam_params[*,0]),n_freq)
      endif
      ; Every param besides the YY beam_params has already been set
      beam_decomp_info.beam_params[pol] = ptr_new(beam_params)

    endelse

  endif

  ; Typical usage is to overresolve in uv-space to avoid aliasing. 
  ;   Overresolution factor between image-space and uv-space: res_super

  ;Build uv-plane of the beam decomposition
  psf_base_superres = call_function(decomp_type + '_decomp',$
    FINDGEN(dimension_super),FINDGEN(dimension_super),(*beam_decomp_info.beam_params[pol])[*,freq_i],$
    ftransform=1,model_npix=beam_decomp_info.model_npix,$
    model_res=(obs.dimension*psf_intermediate_res/psf_image_dim) / beam_decomp_info.model_res,$
    over_res=res_super,conserve_memory=conserve_memory)

  fft_norm_expected = 1/double(res_super)^2
  psf_base_superres *= fft_norm_expected

  ;;This builds a beam image at the hyperresolved image resolution to compare to instrumental
  ;;beam images made in beam_power.pro
  ; image_power_beam = call_function(decomp_type + '_decomp',$
  ;   FINDGEN(psf_image_dim),FINDGEN(psf_image_dim),(*beam_decomp_info.beam_params[pol])[*,freq_i],$
  ;   ftransform=0,model_npix=model_npix,model_res=(obs.dimension*psf_intermediate_res/psf_image_dim) / beam_decomp_info.model_res,$
  ;   over_res=1,)
  ;
  ;;This builds a beam image that can be ffted to match psf_base_superres in scale and resolution
  ; image_power_beam2 =  call_function(decomp_type + '_decomp', $
  ;   FINDGEN(dimension_super),FINDGEN(dimension_super), (*beam_decomp_info.beam_params[pol])[*,freq_i],$
  ;   ftransform=0,model_npix=model_npix,model_res=(obs.dimension*psf_intermediate_res/psf_image_dim) / beam_decomp_info.model_res,$
  ;   over_res=res_super)

end
