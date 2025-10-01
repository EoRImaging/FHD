;Script to apply a delay space filter at the horizon to remove fft artifacts.
;
;Model visibilities are phased to zenith, windowed, transformed to delay space,
;cut at the horizon, transformed back to visibility space, unwindowed, unphased
;from zenith, and frequency cut to match the data.
;
pro vis_delay_filter, vis_model_arr,  params, obs

  print, 'Applying a horizon delay filter'

  ; u,v,w are in light travel time in seconds
  freq_arr = (*obs.baseline_info).freq
  ;for i=1,floor(obs.n_freq/2) do freq_arr = [freq_arr[0]-obs.freq_res*i,freq_arr]
  ;for i=1,floor(obs.n_freq/2) do freq_arr = [freq_arr,freq_arr[obs.n_freq-1]+obs.freq_res*i]
  
  freq_res = obs.freq_res
  n_pol = obs.n_pol
  kbinsize=obs.kpix
  nfreq = n_elements(freq_arr)
  nbl = n_elements(params.uu)
  
  data = Complex(fltarr(nfreq,nbl,n_pol)) ; Stack pols
  
  for pol_i=0,n_pol-1 do data[*,*,pol_i] = *vis_model_arr[pol_i]
  
  
  ;test with removing zeroed visibilities instead
  total_data = total(total(data,1),2)
  bi_use = where(total_data ne 0)
  auto_inds = where(params.antenna1 ne params.antenna2)
  match, bi_use, auto_inds, suba, subb
  bi_use = bi_use[suba]
  
  data = data[*,bi_use,*]
  uu = params.uu[bi_use]
  vv = params.vv[bi_use]
  ww = params.ww[bi_use]
  bb = sqrt(uu^2.+vv^2.+ww^2.)
  nbl = n_elements(bi_use)
  
  ; Phase to zenith -- easier calculations of the location of the horizon when phased to zenith
  dimension=obs.dimension
  apply_astrometry,obs,ra_arr=obs.zenra, dec_arr=obs.zendec, x_arr=x_use, y_arr=y_use, /ad2xy, /refraction
  dx=obs.obsx - obs.zenx
  dy=obs.obsy - obs.zeny
  dx*=(2.*!Pi/dimension)
  dy*=(2.*!Pi/dimension)
  phase=(uu#freq_arr)*dx/kbinsize + (vv#freq_arr)*dy/kbinsize
  rephase_vals=transpose(Complex(Cos(phase),Sin(phase))) ;multiply by vis to phase
  for pol_i=0,n_pol-1 do data[*,*,pol_i] *= rephase_vals
  undefine_fhd, uu, vv, ww
  
  ; Apply window function
  window = spectral_window(nfreq, type='Blackman-Harris',/periodic)
  norm_factor = sqrt(nfreq/total(window^2.))
  window = window * norm_factor
  window_expand = rebin(reform(window,nfreq,1,1), nfreq, nbl, n_pol, /sample)
  data = data * window_expand
 
  ; FFT
  spectra = shift(fft(data,dim=1),nfreq/2,0,0) ; Shift only in fft direction.
  undefine_fhd, data, window
 
  ;Cut at the horizon
  tau_cut=1.
  
  ;Calculate upper and lower delay limits for each baseline
  lower_limit = nfreq*(.5 - bb * tau_cut * freq_res)
  upper_limit = nfreq*(.5 + bb * tau_cut * freq_res)
  
  for freq_i=0, nfreq-1 do begin
    mask_high = freq_i/upper_limit
    mask_high_inds = where(mask_high GT 1., n_count)
    if n_count GT 0 then spectra[freq_i,mask_high_inds,*] = 0
    
    mask_low = freq_i/lower_limit
    mask_low_inds = where(mask_low LT 1., n_count)
    if n_count GT 0 then spectra[freq_i,mask_low_inds,*] = 0
  endfor
  
  masked_data = fft(shift(spectra,nfreq/2,0,0),dim=1,/inverse)
  
  masked_data = masked_data / window_expand
  
  ; UnPhase from zenith and cut to the desired band 
  for pol_i=0,n_pol-1 do begin
    masked_data[*,*,pol_i] *= 1./rephase_vals
    (*vis_model_arr[pol_i]) = (*vis_model_arr[pol_i])[obs.n_freq/4:LONG(3./4.*obs.n_freq-1),*]
    (*vis_model_arr[pol_i])[*,bi_use] = masked_data[obs.n_freq/4:LONG(3./4.*obs.n_freq-1),*,pol_i]
  endfor

  return
  
end