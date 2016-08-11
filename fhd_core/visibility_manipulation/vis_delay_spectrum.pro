pro vis_delay_spectrum, dir, obsid=obsid,spec_window_type=spec_window_type, plotfile=plotfile
  ; This is a script to generate delay spectra from visibilities
  ; TODO: Contruct file_path_fhd from dir and obsid, and then use standard fhd_save_io
  ; TODO: Handle backward compatibility (see read flags lines)
  ; TODO: Filter out flagged baselines better (ie, bl_use array)
  ; TODO: Put data into physical units
  ; TODO: Fix missing delay axis of 2D plot
  
  if not keyword_set(obsid) then obsid = '1061316296'
  if not size(obsid,/type) ne 7 then obsid = number_formatter(obsid)
  i_comp = Complex(0,1)
  
  ;;;;; Read in and prepare data
  restore, dir+'/metadata/'+obsid+'_params.sav'
  restore, dir+'/vis_data/'+obsid+'_flags.sav'
  restore, dir+'/metadata/'+obsid+'_obs.sav'
  ;u,v,w are in light travel time in seconds
  freq_arr = (*obs.baseline_info).freq
  nfreq = n_elements(freq_arr)
  nbl = n_elements(params.uu)
  flags = fltarr(nfreq,nbl,2)
  ;for poli=0,1 do flags[*,*,poli] = *flag_arr[poli] ; TODO: handle backward compatibility
  ;undefine_fhd,flag_arr
  for poli=0,1 do flags[*,*,poli] = *vis_weights[poli]
  undefine_fhd,vis_weights
  w_mat = freq_arr#params.ww ; This should now be in wavelengths
  data = Complex(fltarr(nfreq,nbl,2)) ; Stack pols
  restore, dir+'/vis_data/'+obsid+'_vis_XX.sav'
  data[*,*,0] = *vis_ptr
  restore, dir+'/vis_data/'+obsid+'_vis_YY.sav'
  data[*,*,1] = *vis_ptr
  undefine_fhd,vis_ptr
  data *= flags
  undefine_fhd,flags
  ; Phase to zenith (see Danny for explanation)
  for poli=0,1 do data[*,*,poli] *= exp(i_comp * 2. * !pi * w_mat)
  
  ;;;;; Do the FFT
  ; First, apply spectral window function
  if n_elements(spec_window_type) ne 0 then begin
    window = spectral_window(nfreq, type=spec_window_type,/periodic)
    norm_factor = sqrt(nfreq/total(window^2.))
    window = window * norm_factor
    window_expand = rebin(reform(window,nfreq,1,1), nfreq, nbl, 2, /sample)
    data = data * window_expand
  endif
  ; FFT
  spectra = abs(shift(fft(data,dim=1),nfreq/2,0,0))^2. ; Shift only in fft direction.
  undefine_fhd,data
  ; fold over
  ndelay = nfreq/2.
  spectra[(ndelay+1):*,*,*] += spectra[(ndelay-1):1:-1,*,*]
  spectra = spectra[ndelay:*,*,*]
  ; Bin up
  umag = sqrt(abs(params.uu)^2 + abs(params.vv)^2) * mean(freq_arr)
  umin = min(umag)
  umax = max(umag)
  nbins = 100
  ubin = (umax-umin)/nbins
  uhist = histogram(umag, binsize=ubin, min=umin, omax=umax, locations=u_locs, reverse_indices=u_ri)
  u_centers = u_locs + ubin/2d
  u_edges = [u_locs, max(u_locs) + ubin]
  nbins = n_elements(uhist)
  ; Make 2D image
  delay2d = fltarr(nbins,ndelay,2)
  for i=0,nbins-1 do begin
    if uhist[i] gt 0 then begin
      delay2d[i,*,*] = mean(spectra[*,u_ri[u_ri[i]:u_ri[i+1]-1],*],dim=2)
    endif
  endfor

  ;;;; Get physical units
  speed_of_light = 299792458.
  z0_freq = 1420.40e6 ;; Hz
  redshifts = z0_freq / freq_arr - 1
  cosmology_measures, redshifts, comoving_dist_los = comov_dist_los, hubble_param = hubble_param,$
                         Ez=Ez, wedge_factor=wedge_factor
  z_mpc_mean = float(mean(comov_dist_los))
  kperp_lambda_conv = z_mpc_mean / (2.*!pi)
  kperp_edges = u_edges / kperp_lambda_conv
  delay_delta = 1./(max(freq_arr)-min(freq_arr))
  delay_max = (ndelay-1) * delay_delta
  ; Note extra 1e3 for hubble constant to get it in m/s/Mpc
  kpar_delta = delay_delta * 2*!pi * 100. * hubble_param * 1e3 * z0_freq * mean(Ez) / (speed_of_light * (1+mean(redshifts))^2.)
  kpar_edges = kpar_delta*findgen(ndelay+1)-kpar_delta/2.
  
  if keyword_set(plotfile) then begin
    wedge_amp = mean(wedge_factor) * !dpi / 180d * [20d, 90d]
    kpower_2d_plots, power=delay2d, kperp_edges=kperp_edges, kpar_edges=kpar_edges, $
                       kperp_lambda_conv=kperp_lambda_conv, delay_params=[delay_delta, delay_max], $
                       hubble_param=hubble_param, plotfile=plotfile, /pdf, /hinv,/ delay_axis, $
                       /baseline_axis, /plot_wedge_line, wedge_amp=wedge_amp, $
                       kperp_plot_range=[.007, .3], kpar_plot_range=[.003,4]
  endif
end