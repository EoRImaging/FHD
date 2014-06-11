

function eor_sim, u_arr, v_arr, freq_arr, seed = seed, flat_sigma = flat_sigma, no_distrib = no_distrib, delta_power = delta_power, delta_uv_loc = delta_uv_loc

  if n_elements(seed) eq 0 then seed = systime(1)
  
  delta_u = u_arr[1] - u_arr[0]
  delta_v = v_arr[1] - v_arr[0]
  f_delta = freq_arr[1]-freq_arr[0]
  n_kz = n_elements(freq_arr)
  
  z0_freq = 1420.40 ;; MHz
  ;; make sure frequencies are in MHz
  if min(freq_arr) gt 1e8 then frequencies = freq_arr / 1e6 else frequencies = freq_arr
  
  redshifts = z0_freq/frequencies - 1
  cosmology_measures, redshifts, comoving_dist_los = comov_dist_los
  
  comov_los_diff = comov_dist_los - shift(comov_dist_los, -1)
  comov_los_diff = comov_los_diff[0:n_elements(comov_dist_los)-2]
  z_mpc_delta = float(mean(comov_los_diff))
  z_mpc_mean = float(mean(comov_dist_los))
  
  ;; converting from Jy (in u,v,f) to mK*str (10^-26 * c^2 * 10^3/ (2*f^2*kb))
  conv_factor = float((3e8)^2 / (2. * (frequencies*1e6)^2. * 1.38065))
  
  ;; convert from Jy -> mk*str -> mK*Mpc^2
  conv_factor = conv_factor * z_mpc_mean^2.
  
  ;; convert from uv (in wavelengths) to kx/ky in inverse comoving Mpc
  kx_mpc = u_arr * (2.*!pi) / z_mpc_mean
  kx_mpc_delta = delta_u * (2.*!pi) / z_mpc_mean
  n_kx = n_elements(kx_mpc)
  
  ky_mpc = v_arr * (2.*!pi) / z_mpc_mean
  ky_mpc_delta = delta_v * (2.*!pi) / z_mpc_mean
  n_ky = n_elements(ky_mpc)
  
  z_mpc_length = float(max(comov_dist_los) - min(comov_dist_los) + z_mpc_delta)
  kz_mpc_range =  (2.*!pi) / (z_mpc_delta)
  kz_mpc_delta = (2.*!pi) / z_mpc_length
  kz_mpc = findgen(round(kz_mpc_range / kz_mpc_delta)) * kz_mpc_delta - kz_mpc_range/2.
  if n_elements(kz_mpc) ne n_kz then stop
  
  ;; savefile contains: k_centers, power
  ;restore, base_path('data') + 'eor_data/eor_power_1d.idlsave' ;;k_centers, power
  restore, filepath('eor_power_1d.idlsave',root=rootdir('FHD'),subdir='catalog_data')
  
  npts_log = n_elements(k_centers)
  
  log_diff =  alog10(k_centers) - shift(alog10(k_centers), 1)
  log_diff = log_diff[1:*]
  log_binsize = log_diff[0]
  
  if n_elements(flat_sigma) ne 0 then begin
    power_3d = dblarr(n_kx, n_ky, n_kz) + max(power)
    
  endif else if keyword_set(delta_power) then begin
    no_distrib=1
    
    if n_elements(delta_uv_loc) eq 0 then delta_uv_loc = randomu(seed, 2)*[max(u_arr)-min(u_arr), max(v_arr)-min(v_arr)] + [min(u_arr), min(v_arr)]
    
    temp = u_arr - delta_uv_loc[0]
    u_loc = where(temp eq min(abs(temp)))
    
    temp = v_arr - delta_uv_loc[1]
    v_loc = where(temp eq min(abs(temp)))
    
    kz_loc = where(kz_mpc eq min(abs(kz_mpc)))
    
    power_3d = dblarr(n_kx, n_ky, n_kz)
    power_3d[u_loc, v_loc, kz_loc] = max(power)
  endif else begin
  
    k_arr = sqrt(rebin(kx_mpc, n_kx, n_ky, n_kz, /sample)^2. + rebin(reform(ky_mpc, 1, n_ky), n_kx, n_ky, n_kz, /sample)^2. + $
      rebin(reform(kz_mpc, 1, 1, n_kz), n_kx, n_ky, n_kz, /sample)^2.)
    wh0 = where(k_arr eq 0, count)
    if count ne 0 then k_arr[wh0] = min(k_centers)
    
    result = 10^(interpol(float(alog10(power)), float(alog10(k_centers)), alog10(k_arr)))
    
    power_3d = reform(temporary(result), n_kx, n_ky, n_kz)
    
    ;mu = rebin(reform(abs(kz_mpc), 1, 1, n_kz), n_kx, n_ky, n_kz, /sample) / temporary(k_arr)
    ;power_3d = power_3d * (1 + 2 * mu^2. + mu^4.)
    
    undefine, mu
  endelse
  
  ;; multiply by window function to go from mK^2*Mpc^3 to mK^2*Mpc^6
  ;; for theory, window function = observation volume = (2pi)^3/(delta k)^3
  signal2 = temporary(power_3d) * (2.*!pi)^3./(kx_mpc_delta*ky_mpc_delta*kz_mpc_delta)
  
  ;; take square root & divide by sqrt(2) to get signal amp expectation value in mK*Mpc^3
  signal_amp_exp = sqrt(temporary(signal2)/2.)
  
  if keyword_set(no_distrib) then signal = temporary(signal_amp_exp) else begin
    signal_real = randomn(seed, n_kx, n_ky, n_kz) * signal_amp_exp
    signal_imaginary = randomn(seed, n_kx, n_ky, n_kz) * temporary(signal_amp_exp)
    signal = temporary(signal_real) + complex(0,1) * temporary(signal_imaginary)
  endelse
  
  ;signal_amp = sqrt(temporary(power_3d))
  ;signal_phase = randomu(seed, n_kx, n_ky, n_kz) * 2. * !pi
  
  ;signal = temporary(signal_amp) * exp(complex(0,1) * temporary(signal_phase))
  
  ;; shift it so that it's as expected when we take the fft
  signal = shift(temporary(signal), [0,0,n_kz/2])
  
  print, 'signal^2d integral:', total(abs(signal)^2d)
  print, 'signal^2d integral * 2pi*delta_k^2d:', total(abs(signal)^2d) * kz_mpc_delta * 2d * !dpi
  
  ;;temp = conj(reverse(signal[*,*,1:n_kz-2],3))
  ;;signal = [[[signal]], [[temp]]]
  
  ;; Old convention
  ;; get into uv vs kx,ky -- factor of (2pi)^2 in amplitude
  ;; signal = signal * (2.*!pi)^2.
  
  ;; fourier transform along z direction to get to uvf space (in mK*Mpc^2)
  ;; old convention
  ;; temp = fft(temporary(signal), dimension = 3, /inverse) * kz_mpc_delta
  temp = fft(temporary(signal), dimension = 3, /inverse) * kz_mpc_delta / (2.*!pi)
  
  ;; convert to Jy
  for i=0, n_kz-1 do temp[*,*,i] = temp[*,*,i]/conv_factor[i]
  
  ;; 1st frequency is typically flagged. if flag_sigma and no_distrib is set, cube is only non-zero in 1st freq so shift to put power in next frequency
  if keyword_set(flat_sigma) and keyword_set(no_distrib) then temp = shift(temporary(temp), [0,0,1])
  
  print, 'sum(uvf signal^2)*z_delta:', total(abs(temp)^2d)*z_mpc_delta
  
  return, temp
end
