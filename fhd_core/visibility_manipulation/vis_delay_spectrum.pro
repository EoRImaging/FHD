pro vis_delay_spectrum, dir, obsid=obsid
  ; This is a script to generate delay spectra from visibilities
  ; TODO: Contruct file_path_fhd from dir and obsid, and then use standard fhd_save_io
  ; TODO: Handle backward compatibility (see read flags lines)
  ; TODO: Put data into physical units
  ; TODO: Call plotting routine to generate 2D PS-like plots
  ; TODO: Window function capability
  
  if not keyword_set(obsid) then obsid = '1061316296'
  if not size(obsid,/type) ne 7 then obsid = number_formatter(obsid)
  i_comp = Complex(0,1)
  
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
  u_mat = freq_arr#params.uu/obs.kpix ; This should now be in wavelengths
  v_mat = freq_arr#params.vv/obs.kpix
  w_mat = freq_arr#params.ww/obs.kpix
  data = fltarr(nfreq,nbl,2) ; Stack pols
  restore, dir+'/vis_data/'+obsid+'_vis_XX.sav'
  data[*,*,0] = *vis_ptr
  restore, dir+'/vis_data/'+obsid+'_vis_YY.sav'
  data[*,*,1] = *vis_ptr
  undefine_fhd,vis_ptr
  data *= flags
  undefine_fhd,flags
  ; Phase to zenith (see Danny for explanation)
  for poli=0,1 do data[*,*,poli] *= exp(i_comp * 2. * !pi * w_mat)
  
  ; Do the fft
  spectra = abs(shift(fft(data,dim=1),nfreq/2,0,0))^2 ; Shift only in fft direction.
  undefine_fhd,data
  ; fold over
  ndelay = nfreq/2.
  spectra[(ndelay+1):*,*,*] += spectra[(ndelay-1):1:-1,*,*]
  spectra = spectra[ndelay:*,*,*]
  ; Bin up
  umag = sqrt(abs(params.uu)^2 + abs(params.vv)^2)
  umin = min(umag)
  umax = max(umag)
  nbins = 100
  ubin = (umax-umin)/nbins
  uhist = histogram(umag, binsize=ubin, min=umin, omax=umax, locations=u_locs, reverse_indices=u_ri)
  u_centers = u_locs + ubin/2d
  u_edges = [u_locs, max(u_locs) + ubin]
  nbins = n_elements(uhist)
  delay2d = fltarr(nbins,ndelay,2)
  for i=0,nbins-1 do begin
    if uhist[i] gt 0 then begin
      delay2d[i,*,*] = mean(spectra[*,u_ri[u_ri[i]:u_ri[i+1]-1],*],dim=2)
    endif
  endfor

end