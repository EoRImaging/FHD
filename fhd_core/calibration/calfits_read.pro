Function calfits_read,file_path_fits,obs,params,silent=silent,_Extra=extra


  IF file_test(file_path_fits) EQ 0 THEN message,"File: "+file_path_fits+" not found! Returning"
  
  t_readfits=Systime(1)
  lun = fxposit(file_path_fits, 0,/readonly)
  data_array=mrdfits(lun,0,data_header0,/silent)
  
  ;*********Read-in header
  naxis = sxpar(data_header0,'naxis') ;number of data axes
  data_dims = INTARR(naxis)
  for naxis_i=0, naxis - 1 do data_dims[naxis_i] = sxpar(data_header0,'naxis'+strtrim((naxis_i+1),2)) ;dimension of each data axis
  telescope = sxpar(data_header0,'telescop') ;telescope that took the data
  gain_convention = sxpar(data_header0,'gnconven') ;convention of the gains, either 'divide' or 'multiply'
  n_jones = sxpar(data_header0,'njones') ;number of polarizations/jones with calibration data
  caltype = sxpar(data_header0,'caltype') ;type of calibration, 'gain' or 'delay'
  if STRMATCH(caltype, 'delay*', /FOLD_CASE) then message, 'Input delay calibration not supported at this time.'
  time_integration = sxpar(data_header0,'inttime') ;time integration of the calibration data
  freq_channel_width = sxpar(data_header0,'chwidth') ;frequency channel width
  x_orient = sxpar(data_header0,'xorient') ;Orientation of the X dipole.
  time_range = sxpar(data_header0,'tmerange') ;Time range that the calibration data is valid for
  
  data_types = STRARR(naxis)
  for naxis_i=0, naxis - 1  do data_types[naxis_i] = strtrim(sxpar(data_header0,'ctype'+strtrim((naxis_i+1),2)),2) ;type of each data axis
  ;Find out which axis is which
  data_index = Min(where(strmatch(data_types, 'Narrays', /FOLD_CASE) EQ 1))
  ant_index = Min(where(strmatch(data_types, 'antaxis', /FOLD_CASE) EQ 1))
  freq_index = Min(where(strmatch(data_types, 'freqs', /FOLD_CASE) EQ 1))
  time_index = Min(where(strmatch(data_types, 'time', /FOLD_CASE) EQ 1))
  jones_index = Min(where(strmatch(data_types, 'jones', /FOLD_CASE) EQ 1))
  spec_wind_index = Min(where(strmatch(data_types, 'if', /FOLD_CASE) EQ 1))
  data_narray = sxpar(data_header0,strjoin('crval'+strtrim((data_index+1),2))) ;real(gain), imag(gain), flags, (optional input flags), quality
  freq_start = sxpar(data_header0,strjoin('crval'+strtrim((freq_index+1),2)))
  time_start = sxpar(data_header0,strjoin('crval'+strtrim((time_index+1),2)))
  time_delt = sxpar(data_header0,strjoin('cdelt'+strtrim((time_index+1),2)))
  jones_start = sxpar(data_header0,strjoin('crval'+strtrim((jones_index+1),2)))
  jones_delt = sxpar(data_header0,strjoin('cdelt'+strtrim((jones_index+1),2)))
  ;*********
  
  ;*********Check parameters
  IF (data_index NE 0) OR (ant_index NE 4) OR (freq_index NE 3) OR (time_index NE 2) OR (jones_index NE 1) then begin  ; calfits doesn't conform to the original convention
    IF (data_index EQ 0) AND (ant_index EQ 5) AND (freq_index EQ 3) AND (time_index EQ 2) AND (jones_index EQ 1) AND (spec_wind_index EQ 4) then begin  ; calfits conforms to the Fall 2018 pyuvdata convention
      if (size(data_array))[5] ne 1 then message, 'Calfits file includes more than one spectral window. Note that this feature is not yet supported in FHD.'
      data_array = mean(data_array, dimension=5)  ; Remove spectral window dimension for compatibility
    endif else message, 'Calfits file does not appear to adhere to standard. Please see github:pyuvdata/docs/references'
  endif 

  if ~keyword_set(gain_convention) then gain_convention = 'divide' ;default of the gain convention if undefined
  
  n_ant_data = (size(data_array))[5]
  n_freq = (size(data_array))[4]
  n_time = (size(data_array))[3]

  ;Check whether the number of polarizations specified matches the observation analysis run
  jones_type_matrix = LONARR(data_dims[1])
  for jones_i=1, data_dims[1] do jones_type_matrix[jones_i-1] = jones_start+(jones_delt*jones_i)
  IF data_dims[1] GT obs.n_pol then begin
    if ~keyword_set(silent) then print, 'More polarizations in calibration fits file than in observation analysis. Reducing calibration to match obs.'
    data_dims[1] = obs.n_pol
    jones_type_matrix = jones_type_matrix[0:obs.n_pol-1]
    data_array = data_array[*,0:obs.n_pol-1,*,*,*]
  endif else if data_dims[1] LT obs.n_pol then message, 'Not enough polarizations defined in calibration fits file.'
  
  ;Switch the pol convention to FHD standard if necessary
  if STRMATCH(x_orient, 'north*', /FOLD_CASE) OR STRMATCH(x_orient, 'south*', /FOLD_CASE) then begin
    ypol = data_array[*,0,*,*,*]
    xpol = data_array[*,1,*,*,*]
    data_array[*,0,*,*,*] = xpol
    data_array[*,1,*,*,*] = ypol
    undefine, xpol, ypol
    if obs.n_pol GT 2 then begin
      yxpol = data_array[*,2,*,*,*]
      xypol = data_array[*,3,*,*,*]
      data_array[*,2,*,*,*] = xypol
      data_array[*,3,*,*,*] = yxpol
      undefine, xypol, yxpol
    endif
  endif
  ;*********
  
  ;*********Check to see if the calibration and observation frequency resolution match
  if (freq_channel_width NE obs.freq_res) then begin
    freq_factor = obs.freq_res / freq_channel_width
    if freq_factor GE 1 then logic_test = freq_factor - ULONG(freq_factor) else logic_test = 1./freq_factor - ULONG(1./freq_factor)
    if logic_test NE 0 then message, 'Calfits input freq channel width is not easily castable to the observation, different by a factor of ' + strtrim(freq_factor,2)
    if freq_start NE (*obs.baseline_info).freq[0] then message, 'Calfits input freq start is not equal to observation freq start'
    
    ;Downselect the data array
    if freq_factor GT 1 then begin
      if ~keyword_set(silent) then print, 'Calfits input freq channel width is different by a factor of ' + strtrim(freq_factor,2) + '. Averaging down.'
      
      ;Set flagged indices to NAN to remove them from mean calculation
      flag_inds = where(abs(reform(data_array[2,*,*,*,*])) EQ 1, n_flags) ;value of 1 is flagged data
      for real_imag_i=0,1 do begin
        data_array_temp = reform(data_array[real_imag_i,*,*,*,*])
        if n_flags GT 0 then data_array_temp[flag_inds] = !VALUES.F_NAN
        data_array[real_imag_i,*,*,*,*] = temporary(data_array_temp)
      endfor
      
      data_array_temp = DBLARR(data_dims[0],data_dims[1],data_dims[2],obs.n_freq,data_dims[4])
      for channel_i=0, obs.n_freq -1 do data_array_temp[*,*,*,channel_i,*] = $
        mean(data_array[*,*,*,(channel_i*freq_factor-floor(freq_factor/2.))>0:channel_i*freq_factor+floor(freq_factor/2.),*],/NAN)
      data_array = temporary(data_array_temp)
      
    ;Upselect the data array
    endif else if freq_factor LT 1 then begin
      if ~keyword_set(silent) then print, 'Calfits input freq channel width is different by a factor of ' + strtrim(freq_factor,2) + '. Using linear interpolation.'
      
      data_array_temp = DBLARR(data_dims[0],data_dims[1],data_dims[2],obs.n_freq,data_dims[4])
      for data_i=0,1 do for jones_i=0,n_jones-1 do for times_i=0,n_time-1 do for tile_i=0, n_ant_data-1 do $
        for channel_i=0, n_freq -2 do data_array_temp[data_i,jones_i,times_i,channel_i*(1./freq_factor):(channel_i+1)*(1./freq_factor)-1,tile_i] = $
        interpolate(reform(data_array[data_i,jones_i,times_i,channel_i:channel_i+1,tile_i]),FLTARR(1./freq_factor)*freq_factor)
      data_array_temp[*,*,*,obs.n_freq-1,*] = data_array[*,*,*,n_freq-1,*]
      data_array = temporary(data_array_temp)
    endif
  endif
  ;*********
  
  ;*********Check to see what time range this needs to be applied to, and if pointings are necessary
  if n_time NE 1 then begin
  
    sec_upperlimit = 2000.
    sec_lowerlimit = 1600.
    
    If ((time_integration LT sec_upperlimit) AND (time_integration GT sec_lowerlimit)) OR $
      ((time_delt LT sec_upperlimit) AND (time_delt GT sec_lowerlimit)) then begin
      ;Calibration fits are per-pointing
      obs_pointing = mwa_get_pointing_number(obs)
      obs_julian_date = obs.astr.mjdobs + 2400000.5D
      
      ;Find corresponding pointing in the calfits
      days_since_ref = floor(obs_julian_date) - floor(time_start) ;number of days since Aug23,2013
      obs_pointing_shift_since_ref = ((24D - 23.9344699) / 24D)*double(days_since_ref) ;pointing start shift amount depending on how many days since ref
      pointing_jdhms_ref = [.13611,.15157,.17565,.19963,.22222,.24342593,.26453705,.28574074,.30694,.33657407] ;pointing start time for HH:MM:SS on Aug23 (in JD)
      pointing_jdhms_ref = pointing_jdhms_ref + obs_pointing_shift_since_ref ;comparible pointing start time for reference, using calculated shift
      pointing_num_ref = [-5,-4,-3,-2,-1,0,1,2,3,4]
      pointing_jdhms_calfits = time_start - double(floor(time_start)) ;pointing start time for HH:MM:SS for calfits day (in JD)
      temp = Min(Abs(pointing_jdhms_ref - pointing_jdhms_calfits), pointing_calfits_index) ;find the closest index between reference and calfits
      if pointing_jdhms_calfits LT pointing_jdhms_ref[pointing_calfits_index] then pointing_calfits_index -= 1 ;make sure the index is greater than the pointing start time
      
      if (pointing_jdhms_calfits GT (max(pointing_jdhms_ref) + (pointing_jdhms_ref[1] - pointing_jdhms_ref[0]))) OR $
        (pointing_jdhms_calfits LT (min(pointing_jdhms_ref) - (pointing_jdhms_ref[1] - pointing_jdhms_ref[0]))) then $
        message, 'Calfits does not start between five-pointings-before-zenith and four-pointings-after-zenith. Not suitable for pointing cal at this time.'
      pointing_calfits_start = pointing_num_ref[pointing_calfits_index] ;find which pointing is the start of the calfits data
      if obs_pointing LT pointing_calfits_start then message, 'Calfits file does not contain pointing of obsid'
      obs_pointing_index = abs(pointing_calfits_start - obs_pointing) ;select pointing index in calfits that matches observation
      
      data_array = data_array[*,*,obs_pointing_index,*,*] ;choose the corresponding pointing from the calfits data array
      
    endif else begin
      IF (floor(time_delt) EQ floor(obs.time_res)) then begin
        ;Calibration fits are per-timeres
        if ~keyword_set(silent) then print, 'Averaging calfits to observation length, an FHD requirement at this time'
        data_array_temp = DBLARR(data_dims[0],data_dims[1],1,obs.n_freq,data_dims[4])
        data_array_temp[*,*,0,*,*] = mean(data_array, dimension=3)
        data_array = data_array_temp
      endif else begin
        ;Calibration fits are for a random set of times
        if ~keyword_set(silent) then print, 'Finding closest match in time between calfits and obs. Obs metadata assumed to report start time, calfits metadata assumed to report center time.'
        time_array = DBLARR(n_time)
        time_delta = ((time_integration/60D/60D)/24D)
        for time_i=0, n_time-1 do time_array[time_i] = time_start + time_delta
        obs_julian_date = obs.astr.mjdobs + 2400000.5D + (double((obs.n_time * obs.time_res)/2.)/60D/60D)/24D
        if (obs_julian_date LT (time_array[0]-2.*time_delta)) OR (obs_julian_date GT (time_array[n_time-1]+2.*time_delta)) then $
          message, 'Observation does not seem to fit within the time frame of the calfits'
        temp = Min(Abs(obs_julian_date - time_array), time_index) ;find the closest index between calfits and observation
        data_array = data_array[*,*,time_index,*,*]
      endelse
    endelse
    
  endif
  ;*********
  
  ;*********Check number of tiles
  if n_ant_data NE obs.n_tile then message, 'Number of antennas in calfits file does match observation antenna number'
  ;*********
  
  ;*********Propagate flags
  ;*********
  
  ;return the proper format
  cal=fhd_struct_init_cal(obs,params,skymodel,_Extra=extra)
  for pol_i=0, obs.n_pol-1 do (*cal.gain[pol_i])[*,*] = reform(data_array[0,pol_i,0,*,*]) + Complex(0,1)*reform(data_array[1,pol_i,0,*,*])
  
  t_readfits=Systime(1)-t_readfits
  if ~keyword_set(silent) then print,"Time reading calfits files: "+Strn(t_readfits)
  
  return,cal
  
end
