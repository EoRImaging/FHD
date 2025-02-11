function set_pol_cal_params, pol_cal_params, name, n_pol_cal_params, n_antenna
  if n_elements(pol_cal_params) EQ 0 then begin
    if n_pol_cal_params gt 1 then begin
      pol_cal_params=fltarr(n_pol_cal_params)
    endif else begin
      pol_cal_params=0.
    endelse
  endif else begin
    if n_pol_cal_params lt 2 then begin
      if n_elements(pol_cal_params) gt 1 then begin
        if n_elements(pol_cal_params) ne n_antenna then $
          message, 'n_pol_cal_params is 0 or 1, so pol' + name + '_cal_params must be a single integer or an array the length of n_antenna'
        if min(pol_cal_params) eq max(pol_cal_params) then pol_cal_params = pol_cal_params[0]
      endif
    endif else begin
      sz = size(pol_cal_params)
      if sz[0] gt 1 then begin
        if sz[1] ne n_pol_cal_params or sz[2] ne n_antenna then $
          message, 'pol' + name + '_cal_params must have shape (n_pol_cal_params) or (n_pol_cal_params, n_antenna)'
        n_diff = 0
        for pi = 0, n_pol_cal_params-1 do begin
          wh_diff = where(pol_cal_params[pi, *] ne pol_cal_params[pi, 0], count_diff)
          n_diff += count_diff
        endfor
        if n_diff eq 0 then pol_cal_params = pol_cal_params[*, 0]
      endif else begin
        if sz[1] ne n_pol_cal_params then $
          message, 'pol' + name + '_cal_params must have shape (n_pol_cal_params) or (n_pol_cal_params, n_antenna)'
      endelse
    endelse
  endelse
  return, pol_cal_params
end

function fhd_struct_init_layout, ant_table_header, ant_table_data, $
    array_center = array_center, coordinate_frame = coordinate_frame, $
    gst0 = gst0, earth_degpd = earth_degpd, ref_date = ref_date, time_system = time_system, $
    dut1 = dut1, diff_utc = diff_utc, nleap_sec = nleap_sec, $
    pol_type = pol_type, n_pol_cal_params = n_pol_cal_params, n_antenna = n_antenna, $
    antenna_names = antenna_names, antenna_numbers = antenna_numbers, antenna_coords = antenna_coords, $
    mount_type = mount_type, axis_offset = axis_offset, pola = pola, pola_orientation = pola_orientation, $
    pola_cal_params = pola_cal_params, polb = polb, polb_orientation = polb_orientation, $
    polb_cal_params = polb_cal_params, diameters = diameters, beam_fwhm = beam_fwhm, $
    _Extra=extra

  if n_elements(ant_table_header) ne 0 then begin
    ;; first get values out of header
    keyword_list = strarr(n_elements(ant_table_header))
    for si = 0, n_elements(ant_table_header)-1 do begin
      keyword_list[si] = strlowcase(strtrim(strmid(ant_table_header[si], 0, 8)))
    endfor
    if n_elements(array_center) eq 0 and max(keyword_list EQ 'arrayx') and $
      max(keyword_list EQ 'arrayy') and max(keyword_list EQ 'arrayz') then $
      array_center = [sxpar(ant_table_header,'arrayx'), sxpar(ant_table_header,'arrayy'), sxpar(ant_table_header,'arrayz')]
      
    if n_elements(coordinate_frame) eq 0 and max(keyword_list EQ 'frame') then $
      coordinate_frame = sxpar(ant_table_header,'frame')
    if n_elements(gst0) eq 0 and max(keyword_list EQ 'gstiao') then $
      gst0 = sxpar(ant_table_header,'gstiao')
    if n_elements(earth_degpd) eq 0 and max(keyword_list EQ 'degpdy') then $
      earth_degpd = sxpar(ant_table_header,'degpdy')
    if n_elements(ref_date) eq 0 and max(keyword_list EQ 'rdate') then $
      ref_date = sxpar(ant_table_header,'rdate')
    if n_elements(time_system) eq 0 then begin
      if max(keyword_list EQ 'timesys') then begin
        time_system = sxpar(ant_table_header,'timesys')
      endif else if max(keyword_list EQ 'timsys') then begin ;; cotter misspells this
        time_system = sxpar(ant_table_header,'timsys')
      endif
    endif
    if n_elements(dut1) eq 0 and max(keyword_list EQ 'ut1utc') then $
      dut1 = sxpar(ant_table_header,'ut1utc')
    if n_elements(diff_utc) eq 0 and max(keyword_list EQ 'datutc') then $
      diff_utc = sxpar(ant_table_header,'datutc')
    if n_elements(nleap_sec) eq 0 and n_elements(time_system) then begin
      if time_system eq 'IAT' then begin
        nleap_sec = diff_utc
      endif
    endif
    ;; 'iatutc' is a non-standard fits keyword, but cotter provides it and it contains the
    ;; number of leap seconds, so we capture it here
    if n_elements(nleap_sec) eq 0 then begin
      if max(keyword_list EQ 'iatutc') then nleap_sec = sxpar(ant_table_header,'iatutc')
    endif
    if n_elements(pol_type) eq 0 and max(keyword_list EQ 'poltype') then $
      pol_type = sxpar(ant_table_header,'poltype')
    if n_elements(n_pol_cal_params) eq 0 and max(keyword_list EQ 'nopcal') then $
      n_pol_cal_params = sxpar(ant_table_header,'nopcal')
    if n_elements(n_antenna) eq 0 and max(keyword_list EQ 'naxis2') then $
      n_antenna = sxpar(ant_table_header,'naxis2')
    
      
    ;; now get values out of table data
    if n_elements(antenna_names) eq 0 and Tag_exist(ant_table_data, 'anname') then $
      antenna_names = ant_table_data.anname
    if n_elements(antenna_numbers) eq 0 and Tag_exist(ant_table_data, 'nosta') then $
      antenna_numbers = ant_table_data.nosta
    if n_elements(antenna_coords) eq 0 and Tag_exist(ant_table_data, 'stabxyz') then $
      antenna_coords = ant_table_data.stabxyz
    if n_elements(mount_type) eq 0 and Tag_exist(ant_table_data, 'mntsta') then $
      mount_type = ant_table_data.mntsta
    if n_elements(axis_offset) eq 0 and Tag_exist(ant_table_data, 'staxof') then $
      axis_offset = ant_table_data.staxof
    if n_elements(pola) eq 0 and Tag_exist(ant_table_data, 'poltya') then $
      pola = ant_table_data.poltya
    if n_elements(pola_orientation) eq 0 and Tag_exist(ant_table_data, 'polaa') then $
      pola_orientation = ant_table_data.polaa
    if n_elements(pola_cal_params) eq 0 and Tag_exist(ant_table_data, 'polcala') then $
      pola_cal_params = ant_table_data.polcala
    if n_elements(polb) eq 0 and Tag_exist(ant_table_data, 'poltyb') then $
      polb = ant_table_data.poltyb
    if n_elements(polb_orientation) eq 0 and Tag_exist(ant_table_data, 'polab') then $
      polb_orientation = ant_table_data.polab
    if n_elements(polb_cal_params) eq 0 and Tag_exist(ant_table_data, 'polcalb') then $
      polb_cal_params = ant_table_data.polcalb
    if n_elements(diameters) eq 0 and Tag_exist(ant_table_data, 'diameter') then $
      diameters = ant_table_data.diameter
    if n_elements(beam_fwhm) eq 0 and Tag_exist(ant_table_data, 'beamfwhm') then $
      beam_fwhm = ant_table_data.beamfwhm

    ;; if n_antenna wasn't already set, calculate it from the antenna table data
    if n_elements(n_antenna) eq 0 and n_elements(antenna_numbers) gt 0 then $
      n_antenna = (size(antenna_numbers))[1]
  endif
  
  ;; if no center given, assume MWA center (Tingay et al. 2013, converted from lat/lon using pyuvdata)
  mwa_center = [-2559454.07880307,  5095372.14368305, -2849057.18534633]
  if n_elements(array_center) EQ 0 then begin
    array_center=mwa_center
    coordinate_frame = 'ITRF'
  endif else if n_elements(array_center) NE 3 then message, 'array_center must be a 3 element array.'
  if n_elements(coordinate_frame) EQ 0 then begin
    ;; cotter often leaves this header value out, so test if the array center is close to the
    ;; mwa_center. if it is we're in ITRF.
    if max(abs(array_center - mwa_center)) lt 10 then begin
      coordinate_frame = 'ITRF'
    endif else begin
      coordinate_frame = '????'
    endelse
  endif
  ;; gst0 is the Greenwich sidereal time at midnight on reference date
  if n_elements(gst0) EQ 0 then gst0=-1 ;; set to something that should be obvious it was a default
  ;; earth_degpd is the rotation rate of the earth in degrees per day on the reference date for the array
  if n_elements(earth_degpd) EQ 0 then earth_degpd=360.985 ;; this is approximately correct
  ;; ref_date is the reference date on which other parameters apply
  if n_elements(ref_date) EQ 0 then ref_date='-1' ;; set to something that should be obvious it was a default
  if n_elements(time_system) EQ 0 then time_system='UTC' ;; assume UTC
  if n_elements(dut1) EQ 0 then dut1=0 ;; google it. uvfits calls this UT1UTC
  if n_elements(diff_utc) EQ 0 then diff_utc=0 ;; difference between the time_system and UTC
  if n_elements(nleap_sec) EQ 0 then begin
    if time_system eq 'IAT' then begin
      nleap_sec=diff_utc
    endif else begin
      nleap_sec=-1 ;; set to something that should be obvious it was a default
    endelse
  endif
  
  ;; pol_type options are (from AIPS memo 117):
  ;; ’APPROX’: Linear approximation for circular feeds
  ;; ’X-Y LIN’: Linear approximation for linear feeds
  ;; ’ORI-ELP’: Orientation and ellipticity
  ;; ’VLBI’: VLBI solution form
  if n_elements(pol_type) EQ 0 then pol_type='X-Y LIN'
  ;; the antenna table may carry info about polarization characteristics of the feed if known.
  ;; n_pol_cal_params is the number of such parameters, 0 if no info, 2 if there is info.
  ;; if there is info, the info is in pola_cal_params & polb_cal_params.
  if n_elements(n_pol_cal_params) EQ 0 then n_pol_cal_params=0
  if n_elements(n_antenna) EQ 0 then n_antenna=128
  
  
  if n_elements(antenna_names) EQ 0 then begin
    antenna_names=string(indgen(n_antenna))
  endif else begin
    if n_elements(antenna_names) ne n_antenna then $
      message, 'length of antenna_names must match n_antenna'
  endelse
  if n_elements(antenna_numbers) EQ 0 then begin
    antenna_numbers=indgen(n_antenna)
  endif else begin
    if n_elements(antenna_numbers) ne n_antenna then $
      message, 'length of antenna_names must match n_antenna'
  endelse
  ;; antenna_coords are relative to the array center in the coordinate_frame
  if n_elements(antenna_coords) EQ 0 then begin
    antenna_coords=fltarr(3, n_antenna)
  endif else begin
    sz = size(antenna_coords)
    if sz[0] ne 2 or sz[1] ne 3 or sz[2] ne n_antenna then $
      message, 'antenna_coords must have shape (3, n_antenna)'
  endelse
  
  ;; mount_type codes (from AIPS memo 117): 0 for alt-azimuth, 1 for equatorial, 2 for orbiting, 3 for X-Y,
  ;; 4 for right-handed Naismith, and 5 for left-handed Naismith are defined. Aperture arrays, which are
  ;; steered electronically rather than mechanically, are assigned code 6.
  if n_elements(mount_type) EQ 0 then begin
    mount_type=0
  endif else begin
    if n_elements(mount_type) gt 1 then begin
      if n_elements(mount_type) ne n_antenna then $
        message, 'mount_type must be a single integer or an array the length of n_antenna'
      if min(mount_type) eq max(mount_type) then mount_type = mount_type[0]
    endif
  endelse
  ;; from AIPS memo 117:
  ;; The axis_offset is the position of the antenna phase reference point in the Yoke,
  ;; relative to the antenna pedestal reference point. This is an antenna characteristic
  ;; that should be unchanged when the antenna is moved to a new station. The X component
  ;; of the offset is horizontal along the elevation axis and has no effect on
  ;; interferometer phase. The Z component is vertical and approximately the nominal height
  ;; of the elevation axis above ground for the antenna’s mount. Small variations from the
  ;; nominal value have the same phase effect as the Z component of position, so they can
  ;; be ignored. The value of the STAXOF column gives the value of the Y component of the
  ;; axis offset. That component is horizontal and perpendicular to the elevation axis.
  ;; It produces an elevation-dependent interferometer phase term and, thus, has to be
  ;; accurately calibrated.
  if n_elements(axis_offset) EQ 0 then begin
    axis_offset=0
  endif else begin
    if n_elements(axis_offset) gt 1 then begin
      if n_elements(axis_offset) ne n_antenna then $
        message, 'axis_offset must be a single integer or an array the length of n_antenna'
      if min(axis_offset) eq max(axis_offset) then axis_offset = axis_offset[0]
    endif
  endelse
  
  ;; pola and polb are the feed polarization of feed A & B respectively. Options include:
  ;; A,B: 'X', 'Y'
  ;; A,B: 'R', 'L'
  ;; maybe others if feeds are not orthogonal (?)
  if n_elements(pola) EQ 0 then begin
    pola='X'
  endif else begin
    if n_elements(pola) gt 1 then begin
      if n_elements(pola) ne n_antenna then $
        message, 'pola must be a single string or an array the length of n_antenna'
      wh_diff = where(pola ne pola[0], count_diff)
      if count_diff eq 0 then pola = pola[0]
    endif
  endelse
  ;; pola/polb_orientation are the orientations given in degrees (unclear where this is measured from in AIPS memo 117)
  if n_elements(pola_orientation) EQ 0 then begin
    pola_orientation=0.
  endif else begin
    if n_elements(pola_orientation) gt 1 then begin
      if n_elements(pola_orientation) ne n_antenna then $
        message, 'pola_orientation must be a single integer or an array the length of n_antenna'
      if min(pola_orientation) eq max(pola_orientation) then pola_orientation = pola_orientation[0]
    endif
  endelse
  ;; pola/polb_cal_params are the polarization characteristics of the feed and should have shape (n_pol_cal_params, n_antenna)
  ;; if n_pol_cal_params eq 2, the meanings depend on pol_type. if the value of the POLTYPE keyword is ’APPROX’ or ’X-Y LIN’,
  ;; then the first parameter shall be the real part of the leakage term and the second shall be the imaginary part of the
  ;; leakage term. if the value of the POLTYPE keyword is ’OTI-ELP’, then the first parameter shall be the orientation and
  ;; the second shall be the ellipticity and both shall be given in radians.
  pola_cal_params = set_pol_cal_params(pola_cal_params, 'a', n_pol_cal_params, n_antenna)
  
  ;; same set of things for polb
  if n_elements(polb) EQ 0 then begin
    polb='Y'
  endif else begin
    if n_elements(polb) gt 1 then begin
      if n_elements(polb) ne n_antenna then $
        message, 'polb must be a single string or an array the length of n_antenna'
      wh_diff = where(polb ne polb[0], count_diff)
      if count_diff eq 0 then polb = polb[0]
    endif
  endelse
  if n_elements(polb_orientation) EQ 0 then begin
    polb_orientation=90.
  endif else begin
    if n_elements(polb_orientation) gt 1 then begin
      if n_elements(polb_orientation) ne n_antenna then $
        message, 'polb_orientation must be a single integer or an array the length of n_antenna'
      if min(polb_orientation) eq max(polb_orientation) then polb_orientation = polb_orientation[0]
    endif
  endelse
  polb_cal_params = set_pol_cal_params(polb_cal_params, 'b', n_pol_cal_params, n_antenna)


  layout={array_center: array_center, coordinate_frame: coordinate_frame, $
    gst0: gst0, earth_degpd: earth_degpd, ref_date: ref_date, time_system: time_system, $
    dut1: dut1, diff_utc: diff_utc, nleap_sec: nleap_sec, $
    pol_type: pol_type, n_pol_cal_params: n_pol_cal_params, n_antenna: n_antenna, $
    antenna_names: antenna_names, antenna_numbers: antenna_numbers, antenna_coords: antenna_coords, $
    mount_type: mount_type, axis_offset: axis_offset, pola: pola, pola_orientation: pola_orientation, $
    pola_cal_params: pola_cal_params, polb: polb, polb_orientation: polb_orientation, $
    polb_cal_params: polb_cal_params}

  if n_elements(diameters) then begin
    layout = create_struct(layout, 'diameters', diameters)
  endif
  if n_elements(beam_fwhm) then begin
    layout = create_struct(layout, 'beam_fwhm', beam_fwhm)
  endif

  return, layout
end
