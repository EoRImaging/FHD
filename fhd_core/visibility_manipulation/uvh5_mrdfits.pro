FUNCTION uvh5_mrdfits, filename, data_header0, chan0=chan0, num_chans=num_chans, freq_ref=freq_ref

  fid = H5F_OPEN(filename)

  ;--------------------------------------------------
  ; Metadata
  ;--------------------------------------------------

  ant1 = H5D_READ(H5D_OPEN(fid,'/Header/ant_1_array'))
  ant2 = H5D_READ(H5D_OPEN(fid,'/Header/ant_2_array'))

  uvw_array = H5D_READ(H5D_OPEN(fid,'/Header/uvw_array'))

  time_array = H5D_READ(H5D_OPEN(fid,'/Header/time_array'))

  lst_array = H5D_READ(H5D_OPEN(fid,'/Header/lst_array'))

  integration_time = $
      H5D_READ(H5D_OPEN(fid,'/Header/integration_time'))

  freq_array = $
      H5D_READ(H5D_OPEN(fid,'/Header/freq_array'))

  pol_array = $
      H5D_READ(H5D_OPEN(fid,'/Header/polarization_array'))

  obsra = $
      H5D_READ(H5D_OPEN(fid,'/Header/phase_center_app_ra'))

  obsdec = $
      H5D_READ(H5D_OPEN(fid,'/Header/phase_center_app_dec'))

  lon = H5D_READ(H5D_OPEN(fid,'/Header/longitude'))
  lat = H5D_READ(H5D_OPEN(fid,'/Header/latitude'))
  alt = H5D_READ(H5D_OPEN(fid,'/Header/altitude'))

  nblts = LONG(H5D_READ(H5D_OPEN(fid,'/Header/Nblts')))
  nfreq_total = LONG(H5D_READ(H5D_OPEN(fid,'/Header/Nfreqs')))
  npol = LONG(H5D_READ(H5D_OPEN(fid,'/Header/Npols')))

  ;--------------------------------------------------
  ; Frequency selection
  ;--------------------------------------------------

  IF N_ELEMENTS(chan0) EQ 0 THEN chan0 = 0L

  IF N_ELEMENTS(num_chans) EQ 0 THEN $
      num_chans = nfreq_total

  IF chan0 LT 0 THEN chan0 = 0L

  IF chan0 + num_chans GT nfreq_total THEN $
      num_chans = nfreq_total - chan0

  freq_subset = freq_array[chan0:chan0+num_chans-1]
  freq_ref = mean(freq_array)

  ;--------------------------------------------------
  ; UVW arrays
  ;--------------------------------------------------

  c_m_s = 299792458D

  uu = DOUBLE(uvw_array[0,*]) / c_m_s
  vv = DOUBLE(uvw_array[1,*]) / c_m_s
  ww = DOUBLE(uvw_array[2,*]) / c_m_s

  ;--------------------------------------------------
  ; Parameter block for vis_param_extract
  ;--------------------------------------------------

  ptype_names = [ $
      'UU', $
      'VV', $
      'WW', $
      'DATE', $
      'DATE', $
      'BASELINE', $
      'ANTENNA1', $
      'ANTENNA2', $
      'INTTIM', $
      'LST' ]

  pcount = N_ELEMENTS(ptype_names)

  params = DBLARR(pcount,nblts)

  jd0 = FLOOR(MIN(time_array))

  baseline = LONG(ant1)*256L + LONG(ant2)

  params[0,*] = uu
  params[1,*] = vv
  params[2,*] = ww

  ; MRDFITS-style DATE handling:
  ; after PZERO4 is applied by vis_param_extract, DATE becomes jd0.
  params[3,*] = 0D
  params[4,*] = DOUBLE(time_array) - jd0

  params[5,*] = baseline

  params[6,*] = ant1
  params[7,*] = ant2

  params[8,*] = integration_time
  params[9,*] = DOUBLE(lst_array)

  ;--------------------------------------------------
  ; Synthetic UVFITS header
  ;--------------------------------------------------

  obsra_hdr  = DOUBLE(obsra[0])
  obsdec_hdr = DOUBLE(obsdec[0])
  lst_hdr    = DOUBLE(lst_array[0])
  lon_hdr    = DOUBLE(lon)
  lat_hdr    = DOUBLE(lat)

  IF MAX(ABS(lst_array)) LE 2D*!DPI THEN BEGIN
      params[9,*] = DOUBLE(lst_array) * !RADEG
      lst_hdr = DOUBLE(lst_array[0]) * !RADEG
  ENDIF

  IF lst_hdr LT 0D THEN lst_hdr = lst_hdr + 360D
  IF lst_hdr GE 360D THEN lst_hdr = lst_hdr MOD 360D

  obsname = FILE_BASENAME(filename)
  IF STRMATCH(STRLOWCASE(obsname), '*.uvh5') THEN $
      obsname = FILE_BASENAME(obsname, '.uvh5')
  IF STRMATCH(STRLOWCASE(obsname), '*.h5') THEN $
      obsname = FILE_BASENAME(obsname, '.h5')

  data_header0 = STRARR(256)

  SXADDPAR,data_header0,'NAXIS',7

  SXADDPAR,data_header0,'NAXIS2',3
  SXADDPAR,data_header0,'NAXIS3',npol
  SXADDPAR,data_header0,'NAXIS4',num_chans
  SXADDPAR,data_header0,'NAXIS5',1
  SXADDPAR,data_header0,'NAXIS6',1
  SXADDPAR,data_header0,'NAXIS7',1

  SXADDPAR,data_header0,'PCOUNT',pcount
  SXADDPAR,data_header0,'GCOUNT',nblts

  SXADDPAR,data_header0,'CTYPE3','STOKES'
  SXADDPAR,data_header0,'CTYPE4','FREQ'
  SXADDPAR,data_header0,'CTYPE5','IF'
  SXADDPAR,data_header0,'CTYPE6','RA'
  SXADDPAR,data_header0,'CTYPE7','DEC'

  SXADDPAR,data_header0,'CRPIX4',1D
  SXADDPAR,data_header0,'CRVAL4',DOUBLE(freq_subset[0])

  IF num_chans GT 1 THEN $
      freq_res = DOUBLE(freq_subset[1]-freq_subset[0]) $
  ELSE $
      freq_res = DOUBLE(H5D_READ(H5D_OPEN(fid,'/Header/channel_width')))

  SXADDPAR,data_header0,'CDELT4',freq_res

  SXADDPAR,data_header0,'CRVAL6',obsra_hdr
  SXADDPAR,data_header0,'CRVAL7',obsdec_hdr

  SXADDPAR,data_header0,'OBSRA',obsra_hdr
  SXADDPAR,data_header0,'OBSDEC',obsdec_hdr
  SXADDPAR,data_header0,'ZENRA',lst_hdr
  SXADDPAR,data_header0,'ZENDEC',lat_hdr
  SXADDPAR,data_header0,'OBSNAME',obsname
  SXADDPAR,data_header0,'OBJECT',obsname
  SXADDPAR,data_header0,'DATE-OBS',obsname

  SXADDPAR,data_header0,'LON',lon_hdr
  SXADDPAR,data_header0,'LAT',lat_hdr
  SXADDPAR,data_header0,'ALT',DOUBLE(alt)

  FOR i=0,pcount-1 DO BEGIN

      idx = STRTRIM(i+1,2)

      SXADDPAR,data_header0,'PTYPE'+idx,ptype_names[i]
      SXADDPAR,data_header0,'PSCAL'+idx,1D
      SXADDPAR,data_header0,'PZERO'+idx,0D

  ENDFOR

  SXADDPAR,data_header0,'PZERO4',DOUBLE(jd0)
  SXADDPAR,data_header0,'PZERO5',0D

  ;--------------------------------------------------
  ; Read visibilities
  ;--------------------------------------------------

  did = H5D_OPEN(fid,'/Data/visdata')
  fspace = H5D_GET_SPACE(did)

  dims = H5S_GET_SIMPLE_EXTENT_DIMS(fspace)

  offset = ULONG64([0,chan0,0])
  count  = ULONG64([dims[0],num_chans,dims[2]])

  H5S_SELECT_HYPERSLAB,fspace,offset,count,/reset

  mspace = H5S_CREATE_SIMPLE(count)
  dtype = H5D_GET_TYPE(did)

  visdata = H5D_READ(did, dtype, $
      FILE_SPACE=fspace, MEMORY_SPACE=mspace)

  H5S_CLOSE,fspace
  H5D_CLOSE,did
  H5T_CLOSE,dtype
  H5S_CLOSE,mspace

  ;--------------------------------------------------
  ; Read weights
  ;--------------------------------------------------

  did = H5D_OPEN(fid,'/Data/nsamples')
  fspace = H5D_GET_SPACE(did)

  dims = H5S_GET_SIMPLE_EXTENT_DIMS(fspace)

  offset = ULONG64([0,chan0,0])
  count  = ULONG64([dims[0],num_chans,dims[2]])

  H5S_SELECT_HYPERSLAB,fspace,offset,count,/reset

  mspace = H5S_CREATE_SIMPLE(count)

  weights = H5D_READ(did, $
      FILE_SPACE=fspace, MEMORY_SPACE=mspace)

  H5S_CLOSE,fspace
  H5D_CLOSE,did
  H5S_CLOSE,mspace

  ;--------------------------------------------------
  ; Convert to UVFITS layout
  ;
  ; array[3,npol,num_chans,nblts]
  ;--------------------------------------------------

  array = FLTARR(3,npol,num_chans,nblts)

  array[0,*,*,*] = visdata.r
  array[1,*,*,*] = visdata.i
  array[2,*,*,*] = weights

  H5F_CLOSE,fid

  RETURN, { $
      params:params, $
      array:array $
  }

END