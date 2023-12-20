FUNCTION import_az_za_beam, obs, antenna, file_path_J_matrix,$
  za_arr=za_arr,az_arr=az_arr,psf_image_dim=psf_image_dim
  ; Read in an e-field antenna beam stored in evenly-spaced azimuth-zenith angle (and frequency)
  print, "!!!!!!!! Running import_az_za_beam !!!!!!!!"
  
  n_ant_pol=Max(antenna.n_pol)
  nfreq_bin=Max(antenna.nfreq_bin)
  n_tile=obs.n_tile
  
  freq_center=antenna[0].freq ;all need to be identical, so just use the first
  
  fits_info,file_path_J_matrix,/silent,n_ext=n_ext
  n_ext+=1 ;n_ext starts counting AFTER the 0th extension, which it considers to be the main data unit, but we use that one too
  freq_arr_Jmat=Fltarr(n_ext)
  
  ; Read beam data from fits file
  ; Format is set by pyuvdata conventions, see https://github.com/RadioAstronomySoftwareGroup/pyuvdata for details
  Jmat0=mrdfits(file_path_J_matrix,0,header,status=status,/silent)
  n_za_ang = sxpar(header,'naxis2')
  n_az_ang = sxpar(header,'naxis1')
  n_freq = sxpar(header, 'naxis3')
  n_feed = sxpar(header, 'naxis4')
  n_basis = sxpar(header, 'naxis6')
  n_ang = n_za_ang*n_az_ang
  
  freq_arr_Jmat = findgen(sxpar(header,'naxis3'))*sxpar(header,'cdelt3') + sxpar(header, 'crval3') ; in Hz
  
  ; Make Jmat Array, combining real and imaginary parts into same axis
  Jmat_arr=Dcomplexarr(n_freq,n_basis,n_feed,n_ang)
  FOR freq_i=0,n_freq-1 DO BEGIN
    FOR f_i=0,n_feed-1 DO BEGIN
      FOR b_i=0,n_basis-1 DO BEGIN
        Jmat1 = Reform(Jmat0[*, *, freq_i, f_i, 0, b_i, 0] + Complex(0,1)*Jmat0[*, *, freq_i, f_i, 0, b_i, 1])
        Jmat1 = Transpose(Jmat1)
        Jmat_arr[freq_i,b_i,f_i,*] = Reform(Jmat1, n_ang)
        if b_i eq 1 then Jmat_arr[freq_i,b_i,f_i,*] *= -1  ;flip to align with MWA beam conventions
      ENDFOR
    ENDFOR
  ENDFOR
    
  ; Interpolate in frequency
  Jmat_interp=Ptrarr(n_ant_pol,n_ant_pol,nfreq_bin)
  FOR p_i=0,n_ant_pol-1 DO FOR p_j=0,n_ant_pol-1 DO FOR freq_i=0L,nfreq_bin-1 DO Jmat_interp[p_i,p_j,freq_i]=Ptr_new(Dcomplexarr(n_ang))
  FOR p_i=0,n_ant_pol-1 DO FOR p_j=0,n_ant_pol-1 DO FOR a_i=0L,n_ang-1 DO BEGIN
    Jmat_single_ang=Interpol(Jmat_arr[*,p_i,p_j,a_i],freq_arr_Jmat,freq_center);*norm_factor
    FOR freq_i=0L,nfreq_bin-1 DO (*Jmat_interp[p_i,p_j,freq_i])[a_i]=Jmat_single_ang[freq_i]
  ENDFOR
  
  ; Find pixels within za_arr where za is greater than 90 in order to know how many pixels are within the horizon -
  ; This will determine the set of pixels we need to interpolate onto
  horizon_test=where(abs(za_arr) GE 90.,n_horizon_test,complement=pix_use,ncomplement=n_pix)
  
  ; Set up Jones matrix to receive interpolated values
  Jones_matrix=Ptrarr(n_ant_pol,n_ant_pol,nfreq_bin)
  FOR p_i=0,n_ant_pol-1 DO FOR p_j=0,n_ant_pol-1 DO FOR freq_i=0L,nfreq_bin-1 DO $
    Jones_matrix[p_i,p_j,freq_i]=Ptr_new(Dcomplexarr(n_pix))
    
  ; Get set of za and az pixels to use
  za_use = za_arr[pix_use]
  az_use = az_arr[pix_use]
  
  ; Interpolate in azimulth and zenith angle
  FOR p_i=0,n_ant_pol-1 DO FOR p_j=0,n_ant_pol-1 DO FOR freq_i=0L,nfreq_bin-1 DO BEGIN
    Jmat_use=Reform(*Jmat_interp[p_i,p_j,freq_i],181,360)
    ;Expand,Jmat_use,n_zen_ang,n_az_ang,Jmat_single
    (*Jones_matrix[p_i,p_j,freq_i])=Interpolate(Jmat_use,za_use,az_use,cubic=-0.5)
  ENDFOR
  
  RETURN, Jones_matrix
  END
  
  
  
  
  
  
  
  
  
  
  
  
  