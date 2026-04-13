FUNCTION general_beam_setup_init,obs,antenna_str,_Extra=extra

  ;copied from hera_beam_setup_init
  ;
  ;
  ;polarization 0: x, 1: y

  n_ant_pol=antenna_str.n_pol
  n_tiles=obs.n_tile
  n_dipoles=1
  nfreq_bin=antenna_str.nfreq_bin

  IF N_Elements(antenna_size) EQ 0 THEN antenna_size=10 ;meters A GUESS
  antenna_height=1.5 ;meters A GUESS
  velocity_factor=0.673 ;use MWA number (ignored anyway)

  xc_arr=Fltarr(n_dipoles)
  yc_arr=Fltarr(n_dipoles)
  zc_arr=Fltarr(n_dipoles)

  antenna_coords=Ptrarr(3)
  antenna_coords[0]=Ptr_new(xc_arr)
  antenna_coords[1]=Ptr_new(yc_arr)
  antenna_coords[2]=Ptr_new(zc_arr)

  delay_settings=Ptr_new(Fltarr(n_dipoles))

  freq_center=antenna_str.freq
  FOR i=0L,N_Elements(antenna_str.coupling)-1 DO antenna_str.coupling[i]=Ptr_new(Complex(Identity(n_dipoles)))

  base_gain=fltarr(n_dipoles)+1.
  gain_arr=Ptrarr(n_ant_pol)
  FOR pol_i=0,n_ant_pol-1 DO gain_arr[pol_i]=Ptr_new(Rebin(reform(base_gain,1,n_dipoles),nfreq_bin,n_dipoles,/sample))

  antenna_str.n_ant_elements=n_dipoles
  antenna_str.size_meters=antenna_size
  antenna_str.coords=antenna_coords
  antenna_str.height=antenna_height
  antenna_str.delays=delay_settings
  antenna=replicate(antenna_str,n_tiles)
  FOR t_i=0L,n_tiles-1 DO antenna[t_i].gain=Pointer_copy(gain_arr)

  RETURN, antenna
END