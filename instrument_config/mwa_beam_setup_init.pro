FUNCTION mwa_beam_setup_init,obs,antenna_str,antenna_size=antenna_size,dead_dipole_list=dead_dipole_list,$
    dipole_mutual_coupling_factor=dipole_mutual_coupling_factor
;indices of gain_arr correspond to these antenna locations
;         N
;    0  1  2  3
;    
;    4  5  6  7  
;W                E
;    8  9  10 11   
;    
;    12 13 14 15 
;         S
;polarization 0: x, 1: y

n_ant_pol=antenna_str.n_pol
n_tiles=obs.n_tile
n_dipoles=16
nfreq_bin=antenna_str.nfreq_bin

IF tag_exist(obs,'delays') THEN delay_settings=obs.delays ;delays need to be generalized!
IF N_Elements(dipole_mutual_coupling_factor) EQ 0 THEN dipole_mutual_coupling_factor=1
IF N_Elements(antenna_size) EQ 0 THEN antenna_size=5. ;meters (MWA groundscreen size)
antenna_spacing=1.1 ;meters (Same as M&C SetDelays script) ; Was 1.071 before? Verified in Tingay et al 2013
antenna_length=29.125*2.54/100. ;meters (measured) (NOT USED)
antenna_height=0.29 ;meters (June 2014 e-mail from Brian Crosse) ; Was 0.35 before
velocity_factor=0.673
speed_light=299792458. ;speed of light, in meters/second
base_delay_unit=4.35E-10 ;435 picoseconds is base delay length unit [units in seconds]

xc_arr0=Reform((meshgrid(4,4,1))*antenna_spacing,16)
xc_arr=xc_arr0-Mean(xc_arr0) ;dipole east position (meters)
yc_arr0=Reform(Reverse(meshgrid(4,4,2),2)*antenna_spacing,16)
yc_arr=yc_arr0-Mean(yc_arr0) ;dipole north position (meters)
zc_arr=Fltarr(16)

antenna_coords=Ptrarr(3)
antenna_coords[0]=Ptr_new(xc_arr)
antenna_coords[1]=Ptr_new(yc_arr)
antenna_coords[2]=Ptr_new(zc_arr)

IF not Ptr_valid(delay_settings) THEN BEGIN
    D0_d=xc_arr0*sin((90.-obs.obsalt)*!DtoR)*Sin(obs.obsaz*!DtoR)+yc_arr0*Sin((90.-obs.obsalt)*!DtoR)*Cos(obs.obsaz*!DtoR) 
    D0_d/=speed_light*base_delay_unit
    delay_settings=Ptr_new(Round(D0_d)) ;round to nearest real delay setting
ENDIF
*delay_settings*=base_delay_unit

freq_center=antenna_str.freq
IF Keyword_Set(dipole_mutual_coupling_factor) THEN antenna_str.coupling=mwa_dipole_mutual_coupling(freq_center) $
    ELSE FOR i=0L,N_Elements(antenna_str.coupling)-1 DO antenna_str.coupling[i]=Ptr_new(Complex(Identity(n_dipoles)))

base_gain=fltarr(16)+1.
gain_arr=Ptrarr(n_ant_pol)
FOR pol_i=0,n_ant_pol-1 DO gain_arr[pol_i]=Ptr_new(Rebin(reform(base_gain,1,n_dipoles),nfreq_bin,n_dipoles,/sample))

antenna_str.n_ant_elements=n_dipoles
antenna_str.size_meters=antenna_size
antenna_str.coords=antenna_coords
antenna_str.height=antenna_height
antenna_str.delays=delay_settings
antenna=replicate(antenna_str,n_tiles)
FOR t_i=0L,n_tiles-1 DO antenna[t_i].gain=Pointer_copy(gain_arr)

RETURN,antenna
END