FUNCTION vis_cal_bandpass,cal,obs,cal_remainder=cal_remainder

gain_arr_ptr=cal.gain
n_pol=cal.n_pol
n_freq=cal.n_freq
n_tile=cal.n_tile
IF N_Elements(obs) GT 0 THEN freq_use=where((*obs.baseline_info).freq_use) ELSE freq_use=lindgen(n_freq)
nf_use=N_Elements(freq_use)
freq_arr=cal.freq
IF N_Elements(obs) GT 0 THEN tile_use=where((*obs.baseline_info).tile_use) ELSE tile_use=lindgen(n_tile)
nt_use=N_Elements(tile_use)

n_pol=N_Elements(gain_arr_ptr)

gain_arr_ptr2=Ptrarr(n_pol,/allocate)
gain_arr_ptr3=Ptrarr(n_pol,/allocate)
FOR pol_i=0,n_pol-1 DO BEGIN
    gain=*gain_arr_ptr[pol_i] ;n_freq x n_tile element complex array
    gain_use=extract_subarray(gain,freq_use,tile_use)
    amp=Abs(gain_use)
    phase=Atan(gain_use,/phase)
    amp2=fltarr(nf_use,nt_use)
    FOR tile_i=0,nt_use-1 DO amp2[*,tile_i]=(Median(amp[*,tile_i]) EQ 0) ? 0:(amp[*,tile_i]/Median(amp[*,tile_i]))
    bandpass_single=Median(amp2,dimension=2)
    gain2=Complexarr(size(gain,/dimension))
    FOR tile_i=0,n_tile-1 DO gain2[freq_use,tile_i]=bandpass_single
    *gain_arr_ptr2[pol_i]=gain2
    gain3=gain
    FOR tile_i=0,n_tile-1 DO gain3[freq_use,tile_i]/=bandpass_single
    *gain_arr_ptr3[pol_i]=gain3
ENDFOR
cal_bandpass=cal
cal_bandpass.gain=gain_arr_ptr2
cal_remainder=cal
cal_remainder.gain=gain_arr_ptr3

RETURN,cal_bandpass
END