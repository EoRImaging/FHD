PRO gain_cal_auto_remultiply,obs,cal,auto_gain,auto_tile_i=auto_tile_i

n_pol=cal.n_pol
n_freq=obs.n_freq
n_tile=obs.n_tile
i_comp=Complex(0,1)

n_tile_use=N_Elements(auto_tile_i)

gain_cross=cal.gain
FOR pol_i=0,n_pol-1 DO BEGIN
    FOR tile_i_i=0L,n_tile_use-1 DO BEGIN
        tile_i=auto_tile_i[tile_i_i]
        gain_tile_i = (*gain_cross[pol_i])[*,tile_i]
        gain_auto_single=Abs((*auto_gain[pol_i])[*,tile_i])
        FOR freq_i=0L,n_freq-1 DO BEGIN
            gain_tile_i[freq_i] *=  gain_auto_single[freq_i]
        ENDFOR
        (*cal.gain[pol_i])[*,tile_i] = gain_tile_i
    ENDFOR
ENDFOR
END
