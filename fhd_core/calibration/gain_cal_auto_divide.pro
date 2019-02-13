FUNCTION gain_cal_auto_divide,obs,cal,vis_auto=vis_auto,auto_tile_i=auto_tile_i

n_pol=cal.n_pol
n_freq=obs.n_freq
n_tile=obs.n_tile
i_comp=Complex(0,1)

n_tile_use=N_Elements(auto_tile_i)

auto_gain=Ptrarr(n_pol)
FOR pol_i=0,n_pol-1 DO BEGIN
    gain_arr=Complexarr(n_freq,n_tile)+1.
    v0 = (*vis_auto[pol_i])[*,cal.ref_antenna]
    FOR freq_i=0L,n_freq-1 DO BEGIN
        FOR tile_i=0,n_tile_use-1 DO BEGIN
            gain_single=Sqrt((*vis_auto[pol_i])[freq_i,tile_i]*weight_invert(v0[freq_i]))
            gain_arr[freq_i,auto_tile_i[tile_i]]=gain_single
        ENDFOR
    ENDFOR
    auto_gain[pol_i]=Ptr_new(gain_arr)
ENDFOR

gain_cross=cal.gain
FOR pol_i=0,n_pol-1 DO BEGIN
    FOR tile_i_i=0L,n_tile_use-1 DO BEGIN
        tile_i=auto_tile_i[tile_i_i]
        gain_tile_i = (*gain_cross[pol_i])[*,tile_i]
        gain_auto_single=Abs((*auto_gain[pol_i])[*,tile_i])
        FOR freq_i=0L,n_freq-1 DO BEGIN
            gain_tile_i[freq_i] /=  gain_auto_single[freq_i]
        ENDFOR
        (*cal.gain[pol_i])[*,tile_i] = gain_tile_i
    ENDFOR
ENDFOR
RETURN,auto_gain
END
