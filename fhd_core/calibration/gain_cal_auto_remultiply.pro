PRO gain_cal_auto_remultiply,obs,cal,auto_gain,auto_tile_i=auto_tile_i

dimension=obs.dimension
elements=obs.elements
n_pol=cal.n_pol ;do not use the cross-polarizations if they are present!
n_freq=obs.n_freq
n_tile=obs.n_tile
i_comp=Complex(0,1)
n_spectral=obs.degrid_spectral_terms

frequency_array=(*obs.baseline_info).freq
freq_delta=(frequency_array-obs.freq_center)/obs.freq_center
n_tile_use=N_Elements(auto_tile_i)
freq_bin_i=(*obs.baseline_info).fbin_i
nfreq_bin=Max(freq_bin_i)+1

freq_i_use=where((*obs.baseline_info).freq_use)
freq_i_flag=where((*obs.baseline_info).freq_use EQ 0, n_freq_flag)
IF n_freq_flag GT 0 THEN BEGIN
    freq_flag=intarr(n_freq)
    freq_flag[freq_i_use]=1
    FOR freq_i=0,n_freq_flag-1 DO freq_flag[(freq_i_flag[freq_i]-1)>0:(freq_i_flag[freq_i]+1)<(n_freq-1)]=0
    freq_i_use2=where(freq_flag)
ENDIF ELSE freq_i_use2=freq_i_use

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
