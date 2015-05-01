FUNCTION vis_cal_auto_fit,obs,cal,vis_auto=vis_auto,vis_model_auto=vis_model_auto,auto_tile_i=auto_tile_i

print,"FITTING CALIBRATION SOLUTIONS USING AUTOCORRELATIONS!"
dimension=obs.dimension
elements=obs.elements
n_pol=cal.n_pol ;do not use the cross-polarizations if they are present!
n_freq=obs.n_freq
n_tile=obs.n_tile
i_comp=Complex(0,1)

frequency_array=(*obs.baseline_info).freq
freq_delta=(frequency_array-obs.freq_center)/obs.freq_center
n_tile_use=N_Elements(auto_tile_i)

freq_i_use=where((*obs.baseline_info).freq_use)
freq_i_flag=where((*obs.baseline_info).freq_use EQ 0, n_freq_flag)
IF n_freq_flag GT 0 THEN BEGIN
    freq_flag=intarr(n_freq)
    freq_flag[freq_i_use]=1
    FOR freq_i=0,n_freq_flag-1 DO freq_flag[(freq_i_flag[freq_i]-1)>0:(freq_i_flag[freq_i]+1)<(n_freq-1)]=0
    freq_i_use2=where(freq_flag)
ENDIF ELSE freq_i_use2=freq_i_use

auto_gain=Ptrarr(n_pol)
FOR pol_i=0,n_pol-1 DO BEGIN
    gain_arr=Complexarr(n_freq,n_tile)+1.
    FOR freq_i=0L,n_freq-1 DO BEGIN
        FOR tile_i=0,n_tile_use-1 DO BEGIN
            gain_single=Sqrt((*vis_auto[pol_i])[freq_i,tile_i]*weight_invert((*vis_model_auto[pol_i])[freq_i,tile_i]))
            gain_arr[freq_i,auto_tile_i[tile_i]]=gain_single
        ENDFOR
    ENDFOR
    auto_gain[pol_i]=Ptr_new(gain_arr)
ENDFOR

gain_cross=cal.gain
fit_slope=Fltarr(n_pol,n_tile)
fit_offset=Fltarr(n_pol,n_tile)

fit_gain=Pointer_copy(gain_cross)
FOR pol_i=0,n_pol-1 DO BEGIN
    FOR tile_i_i=0L,n_tile_use-1 DO BEGIN
        tile_i=auto_tile_i[tile_i_i]
        phase_cross_single=Atan((*gain_cross[pol_i])[*,tile_i],/phase)
        gain_auto_single=Abs((*auto_gain[pol_i])[*,tile_i])
        gain_cross_single=Abs((*gain_cross[pol_i])[*,tile_i])
        fit_single=linfit(gain_auto_single[freq_i_use2],gain_cross_single[freq_i_use2])
        (*fit_gain[pol_i])[*,tile_i]=(gain_auto_single*fit_single[1]+fit_single[0])*Exp(i_comp*phase_cross_single)
        fit_slope[pol_i,tile_i]=fit_single[1]
        fit_offset[pol_i,tile_i]=fit_single[0]
    ENDFOR
ENDFOR

cal_fit=cal
cal_fit.gain=fit_gain
RETURN,cal_fit
END