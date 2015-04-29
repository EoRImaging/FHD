FUNCTION vis_cal_auto_fit,obs,cal,vis_arr=vis_arr,vis_model_arr=vis_model_arr

print,"FITTING CALIBRATION SOLUTIONS USING AUTOCORRELATIONS!"
dimension=obs.dimension
elements=obs.elements
n_pol=cal.n_pol ;do not use the cross-polarizations if they are present!
n_freq=obs.n_freq
n_tile=obs.n_tile
i_comp=Complex(0,1)

frequency_array=(*obs.baseline_info).freq
freq_delta=(frequency_array-obs.freq_center)/obs.freq_center
auto_corr=vis_extract_autocorr(obs,vis_arr = vis_arr,/time_average,auto_tile_i=auto_tile_i)
auto_corr_model=vis_extract_autocorr(obs,vis_arr = vis_model_arr,/time_average,auto_tile_i=auto_tile_i)
n_tile_use=N_Elements(auto_tile_i)

freq_i_use=where((*obs.baseline_info).freq_use)

auto_gain=Ptrarr(n_pol)
FOR pol_i=0,n_pol-1 DO BEGIN
    gain_arr=Complexarr(n_freq,n_tile)+1.
    FOR freq_i=0L,n_freq-1 DO BEGIN
        FOR tile_i=0,n_tile_use-1 DO BEGIN
            gain_single=Sqrt((*auto_corr[pol_i])[freq_i,tile_i]*weight_invert((*auto_corr_model[pol_i])[freq_i,tile_i]))
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
        phase_cross_single=Atan((*gain_cross[pol_i])[freq_i_use,tile_i],/phase)
        gain_auto_single=Abs((*auto_gain[pol_i])[freq_i_use,tile_i])
        gain_cross_single=Abs((*gain_cross[pol_i])[freq_i_use,tile_i])
        fit_single=linfit(gain_auto_single,gain_cross_single,yfit=gain_fit_single)
        (*fit_gain[pol_i])[freq_i_use,tile_i]=gain_fit_single*Exp(i_comp*phase_cross_single)
        fit_slope[pol_i,tile_i]=fit_single[1]
        fit_offset[pol_i,tile_i]=fit_single[0]
    ENDFOR
ENDFOR

cal_fit=cal
cal_fit.gain=fit_gain
RETURN,cal_fit
END