FUNCTION vis_cal_auto_init,obs,psf,cal,vis_arr=vis_arr,vis_model_arr=vis_model_arr

dimension=obs.dimension
elements=obs.elements
n_pol=cal.n_pol ;do not use the cross-polarizations if they are present!
n_freq=obs.n_freq
n_tile=obs.n_tile
n_spectral=obs.degrid_spectral_terms
frequency_array=(*obs.baseline_info).freq
freq_delta=(frequency_array-obs.freq_center)/obs.freq_center
vis_auto=vis_extract_autocorr(obs,vis_arr = vis_arr,/time_average,auto_tile_i=auto_tile_i)
vis_model_auto=vis_extract_autocorr(obs,vis_arr = vis_model_arr,/time_average,auto_tile_i=auto_tile_i)
n_tile_use=N_Elements(auto_tile_i)
freq_bin_i=(*obs.baseline_info).fbin_i
nfreq_bin=Max(freq_bin_i)+1
freq_i_use=where((*obs.baseline_info).freq_use)
tile_i_use=where((*obs.baseline_info).tile_use)

auto_scale=Fltarr(n_pol)
FOR pol_i=0,n_pol-1 DO BEGIN
    resistant_mean,Abs((*vis_arr[pol_i])[freq_i_use,*]),2,res_mean_data
    resistant_mean,Abs((*vis_model_arr[pol_i])[freq_i_use,*]),2,res_mean_model
    auto_scale[pol_i]=Sqrt(res_mean_data/res_mean_model)
ENDFOR

auto_gain=Ptrarr(n_pol)
FOR pol_i=0,n_pol-1 DO BEGIN
    gain_arr=Complexarr(n_freq,n_tile)+1.
    FOR freq_i=0L,n_freq-1 DO BEGIN
        FOR tile_i=0,n_tile_use-1 DO BEGIN
            gain_single=Sqrt((*vis_auto[pol_i])[freq_i,tile_i]*weight_invert((*vis_model_auto[pol_i])[freq_i,tile_i]))
            gain_arr[freq_i,auto_tile_i[tile_i]]=gain_single
        ENDFOR
    ENDFOR
    gain_arr*=auto_scale[pol_i]*weight_invert(Mean(gain_arr))
    IF nan_test(gain_arr) THEN gain_arr[where(Finite(gain_arr,/nan))]=1.
    zero_i=where(gain_arr LE 0,n_zero)
    IF n_zero GT 0 THEN gain_arr[zero_i]=1.
    auto_gain[pol_i]=Ptr_new(gain_arr)
ENDFOR

cal_init=cal
cal_init.gain=auto_gain
RETURN,cal_init
END
