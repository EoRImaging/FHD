FUNCTION vis_cal_auto_init,obs,psf,cal,vis_arr=vis_arr,vis_model_arr=vis_model_arr

dimension=obs.dimension
elements=obs.elements
n_pol=cal.n_pol ;do not use the cross-polarizations if they are present!
n_freq=obs.n_freq
n_tile=obs.n_tile
psf_dim=psf.dim
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

;IF Keyword_Set(n_spectral) THEN BEGIN
;    prefactor=Ptrarr(n_spectral)
;    FOR s_i=0,n_spectral-1 DO prefactor[s_i]=Ptr_new(deriv_coefficients(s_i+1,/divide_factorial))
;ENDIF
;
;sky_signal=Fltarr(n_pol,n_freq)
;FOR pol_i=0,n_pol-1 DO BEGIN
;
;    FOR freq_i=0L,n_freq-1 DO BEGIN
;        psf_use=Reform(*(*(*psf.beam_ptr)[pol_i,freq_bin_i[freq_i],0])[0],psf_dim,psf_dim)
;;        psf_use*=Conj(psf_use)
;        sky_signal[pol_i,*]=real_part(Total((*model_uv_arr[pol_i])[dimension/2-psf_dim/2:dimension/2+psf_dim/2-1,elements/2-psf_dim/2:elements/2+psf_dim/2-1]*psf_use)*weight_invert(Max(abs(psf_use))))
;    ;    sky_signal[pol_i,*]=real_part(Total((*model_uv_arr[pol_i])[dimension/2,elements/2]))
;        FOR s_i=0,n_spectral-1 DO BEGIN
;            prefactor_use=*prefactor[s_i]
;            FOR s_i_i=0,s_i DO sky_signal[pol_i,freq_i]+=prefactor_use[s_i_i]*(freq_delta[freq_i])^(s_i_i+1.)*$
;                real_part(Total((*spectral_model_uv_arr[s_i_i])[dimension/2-psf_dim/2:dimension/2+psf_dim/2-1,elements/2-psf_dim/2:elements/2+psf_dim/2-1]*psf_use)*weight_invert(Max(abs(psf_use))))
;;            FOR s_i_i=0,s_i DO sky_signal[pol_i,freq_i]+=prefactor_use[s_i_i]*(freq_delta[freq_i])^(s_i_i+1.)*$
;;                real_part(Total((*spectral_model_uv_arr[s_i_i])[dimension/2,elements/2]))
;        ENDFOR
;    ENDFOR
;ENDFOR

auto_gain=Ptrarr(n_pol)
FOR pol_i=0,n_pol-1 DO BEGIN
    gain_arr=Complexarr(n_freq,n_tile)+1.
    FOR freq_i=0L,n_freq-1 DO BEGIN
        FOR tile_i=0,n_tile_use-1 DO BEGIN
            gain_single=Sqrt((*vis_auto[pol_i])[freq_i,tile_i]*weight_invert((*vis_model_auto[pol_i])[freq_i,tile_i]))
            gain_arr[freq_i,auto_tile_i[tile_i]]=gain_single
        ENDFOR
    ENDFOR
    gain_arr*=auto_scale[pol_i]/Mean(gain_arr)
    auto_gain[pol_i]=Ptr_new(gain_arr)
ENDFOR

;cal_cross=getvar_savefile('D:\MWA\DATA3\128T\testcal4\fhd_firstpass_new\calibration\1061316296_cal.sav','cal')
;gain_cross=cal_cross.gain
;fit_slope=Fltarr(n_pol,n_tile)
;fit_offset=Fltarr(n_pol,n_tile)
;FOR pol_i=0,n_pol-1 DO BEGIN
;    FOR tile_i=0L,n_tile-1 DO BEGIN
;        gain_auto_single=Abs((*auto_gain[pol_i])[freq_i_use,tile_i])
;        gain_cross_single=Abs((*gain_cross[pol_i])[freq_i_use,tile_i])
;        fit_single=linfit(gain_auto_single,gain_cross_single,yfit=gain_fit_single)
;        fit_slope[pol_i,tile_i]=fit_single[1]
;        fit_offset[pol_i,tile_i]=fit_single[0]
;    ENDFOR
;ENDFOR

cal_init=cal
cal_init.gain=auto_gain
RETURN,cal_init
END