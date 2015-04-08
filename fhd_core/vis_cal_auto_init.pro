FUNCTION vis_cal_auto_init,obs,psf,cal,vis_arr=vis_arr,model_uv_arr=model_uv_arr,$
    spectral_model_uv_arr=spectral_model_uv_arr,_Extra=extra

dimension=obs.dimension
elements=obs.elements
n_pol=cal.n_pol ;do not use the cross-polarizations if they are present!
n_freq=obs.n_freq
n_tile=obs.n_tile
psf_dim=psf.dim
n_spectral=obs.degrid_spectral_terms
frequency_array=(*obs.baseline_info).freq
freq_delta=(frequency_array-obs.freq_center)/obs.freq_center
auto_corr=vis_extract_autocorr(obs,vis_arr = vis_arr,/time_average)

IF Keyword_Set(n_spectral) THEN BEGIN
    prefactor=Ptrarr(n_spectral)
    FOR s_i=0,n_spectral-1 DO prefactor[s_i]=Ptr_new(deriv_coefficients(s_i+1,/divide_factorial))
ENDIF

sky_signal=Fltarr(n_pol,n_freq)
FOR pol_i=0,n_pol-1 DO BEGIN
    sky_signal[pol_i,*]=real_part(Total((*model_uv_arr[pol_i])[dimension/2-psf.dim/2:dimension/2+psf.dim/2-1,elements/2-psf.dim/2:elements/2+psf.dim/2-1]))

    FOR freq_i=0L,n_freq-1 DO BEGIN
        FOR s_i=0,n_spectral-1 DO BEGIN
            prefactor_use=*prefactor[s_i]
            FOR s_i_i=0,s_i DO sky_signal[pol_i,freq_i]+=prefactor_use[s_i_i]*(freq_delta[freq_i])^(s_i_i+1.)*$
                real_part(Total((*spectral_model_uv_arr[s_i_i])[dimension/2-psf.dim/2:dimension/2+psf.dim/2-1,elements/2-psf.dim/2:elements/2+psf.dim/2-1]))
        ENDFOR
    ENDFOR
ENDFOR

auto_gain=Ptrarr(n_pol)
FOR pol_i=0,n_pol-1 DO BEGIN
    gain_arr=Complexarr(n_freq,n_tile)+1.
    FOR freq_i=0L,n_freq-1 DO BEGIN
        
    ENDFOR
    auto_gain[pol_i]=Ptr_new(gain_arr)
ENDFOR

cal_init=cal
cal_init.gain=auto_gain
RETURN,cal_init
END