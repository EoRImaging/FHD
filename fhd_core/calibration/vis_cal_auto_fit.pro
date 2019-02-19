FUNCTION vis_cal_auto_fit,obs,cal,psf,vis_auto=vis_auto,vis_model_auto=vis_model_auto,auto_tile_i=auto_tile_i,model_uv_arr=model_uv_arr,spectral_model_uv_arr=spectral_model_uv_arr

;print,"FITTING CALIBRATION SOLUTIONS USING AUTOCORRELATIONS!"
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

;IF Keyword_Set(n_spectral) THEN BEGIN
;    prefactor=Ptrarr(n_spectral)
;    FOR s_i=0,n_spectral-1 DO prefactor[s_i]=Ptr_new(deriv_coefficients(s_i+1,/divide_factorial))
;ENDIF
;sky_signal=Fltarr(n_pol,n_freq)
;FOR pol_i=0,n_pol-1 DO BEGIN
;
;    FOR freq_i=0L,n_freq-1 DO BEGIN
;        psf_use=Reform(*(*(*psf.beam_ptr)[pol_i,freq_bin_i[freq_i],0])[0],psf_dim,psf_dim)
;        sky_signal[pol_i,freq_i]=abs(Total((*model_uv_arr[pol_i])[dimension/2-psf_dim/2+1:dimension/2+psf_dim/2,elements/2-psf_dim/2+1:elements/2+psf_dim/2]*psf_use))
;        FOR s_i=0,n_spectral-1 DO BEGIN
;            prefactor_use=*prefactor[s_i]
;            FOR s_i_i=0,s_i DO sky_signal[pol_i,freq_i]+=prefactor_use[s_i_i]*(freq_delta[freq_i])^(s_i_i+1.)*$
;                abs(Total((*spectral_model_uv_arr[s_i_i])[dimension/2-psf_dim/2:dimension/2+psf_dim/2-1,elements/2-psf_dim/2:elements/2+psf_dim/2-1]*psf_use))
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
auto_scale=cal.auto_scale
auto_scale[0:n_pol-1]=Total(fit_slope,2)/n_tile_use 
auto_params=cal.auto_params
FOR pol_i=0,n_pol-1 DO BEGIN
    params=Fltarr(2,n_tile)
    params[0,*]=fit_offset[pol_i,*]
    params[1,*]=fit_slope[pol_i,*]
    auto_params[pol_i]=Ptr_new(params)
ENDFOR

cal.auto_scale=auto_scale
cal.auto_params=auto_params
cal_fit=cal
cal_fit.gain=fit_gain
RETURN,cal_fit
END
