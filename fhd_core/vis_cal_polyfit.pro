FUNCTION vis_cal_polyfit,cal,obs,degree=degree,remove_polyfit=remove_polyfit;,phase_degree=phase_degree

IF N_Elements(degree) EQ 0 THEN degree=2 ELSE degree=Round(degree)>1
n_pol=cal.n_pol
n_freq=cal.n_freq
n_tile=cal.n_tile
IF N_Elements(obs) GT 0 THEN freq_use=where((*obs.baseline_info).freq_use,nf_use) ELSE freq_use=lindgen(n_freq)
freq_arr=cal.freq

cal_return=cal

FOR pol_i=0,n_pol-1 DO BEGIN
    gain_arr=*cal.gain[pol_i]
    gain_amp=Abs(gain_arr)
    FOR tile_i=0L,n_tile-1 DO BEGIN
        gain=reform(gain_amp[freq_use,tile_i])
        fit_params=poly_fit(freq_use,gain,degree,yfit=gain_fit)
        IF Keyword_Set(remove_polyfit) THEN BEGIN
            gain_fit/=Mean(gain_fit)
            gain_arr[freq_use,tile_i]*=weight_invert(gain_fit) 
        ENDIF ELSE gain_arr[freq_use,tile_i]*=gain_fit*weight_invert(gain)
    ENDFOR
    *cal_return.gain[pol_i]=gain_arr
ENDFOR

RETURN,cal_return
END