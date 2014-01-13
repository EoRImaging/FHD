FUNCTION vis_cal_polyfit,cal,obs,degree=degree,phase_degree=phase_degree

IF N_Elements(degree) EQ 0 THEN degree=2 ELSE degree=Round(degree)>1
IF N_Elements(phase_degree) EQ 0 THEN phase_degree=degree-1.

n_pol=cal.n_pol
n_freq=cal.n_freq
n_tile=cal.n_tile
IF N_Elements(obs) GT 0 THEN freq_use=where((*obs.baseline_info).freq_use,nf_use) ELSE freq_use=lindgen(n_freq)
freq_arr=cal.freq
i_comp=Complex(0,1)
cal_return=cal
FOR pol_i=0,n_pol-1 DO cal_return.gain[pol_i]=Ptr_new(*cal.gain[pol_i]) ;essential, to keep original cal gains from being overwritten!

FOR pol_i=0,n_pol-1 DO BEGIN
    gain_arr=*cal_return.gain[pol_i]
    gain_amp=Abs(gain_arr)
    gain_phase=Atan(gain_arr,/phase)
    FOR tile_i=0L,n_tile-1 DO BEGIN
        gain=reform(gain_amp[freq_use,tile_i])
        fit_params=poly_fit(freq_use,gain,degree,yfit=gain_fit)
        
        IF phase_degree GT 0 THEN BEGIN
            phase_use=PhUnwrap(reform(gain_phase[freq_use,tile_i]))
            phase_params=poly_fit(freq_use,phase_use,phase_degree,yfit=phase_fit)
            gain_arr[freq_use,tile_i]=gain_fit*Exp(i_comp*phase_fit)
        ENDIF ELSE gain_arr[freq_use,tile_i]*=gain_fit*weight_invert(gain) ;this preserves the original phase
    ENDFOR
    *cal_return.gain[pol_i]=gain_arr
ENDFOR

RETURN,cal_return
END