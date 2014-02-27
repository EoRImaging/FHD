FUNCTION vis_cal_polyfit,cal,obs,degree=degree,phase_degree=phase_degree,$
    cal_step_fit=cal_step_fit,cal_neighbor_freq_flag=cal_neighbor_freq_flag,cal_mode_fit=cal_mode_fit,_Extra=extra

IF N_Elements(degree) EQ 0 THEN degree=2 ELSE degree=Round(degree)>1
IF N_Elements(phase_degree) EQ 0 THEN phase_degree=degree-1.

n_pol=cal.n_pol
n_freq=cal.n_freq
n_tile=cal.n_tile
freq_arr=cal.freq
IF N_Elements(obs) GT 0 THEN freq_use=where((*obs.baseline_info).freq_use,nf_use) ELSE freq_use=lindgen(n_freq)
IF N_Elements(obs) GT 0 THEN tile_use=where((*obs.baseline_info).tile_use,nt_use) ELSE tile_use=lindgen(n_tile)
IF Keyword_Set(cal_neighbor_freq_flag) THEN BEGIN
    freq_use=(*obs.baseline_info).freq_use
    freq_flag=where(freq_use EQ 0,nf_flag)
    IF nf_flag GT 0 THEN BEGIN
        FOR fi=0L,nf_flag-1 DO freq_use[((freq_flag[fi]-cal_neighbor_freq_flag)>0):((freq_flag[fi]+cal_neighbor_freq_flag)<(n_freq-1))]=0
    ENDIF
    freq_use=where(freq_use,nf_use)
ENDIF


i_comp=Complex(0,1)
cal_return=cal
FOR pol_i=0,n_pol-1 DO cal_return.gain[pol_i]=Ptr_new(*cal.gain[pol_i]) ;essential, to keep original cal gains from being overwritten!

IF Keyword_Set(cal_step_fit) THEN BEGIN
    cal_bandpass=vis_cal_bandpass(cal_return,obs)
    bandpass_test=(*cal_bandpass.gain[0]+*cal_bandpass.gain[1])/2. ;average x and y together, since we're just looking for global steps
    bandpass_filtered=Median(bandpass_test[freq_use],5)
    jump_test=[0,bandpass_filtered[1:nf_use-1]-bandpass_filtered[0:nf_use-2]]
    sigma_thresh=5.
    sigma_test=stddev(jump_test)
    step_i=where(Abs(jump_test) GT sigma_thresh*sigma_test,n_step)
    FOR pol_i=0,n_pol-1 DO BEGIN
        gain_arr=*cal_return.gain[pol_i]
        gain_amp=Abs(gain_arr)
        gain_phase=Atan(gain_arr,/phase)
        FOR si=0L,n_step-1 DO gain_amp[freq_use[step_i[si]]:*,*]-=jump_test[step_i[si]]
        gain_arr=gain_amp*Exp(i_comp*gain_phase)
        *cal_return.gain[pol_i]=gain_arr
    ENDFOR
ENDIF

gain_residual=ptrarr(n_pol,n_tile)
FOR pol_i=0,n_pol-1 DO BEGIN
    gain_arr=*cal_return.gain[pol_i]
    gain_amp=Abs(gain_arr)
    gain_phase=Atan(gain_arr,/phase)
    FOR tile_i=0L,n_tile-1 DO BEGIN
        gain=reform(gain_amp[freq_use,tile_i])
        fit_params=poly_fit(freq_use,gain,degree)
        gain_fit=fltarr(n_freq)
        FOR di=0L,degree DO gain_fit+=fit_params[di]*findgen(n_freq)^di
        
        gain_residual[pol_i,tile_i]=Ptr_new(Reform(gain_amp[*,tile_i])-gain_fit)
        
        IF Keyword_Set(cal_step_fit) THEN FOR si=0L,n_step-1 DO gain_fit[freq_use[step_i[si]]:*,*]+=jump_test[step_i[si]]
        IF phase_degree GT 0 THEN BEGIN
            phase_use=PhUnwrap(reform(gain_phase[freq_use,tile_i]))
            phase_params=poly_fit(freq_use,phase_use,phase_degree,yfit=phase_fit)
            phase_fit=fltarr(n_freq)
            FOR di=0L,phase_degree DO phase_fit+=phase_params[di]*findgen(n_freq)^di
            gain_arr[*,tile_i]=gain_fit*Exp(i_comp*phase_fit)
        ENDIF ELSE gain_arr[*,tile_i]*=gain_fit*weight_invert(gain_amp[*,tile_i]) ;this preserves the original phase
    ENDFOR
    *cal_return.gain[pol_i]=gain_arr
ENDFOR

IF Keyword_Set(cal_mode_fit) THEN BEGIN
    IF cal_mode_fit EQ 1 THEN BEGIN
        spec_mask=fltarr(n_freq)
        spec_mask[freq_use]=1
        freq_cut=where(spec_mask EQ 0,n_mask)
        spec_psf=(Abs(FFT(spec_mask)))
        spec_inds=lindgen(n_freq/2)
        spec_psf=spec_psf[spec_inds]
        spectrum=fltarr(n_freq/2)
        FOR pol_i=0,n_pol-1 DO BEGIN
            FOR ti=0L,nt_use-1 DO BEGIN
                tile_i=tile_use[ti]
                spec0=Abs(FFT(*gain_residual[pol_i,tile_i]))
                spectrum+=spec0[spec_inds]
            ENDFOR
        ENDFOR
        mode_test=spectrum
        psf_mask=fltarr(n_freq/2)
        IF n_mask GT 0 THEN BEGIN
            psf_mask[where(spec_psf GT Max(spec_psf)/1E3)]=1
            psf_mask=smooth(psf_mask,5,/edge_truncate)
            mask_i=where(psf_mask,n_mask2)
            IF n_mask2 GT 0 THEN mode_test[mask_i]=0
        ENDIF
        mode_max=Max(mode_test,mode_i)
    ENDIF ELSE mode_i=cal_mode_fit
    
    FOR pol_i=0,n_pol-1 DO BEGIN
        FOR ti=0L,nt_use-1 DO BEGIN
            tile_i=tile_use[ti]
            spec0=FFT(*gain_residual[pol_i,tile_i])
            phase_use=Atan(spec0[mode_i],/phase)
            amp_use=Abs(spec0[mode_i])
            sin_fit=amp_use*Sin(Float(mode_i)*findgen(n_freq)/n_freq-phase_use)
            debug=1
        ENDFOR
    ENDFOR
    cal_mode_fit=mode_i
ENDIF
undefine_fhd,gain_residual
RETURN,cal_return
END