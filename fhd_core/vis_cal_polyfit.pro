FUNCTION vis_cal_polyfit,cal,obs,degree=degree,phase_degree=phase_degree,$
    cal_step_fit=cal_step_fit,cal_neighbor_freq_flag=cal_neighbor_freq_flag,$
    cal_cable_reflection_fit=cal_cable_reflection_fit,cal_cable_reflection_correct=cal_cable_reflection_correct,_Extra=extra

IF N_Elements(degree) EQ 0 THEN degree=2 ELSE degree=Round(degree)>1
IF N_Elements(phase_degree) EQ 0 THEN phase_degree=degree-1.
IF Keyword_Set(cal_cable_reflection_fit) THEN cal.mode_fit=1.
cal_mode_fit=cal.mode_fit

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

c_light=299792458.
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
        cal.amp_params[pol_i,tile_i]=Ptr_new(fit_params)
        gain_fit=fltarr(n_freq)
        FOR di=0L,degree DO gain_fit+=fit_params[di]*findgen(n_freq)^di
        
        gain_residual[pol_i,tile_i]=Ptr_new(Reform(gain_amp[*,tile_i])-gain_fit)
        
        IF Keyword_Set(cal_step_fit) THEN FOR si=0L,n_step-1 DO gain_fit[freq_use[step_i[si]]:*,*]+=jump_test[step_i[si]]
        IF phase_degree GT 0 THEN BEGIN
            phase_use=PhUnwrap(reform(gain_phase[freq_use,tile_i]))
            phase_params=poly_fit(freq_use,phase_use,phase_degree,yfit=phase_fit)
            cal.phase_params[pol_i,tile_i]=Ptr_new(phase_params)
            phase_fit=fltarr(n_freq)
            FOR di=0L,phase_degree DO phase_fit+=phase_params[di]*findgen(n_freq)^di
            gain_arr[*,tile_i]=gain_fit*Exp(i_comp*phase_fit)
        ENDIF ELSE gain_arr[*,tile_i]*=gain_fit*weight_invert(gain_amp[*,tile_i]) ;this preserves the original phase
    ENDFOR
    *cal_return.gain[pol_i]=gain_arr
ENDFOR

IF Keyword_Set(cal_mode_fit) THEN BEGIN
    CASE 1 OF
        Keyword_Set(cal_cable_reflection_fit): BEGIN
            cable_filepath=filepath(obs.instrument+'_cable_length.txt',root=rootdir('FHD'),subdir='instrument_config')
            textfast,data_array,/read,file_path=cable_filepath,first_line=1
            tile_i_file=Reform(data_array[0,*])
            tile_name_file=Reform(data_array[1,*])
            cable_len=Reform(data_array[2,*])
            cable_vf=Reform(data_array[3,*])
            tile_ref_flag=0>Reform(data_array[4,*])<1
            IF cal_cable_reflection_fit GT 1 THEN BEGIN
                cable_cut_i=where(cable_len NE cal_cable_reflection_fit,n_cable_cut)
                IF n_cable_cut GT 0 THEN tile_ref_flag[cable_cut_i]=0
            ENDIF ELSE IF cal_cable_reflection_fit LT -1 THEN BEGIN
                cable_cut_i=where(cable_len EQ Abs(cal_cable_reflection_fit),n_cable_cut)
                IF n_cable_cut GT 0 THEN tile_ref_flag[cable_cut_i]=0
            ENDIF
            
            reflect_time=2.*cable_len/(c_light*cable_vf)
            bandwidth=(Max(freq_arr)-Min(freq_arr))*n_freq/(n_freq-1)
            mode_i_arr=Fltarr(n_pol,n_tile)
            FOR pol_i=0,n_pol-1 DO mode_i_arr[pol_i,*]=bandwidth*reflect_time*tile_ref_flag
        END
        Keyword_Set(cal_cable_reflection_correct): BEGIN
            IF size(cal_cable_reflection_correct,/type) EQ 7 THEN mode_filepath=cal_cable_reflection_correct ELSE $
                mode_filepath=filepath(obs.instrument+'_cable_reflection_coefficients.txt',root=rootdir('FHD'),subdir='instrument_config')
            textfast,data_array,/read,file_path=mode_filepath,first_line=1
            tile_i_file=Reform(data_array[0,*])
            tile_name_file=Reform(data_array[1,*])
            cable_len=Reform(data_array[2,*])
            cable_vf=Reform(data_array[3,*])
            tile_ref_flag=0>Reform(data_array[4,*])<1
            tile_mode_X=Reform(data_array[5,*])
            tile_amp_X=Reform(data_array[6,*])
            tile_phase_X=Reform(data_array[7,*])
            tile_mode_Y=Reform(data_array[8,*])
            tile_amp_Y=Reform(data_array[9,*])
            tile_phase_Y=Reform(data_array[10,*])
            IF size(cal_cable_reflection_correct,/type) NE 7 THEN BEGIN
                IF cal_cable_reflection_correct GT 1 THEN BEGIN
                    cable_cut_i=where(cable_len NE cal_cable_reflection_correct,n_cable_cut)
                    IF n_cable_cut GT 0 THEN tile_ref_flag[cable_cut_i]=0
                ENDIF ELSE IF cal_cable_reflection_correct LT -1 THEN BEGIN
                    cable_cut_i=where(cable_len EQ Abs(cal_cable_reflection_correct),n_cable_cut)
                    IF n_cable_cut GT 0 THEN tile_ref_flag[cable_cut_i]=0
                ENDIF
            ENDIF
            reflect_time=2.*cable_len/(c_light*cable_vf)
            bandwidth=(Max(freq_arr)-Min(freq_arr))*n_freq/(n_freq-1)
            mode_i_arr=Fltarr(n_pol,n_tile)
            FOR pol_i=0,n_pol-1 DO mode_i_arr[pol_i,*]=bandwidth*reflect_time*tile_ref_flag
            
            amp_arr=Fltarr(2,n_tile)
            phase_arr=Fltarr(2,n_tile)
            amp_arr[0,*]=tile_amp_X
            amp_arr[1,*]=tile_amp_Y
            phase_arr[0,*]=tile_phase_X
            phase_arr[1,*]=tile_phase_Y
        END
        (cal_mode_fit EQ -1): BEGIN
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
            mode_i_arr=Fltarr(n_pol,n_tile)+mode_i
        END
        ELSE: mode_i_arr=Fltarr(n_pol,n_tile)+cal_mode_fit
    ENDCASE
    
    FOR pol_i=0,n_pol-1 DO BEGIN
        gain_arr=*cal.gain[pol_i]
;        gain_amp=Abs(gain_arr)
;        gain_phase=Atan(gain_arr,/phase)
        gain_arr_fit=*cal_return.gain[pol_i]
        FOR ti=0L,nt_use-1 DO BEGIN
            tile_i=tile_use[ti]
            mode_i=mode_i_arr[pol_i,tile_i]
            IF mode_i EQ 0 THEN CONTINUE
            IF Keyword_Set(amp_arr) OR Keyword_Set(phase_arr) THEN BEGIN
                amp_use=amp_arr[pol_i,tile_i]
                phase_use=phase_arr[pol_i,tile_i]
            ENDIF ELSE BEGIN
                mode_fit=Total(exp(i_comp*2.*!Pi/n_freq*(mode_i)*freq_use)*Reform(gain_arr[freq_use,tile_i]))
                amp_use=abs(mode_fit)/nf_use ;why factor of 2? Check FFT normalization
                phase_use=atan(mode_fit,/phase)
            ENDELSE
            gain_mode_fit=amp_use*exp(-i_comp*2.*!Pi*(mode_i*findgen(n_freq)/n_freq)+i_comp*phase_use)
            gain_arr_fit[*,tile_i]+=gain_mode_fit
            cal_return.mode_params[pol_i,tile_i]=Ptr_new([mode_i,amp_use,phase_use])
            debug=1
        ENDFOR
        *cal_return.gain[pol_i]=gain_arr_fit
    ENDFOR
ENDIF
undefine_fhd,gain_residual
RETURN,cal_return
END