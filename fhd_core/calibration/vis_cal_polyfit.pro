FUNCTION vis_cal_polyfit,cal,obs,cal_step_fit=cal_step_fit,cal_neighbor_freq_flag=cal_neighbor_freq_flag,$
    cal_reflection_hyperresolve=cal_reflection_hyperresolve,cal_reflection_mode_theory=cal_reflection_mode_theory,$
    cal_reflection_mode_file=cal_reflection_mode_file,cal_reflection_mode_delay=cal_reflection_mode_delay,auto_ratio=auto_ratio,$
    no_phase_calibration=no_phase_calibration,digital_gain_jump_polyfit=digital_gain_jump_polyfit,_Extra=extra
    
  IF Keyword_Set(cal_reflection_mode_theory) OR Keyword_Set(cal_reflection_mode_file) $
    OR Keyword_Set(cal_reflection_mode_delay) OR Keyword_Set(cal_reflection_hyperresolve) THEN BEGIN
    IF not Keyword_Set(cal.mode_fit) THEN cal.mode_fit=1.
  ENDIF
  cal_mode_fit=cal.mode_fit
  IF Keyword_Set(cal_mode_fit) AND keyword_set(cal_reflection_mode_theory) then $
    if (abs(cal_reflection_mode_theory) GT 1) then cal_mode_fit = cal_reflection_mode_theory
  
  IF Tag_exist(cal,"amp_degree") THEN amp_degree = cal.amp_degree ELSE amp_degree=2
  IF Tag_exist(cal,"phase_degree") THEN phase_degree = cal.phase_degree ELSE phase_degree=1
  
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
      FOR fi=0L,nf_flag-1 DO BEGIN
        freq_use[((freq_flag[fi]-cal_neighbor_freq_flag)>0):((freq_flag[fi]+cal_neighbor_freq_flag)<(n_freq-1))]=0
      ENDFOR
    ENDIF
    freq_use=where(freq_use,nf_use)
  ENDIF
  
  c_light=299792458.
  i_comp=Complex(0,1)
  cal_return=cal
  cal_return.gain=Pointer_copy(cal.gain) ;essential, to keep original cal gains from being overwritten!
  
  ;Find any steps (i.e digital gain jumps) that are outliers beyond 5sigma, and remove them
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
  
  ;*******************Begin polynomial fitting over the frequency band
  gain_residual=ptrarr(n_pol,n_tile)
  FOR pol_i=0,n_pol-1 DO BEGIN
    gain_arr=*cal_return.gain[pol_i]
    gain_amp=Abs(gain_arr)
    gain_phase=Atan(gain_arr,/phase)
    FOR tile_i=0L,n_tile-1 DO BEGIN
      gain=reform(gain_amp[freq_use,tile_i])
      
      ;*****Fit for amplitude
      IF keyword_set(amp_degree) THEN BEGIN
        ;Only fit for amplitude if amp_degree is set and greater than zero
        fit_params=poly_fit(freq_use,gain,amp_degree)
        cal_return.amp_params[pol_i,tile_i]=Ptr_new(fit_params)
        gain_fit=fltarr(n_freq)
        FOR di=0L,amp_degree DO gain_fit+=fit_params[di]*findgen(n_freq)^di

        ;Fit pre- and post-digital gain jump separately in highband MWA data
        IF keyword_set(digital_gain_jump_polyfit) then begin
          gain_fit=fltarr(n_freq)
          pre_dig_inds = where((*obs.baseline_info).freq[freq_use] LT 187.515E6,n_count)
          if (obs.instrument NE 'mwa') OR (n_count LT 0) then begin
            print, 'digital_gain_jump_polyfit keyword only works with highband mwa data. Full band polyfit applied instead'
            continue
          endif
          f_d = max(pre_dig_inds)
          f_end = N_elements(freq_use)-1
          fit_params1=poly_fit(freq_use[0:f_d],gain[0:f_d],amp_degree-1)
          fit_params2=poly_fit(freq_use[f_d+1:f_end],gain[f_d+1:f_end],amp_degree-1)
          FOR di=0L,amp_degree-1 DO gain_fit[freq_use[0]:freq_use[f_d]] += fit_params1[di]*findgen(freq_use[f_d])^di
          FOR di=0L,amp_degree-1 DO gain_fit[freq_use[f_d+1]:freq_use[f_end]] += $
            fit_params2[di]*(findgen(freq_use[f_end] - freq_use[f_d+1]+1) + freq_use[f_d+1])^di
          fit_params = [fit_params1,fit_params2]
          cal_return.amp_params[pol_i,tile_i]=Ptr_new(fit_params)
        endif

      ENDIF ELSE gain_fit=fltarr(n_freq)+1.
      
      gain_residual[pol_i,tile_i]=Ptr_new(Reform(gain_amp[*,tile_i])-gain_fit)
      IF Keyword_Set(cal_step_fit) THEN FOR si=0L,n_step-1 DO gain_fit[freq_use[step_i[si]]:*,*]+=jump_test[step_i[si]]
      ;*****
      
      ;*****Fit for phase
      IF N_elements(phase_degree) GT 0 THEN BEGIN
        phase_use=PhUnwrap(reform(gain_phase[freq_use,tile_i]))
        phase_params=poly_fit(freq_use,phase_use,phase_degree,yfit=phase_fit)
        cal_return.phase_params[pol_i,tile_i]=Ptr_new(phase_params)
        phase_fit=fltarr(n_freq)
        FOR di=0L,phase_degree DO phase_fit+=phase_params[di]*findgen(n_freq)^di
        gain_arr[*,tile_i]=gain_fit*Exp(i_comp*phase_fit)
      ENDIF ELSE gain_arr[*,tile_i]*=gain_fit*weight_invert(gain_amp[*,tile_i]) ;this preserves the original phase
    ;*****
    ENDFOR
    *cal_return.gain[pol_i]=gain_arr
  ENDFOR
  ;*******************End polynomial fitting over the frequency band
  
  ;*******************Begin cable reflection fitting
  IF Keyword_Set(cal_mode_fit) THEN BEGIN
    logic_test = keyword_set(cal_reflection_mode_file) + keyword_set(cal_reflection_mode_theory) + keyword_set(cal_reflection_mode_delay)
    if logic_test GT 1 then begin
      print, 'More than one nominal mode-fitting procedure specified for calibration reflection fits. Using cal_reflection_mode_theory'
      cal_reflection_mode_file = 0
      cal_reflection_mode_delay = 0
      cal_reflection_mode_theory = 1
    endif
    if logic_test EQ 0 then begin
      print, 'No nominal mode-fitting procedure was specified for calibration reflection fits. Using cal_reflection_mode_delay'
      cal_reflection_mode_delay = 1
    endif
    
    CASE 1 OF
    
      Keyword_Set(cal_reflection_mode_file): BEGIN
        ; Use predetermined nominal reflection modes from a text file.
      
        IF size(cal_reflection_mode_file,/type) EQ 7 THEN mode_filepath=cal_reflection_mode_file ELSE $
          mode_filepath=filepath(obs.instrument+'_cable_reflection_coefficients.txt',root=rootdir('FHD'),subdir='instrument_config')
        ; read in text file and reorganize the data
        print, 'Using calibration reflection fits from ' + mode_filepath
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
        
        mode_i_arr=Fltarr(n_pol,n_tile) ; Modes in fourier transform units
        mode_i_arr[0,*]=tile_mode_X*tile_ref_flag
        mode_i_arr[1,*]=tile_mode_Y*tile_ref_flag
        
        amp_arr=Fltarr(2,n_tile)
        phase_arr=Fltarr(2,n_tile)
        amp_arr[0,*]=tile_amp_X
        amp_arr[1,*]=tile_amp_Y
        phase_arr[0,*]=tile_phase_X
        phase_arr[1,*]=tile_phase_Y
      END
      
      Keyword_Set(cal_reflection_mode_theory): BEGIN
        ; Calculate nominal theoretical modes for the reflections
        print, 'Using theory calculation in nominal reflection mode calibration'
        
        ; Get the nominal tile lengths and velocity factors:
        cable_filepath=filepath(obs.instrument+'_cable_length.txt',root=rootdir('FHD'),subdir='instrument_config')
        textfast,data_array,/read,file_path=cable_filepath,first_line=1
        tile_i_file=Reform(data_array[0,*])
        tile_name_file=Reform(data_array[1,*])
        cable_len=Reform(data_array[2,*])
        cable_vf=Reform(data_array[3,*])
        tile_ref_flag=0>Reform(data_array[4,*])<1
        
        reflect_time=2.*cable_len/(c_light*cable_vf) ; nominal reflect time
        bandwidth=(Max(freq_arr)-Min(freq_arr))*n_freq/(n_freq-1)
        mode_i_arr=Fltarr(n_pol,n_tile) ; Modes in fourier transform units (see fit below)
        FOR pol_i=0,n_pol-1 DO mode_i_arr[pol_i,*]=bandwidth*reflect_time*tile_ref_flag
      END
      
      keyword_set(cal_reflection_mode_delay): BEGIN
        ; Calculate nominal modes in calibration delay space
        print, 'Using calibration delay spectrum to calculate nominal reflection modes'
        
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
        
        ;Remove channels contaminated from frequency flagging
        IF n_mask GT 0 THEN BEGIN
          psf_mask[where(spec_psf GT Max(spec_psf)/1E3)]=1
          psf_mask=smooth(psf_mask,5,/edge_truncate)
          mask_i=where(psf_mask,n_mask2)
          IF n_mask2 GT 0 THEN mode_test[mask_i]=0
        ENDIF
        
        mode_max=Max(mode_test,mode_i)
        mode_i_arr=Fltarr(n_pol,n_tile)+mode_i
      END
    ENDCASE
    
    
    ;Option to fit only certain cable lengths, can specify more than one length
    ;Positive length indicates fit mode, negative length indicates exclude mode
    IF total(cal_mode_fit) NE 1 AND ~Keyword_Set(auto_ratio) THEN BEGIN
      tile_ref_logic = FLTARR(n_tile)
      if ~keyword_set(cable_len) then begin
        cable_filepath=filepath(obs.instrument+'_cable_length.txt',root=rootdir('FHD'),subdir='instrument_config')
        textfast,data_array,/read,file_path=cable_filepath,first_line=1
        cable_len=Reform(data_array[2,*])
      endif
      
      for cable_i=0, N_elements(cal_mode_fit)-1 do begin
        if cal_mode_fit[cable_i] GT 0 then begin
          cable_cut_i=where(cable_len NE cal_mode_fit[cable_i],n_cable_cut)
          IF n_cable_cut GT 0 THEN tile_ref_logic[cable_cut_i] -= 1
        endif else begin
          cable_cut_i=where(cable_len eq abs(cal_mode_fit[cable_i]),n_cable_cut)
          IF n_cable_cut GT 0 THEN tile_ref_logic[cable_cut_i] = -N_elements(cal_mode_fit)
        endelse
      endfor
      cable_cut = where(tile_ref_logic LE -N_elements(cal_mode_fit),n_count)
      tile_ref_flag = FLTARR(n_tile) + 1.
      if n_count GT 0 then tile_ref_flag[cable_cut] = 0
      FOR pol_i=0,n_pol-1 DO mode_i_arr[pol_i,*]=mode_i_arr[pol_i,*]*tile_ref_flag
    ENDIF
    
    
    FOR pol_i=0,n_pol-1 DO BEGIN
      gain_arr=*cal.gain[pol_i]
      gain_arr_fit=*cal_return.gain[pol_i]
      gain_arr = gain_arr / gain_arr_fit ; Divide the polyfit to reveal residual cable reflections better

      FOR ti=0L,nt_use-1 DO BEGIN
        tile_i=tile_use[ti]
        mode_i=mode_i_arr[pol_i,tile_i]
        IF mode_i EQ 0 THEN CONTINUE
        
        ;Options to hyperresolve or fit the reflection modes/amp/phase given the nominal calculations
        IF Keyword_Set(cal_reflection_hyperresolve) THEN BEGIN
          mode0=mode_i ; start with nominal cable length
          dmode=0.05 ; overresolve the FT used for the fit (normal resolution would be dmode=1)
          nmodes=101 ; range around the central mode to test
          modes=(dindgen(nmodes)-nmodes/2)*dmode+mode0 ; array of modes to try
          modes=rebin(modes,nmodes,nf_use) ; reshape for ease of computing

          IF Keyword_Set(auto_ratio) THEN BEGIN
            ;Find tiles which will *not* be accidently coherent in their cable reflection in order to reduce bias
            inds = where((*obs.baseline_info).tile_use AND mode_i_arr[pol_i,*] gt 0 AND Abs(mode_i_arr[pol_i,*]-mode_i) gt 0.01, ninds)

            ;mean over frequency for each tile
            freq_mean = mean((*auto_ratio[pol_i]), dimension=1,/NAN)
            ;normalized autos using each tile's freq mean
            norm_autos = (*auto_ratio[pol_i]) / rebin(transpose(freq_mean),n_freq,n_tile)
            ;mean over all tiles which *are not* accidently coherent as a func of freq
            incoherent_mean = mean(norm_autos[*,inds],dimension=2,/NAN) 

            ;Residual and normalized (using incoherent mean) auto-correlation
            resautos = (norm_autos[*,tile_i] / incoherent_mean) - mean(norm_autos[*,tile_i] / incoherent_mean,/NAN) 
            gain_temp=rebin(transpose(reform(resautos[freq_use])),nmodes,nf_use)

          ENDIF ELSE BEGIN
            ; dimension manipulation, add dim for mode fitting
            gain_temp=rebin_complex(transpose(reform(gain_arr[freq_use,tile_i] - mean(gain_arr[freq_use,tile_i]))),nmodes,nf_use)
          ENDELSE
          freq_mat=rebin(transpose(freq_use),nmodes,nf_use) ; freq_use matrix to multiply/collapse in fit
          test_fits=Total(exp(i_comp*2.*!Pi/n_freq*modes*freq_mat)*gain_temp,2) ; Perform DFT of gains to test modes
          amp_use=max(abs(test_fits),mode_ind)/nf_use ; Pick out highest amplitude fit (mode_ind gives the index of the mode)
          phase_use=atan(test_fits[mode_ind],/phase) ; Phase of said fit
          mode_i=modes[mode_ind,0] ; And the actual mode

          ;Using the mode selected from the gains, optionally use the phase to find the amp and phase
          if keyword_set(cal_reflection_hyperresolve_incoherent) OR keyword_set(auto_ratio) then begin
            ;Find tiles which will *not* be accidently coherent in their cable reflection in order to reduce bias
            inds = where((*obs.baseline_info).tile_use AND mode_i_arr[pol_i,*] gt 0 AND Abs(mode_i_arr[pol_i,*]-mode_i) gt 0.01, ninds)
            residual_phase = atan(gain_arr[freq_use,*],/phase)
            incoherent_residual_phase = residual_phase[*,tile_i] - Mean(residual_phase[*,inds], dimension=2, /NAN)

            test_fits=Total(exp(i_comp*2.*!Pi/n_freq*mode_i*freq_use)*incoherent_residual_phase)
            amp_use=2*abs(test_fits)/nf_use ;factor of 2 from fitting just the phase
            phase_use=-atan(test_fits,/phase)+!pi/2 ;factor of pi/2 from just fitting the phase
          endif

        ENDIF ELSE IF Keyword_Set(amp_arr) OR Keyword_Set(phase_arr) THEN BEGIN
          ; use predetermined fits
          amp_use=amp_arr[pol_i,tile_i]
          phase_use=phase_arr[pol_i,tile_i]
        ENDIF ELSE BEGIN
          ; use nominal delay mode, but fit amplitude and phase of reflections
          mode_fit=Total(exp(i_comp*2.*!Pi/n_freq*(mode_i)*freq_use)*Reform(gain_arr[freq_use,tile_i]))
          amp_use=abs(mode_fit)/nf_use ;why factor of 2? Check FFT normalization
          phase_use=atan(mode_fit,/phase)
        ENDELSE
        
        gain_mode_fit=amp_use*exp(i_comp*2.*!Pi*(mode_i*findgen(n_freq)/n_freq)+i_comp*phase_use)
        if keyword_set(reflection_phase_only) or keyword_set(auto_ratio) then begin
          gain_arr_fit[*,tile_i]*=exp(i_comp*imaginary(gain_mode_fit))
        endif else begin
          gain_arr_fit[*,tile_i]*=(1+gain_mode_fit)
        endelse
        
        cal_return.mode_params[pol_i,tile_i]=Ptr_new([mode_i,amp_use,phase_use])
      ENDFOR
      *cal_return.gain[pol_i]=gain_arr_fit
    ENDFOR
  ENDIF
  ;*******************End cable reflection fitting
  
  IF Keyword_Set(no_phase_calibration) THEN FOR pol_i=0,n_pol-1 DO *cal_return.gain[pol_i]=Abs(*cal_return.gain[pol_i])
  undefine_fhd,gain_residual
  RETURN,cal_return
END
