PRO vis_calibration_flag,obs,cal,error=error,degree=degree,phase_degree=phase_degree

IF ~Keyword_Set(degree) THEN degree=2.
IF ~Keyword_Set(phase_degree) THEN phase_degree=degree-1.
amp_sigma_threshold=5.
phase_sigma_threshold=5.
n_tile=obs.n_tile
n_freq=obs.n_freq
n_pol=cal.n_pol

gain_arr=cal.gain ;pointer array
obs_info=*obs.baseline_info
tile_use=obs_info.tile_use
freq_use=obs_info.freq_use


FOR pol_i=0,n_pol-1 DO BEGIN
    tile_use_i0=where(tile_use,n_tile_use)
    freq_use_i0=where(freq_use,n_freq_use)
    
    gain=*gain_arr[pol_i]
    phase=Atan(gain,/phase)
    amp=Abs(gain)
    
    ;first flag based on overall amplitude
    amp_sub=extract_subarray(amp,freq_use_i0,tile_use_i0)
    gain_freq_test=Median(amp_sub,dimension=2)
    gain_freq_fom=fltarr(n_freq_use) 
    FOR freq_i=0,n_freq_use-1 DO gain_freq_fom[freq_i]=Stddev(amp_sub[freq_i,*])
    gain_tile_fom=fltarr(n_tile_use)
    gain_tile_avg=fltarr(n_tile_use) 
    FOR tile_i=0,n_tile_use-1 DO BEGIN
        amp_sub2=amp_sub[*,tile_i]
        fit_params=poly_fit(freq_use_i0,amp_sub2,degree,yfit=amp_sub2_fit)
        gain_tile_fom[tile_i]=Stddev(amp_sub2-amp_sub2_fit)
        gain_tile_avg[tile_i]=Median(amp_sub2)
    ENDFOR
    
    freq_cut_i=where(gain_freq_fom EQ 0,n_freq_cut,ncomp=n_freq_uncut,complement=freq_uncut_i)
    tile_cut_i=where(gain_tile_fom EQ 0,n_tile_cut,ncomp=n_tile_uncut,complement=tile_uncut_i)
    IF n_freq_cut GT 0 THEN freq_use[freq_use_i0[freq_cut_i]]=0
    IF n_tile_cut GT 0 THEN tile_use[tile_use_i0[tile_cut_i]]=0
    IF (n_freq_uncut EQ 0) OR (n_tile_uncut EQ 0) THEN BEGIN
        error=1
        CONTINUE
    ENDIF
    
    n_addl_cut=(n_freq_cut+n_tile_cut)>1
    n_cut=n_freq_cut+n_tile_cut
    iter=0
    WHILE n_addl_cut GT 0 DO BEGIN
        gain_freq_sigma=Stddev(gain_freq_fom[freq_uncut_i])
        gain_tile_sigma=Stddev(gain_tile_fom[tile_uncut_i])
        freq_cut_test=(gain_freq_fom-Median(gain_freq_fom[freq_uncut_i])-amp_sigma_threshold*gain_freq_sigma) GT 0
        freq_cut_i=where(freq_cut_test,n_freq_cut,ncomp=n_freq_uncut,complement=freq_uncut_i)
        tile_cut_test1=(gain_tile_fom-Median(gain_tile_fom[tile_uncut_i])-amp_sigma_threshold*gain_tile_sigma) GT 0
        tile_cut_test2=gain_tile_avg LT Median(gain_tile_avg)/2.
        tile_cut_i=where(tile_cut_test1 OR tile_cut_test2,n_tile_cut,ncomp=n_tile_uncut,complement=tile_uncut_i)
        n_addl_cut=(n_freq_cut+n_tile_cut)-n_cut
        n_cut=n_freq_cut+n_tile_cut
        iter+=1
        IF iter GE 3 THEN BREAK
    ENDWHILE
    IF n_freq_cut GT 0 THEN freq_use[freq_use_i0[freq_cut_i]]=0
    IF n_tile_cut GT 0 THEN tile_use[tile_use_i0[tile_cut_i]]=0
    
    tile_use_i1=where(tile_use,n_tile_use)
    freq_use_i1=where(freq_use,n_freq_use)
    
;    ;now flag based on phase
    phase_sub=extract_subarray(phase,freq_use_i1,tile_use_i1)
    
    phase_slope_arr=fltarr(n_tile_use)
    phase_sigma_arr=fltarr(n_tile_use)
    FOR tile_i=0L,n_tile_use-1 DO BEGIN
        phase_use=PhUnwrap(phase_sub[*,tile_i])
        phase_fit=fltarr(n_freq_use)
        fi_use2=indgen(n_freq_use)
;        FOR iter=0,2 DO BEGIN
            phase_use2=phase_use[fi_use2]-phase_fit
            phase_params=poly_fit(freq_use_i1[fi_use2],phase_use2,phase_degree,yfit=phase_fit)
            phase_sigma2=Stddev(phase_use2-phase_fit)
            fi_use2_i=where(Abs(phase_use2-phase_fit) LT 3.*phase_sigma2,n_fi2)
;            IF n_fi2 LE 2 THEN BREAK
;            fi_use2=fi_use2[fi_use2_i]
;        ENDFOR
        IF N_Elements(phase_params) GE 2 THEN slope=phase_params[1] ELSE slope=0.
        phase_slope_arr[tile_i]=slope
        phase_sigma_arr[tile_i]=phase_sigma2
    ENDFOR
    iter=0
    n_addl_cut=1
    n_cut=0
    WHILE n_addl_cut GT 0 DO BEGIN
        slope_sigma=Stddev(phase_slope_arr)
        tile_cut_test1=(Abs(phase_slope_arr)-Median(Abs(phase_slope_arr))) GT phase_sigma_threshold*slope_sigma
        tile_cut_test2=(phase_sigma_arr-Median(phase_sigma_arr)) GT phase_sigma_threshold*Stddev(phase_sigma_arr)
        tile_cut_i=where(tile_cut_test1 OR tile_cut_test2,n_tile_cut,ncomp=n_tile_uncut,complement=tile_uncut_i)
        n_addl_cut=(n_tile_cut)-n_cut
        n_cut=n_tile_cut
        iter+=1
        IF iter GE 3 THEN BREAK
    ENDWHILE
    IF n_tile_cut GT 0 THEN tile_use[tile_use_i1[tile_cut_i]]=0
ENDFOR
obs_info.tile_use=tile_use
obs_info.freq_use=freq_use
*obs.baseline_info=obs_info
END