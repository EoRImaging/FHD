PRO vis_calibration_flag,obs,cal

sigma_threshold=5.
n_tile=obs.n_tile
n_freq=obs.n_freq
n_pol=obs.n_pol

gain_arr=cal.gain ;pointer array
tile_use=(*obs.baseline_info).tile_use
freq_use=(*obs.baseline_info).freq_use


FOR pol_i=0,n_pol-1 DO BEGIN
    tile_use_i0=where(tile_use,n_tile_use)
    freq_use_i0=where(freq_use,n_freq_use)
    
    gain=*gain_arr[pol_i]
    phase=Atan(gain,/phase)
    phase=PhUnwrap(phase)
    amp=Abs(gain)
    
    amp_sub=extract_subarray(amp,freq_use_i0,tile_use_i0)
    gain_freq_test=Median(amp_sub,dimension=2)
    gain_freq_fom=fltarr(n_freq_use) 
    FOR freq_i=0,n_freq_use-1 DO gain_freq_fom[freq_i]=Stddev(amp_sub[freq_i,*])
    gain_tile_fom=fltarr(n_tile_use) 
    FOR tile_i=0,n_tile_use-1 DO gain_tile_fom[tile_i]=Stddev(amp_sub[*,tile_i])
    freq_cut_i=where(gain_freq_fom EQ 0,n_freq_cut)
    tile_cut_i=where(gain_tile_fom EQ 0,n_tile_cut)
    IF n_freq_cut GT 0 THEN freq_use[freq_use_i0[freq_cut_i]]=0
    IF n_tile_cut GT 0 THEN tile_use[tile_use_i0[tile_cut_i]]=0
    
;    tile_use_i1=where(tile_use,n_tile_use)
;    freq_use_i1=where(freq_use,n_freq_use)
;    
;    amp_sub=extract_subarray(amp,freq_use_i1,tile_use_i1)
;    gain_freq_test=Median(amp_sub,dimension=2)
;    gain_tile_test=Median(amp_sub,dimension=1)
;    
;    gain_freq_test-=Median(gain_freq_test)
;    gain_tile_test-=Median(gain_tile_test)
;     
;    freq_fit_params=svdfit(freq_use_i1,gain_freq_test,4,yfit=freq_fit)
;    tile_fit_params=svdfit(tile_use_i1,gain_tile_test,4,yfit=tile_fit)
;    
;    gain_freq_test-=freq_fit
;    gain_tile_test-=tile_fit
;    
;    tile_cut=where(Abs(gain_tile_test) GT sigma_threshold*Stddev(gain_tile_test),n_tile_cut)
;    freq_cut=where(Abs(gain_freq_test) GT sigma_threshold*Stddev(gain_freq_test),n_freq_cut)
;    IF n_freq_cut GT 0 THEN freq_use[freq_use_i1[freq_cut_i]]=0
;    IF n_tile_cut GT 0 THEN tile_use[tile_use_i1[tile_cut_i]]=0
    
    


;    tile_mask=fltarr(n_tile) & tile_mask[tile_use_i]=1
;    freq_mask=fltarr(n_freq) & freq_mask[freq_use_i]=1
;    gain_sub=extract_subarray(amp,freq_use_i,tile_use_i)
;    gain_vals=gain_sub[sort(gain_sub)]
;    n_vals=N_Elements(gain_vals)
;    sigma_use=stddev(gain_vals[n_vals/4.:(3.*n_vals/4.)],/nan,/double)
;    tile_use_i2=where((Abs(gain_tile_test-Median(gain_tile_test[tile_use_i])) LE sigma_threshold*sigma_use) AND tile_mask,$
;        n_tile_use2,complement=tile_cut,ncomplement=n_tile_cut)
;    IF n_tile_cut GT 0 THEN tile_mask[tile_cut]=0
;    freq_use_i2=where((Abs(gain_freq_test-Median(gain_freq_test[freq_use_i])) LE sigma_threshold*sigma_use) AND freq_mask,$
;        n_freq_use2,complement=freq_cut,ncomplement=n_freq_cut)
;    IF n_freq_cut GT 0 THEN freq_mask[freq_cut]=0
;    
;    IF n_tile_cut GT 0 THEN BEGIN
;        gain[*,tile_cut]=1.
;        tile_cut_full=tile_cut#Replicate(1.,n_time)+Replicate(1.,n_tile_cut)#bin_offset
;        FOR pol_i2=0,n_pol-1 DO (*flag_ptr_use[pol_i2])[*,tile_cut_full]=0
;    ENDIF
;    IF n_freq_cut GT 0 THEN BEGIN
;        gain[freq_cut,*]=1.
;        FOR pol_i2=0,n_pol-1 DO (*flag_ptr_use[pol_i2])[freq_cut,*]=0
;    ENDIF
ENDFOR
END