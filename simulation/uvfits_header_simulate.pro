FUNCTION uvfits_header_simulate,instrument=instrument,nbaselines=nbaselines,n_tile=n_tile,n_pol=n_pol,n_freq=n_freq,$
    date_obs=date_obs,jd0=Jdate0,frequency_resolution=frequency_resolution,frequency_array=frequency_array,$
    reference_frequency=reference_frequency,ref_freq_i=ref_freq_i,$
    obsra=obsra,obsdec=obsdec,time_resolution=time_resolution,time_integration=time_integration

IF N_Elements(instrument) EQ 0 THEN instrument='mwa'
IF N_Elements(n_tile) EQ 0 THEN n_tile=128.
IF N_Elements(n_pol) EQ 0 THEN n_pol=2
IF N_Elements(n_freq) EQ 0 THEN n_freq=384.
IF N_Elements(frequency_resolution) EQ 0 THEN frequency_resolution=80000. ;Hz
IF N_Elements(time_resolution) EQ 0 THEN time_resolution=2. ;seconds
IF N_Elements(time_integration) EQ 0 THEN time_integration=112. ;seconds

IF N_Elements(nbaselines) EQ 0 THEN nbaselines=n_tile*(n_tile-1.)/2.
IF N_Elements(frequency_array) EQ 0 THEN BEGIN
    IF N_Elements(ref_freq_i) EQ 0 THEN ref_freq_i=Ceil(n_freq/2)  ;NOTE!!! THIS USES THEN FITS CONVENTION THAT ARRAY INDICES START FROM 1
    IF N_Elements(reference_frequency) EQ 0 THEN reference_frequency=1.5424E8 ;Hz
ENDIF


RETURN,hdr
END