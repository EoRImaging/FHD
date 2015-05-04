PRO mwa_simulation_instr_config,n_tile=n_tile,n_pol=n_pol,n_freq=n_freq,lon=lon,lat=lat,alt=alt,$
    freq_res=freq_res,reference_frequency=reference_frequency

IF N_Elements(n_tile) EQ 0 THEN n_tile=128.
IF N_Elements(n_pol) EQ 0 THEN n_pol=2
IF N_Elements(n_freq) EQ 0 THEN n_freq=384.
IF N_Elements(freq_res) EQ 0 THEN freq_res=80000. ;Hz
;IF N_Elements(time_resolution) EQ 0 THEN time_resolution=2. ;seconds
;IF N_Elements(time_integration) EQ 0 THEN time_integration=112. ;seconds
IF N_Elements(reference_frequency) EQ 0 THEN reference_frequency=1.5424E8 ;Hz
IF N_Elements(lon) EQ 0 THEN lon=116.67081524 ;degrees (MWA, from Tingay et al. 2013)
IF N_Elements(lat) EQ 0 THEN lat=-26.7033194 ;degrees (MWA, from Tingay et al. 2013)
IF N_Elements(alt) EQ 0 THEN alt=377.827 ;altitude (meters) (MWA, from Tingay et al. 2013)
END