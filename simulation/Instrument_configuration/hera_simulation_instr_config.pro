PRO mwa_simulation_instr_config,n_tile=n_tile,n_pol=n_pol,n_freq=n_freq,lon=lon,lat=lat,alt=alt,$
    freq_res=freq_res,reference_frequency=reference_frequency


IF N_Elements(n_tile) EQ 0 THEN n_tile=fill_value
IF N_Elements(n_pol) EQ 0 THEN n_pol=2
IF N_Elements(n_freq) EQ 0 THEN n_freq=fill_value
IF N_Elements(freq_res) EQ 0 THEN freq_res=fill_value ;Hz
IF N_Elements(reference_frequency) EQ 0 THEN reference_frequency=fill_value ;Hz
IF N_Elements(lon) EQ 0 THEN lon=fill_value ; longitude in decimal degrees
IF N_Elements(lat) EQ 0 THEN lat=fill_value ; latitude in decimal degrees
IF N_Elements(alt) EQ 0 THEN alt=fill_value ;altitude (meters)
END