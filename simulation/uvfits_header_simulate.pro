FUNCTION uvfits_header_simulate,hdr_in,instrument=instrument,nbaselines=nbaselines,n_tile=n_tile,n_pol=n_pol,n_freq=n_freq,$
    date_obs=date_obs,jd0=Jdate0,frequency_resolution=frequency_resolution,frequency_array=frequency_array,$
    reference_frequency=reference_frequency,ref_freq_i=ref_freq_i,lon=lon,lat=lat,alt=alt,$
    obsra=obsra,obsdec=obsdec;,time_resolution=time_resolution,time_integration=time_integration
    
;    hdr={Xn_params:Xn_grp_params,nbaselines:nbaselines,n_tile:n_tile,n_pol:n_polarizations,n_freq:n_frequencies,$
;        freq_res:freq_res,freq_arr:frequency_array,obsra:obsra,obsdec:obsdec,date:date_obs,jd0:Jdate0}

IF N_Elements(instrument) EQ 0 THEN instrument='mwa'
IF Keyword_Set(hdr_in) THEN BEGIN
    IF N_Elements(n_tile) EQ 0 THEN n_tile=hdr_in.n_tile
    IF N_Elements(n_pol) EQ 0 THEN n_pol=hdr_in.n_pol
    IF N_Elements(n_freq) EQ 0 THEN n_freq=hdr_in.n_freq
    IF N_Elements(lat) EQ 0 THEN lat=hdr_in.lat ;latitude of the array, in degrees
    IF N_Elements(lon) EQ 0 THEN lon=hdr_in.lon ;longitude of the array, in degrees
    IF N_Elements(alt) EQ 0 THEN alt=hdr_in.alt ;altitude above sea level of the array, in meters
    IF N_Elements(obsra) EQ 0 THEN obsra=hdr_in.obsra
    IF N_Elements(obsdec) EQ 0 THEN obsdec=hdr_in.obsdec
    
    CASE 1 OF
        Keyword_Set(date_obs): julian_date_start=date_conv(date_obs,type='julian') ;NOT debugged
        Keyword_Set(julian_date_start):date_obs=date_conv(julian_date_start,type='fits')
        ELSE: BEGIN
            date_obs=hdr.date_obs
            julian_date_start=hdr.jd0
        ENDELSE
    ENDCASE
    
    IF N_Elements(frequency_array) EQ 0 THEN BEGIN
        IF N_Elements(frequency_resolution) EQ 0 THEN frequency_resolution=hdr_in.freq_res
        IF N_Elements(reference_frequency) EQ 0 THEN reference_frequency=1.5424E8 ;Hz
        IF N_Elements(ref_freq_i) EQ 0 THEN ref_freq_i=Ceil(n_freq/2)
    ENDIF ELSE BEGIN
        IF N_Elements(frequency_resolution) EQ 0 THEN frequency_resolution=Median(frequency_array-shift(frequency_array,1))
    ENDELSE
    
;    IF N_Elements(time_resolution) EQ 0 THEN time_resolution=2. ;seconds
;    IF N_Elements(time_integration) EQ 0 THEN time_integration=112. ;seconds
ENDIF ELSE BEGIN
    inst_settings_fn=instrument+'_simulation_instr_config' ;mwa_simulation_instr_config
    Call_Procedure,inst_settings_fn,n_tile=n_tile,n_pol=n_pol,n_freq=n_freq,lon=lon,lat=lat,alt=alt,$
        frequency_resolution=frequency_resolution,reference_frequency=reference_frequency
    
    CASE 1 OF
        Keyword_Set(date_obs): julian_date_start=date_conv(date_obs,type='julian') ;NOT debugged
        Keyword_Set(julian_date_start):date_obs=date_conv(julian_date_start,type='fits')
        ELSE: BEGIN
            ;figure out a sensible time and date to use if nothing is supplied
            ;if obsra is supplied, find the time when obsra transits zenith closest to midnight within a year of the current date
            time_zone=Round(lon/15.)
            current_time=Systime(/julian)
            midnight_arr=(Floor(current_time)+0.5)-time_zone/24.+Findgen(366)-183.
            IF Keyword_Set(obsra) THEN BEGIN
                EQ2Hor,obsra,lat,midnight_arr,alt,az,HA,lat=lat,lon=lon
                HA_min=Min(HA,jd_i)
                julian_date_start=midnight_arr[jd_i]
                date_obs=date_conv(julian_date_start,'fits')
            ENDIF
        ENDELSE
    ENDCASE
ENDELSE

IF N_Elements(nbaselines) EQ 0 THEN nbaselines=n_tile*(n_tile-1.)/2.
IF N_Elements(frequency_array) EQ 0 THEN BEGIN
    IF N_Elements(ref_freq_i) EQ 0 THEN ref_freq_i=Ceil(n_freq/2) +1 ;NOTE!!! THIS USES THE FITS CONVENTION THAT ARRAY INDICES START FROM 1
    frequency_array=(Findgen(n_freq)-(ref_freq_i-1))*frequency_resolution+reference_frequency
ENDIF



RETURN,hdr
END