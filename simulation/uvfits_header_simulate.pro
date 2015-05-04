FUNCTION uvfits_header_simulate,hdr_in,instrument=instrument,nbaselines=nbaselines,n_tile=n_tile,n_pol=n_pol,n_freq=n_freq,$
    date_obs=date_obs,jd0=Jdate0,freq_res=freq_res,frequency_array=frequency_array,$
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
        IF N_Elements(freq_res) EQ 0 THEN freq_res=hdr_in.freq_res
        IF N_Elements(reference_frequency) EQ 0 THEN reference_frequency=1.5424E8 ;Hz
        IF N_Elements(ref_freq_i) EQ 0 THEN ref_freq_i=Ceil(n_freq/2)
    ENDIF ELSE BEGIN
        IF N_Elements(freq_res) EQ 0 THEN freq_res=Median(frequency_array-shift(frequency_array,1))
    ENDELSE
    
;    IF N_Elements(time_resolution) EQ 0 THEN time_resolution=2. ;seconds
;    IF N_Elements(time_integration) EQ 0 THEN time_integration=112. ;seconds
ENDIF ELSE BEGIN
    inst_settings_fn=instrument+'_simulation_instr_config' ;mwa_simulation_instr_config
    Call_Procedure,inst_settings_fn,n_tile=n_tile,n_pol=n_pol,n_freq=n_freq,lon=lon,lat=lat,alt=alt,$
        freq_res=freq_res,reference_frequency=reference_frequency
    
    CASE 1 OF
        Keyword_Set(date_obs): julian_date_start=date_conv(date_obs,type='julian') ;NOT debugged
        Keyword_Set(julian_date_start):date_obs=date_conv(julian_date_start,type='fits')
        ELSE: BEGIN
            ;figure out a sensible time and date to use if nothing is supplied
            ;if obsra is supplied, find the time when obsra transits zenith closest to midnight within a year of the current date
            time_zone=Double(Round(lon/15.))
            current_time=Systime(/julian)
            n_day=366
            midnight_arr=(Floor(current_time-.5)+0.5)-time_zone/24.+Findgen(n_day)-Floor(n_day/2)
            IF N_Elements(obsra) EQ 0 THEN obsra=0
            IF N_Elements(obsdec) EQ 0 THEN obsdec=lat
            zenpos2,midnight_arr,zenra,zendec,lat=lat,lng=lon,/degree
            ang=angle_difference(zendec,zenra,lat,obsra,/degree,/nearest)
            min_ang=Min(ang,jd_i)
            julian_date_start0=midnight_arr[jd_i]
            ;now, refine the start minute of the observation
            n_minute=120
            midnight_arr2=julian_date_start0+(Findgen(n_minute)-Floor(n_minute/2))/(24.*60.)
            zenpos2,midnight_arr2,zenra2,zendec2,lat=lat,lng=lon,/degree
            ang2=angle_difference(zendec2,zenra2,lat,obsra,/degree,/nearest)
            min_ang2=Min(ang2,jd_i2)
            julian_date_start=midnight_arr2[jd_i2]
            date_obs=date_conv(julian_date_start,'fits')
        ENDELSE
    ENDCASE
    zenpos2,julian_date_start,zenra_use,zendec_use,lat=lat,lng=lon,/degree
    IF N_Elements(obsra) EQ 0 THEN obsra=zenra
    IF N_Elements(obsdec) EQ 0 THEN obsdec=zendec
ENDELSE

IF N_Elements(nbaselines) EQ 0 THEN nbaselines=n_tile*(n_tile-1.)/2.
IF N_Elements(frequency_array) EQ 0 THEN BEGIN
    IF N_Elements(ref_freq_i) EQ 0 THEN ref_freq_i=Ceil(n_freq/2) +1 ;NOTE!!! THIS USES THE FITS CONVENTION THAT ARRAY INDICES START FROM 1
    frequency_array=(Findgen(n_freq)-(ref_freq_i-1))*freq_res+reference_frequency
ENDIF

hdr=fhd_struct_init_hdr(nbaselines=nbaselines,n_tile=n_tile,n_pol=n_pol,n_freq=n_freq,$
    freq_res=freq_res,freq_arr=frequency_array,lon=lon,lat=lat,alt=alt,obsra=obsra,obsdec=obsdec,$
    jd0=julian_date_start,date_obs=date_obs)

RETURN,hdr
END