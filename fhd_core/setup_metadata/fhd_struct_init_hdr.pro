FUNCTION fhd_struct_init_hdr,n_grp_params=n_grp_params,nbaselines=nbaselines,n_tile=n_tile,n_pol=n_pol,n_freq=n_freq,$
    freq_res=freq_res,freq_arr=freq_arr,lon=lon,lat=lat,alt=alt,obsra=obsra,obsdec=obsdec,$
    uu_i=uu_i,vv_i=vv_i,ww_i=ww_i,baseline_i=baseline_i,date_i=date_i,jd0=jd0,date_obs=date_obs,$
    pol_dim=pol_dim,freq_dim=freq_dim,real_index=real_index,imaginary_index=imaginary_index,$
    weights_index=weights_index, ant1_i=ant1_i, ant2_i=ant2_i

IF N_Elements(n_tile) EQ 0 THEN n_tile=128.
IF N_Elements(pol_dim) EQ 0 THEN pol_dim=2
IF N_Elements(freq_dim) EQ 0 THEN freq_dim=4
IF N_Elements(real_index) EQ 0 THEN real_index=0
IF N_Elements(imaginary_index) EQ 0 THEN imaginary_index=1
IF N_Elements(weights_index) EQ 0 THEN weights_index=2

IF N_Elements(n_grp_params) EQ 0 THEN n_grp_params=5
IF N_Elements(nbaselines) EQ 0 THEN nbaselines=n_tile*(n_tile-1)/2.
IF N_Elements(n_pol) EQ 0 THEN n_pol=4.; columns are xx, yy, xy, yx
IF N_Elements(n_freq) EQ 0 THEN n_freq=768.
IF N_Elements(pol_dim) EQ 0 THEN freq_ref=1.5424E8
IF N_Elements(freq_res) EQ 0 THEN freq_res=40000.
IF N_Elements(freq_arr) EQ 0 THEN freq_arr=freq_ref+(findgen(n_freq)-n_freq/2)*freq_res


IF N_Elements(lon) EQ 0 THEN lon=116.67081524;degrees (MWA, from Tingay et al. 2013)
IF N_Elements(lat) EQ 0 THEN lat=-26.7033194;degrees (MWA, from Tingay et al. 2013)
IF N_Elements(alt) EQ 0 THEN alt=377.827 ;altitude (meters) (MWA, from Tingay et al. 2013)

IF N_Elements(baseline_i) EQ 0 THEN baseline_i=-1
IF N_Elements(uu_i) EQ 0 THEN uu_i=-1
IF N_Elements(vv_i) EQ 0 THEN vv_i=-1
IF N_Elements(ww_i) EQ 0 THEN ww_i=-1
IF N_Elements(date_i) EQ 0 THEN date_i=-1
IF N_Elements(ant1_i) EQ 0 THEN ant1_i=-1
IF N_Elements(ant2_i) EQ 0 THEN ant2_i=-1
CASE 1 OF
    Keyword_Set(jd0):date_obs=date_conv(jd0,'fits')
    Keyword_Set(date_obs): jd0=date_conv(date_obs,'julian') 
    ELSE: BEGIN
        jd0=Systime(/julian)
        date_obs=date_conv(jd0,'fits')
    ENDELSE
ENDCASE

IF N_Elements(obsra) EQ 0 THEN obsra=-1. ;set to something that should be obvious it was a default
IF N_Elements(obsdec) EQ 0 THEN obsdec=-1. ;set to something that should be obvious it was a default
        
hdr={n_params:n_grp_params,nbaselines:nbaselines,n_tile:n_tile,n_pol:n_pol,n_freq:n_freq,$
    freq_res:freq_res,freq_arr:freq_arr,lon:lon,lat:lat,alt:alt,obsra:obsra,obsdec:obsdec,$
    uu_i:uu_i,vv_i:vv_i,ww_i:ww_i,baseline_i:baseline_i,date_i:date_i,jd0:jd0,date_obs:date_obs,$
    pol_dim:pol_dim,freq_dim:freq_dim,real_index:real_index,imaginary_index:imaginary_index,$
    weights_index:weights_index, ant1_i:ant1_i, ant2_i:ant2_i}
RETURN,hdr
END