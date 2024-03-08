;+
; :Description:
;    Function designed to replace xy2ad and ad2xy from the astronomy library. 
;    Those programs cannot account for effects such as refraction
;
; :Params:
; 
;    x_arr - array of pixel x values. Input if xy2ad is set, output if ad2xy is set
;    
;    y_arr - array of pixel y values. Input if xy2ad is set, output if ad2xy is set
;    
;    obs - standard FHD obs structure
;    
;    ra_arr  - array of RA coordinates (degrees). Input if ad2xy is set, output if xy2ad is set
;    
;    dec_arr  - array of Dec coordinates (degrees). Input if ad2xy is set, output if xy2ad is set
;
; :Keywords:
; 
;    xy2ad - set to convert from pixel to celestial coordinates
;    
;    ad2xy - set to convert from celestial to pixel coordinates
;    
;    refraction - set to take refraction into account (it's false by default for speed, backwards compatibility and testing)
;    
;    _Extra - passes any additional keywords (like temperature (Kelvin) or pressure (millibars) to co_refract
;
; :Author: Ian
;-
PRO apply_astrometry,obs, x_arr=x_arr, y_arr=y_arr, ra_arr=ra_arr, dec_arr=dec_arr, xy2ad=xy2ad, ad2xy=ad2xy, $
    astr=astr, JDate=JDate, refraction=refraction, _Extra=extra

IF not (Keyword_Set(xy2ad) OR Keyword_Set(ad2xy)) THEN $
    message, "ERROR! At least one direction keyword (xy2ad or ad2xy) must be set."

IF (Keyword_Set(xy2ad) AND Keyword_Set(ad2xy)) THEN $
    message, "ERROR! Only one direction keyword (xy2ad or ad2xy) may be set."

IF size(obs,/type) EQ 8 THEN BEGIN 
    astr=obs.astr
    JDate=obs.JD0
ENDIF ELSE refraction=0

if Keyword_Set(refraction) THEN refraction=1 ELSE refraction=0

precess_forward=0
precess_reverse=0
IF Keyword_Set(xy2ad) THEN BEGIN
    xy2ad, x_arr, y_arr, astr, ra_arr, dec_arr
    
    IF Keyword_Set(refraction) THEN BEGIN   
        i_nan=where(Finite(ra_arr,/nan),n_nan,complement=i_use)
        IF n_nan EQ 0 THEN BEGIN
            Eq2Hor,ra_arr, dec_arr, JDate, alt_arr, az_arr, nutate=precess_reverse,precess=precess_reverse,aberration=precess_reverse, refract=0, lon=obs.lon, alt=obs.alt, lat=obs.lat
            alt_arr_new=CO_REFRACT(alt_arr, altitude=obs.alt, _Extra=extra)
            Hor2Eq, alt_arr_new, az_arr, JDate, ra_arr, dec_arr, precess=precess_forward, nutate=precess_forward,aberration=precess_forward, refract=0, lon=obs.lon, alt=obs.alt, lat=obs.lat
        ENDIF ELSE BEGIN
            ra_vals=ra_arr[i_use] & dec_vals=dec_arr[i_use]
            Eq2Hor,ra_vals, dec_vals, JDate, alt_arr, az_arr, nutate=precess_reverse,precess=precess_reverse,aberration=precess_reverse, refract=0, lon=obs.lon, alt=obs.alt, lat=obs.lat
            alt_arr_new=CO_REFRACT(alt_arr, altitude=obs.alt, _Extra=extra)
            Hor2Eq, alt_arr_new, az_arr, JDate, ra_vals, dec_vals, precess=precess_forward, nutate=precess_forward,aberration=precess_forward, refract=0, lon=obs.lon, alt=obs.alt, lat=obs.lat
            ra_arr[i_use]=ra_vals & dec_arr[i_use]=dec_vals
        ENDELSE
    ENDIF
ENDIF

IF Keyword_Set(ad2xy) THEN BEGIN    
    ra_arr_new = ra_arr
    dec_arr_new = dec_arr
    i_nan=where(Finite(ra_arr_new,/nan),n_nan,complement=i_use)
    IF n_nan EQ 0 THEN BEGIN
        Eq2Hor,ra_arr_new, dec_arr_new, JDate, alt_arr, az_arr, nutate=precess_reverse,precess=precess_reverse,aberration=precess_reverse, refract=0, lon=obs.lon, alt=obs.alt, lat=obs.lat
    ENDIF ELSE BEGIN
        Eq2Hor,ra_arr_new[i_use], dec_arr_new[i_use], JDate, alt_arr, az_arr, nutate=precess_reverse,precess=precess_reverse,aberration=precess_reverse, refract=0, lon=obs.lon, alt=obs.alt, lat=obs.lat
    ENDELSE
    horizon_i = where(alt_arr LE 0, n_horizon, complement=h_use)
    IF n_horizon GT 0 THEN BEGIN
        alt_arr = alt_arr[h_use]
        az_arr = az_arr[h_use]
        ra_arr_new[horizon_i] = !Values.F_NAN
        dec_arr_new[horizon_i] = !Values.F_NAN
        i_nan=where(Finite(ra_arr_new,/nan),n_nan,complement=i_use)
    ENDIF
    IF Keyword_Set(refraction) THEN BEGIN    
        ra_arr_new = ra_arr
        dec_arr_new = dec_arr
        IF n_nan EQ 0 THEN BEGIN
            Eq2Hor,ra_arr_new, dec_arr_new, JDate, alt_arr, az_arr, nutate=precess_reverse,precess=precess_reverse,aberration=precess_reverse, refract=0, lon=obs.lon, alt=obs.alt, lat=obs.lat
            alt_arr_new=CO_REFRACT(alt_arr, altitude=obs.alt, /to_observed, _Extra=extra)
            Hor2Eq, alt_arr_new, az_arr, JDate, ra_arr_new, dec_arr_new, precess=precess_forward, nutate=precess_forward,aberration=precess_forward, refract=0, lon=obs.lon, alt=obs.alt, lat=obs.lat
        ENDIF ELSE BEGIN
            ra_arr_new[i_nan] = !Values.F_NAN
            dec_arr_new[i_nan] = !Values.F_NAN
            ra_vals=ra_arr_new[i_use] & dec_vals=dec_arr_new[i_use]
            Eq2Hor,ra_vals, dec_vals, JDate, alt_arr, az_arr, nutate=precess_reverse,precess=precess_reverse,aberration=precess_reverse, refract=0, lon=obs.lon, alt=obs.alt, lat=obs.lat
            alt_arr_new=CO_REFRACT(alt_arr, altitude=obs.alt, /to_observed, _Extra=extra)
            Hor2Eq, alt_arr_new, az_arr, JDate, ra_vals, dec_vals, precess=precess_forward, nutate=precess_forward,aberration=precess_forward, refract=0, lon=obs.lon, alt=obs.alt, lat=obs.lat
            ra_arr_new[i_use]=ra_vals & dec_arr_new[i_use]=dec_vals
        ENDELSE
;        alt_arr_new=CO_REFRACT(alt_arr, altitude=obs.alt, /to_observed, _Extra=extra)
;        Hor2Eq, alt_arr_new, az_arr, JDate, ra_vals, dec_vals, precess=precess_forward, nutate=precess_forward,aberration=precess_forward, refract=0, lon=obs.lon, alt=obs.alt, lat=obs.lat
;        ra_arr_new[i_use]=ra_vals & dec_arr_new[i_use]=dec_vals
    ENDIF
    ad2xy, ra_arr_new, dec_arr_new, astr, x_arr, y_arr
ENDIF

END