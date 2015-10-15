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
;    ignore_refraction - set to ignore refraction (for backwards compatibility and testing)
;
; :Author: Ian
;-
PRO apply_astrometry,obs, x_arr=x_arr, y_arr=y_arr, ra_arr=ra_arr, dec_arr=dec_arr, xy2ad=xy2ad, ad2xy=ad2xy, $
    astr=astr, JDate=JDate, ignore_refraction=ignore_refraction

IF not (Keyword_Set(xy2ad) OR Keyword_Set(ad2xy)) THEN $
    message, "ERROR! At least one direction keyword (xy2ad or ad2xy) must be set."

IF (Keyword_Set(xy2ad) AND Keyword_Set(ad2xy)) THEN $
    message, "ERROR! Only one direction keyword (xy2ad or ad2xy) may be set."

IF size(obs,/type) EQ 8 THEN BEGIN 
    astr=obs.astr
    JDate=obs.JD0
ENDIF ELSE ignore_refraction=1
IF Keyword_Set(xy2ad) THEN BEGIN
    xy2ad, x_arr, y_arr, astr, ra_arr, dec_arr
    
    IF ~Keyword_Set(ignore_refraction) THEN BEGIN   
        i_use=where(Finite(ra_arr,/nan),n_use,ncomplement=n_nan)
        IF n_nan EQ 0 THEN BEGIN
            Eq2Hor,ra_arr, dec_arr, JDate, alt_arr, az_arr, nutate=0,precess=0, refract=0
            alt_arr_new=CO_REFRACT(alt_arr, altitude=obs.alt)
            Hor2Eq, alt_arr_new, az_arr, JDate, ra_arr, dec_arr, precess=0, nutate=0, refract=0
        ENDIF ELSE BEGIN
            ra_vals=ra_arr[i_use] & dec_vals=dec_arr[i_use]
            Eq2Hor,ra_vals, dec_vals, JDate, alt_arr, az_arr, nutate=0,precess=0, refract=0
            alt_arr_new=CO_REFRACT(alt_arr, altitude=obs.alt)
            Hor2Eq, alt_arr_new, az_arr, JDate, ra_vals, dec_vals, precess=0, nutate=0, refract=0
            ra_arr[i_use]=ra_vals & dec_arr[i_use]=dec_vals
        ENDELSE
    ENDIF
ENDIF

IF Keyword_Set(ad2xy) THEN BEGIN    
    IF ~Keyword_Set(ignore_refraction) THEN BEGIN
        i_nan=where(Finite(ra_arr,/nan),n_nan,ncomplement=n_nan,complement=i_use)
        IF n_nan EQ 0 THEN BEGIN
            Eq2Hor,ra_arr, dec_arr, JDate, alt_arr, az_arr, nutate=0,precess=0, refract=0, lon=obs.lon, alt=obs.alt, lat=obs.lat
            alt_arr_new=CO_REFRACT(alt_arr, altitude=obs.alt, /to_observed)
            Hor2Eq, alt_arr_new, az_arr, JDate, ra_arr, dec_arr, precess=0, nutate=0, refract=0, lon=obs.lon, alt=obs.alt, lat=obs.lat
        ENDIF ELSE BEGIN
            ra_vals=ra_arr[i_use] & dec_vals=dec_arr[i_use]
            Eq2Hor,ra_vals, dec_vals, JDate, alt_arr, az_arr, nutate=0,precess=0, refract=0, lon=obs.lon, alt=obs.alt, lat=obs.lat
            alt_arr_new=CO_REFRACT(alt_arr, altitude=obs.alt, /to_observed)
            Hor2Eq, alt_arr_new, az_arr, JDate, ra_vals, dec_vals, precess=0, nutate=0, refract=0, lon=obs.lon, alt=obs.alt, lat=obs.lat
            ra_arr[i_use]=ra_vals & dec_arr[i_use]=dec_vals
        ENDELSE
    ENDIF
    ad2xy, ra_arr, dec_arr, astr, x_arr, y_arr
ENDIF

END