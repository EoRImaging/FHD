FUNCTION horizon_mask, obs, horizon_threshold=horizon_threshold

IF N_Elements(horizon_threshold) EQ 0 THEN horizon_threshold = 10. ;degrees above the horizon to exclude
dimension = obs.dimension
elements = obs.elements
horizon_mask=intarr(dimension,elements)+1

;ignore refraction since it's only being used to calculate a pixel mask for the horizon
; UPDATE: refraction is now ignored by default in apply_astrometry 
apply_astrometry, obs, x_arr=meshgrid(dimension,elements,1), y_arr=meshgrid(dimension,elements,2), $
    ra_arr=ra_arr, dec_arr=dec_arr, /xy2ad
nan_i=where(Finite(ra_arr,/nan),n_nan,complement=horizon_i)
IF n_nan GT 0 THEN horizon_mask[nan_i]=0
ra_use=ra_arr[horizon_i]
dec_use=dec_arr[horizon_i]
Eq2Hor,ra_use,dec_use,obs.Jd0,alt_arr1,az_arr1,lat=obs.lat,lon=obs.lon,alt=obs.alt,precess=1,/nutate, refract=0
horizon_cut=where(alt_arr1 LT horizon_threshold,n_hor_cut)
IF n_hor_cut GT 0 THEN horizon_mask[horizon_i[horizon_cut]]=0
RETURN, horizon_mask
END