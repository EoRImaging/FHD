FUNCTION healpix_interpolate,healpix_map,obs,nside=nside,hpx_inds=hpx_inds,from_kelvin=from_kelvin,$
    from_jy_per_sr=from_jy_per_sr,hpx_ordering=hpx_ordering,coord_sys=coord_sys,Jdate_use=Jdate_use

IF N_Elements(hpx_ordering) EQ 0 THEN hpx_ordering='ring' ELSE hpx_ordering=StrLowCase(hpx_ordering) ;other ordering is 'nested'
IF N_Elements(coord_sys) EQ 0 THEN coord_sys='celestial' ELSE coord_sys=StrLowCase(coord_sys)
IF N_Elements(Jdate_use) EQ 0 THEN Jdate_use=obs.JD0
n_hpx=nside2npix(nside)
IF N_Elements(hpx_inds) EQ 0 THEN hpx_inds=Lindgen(n_hpx)
hpx_res = sqrt(4*!Pi/(n_hpx))*!RaDeg
dimension_hpx = ceil((obs.dimension*obs.degpix/hpx_res)*(1/2.))*2    ; Ensure even.
elements_hpx = ceil((obs.elements*obs.degpix/hpx_res)*(1/2.))*2
dimension = obs.dimension>dimension_hpx    ; Choose the larger dimension
elements = obs.elements>dimension_hpx

                    ; likewise with elements
; give new dimension/elements to update_obs
obs_hpx = fhd_struct_update_obs(obs,dimension=dimension, elements=elements)
astr=obs_hpx.astr

apply_astrometry,obs_hpx, x_arr=meshgrid(dimension,elements,1), y_arr=meshgrid(dimension, elements, 2), ra_arr=ra_arr, dec_arr=dec_arr, /xy2ad
radec_i=where(Finite(ra_arr))

IF size(healpix_map,/type) EQ 10 THEN BEGIN ;check if pointer type, and if so allow it to be a pointer array
    ptr_flag=1
    n_map=N_Elements(healpix_map)
    IF n_map GT 1 THEN BEGIN
        map_interp=Ptrarr(n_map)
        FOR i=0,n_map-1 DO map_interp[i]=Ptr_new(Fltarr(obs.dimension,obs.elements))
    ENDIF ELSE map_interp=Ptr_new(Fltarr(obs.dimension,obs.elements))
ENDIF ELSE BEGIN
    ptr_flag=0
    n_map=1
    map_interp=Fltarr(obs.dimension,obs.elements)
ENDELSE

IF hpx_ordering EQ 'ring' THEN pix2vec_ring,nside,hpx_inds,pix_coords $
    ELSE pix2vec_nest,nside,hpx_inds,pix_coords
    

vec2ang,pix_coords,pix_dec,pix_ra,/astro
IF coord_sys EQ 'galactic' THEN glactc,pix_ra,pix_dec,2000.,pix_ra,pix_dec,2, /degree
IF coord_sys EQ 'equatorial' THEN Hor2Eq,pix_dec,pix_ra,Jdate_use,pix_ra,pix_dec,lat=obs_hpx.lat,lon=obs_hpx.lon,alt=obs_hpx.alt,precess=1,/nutate

apply_astrometry, obs_hpx, ra_arr=pix_ra, dec_arr=pix_dec, x_arr=xv_hpx, y_arr=yv_hpx, /ad2xy

hpx_i_use=where((xv_hpx GT 0) AND (xv_hpx LT (dimension-1)) AND (yv_hpx GT 0) AND (yv_hpx LT (elements-1)),n_hpx_use) 
IF n_hpx_use EQ 0 THEN BEGIN
    print,"Error: Map has no valid Healpix indices"
    RETURN,map_interp
ENDIF
xv_hpx=xv_hpx[hpx_i_use]
yv_hpx=yv_hpx[hpx_i_use]

image_mask=Fltarr(dimension,elements)
image_mask[Min(Floor(xv_hpx)):Max(Ceil(xv_hpx)),Min(Floor(yv_hpx)):Max(Ceil(yv_hpx))]=1

x_frac=1.-(xv_hpx-Floor(xv_hpx))
y_frac=1.-(yv_hpx-Floor(yv_hpx))
IF Keyword_Set(from_kelvin) THEN pixel_area_cnv=convert_kelvin_jansky(1.,nside=nside,freq=obs.freq_center) $
    ELSE IF keyword_set(from_jy_per_sr) THEN pixel_area_cnv=(obs.degpix*!DtoR)^2. $ ; (steradian/new pixel)
        ELSE pixel_area_cnv=((obs.degpix*!DtoR)^2.)/(4.*!Pi/n_hpx) ; (steradian/new pixel)/(steradian/old pixel)

area_ratio=(4.*!Pi/n_hpx)/((obs.degpix*!DtoR)^2.)
weights_img=fltarr(dimension,elements)
weights_img[Floor(xv_hpx),Floor(yv_hpx)]+=x_frac*y_frac
weights_img[Floor(xv_hpx),Ceil(yv_hpx)]+=x_frac*(1-y_frac)
weights_img[Ceil(xv_hpx),Floor(yv_hpx)]+=(1-x_frac)*y_frac
weights_img[Ceil(xv_hpx),Ceil(yv_hpx)]+=(1-x_frac)*(1-y_frac)
FOR map_i=0,n_map-1 DO BEGIN
    model_img=fltarr(dimension,elements)
    IF Ptr_flag THEN hpx_vals=(*healpix_map[map_i])[hpx_i_use] ELSE hpx_vals=healpix_map[hpx_i_use] 
    hpx_vals*=pixel_area_cnv ;convert to Jy/pixel for the new Healpix pixels
    
    model_img[Floor(xv_hpx),Floor(yv_hpx)]+=x_frac*y_frac*hpx_vals
    model_img[Floor(xv_hpx),Ceil(yv_hpx)]+=x_frac*(1-y_frac)*hpx_vals
    model_img[Ceil(xv_hpx),Floor(yv_hpx)]+=(1-x_frac)*y_frac*hpx_vals
    model_img[Ceil(xv_hpx),Ceil(yv_hpx)]+=(1-x_frac)*(1-y_frac)*hpx_vals
    model_img*=weight_invert(weights_img)
    
    mask=fltarr(dimension,elements)
    mask[radec_i]=1
    interp_i=where((model_img EQ 0) AND (mask GT 0),n_interp)
    IF n_interp GT 0 THEN BEGIN
        fraction_int=n_interp/Total(mask)
        min_valid=4.
        min_width=Ceil(2.*Sqrt(min_valid/(!Pi*fraction_int)))>3.
        model_int=model_img
        model_int[interp_i]=!Values.F_NAN
        model_filtered=Median(model_int,min_width,/even)
        i_nan=where(Finite(model_filtered,/nan),n_nan)
        iter=0
        WHILE n_nan GT 0 DO BEGIN
            IF iter GT 5 THEN BREAK
            nan_x=i_nan mod dimension
            nan_y=Floor(i_nan/dimension)
            width_use=Ceil(min_width*(1.+iter)/2.)
            FOR i=0L,n_nan-1 DO model_filtered[i_nan[i]]=Median(model_filtered[(nan_x[i]-width_use)>0:(nan_x[i]+width_use)<(dimension-1),(nan_y[i]-width_use)>0:(nan_y[i]+width_use)<(elements-1)],/even)
            i_nan=where(Finite(model_filtered,/nan),n_nan)
            iter+=1
        ENDWHILE
        IF n_nan GT 0 THEN model_filtered[i_nan]=0.
        model_img[interp_i]=model_filtered[interp_i]
    ENDIF
    model_uv = fft_shift(FFT(model_img * image_mask))

    ; Zero pad or truncate to the true orthoslant dimension
    dim_out = obs.dimension
    ele_out = obs.elements
    IF dim_out LT dimension THEN BEGIN
        ; Truncate
        model_uv_full = model_uv[dimension/2. - dim_out/2.: dimension/2. + dim_out/2.-1,$
                                   elements/2. - ele_out/2.: elements/2. + ele_out/2.-1 ]
    ENDIF ELSE BEGIN
        ; Zero-pad
        model_uv_full = Complexarr(dim_out,ele_out)
        model_uv_full[dim_out/2. - dimension/2.: dim_out/2. + dimension/2.-1,$
            ele_out/2. - elements/2.: ele_out/2. + elements/2. -1] = model_uv
    ENDELSE
    model_img = FFT(fft_shift(model_uv_full),/inverse)
    
    IF Ptr_flag THEN *map_interp[map_i]=model_img ELSE map_interp=model_img
ENDFOR

;pixel_area_factor=pixel_area(obs,/relative)
;IF Ptr_flag THEN BEGIN
;    FOR p_i=0L,N_Elements(map_interp)-1 DO *map_interp[p_i]*=weight_invert(pixel_area_factor)
;ENDIF ELSE map_interp*=weight_invert(pixel_area_factor)

RETURN,map_interp
END
