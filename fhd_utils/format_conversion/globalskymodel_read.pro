FUNCTION globalskymodel_read,obs,frequency,components=components,haslam_filtered=haslam_filtered,_Extra=extra
;gl supplied galactic longitude (or RA if celestial_coord is set)
;gb supplied galactic latitude (or Dec if celestial_coord is set)
;returns the model temperatures from the Global Sky Model at the specified galactic longitude and latitude
;output maps should be in units of Kelvin
IF N_Elements(frequency) EQ 0 THEN frequency=300. ;MHz

dimension=obs.dimension
elements=obs.elements
astr=obs.astr
degpix=obs.degpix
n_pol=obs.n_pol
apply_astrometry, obs, x_arr=meshgrid(dimension, elements, 1), y_arr=meshgrid(dimension, elements, 2), ra_arr=ra_arr, dec_arr=dec_arr, /xy2ad

file_path_base=filepath('',root=rootdir('FHD'),sub='catalog_data')
IF Keyword_Set(haslam_filtered) THEN BEGIN
    print,"Using 408 MHz filtered Haslam map"
    Fitsfast,Temperature,header,/read,file_path=file_path_base+'lambda_haslam408_dsds'  ;temperature in K    
    model_freq=408. ;MHz 
    spectral_index=-0.8
    
    npix=N_Elements(Temperature) ;should equal 12.*512^2.
    nside=npix2nside(npix)
;    pix_area=4.*!Pi/npix
    
    radec_i=where(Finite(ra_arr))
    ra_use=ra_arr[radec_i]
    dec_use=dec_arr[radec_i]
    
    
;    GlactC,ra_use,dec_use,2000.,gl_use,gb_use,1,/degree
;    ang2vec,gb_use,gl_use,vec_use,/astro
;;    ang2vec,dec_use,ra_use,vec_use,/astro
;    vec2pix_nest,nside,vec_use,ipnest
    
;    Temperature=Temperature[ipnest]*pix_area*(model_freq/mean(frequency))^spectral_index
;;    Temperature=Temperature[ipnest]*(model_freq/mean(frequency))^spectral_index
;    model0=fltarr(size(ra_arr,/dimension))
;    model0[radec_i]=Temperature
;    model=Ptr_new(model0)
;    RETURN,model 
    
    pix2vec_nest,nside,Lindgen(npix),pix_coords
    vec2ang,pix_coords,pix_gb,pix_gl,/astro
    GlactC,pix_ra,pix_dec,2000.,pix_gl,pix_gb,2,/degree
    Eq2hor,pix_ra,pix_dec,obs.JD0,pix_alt,pix_az,lat=obs.lat,lon=obs.lon,alt=obs.alt, /precess, /nutate, refract=0
    
    apply_astrometry,obs, ra_arr=pix_ra, dec_arr=pix_dec, x_arr=xv_hpx, y_arr=yv_hpx, /ad2xy, /refraction
    hpx_i_use=where((xv_hpx GT 0) AND (xv_hpx LT (dimension-1)) AND (yv_hpx GT 0) AND (yv_hpx LT (elements-1)) AND (pix_alt GT 0),n_hpx_use,complement=hpx_i_cut) 
;    hpx_i_use=hpx_i_cut
    IF n_hpx_use EQ 0 THEN RETURN,Ptrarr(n_pol)
    xv_hpx=xv_hpx[hpx_i_use]
    yv_hpx=yv_hpx[hpx_i_use]
    
    x_frac=1.-(xv_hpx-Floor(xv_hpx))
    y_frac=1.-(yv_hpx-Floor(yv_hpx))
    
    weights_img=fltarr(dimension,elements)
    weights_img[Floor(xv_hpx),Floor(yv_hpx)]+=x_frac*y_frac
    weights_img[Floor(xv_hpx),Ceil(yv_hpx)]+=x_frac*(1-y_frac)
    weights_img[Ceil(xv_hpx),Floor(yv_hpx)]+=(1-x_frac)*y_frac
    weights_img[Ceil(xv_hpx),Ceil(yv_hpx)]+=(1-x_frac)*(1-y_frac)
    
    hpx_vals=Temperature[hpx_i_use]
    Temperature=fltarr(dimension,elements)
    Temperature[Floor(xv_hpx),Floor(yv_hpx)]+=x_frac*y_frac*hpx_vals
    Temperature[Floor(xv_hpx),Ceil(yv_hpx)]+=x_frac*(1-y_frac)*hpx_vals
    Temperature[Ceil(xv_hpx),Floor(yv_hpx)]+=(1-x_frac)*y_frac*hpx_vals
    Temperature[Ceil(xv_hpx),Ceil(yv_hpx)]+=(1-x_frac)*(1-y_frac)*hpx_vals
    Temperature*=weight_invert(weights_img)
    Temperature=Temperature*(model_freq/mean(frequency))^spectral_index ;scale temperature to correct frequency
    
    ;convert from Kelvin to Jy/pixel
    Jy_per_pixel=convert_kelvin_jansky(Temperature,nside=nside,frequency=frequency*1E6)
    
    mask=fltarr(dimension,elements)
    mask[radec_i]=1
    interp_i=where((Jy_per_pixel EQ 0) AND (mask GT 0),n_interp)
    IF n_interp GT 0 THEN BEGIN
        fraction_int=n_interp/Total(mask)
        min_valid=4.
        min_width=Ceil(2.*Sqrt(min_valid/(!Pi*fraction_int)))>3.
        Brightness_int=Jy_per_pixel
        Brightness_int[interp_i]=!Values.F_NAN
        Brightness_filtered=Median(Brightness_int,min_width,/even)
        i_nan=where(Finite(Brightness_filtered,/nan),n_nan)
        iter=0
        WHILE n_nan GT 0 DO BEGIN
            IF iter GT 5 THEN BREAK
            nan_x=i_nan mod dimension
            nan_y=Floor(i_nan/dimension)
            width_use=Ceil(min_width*(1.+iter)/2.)
            FOR i=0L,n_nan-1 DO Brightness_filtered[i_nan[i]]=Median(Brightness_filtered[(nan_x[i]-width_use)>0:(nan_x[i]+width_use)<(dimension-1),(nan_y[i]-width_use)>0:(nan_y[i]+width_use)<(elements-1)],/even)
            i_nan=where(Finite(Brightness_filtered,/nan),n_nan)
            iter+=1
        ENDWHILE
        IF n_nan GT 0 THEN Brightness_filtered[i_nan]=0.
        Jy_per_pixel[interp_i]=Brightness_filtered[interp_i]
    ENDIF
    RETURN,Ptr_new(Jy_per_pixel)
    
ENDIF ELSE BEGIN
    print,"Using unfiltered Global Sky Model"
    ;the first time the file is read in, convert it to FITS format (MUCH faster to read when called again later!)
    IF file_test(file_path_base+'components.fits') EQ 0 THEN BEGIN
        textfast,component_list,/read,file_path=file_path_base+'components.dat',extension=0
        textfast,maps_408,/read,file_path=file_path_base+'component_maps_408locked.dat',extension=0
        Fitsfast,component_list,/write,file_path=file_path_base+'components'
        Fitsfast,maps_408,/write,file_path=file_path_base+'component_maps_408locked'
    ENDIF
    Fitsfast,component_list,/read,file_path=file_path_base+'components'
    Fitsfast,maps_408,/read,file_path=file_path_base+'component_maps_408locked'
ENDELSE

npix=(size(maps_408,/dimension))[1] ;should equal 12.*512^2.
nside=npix2nside(npix)
pix_area=4.*!Pi/npix

radec_i=where(Finite(ra_arr))
ra_use=ra_arr[radec_i]
dec_use=dec_arr[radec_i]

GlactC,ra_use,dec_use,2000.,gl_use,gb_use,1,/degree
ang2vec,gb_use,gl_use,vec_use,/astro
;ang2vec,dec_use,ra_use,vec_use,/astro
vec2pix_ring,nside,vec_use,ipring

;GlactC,ra_use,dec_use,2000.,gl_use,gb_use,1,/degree
;
;theta=(gb_use+90.)*!DtoR
;phi=gl_use*!DtoR
;ang2pix_ring, nside, theta, phi, ipring

maps_408=maps_408[*,ipring]*pix_area
;maps_408=maps_408[*,ipring]
n_comp=(size(maps_408,/dimension))[0]
IF Keyword_Set(components) THEN BEGIN
    comp_arr=Ptrarr(n_comp)
    
    FOR ci=0,n_comp-1 DO BEGIN
        comp0=fltarr(size(ra_arr,/dimension))
        comp0[radec_i]=Reform(maps_408[ci,*])
        comp_arr[ci]=Ptr_new(comp0)
    ENDFOR
    RETURN,comp_arr
ENDIF

n_freq=N_Elements(frequency)
ncomp=3.
freq10_list=ALOG10(reform(component_list[0,*]))
freq10=ALOG10(frequency)
component_arr=component_list[1:ncomp,*]
norm_arr=Reform(component_list[ncomp+1,*])
norm=(interpol(norm_arr,freq10_list,freq10,/spline))[0]
components=fltarr(n_freq,ncomp)
FOR j=0L,ncomp-1 DO FOR fi=0L,n_freq-1 DO components[fi,j]=interpol(component_arr[j,*],freq10_list,freq10[fi],/spline)

Temperature=(components#maps_408)*norm

model=Ptrarr(n_freq)
FOR fi=0L,n_freq-1 DO BEGIN
    model0=fltarr(size(ra_arr,/dimension))
    model0[radec_i]=Temperature[fi,*]
    model[fi]=Ptr_new(model0)
ENDFOR
RETURN,model
END