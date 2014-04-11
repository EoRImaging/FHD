FUNCTION globalskymodel_read,frequency,ra_arr=ra_arr,dec_arr=dec_arr,components=components,haslam_filtered=haslam_filtered,_Extra=extra
;gl supplied galactic longitude (or RA if celestial_coord is set)
;gb supplied galactic latitude (or Dec if celestial_coord is set)
;returns the model temperatures from the Global Sky Model at the specified galactic longitude and latitude
;output maps should be in units of K*steradian
IF N_Elements(frequency) EQ 0 THEN frequency=300. ;MHz

file_path_base=filepath('',root=rootdir('FHD'),sub='catalog_data')
IF Keyword_Set(haslam_filtered) THEN BEGIN
    print,"Using 408 MHz filtered Haslam map"
    Fitsfast,Temperature,header,/read,file_path=file_path_base+'lambda_haslam408_dsds'  ;temperature in K    
    model_freq=408. ;MHz 
    spectral_index=-0.8
    
    npix=N_Elements(Temperature) ;should equal 12.*512^2.
    nside=npix2nside(npix)
    pix_area=4.*!Pi/npix
    
    radec_i=where(Finite(ra_arr))
    ra_use=ra_arr[radec_i]
    dec_use=dec_arr[radec_i]
    
    GlactC,ra_use,dec_use,2000.,gl_use,gb_use,1,/degree
    ang2vec,gb_use,gl_use,vec_use,/astro
;    ang2vec,dec_use,ra_use,vec_use,/astro
    vec2pix_nest,nside,vec_use,ipring
    
    Temperature=Temperature[ipring]*pix_area*(model_freq/mean(frequency))^spectral_index
    model0=fltarr(size(ra_arr,/dimension))
    model0[radec_i]=Temperature
    model=Ptr_new(model0)
    RETURN,model 
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