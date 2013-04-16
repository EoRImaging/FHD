FUNCTION globalskymodel_read,frequency,ra_arr=ra_arr,dec_arr=dec_arr
;gl supplied galactic longitude (or RA if celestial_coord is set)
;gb supplied galactic latitude (or Dec if celestial_coord is set)
;returns the model temperatures from the Global Sky Model at the specified galactic longitude and latitude
IF N_Elements(frequency) EQ 0 THEN frequency=300. ;MHz
file_path_base=filepath('',root=rootdir('mwa'),sub=['DATA','Galaxy model','gsm'])
;the first time the file is read in, convert it to FITS format (MUCH faster to read when called again later!)
IF file_test(file_path_base+'components.fits') EQ 0 THEN BEGIN
    textfast,component_list,/read,file_path=file_path_base+'components.dat',extension=0
    textfast,maps_408,/read,file_path=file_path_base+'component_maps_408locked.dat',extension=0
    Fitsfast,component_list,/write,file_path=file_path_base+'components'
    Fitsfast,maps_408,/write,file_path=file_path_base+'component_maps_408locked'
ENDIF
Fitsfast,component_list,/read,file_path=file_path_base+'components'
Fitsfast,maps_408,/read,file_path=file_path_base+'component_maps_408locked'

n_freq=N_Elements(frequency)
ncomp=3.
freq10_list=ALOG10(reform(component_list[0,*]))
freq10=ALOG10(frequency)
component_arr=component_list[1:ncomp,*]
norm_arr=Reform(component_list[ncomp+1,*])
norm=(interpol(norm_arr,freq10_list,freq10,/spline))[0]
components=fltarr(n_freq,ncomp)
FOR j=0L,ncomp-1 DO FOR fi=0L,n_freq-1 DO components[fi,j]=interpol(component_arr[j,*],freq10_list,freq10[fi],/spline)

npix=(size(maps_408,/dimension))[1] ;should equal 12.*512^2.
nside=Sqrt(npix/12.)
Temperature=Reform(components#maps_408)*norm

radec_i=where(Finite(ra_arr))
ra_use=ra_arr[radec_i]
dec_use=dec_arr[radec_i]
GlactC,ra_use,dec_use,2000.,gl_use,gb_use,1,/degree

theta=(gb_use+90.)*!DtoR
phi=gl_use*!DtoR
ang2pix_ring, nside, theta, phi, ipring
model=Ptrarr(n_freq)
FOR fi=0L,n_freq-1 DO BEGIN
    model0=fltarr(size(ra_arr,/dimension))
    model0[radec_i]=Temperature[fi,ipring]
    model[fi]=Ptr_new(model0)
ENDFOR
RETURN,model
END