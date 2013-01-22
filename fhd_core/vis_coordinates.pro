;+
; :Description:
;    Calculates RA and Dec for every pixel of an image. 
;    
;    Uses a slant orthographic projection about (zenra, zendec) centered at (obsra, obsdec) 
;    
;    NOTE: Updates obs to contain correct x and y pixel coordinates of zenith
;
; :Params:
;    obs
;    ra_arr
;    dec_arr
;
; :Keywords:
;    astr - structure containing standard astrometry data
;
; :Author: isullivan 2012
;-
PRO vis_coordinates,obs,ra_arr,dec_arr,astr=astr,valid_i=valid_i,$
    degpix=degpix,obsra=obsra,obsdec=obsdec,zenra=zenra,zendec=zendec,$
    dimension=dimension,elements=elements,rotation=rotation,obsx=obsx,obsy=obsy,$
    zenx=zenx,zeny=zeny,zen_astr=zen_astr

;extract values from obs
IF Keyword_Set(obs) THEN BEGIN
    degpix=obs.degpix
    obsra=obs.obsra
    obsdec=obs.obsdec
    zenra=obs.zenra
    zendec=obs.zendec
    dimension=obs.dimension
    elements=obs.elements
    rotation=obs.rotation
    obsx=obs.obsx
    obsy=obs.obsy
    astr=(*obs.bin).astr
    xvals=meshgrid(dimension,elements,1)
    yvals=meshgrid(dimension,elements,2)
    xy2ad,xvals,yvals,astr,ra_arr,dec_arr
    ad2xy,zenra,zendec,astr,zenx,zeny
    obs.zenx=zenx
    obs.zeny=zeny
    RETURN
ENDIF ELSE BEGIN
    IF N_Elements(elements) EQ 0 THEN elements=dimension
    IF N_Elements(obsx) EQ 0 THEN obsx=dimension/2.
    IF N_Elements(obsy) EQ 0 THEN obsy=elements/2.
    IF N_Elements(rotation) EQ 0 THEN rotation=0.
    IF N_Elements(obsra) EQ 0 THEN obsra=0.
    IF N_Elements(obsdec) EQ 0 THEN obsdec=0.
    IF N_Elements(zenra) EQ 0 THEN zenra=obsra
    IF N_Elements(zendec) EQ 0 THEN zendec=obsdec    
ENDELSE

IF Abs(obsra-zenra) GT 90. THEN lon_offset=obsra-((obsra GT zenra) ? 360.:(-360.))-zenra ELSE lon_offset=obsra-zenra

lat_offset=-(zendec-obsdec)

;degpix_use=[Cos(lon_offset*!DtoR/Cos(obsdec*!DtoR)),Cos(lat_offset*!DtoR)]*degpix
degpix_use=[Cos(lon_offset*!DtoR*Cos(obsdec*!DtoR)),Cos(lat_offset*!DtoR)]*degpix
;degpix_use=[Cos(lon_offset*!DtoR),Cos(lat_offset*!DtoR)]*degpix
;degpix_use=[1.,Cos(lat_offset*!DtoR)]*degpix
;degpix_use=replicate(degpix,2)*Cos(lat_offset*!DtoR)
;degpix_use=replicate(degpix,2)

projection='SIN'
;image is centered at obsra, obsdec, but projection is centered at zenra, zendec
xvals=meshgrid(dimension,elements,1)
yvals=meshgrid(dimension,elements,2)
projection_slant_orthographic,xvals,yvals,raz_arr,decz_arr,zen_astr,x_c=obsx,y_c=obsy,$
    lon_c=zenra,lat_c=zendec,rotation=rotation,pixel_scale=degpix_use,proj=projection,$
    lon_offset=lon_offset,lat_offset=lat_offset
    
ad2xy,obsra,obsdec,zen_astr,xcen,ycen    
zenx=dimension-xcen
zeny=elements-ycen

IF Keyword_Set(obs) THEN BEGIN
    obs.zenx=zenx
    obs.zeny=zeny
ENDIF
projection_slant_orthographic,xvals,yvals,ra_arr,dec_arr,astr,x_c=zenx,y_c=zeny,$
    lon_c=zenra,lat_c=zendec,rotation=rotation,pixel_scale=degpix_use,proj=projection,$
    lon_offset=lon_offset,lat_offset=lat_offset
valid_i=where(Finite(ra_arr),complement=invalid_i,ncomplement=n_invalid)
IF n_invalid GT 0 THEN BEGIN
    ra_arr[invalid_i]=0
    dec_arr[invalid_i]=0
ENDIF
END