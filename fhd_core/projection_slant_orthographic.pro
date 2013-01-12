PRO projection_slant_orthographic,x,y,lon,lat,astr,x_c=x_c,y_c=y_c,lon_c=lon_c,lat_c=lat_c,$
    rotation=rotation,pixel_scale=pixel_scale,projection_name=projection_name,$
    inverse=inverse,dimension=dimension,elements=elements,lon_offset=lon_offset,lat_offset=lat_offset,cd=cd
;Use this program to convert between projected x,y and spherical coordinates using a slant orthographic projection
;values can be supplied as a regular array, or as vectors of values. If vectors, then dimension and elements must be supplied
;(x,y) center of pixel coordinates is defined by x_c and y_c
;(lon,lat) of x_c and y_c given by lon_c and lat_c
;pixel_scale is degrees/pixel 
;input and output are in degrees
;set keyword inverse to convert instead from (lon,lat) to (x,y)
;by default, the line of constant longitude at the center is parallel to the x axis at the center. 
;   Set rotation= to rotate the line of constant longitude counter-clockwise at the center by the supplied degrees (or radians if /radians is set)
;
IF N_Elements(inverse) EQ 0 THEN inverse=0
IF N_Elements(radians) EQ 0 THEN radians=0
IF N_Elements(lon_offset) EQ 0 THEN lon_offset=0
IF N_Elements(lat_offset) EQ 0 THEN lat_offset=0
IF N_Elements(pixel_scale) EQ 0 THEN pixel_scale=1. ELSE pixel_scale=Float(pixel_scale)
IF N_Elements(dimension) EQ 0 THEN IF Keyword_Set(inverse) $
    THEN dimension=(size(lon,/dimension))[0] $
    ELSE dimension=(size(x,/dimension))[0]
IF N_Elements(elements) EQ 0 THEN IF Keyword_Set(inverse) $
    THEN elements=(size(lat,/dimension))[1] $
    ELSE elements=(size(y,/dimension))[1]
IF N_Elements(x_c) EQ 0 THEN x_c=dimension/2.
IF N_Elements(y_c) EQ 0 THEN y_c=elements/2.
IF N_Elements(rotation) EQ 0 THEN rotation=0.
rotation_rad=rotation*!DtoR
IF N_Elements(cd) NE 4 THEN cd=[[cos(rotation_rad),-sin(rotation_rad)],[sin(rotation_rad),cos(rotation_rad)]]
IF N_Elements(projection_name) EQ 0 THEN projection_name='SIN' ELSE projection_name=StrUpCase(projection_name)

CTYPE=['RA---'+projection_name,'DEC--'+projection_name]
IF N_Elements(pixel_scale) EQ 2 THEN delt=pixel_scale ELSE delt=[pixel_scale,pixel_scale]

;CTYPE=['RA---SIN','DEC--SIN']
;PV2_1=Sin(lon_offset*!DtoR)/Tan((90.-lat_offset)*!DtoR)
;PV2_2=-Cos(lon_offset*!DtoR)/Tan((90.-lat_offset)*!DtoR)
;PV2_1=-Sin(lon_offset*!DtoR)/Tan((90.-lat_offset)*!DtoR)
;PV2_2=-Cos(lon_offset*!DtoR)/Tan((90.-lat_offset)*!DtoR)
PV2_1=0.
PV2_2=0.
MAKE_ASTR, astr, CD = cd , DELT = delt, CRPIX = [x_c+1.,y_c+1.], $
    CRVAL = [lon_c,lat_c], CTYPE = CTYPE, PV2=[PV2_1,PV2_2],$
    LATPOLE = 0., LONGPOLE = 180.
;MAKE_ASTR, astr, CD = cd , DELT = delt, CRPIX = [x_c,y_c], $
;    CRVAL = [lon_c,lat_c], CTYPE = CTYPE, PV2=[PV2_1,PV2_2],$
;    LATPOLE = 0., LONGPOLE = 180.
;precess
IF Keyword_Set(inverse) THEN ad2xy,lon,lat,astr,x,y $
                        ELSE xy2ad,x,y,astr,lon,lat
END
