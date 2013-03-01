PRO projection_slant_orthographic,obs,ra_arr,dec_arr,astr=astr,valid_i=valid_i,$
    degpix=degpix,obsra=obsra,obsdec=obsdec,zenra=zenra,zendec=zendec,$
    dimension=dimension,elements=elements,obsx=obsx,obsy=obsy,$
    zenx=zenx,zeny=zeny,phasera=phasera,phasedec=phasedec

;extract values from obs
IF Keyword_Set(obs) THEN BEGIN
    degpix=obs.degpix
    obsra=obs.obsra
    obsdec=obs.obsdec
    zenra=obs.zenra
    zendec=obs.zendec
    dimension=obs.dimension
    elements=obs.elements
;    rotation=obs.rotation
    obsx=obs.obsx
    obsy=obs.obsy
    astr=obs.astr
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
;    IF N_Elements(rotation) EQ 0 THEN rotation=0.
    IF N_Elements(obsra) EQ 0 THEN obsra=0.
    IF N_Elements(obsdec) EQ 0 THEN obsdec=0.
    IF N_Elements(phasera) EQ 0 THEN phasera=obsra
    IF N_Elements(phasedec) EQ 0 THEN phasedec=obsdec
    IF N_Elements(zenra) EQ 0 THEN zenra=obsra
    IF N_Elements(zendec) EQ 0 THEN zendec=obsdec    
ENDELSE

IF Abs(obsra-zenra) GT 90. THEN lon_offset=obsra-((obsra GT zenra) ? 360.:(-360.))-zenra ELSE lon_offset=obsra-zenra

lat_offset=-(zendec-obsdec)

zenith_ang=angle_difference(obsdec,obsra,zendec,zenra,/degree)
;parallactic_ang_argument=((Sin(zendec*!DtoR)-Sin(obsdec*!DtoR))*Cos(zenith_ang*!DtoR))/(Cos(obsdec*!DtoR)*Sin(zenith_ang*!DtoR))
;IF parallactic_ang_argument GT 1 THEN parallactic_ang_argument=1.-(parallactic_ang_argument-1.)
hour_angle=lon_offset
;parallactic angle from http://www.astron.nl/aips++/docs/glossary/p.html#parallactic_angle with lambda=zendec, h=hour_angle, delta=obsdec
;minus sign??
parallactic_ang=-!Radeg*atan(-sin(!DtoR*hour_angle),cos(!DtoR*obsdec)*tan(!DtoR*zendec)-sin(!DtoR*obsdec)*cos(!DtoR*hour_angle))
;parallactic_ang=Atan(parallactic_ang_argument<1)*!RaDeg ;

;rotation=0.;parallactic_ang;*2.;(parallactic_ang-90.);*2
;parallactic_ang=(parallactic_ang-90.)
xi=-Tan(zenith_ang*!DtoR)*Sin(parallactic_ang*!DtoR)
eta=Tan(zenith_ang*!DtoR)*Cos(parallactic_ang*!DtoR)

projection_name='SIN'
;image is centered at obsra, obsdec, but projection is centered at zenra, zendec

CTYPE=['RA---'+projection_name,'DEC--'+projection_name]
IF N_Elements(degpix) EQ 2 THEN delt=degpix ELSE delt=[degpix,degpix]
cd=[[1.,0.],[0.,1.]]
;;cd=[[Cos(rotation*!DtoR),Sin(rotation*!DtoR)],[-Sin(rotation*!DtoR),Cos(rotation*!DtoR)]]
PV2_1=xi
PV2_2=eta
;PV2_1=eta
;PV2_2=xi
x_c=obsx
y_c=obsy
lon_c=obsra
lat_c=obsdec
MAKE_ASTR, astr, CD = cd , DELT = delt, CRPIX = [x_c+1.,y_c+1.], $
    CRVAL = [lon_c,lat_c], CTYPE = CTYPE, PV2=[PV2_1,PV2_2],$
    LATPOLE = 0., LONGPOLE = 180.

IF (phasera NE obsra) OR (phasedec NE obsdec) THEN BEGIN
    ad2xy,phasera,phasedec,astr,phasex,phasey
    dx=obsx-phasex
    dy=obsy-phasey
    obsx-=dx
    obsy-=dy
    IF Keyword_Set(obs) THEN BEGIN
        obs.obsx=obsx
        obs.obsy=obsy
    ENDIF
    MAKE_ASTR, astr, CD = cd , DELT = delt, CRPIX = [x_c+1.,y_c+1.], $
        CRVAL = [lon_c,lat_c], CTYPE = CTYPE, PV2=[PV2_1,PV2_2],$
        LATPOLE = 0., LONGPOLE = 180.
ENDIF

ad2xy,zenra,zendec,astr,zenx,zeny

IF Keyword_Set(obs) THEN BEGIN
    obs.zenx=zenx
    obs.zeny=zeny
ENDIF

IF arg_present(ra_arr) THEN BEGIN
    xvals=meshgrid(dimension,elements,1)
    yvals=meshgrid(dimension,elements,2)
    valid_i=where(Finite(ra_arr),complement=invalid_i,ncomplement=n_invalid)
    IF n_invalid GT 0 THEN BEGIN
        ra_arr[invalid_i]=0
        dec_arr[invalid_i]=0
    ENDIF
ENDIF
END