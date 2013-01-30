PRO projection_slant_orthographic,obs,ra_arr,dec_arr,astr=astr,valid_i=valid_i,$
    degpix=degpix,obsra=obsra,obsdec=obsdec,zenra=zenra,zendec=zendec,$
    dimension=dimension,elements=elements,obsx=obsx,obsy=obsy,$
    zenx=zenx,zeny=zeny

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

zenith_ang=angle_difference(obsdec,obsra,zendec,zenra,/degree)
parallactic_ang=ASin(((Sin(zendec*!DtoR)-Sin(obsdec*!DtoR))*Cos(zenith_ang*!DtoR))/(Cos(obsdec*!DtoR)*Sin(zenith_ang*!DtoR)))*!RaDeg

        dec_diff=obsdec-zendec
        lon_diff=lon_offset*Cos(zendec*!DtoR)
;        rotation=-Float(Atan(Sin(Double(lon_diff*!DtoR)),(Cos(Double(lon_diff*!DtoR))*Cos(Double(obsdec*!DtoR))))*!RaDeg) ;calculated from rotation matrices
;        rotation*=Sin(zendec*!DtoR)*Cos(dec_diff*!DtoR)^2

rotation=0.;parallactic_ang;*2.;(parallactic_ang-90.);*2
;parallactic_ang=(parallactic_ang-90.)
eta=-Tan(zenith_ang*!DtoR)*Sin(parallactic_ang*!DtoR)
xi=Tan(zenith_ang*!DtoR)*Cos(parallactic_ang*!DtoR)

projection_name='SIN'
;image is centered at obsra, obsdec, but projection is centered at zenra, zendec

CTYPE=['RA---'+projection_name,'DEC--'+projection_name]
;;;delt=[degpix/Cos(zendec*!DtoR),degpix]
;;delt=[degpix,degpix]
;;;cd=[[1.,0.],[0.,1.]]
;;cd=[[Cos(rotation*!DtoR),-Sin(rotation*!DtoR)],[Sin(rotation*!DtoR),Cos(rotation*!DtoR)]]
;;PV2_1=xi
;;PV2_2=eta
;;x_c=dimension/2.
;;y_c=elements/2.
;;lon_c=obsra
;;lat_c=obsdec
;;MAKE_ASTR, zen_astr, CD = cd , DELT = delt, CRPIX = [x_c+1.,y_c+1.], $
;;    CRVAL = [lon_c,lat_c], CTYPE = CTYPE, PV2=[PV2_1,PV2_2],$
;;    LATPOLE = 0., LONGPOLE = 180.
;;
;;ad2xy,zenra,zendec,zen_astr,zenx,zeny    
;
;;dq_u=(Cos(obsdec*!DtoR)*Sin(obsra*!DtoR)-Cos(zendec*!DtoR)*Sin(zenra*!DtoR))
;;dq_v=-(Cos(obsdec*!DtoR)*Cos(obsra*!DtoR)-Cos(zendec*!DtoR)*Cos(zenra*!DtoR))
;;dq_w=Sin(obsdec*!DtoR)-Sin(zendec*!DtoR)
;dq_u=Sin(dec_diff*!DtoR)*Cos(lon_diff*!DtoR)
;dq_v=Sin(dec_diff*!DtoR)*Sin(lon_diff*!DtoR)
;dq_w=Cos(dec_diff*!DtoR);-1
;dx=!RaDeg*(dq_u+xi*dq_w)/degpix
;dy=!RaDeg*(dq_v-eta*dq_w)/degpix
;zenx=obsx-dx
;zeny=obsy-dy
;
;IF Keyword_Set(obs) THEN BEGIN
;    obs.zenx=zenx
;    obs.zeny=zeny
;ENDIF
;delt=[degpix/Cos(zendec*!DtoR),degpix]
delt=[degpix,degpix]
;cd=[[1.,0.],[0.,1.]]
cd=[[Cos(rotation*!DtoR),Sin(rotation*!DtoR)],[-Sin(rotation*!DtoR),Cos(rotation*!DtoR)]]
PV2_1=xi
PV2_2=eta
x_c=obsx
y_c=obsy
lon_c=obsra
lat_c=obsdec
MAKE_ASTR, astr, CD = cd , DELT = delt, CRPIX = [x_c+1.,y_c+1.], $
    CRVAL = [lon_c,lat_c], CTYPE = CTYPE, PV2=[PV2_1,PV2_2],$
    LATPOLE = 0., LONGPOLE = 180.

ad2xy,zenra,zendec,astr,zenx,zeny
IF Keyword_Set(obs) THEN BEGIN
    obs.zenx=zenx
    obs.zeny=zeny
ENDIF
;print,zenx,zeny
;xy2ad,xvals,yvals,astr,ra_arr,dec_arr
;ad2xy,obsra,obsdec,astr,obsx_test,obsy_test
;print,obsx-obsx_test,obsy-obsy_test
;xy2ad,zenx,zeny,astr,zenra_test,zendec_test
;print,zenra-zenra_test,zendec-zendec_test

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