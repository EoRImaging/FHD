PRO projection_slant_orthographic,obs,astr=astr,valid_i=valid_i,$
    degpix=degpix,obsra=obsra,obsdec=obsdec,zenra=zenra,zendec=zendec,$
    dimension=dimension,elements=elements,obsx=obsx,obsy=obsy,$
    zenx=zenx,zeny=zeny,phasera=phasera,phasedec=phasedec,$
    epoch=epoch,JDate=JDate

;extract values from obs
IF Keyword_Set(obs) THEN BEGIN
    degpix=obs.degpix
    obsra=obs.obsra
    obsdec=obs.obsdec
    zenra=obs.zenra
    zendec=obs.zendec
    dimension=obs.dimension
    elements=obs.elements
    obsx=obs.obsx
    obsy=obs.obsy
    astr=obs.astr
    ad2xy,zenra,zendec,astr,zenx,zeny
    obs.zenx=zenx
    obs.zeny=zeny
    RETURN
ENDIF ELSE BEGIN
    IF N_Elements(elements) EQ 0 THEN elements=dimension
    IF N_Elements(obsx) EQ 0 THEN obsx=dimension/2.
    IF N_Elements(obsy) EQ 0 THEN obsy=elements/2.
    IF N_Elements(obsra) EQ 0 THEN obsra=0.
    IF N_Elements(obsdec) EQ 0 THEN obsdec=0.
    IF N_Elements(phasera) EQ 0 THEN phasera=obsra
    IF N_Elements(phasedec) EQ 0 THEN phasedec=obsdec
    IF N_Elements(zenra) EQ 0 THEN zenra=obsra
    IF N_Elements(zendec) EQ 0 THEN zendec=obsdec    
ENDELSE

IF N_Elements(epoch) EQ 0 THEN epoch=2000.
IF N_Elements(JDate) EQ 0 THEN JDate=Julday(1,1,2000)

IF Abs(phasera-zenra) GT 90. THEN lon_offset=phasera-((phasera GT zenra) ? 360.:(-360.))-zenra $
    ELSE lon_offset=phasera-zenra

lat_offset=-(zendec-phasedec)

zenith_ang=angle_difference(phasedec,phasera,zendec,zenra,/degree)
hour_angle=lon_offset
parallactic_ang = parallactic_angle(dec=phasedec, hour_angle=hour_angle, latitude=zendec)

xi=-Tan(zenith_ang*!DtoR)*Sin(parallactic_ang*!DtoR)
eta=Tan(zenith_ang*!DtoR)*Cos(parallactic_ang*!DtoR)

projection_name='SIN'

CTYPE=['RA---'+projection_name,'DEC--'+projection_name]
IF N_Elements(degpix) EQ 2 THEN delt=degpix ELSE delt=[degpix,degpix]
cd=[[1.,0.],[0.,1.]]
PV2_1=Double(xi)
PV2_2=Double(eta)
x_c=obsx
y_c=obsy
lon_c=phasera
lat_c=phasedec
MAKE_ASTR, astr, CD = Float(cd) , DELT = Float(delt), CRPIX = Float([x_c+1.,y_c+1.]), $
    CRVAL = Double([lon_c,lat_c]), CTYPE = CTYPE, PV2=[PV2_1,PV2_2],$
    LATPOLE = 0., LONGPOLE = 180.,EQUINOX=epoch,MJD_OBS=JDate-2400000.5,$
    DATE_OBS=date_conv(JDate,'FITS'),NAXIS=Float([dimension,elements]),axes=[1,2]

ad2xy,zenra,zendec,astr,zenx,zeny

END
