PRO obs2radec,obs,ra,dec,missing=missing,xvals=xvals,yvals=yvals,i_use=i_use,AltAz=AltAz

dimension=obs.dimension
elements=obs.elements
astr=obs.astr
IF N_Elements(xvals) EQ 0 THEN xv=meshgrid(dimension,elements,1) ELSE xv=xvals
IF N_Elements(yvals) EQ 0 THEN yv=meshgrid(dimension,elements,2) ELSE yv=yvals
ra=(dec=fltarr(dimension,elements))
IF Keyword_Set(missing) THEN BEGIN ra+=missing & dec+=missing & ENDIF

xy2ad,xv,yv,astr,ra_vals,dec_vals

i_use=where(Finite(ra_vals),n_use)

IF n_use GT 0 THEN BEGIN
    ra_vals=ra_vals[i_use]
    dec_vals=dec_vals[i_use]
    IF Keyword_Set(AltAz) THEN Eq2Hor,ra_vals,dec_vals,replicate(obs.Jd0,n_use),dec_vals,ra_vals,lat=obs.lat,lon=obs.lon,alt=obs.alt,precess=1
    ra[i_use]=ra_vals
    dec[i_use]=dec_vals
    
    
ENDIF

END