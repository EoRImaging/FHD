PRO healpix_image,file_path,mollwiede=mollwiede,cartesian=cartesian,gnomic=gnomic,orthographic=orthographic,_Extra=extra,$
    max=max,min=min,png_write=png_write,ps_write=ps_write,silent=silent,title=title,degpix=degpix,logplot=logplot,hist_equal=hist_equal,$
    lon_center=lon_center,lat_center=lat_center,color_table=color_table,proj_routine=proj_routine;,half_sky=half_sky
;view healpix-gridded data. Optionally export to .png or .eps format images
IF N_Elements(silent) EQ 0 THEN silent=1
;IF N_Elements(nside) NE 1 THEN RETURN
IF N_Elements(proj_routine) EQ 0 THEN BEGIN
    CASE 1 OF
        Keyword_Set(orthographic):proj_routine='orthview'
        Keyword_Set(mollwiede):proj_routine='mollview'
        Keyword_Set(gnomic):proj_routine='gnomview'
        Keyword_Set(cartesian):proj_routine='cartview'
        ELSE:proj_routine='orthview'
    ENDCASE
ENDIF
IF Keyword_Set(degpix) THEN resolution=degpix*60. ELSE resolution=4. ;output resolution in arcminutes
IF N_Elements(color_table) EQ 0 THEN color_table=33

pxsize=360.*60./resolution
charsize=Ceil(pxsize/800.)
IF Keyword_Set(orthographic) THEN pxsize/=2.

;IF Keyword_Set(half_sky) THEN pxsize/=2.
IF Keyword_Set(lon_center) THEN lon0=lon_center ELSE lon0=0.
IF Keyword_Set(lat_center) THEN lat0=lat_center ELSE lat0=0.

rot=[lon0,lat0]

hsize_cm=26.
IF Keyword_Set(png_write) THEN png_filename=file_path+'.png' ELSE png_filename=0
IF Keyword_Set(ps_write) THEN ps_filename=file_path+'.ps' ELSE ps_filename=0
Call_procedure,proj_routine,file_path+'.fits',_Extra=extra,max=max,min=min,png=png_filename,ps=ps_filename,$
    retain=1,silent=silent,transparent=1,title=title,asinh=logplot,hist_equal=hist_equal,$
    rot=rot,graticule=20.,charsize=charsize,pxsize=pxsize,glsize=1.,window=-1,units='Jy',colt=color_table;,Coord=['C'];,hxsize=hsize_cm

END