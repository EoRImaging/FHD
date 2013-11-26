pro healpix_quickimage, data, pixels, nside, ordering=ordering, mollwiede=mollwiede,cartesian=cartesian,gnomic=gnomic,orthographic=orthographic,_Extra=extra,$
    max=max,min=min,png_write=png_write,ps_write=ps_write,silent=silent,title=title,degpix=degpix,logplot=logplot,hist_equal=hist_equal,$
    lon_center=lon_center,lat_center=lat_center,color_table=color_table,projection=projection, half_sky = half_sky, coord_in=coord_in, coord_out = coord_out
    
    
  IF N_Elements(silent) EQ 0 THEN silent=1
  IF N_Elements(nside) NE 1 THEN print, 'nside must be specified', RETURN
  
  if n_elements(ordering) eq 0 then ordering='ring'
  case strlowcase(ordering) of
    'ring': pix_type='R'
    'nested': pix_type='N'
  endcase
  
  if n_elements(coord_in) eq 0 then coord_in='C'
  
  IF N_Elements(projection) EQ 0 THEN BEGIN
    CASE 1 OF
      Keyword_Set(orthographic):projection='orthographic'
      Keyword_Set(mollwiede):projection='mollwiede'
      Keyword_Set(gnomic):projection='gnomic'
      Keyword_Set(cartesian):projection='cartesian'
      ELSE:projection='orthographic'
    ENDCASE
  ENDIF
  
  IF Keyword_Set(degpix) THEN resolution=degpix*60. ELSE resolution=4. ;output resolution in arcminutes
  IF N_Elements(color_table) EQ 0 THEN color_table=33
  
  pxsize=360.*60./resolution
  charsize=Ceil(pxsize/800.)
  IF projection eq 'orthographic' THEN pxsize/=2.
  
  ;IF Keyword_Set(half_sky) THEN pxsize/=2.
  IF Keyword_Set(lon_center) THEN lon0=lon_center ELSE lon0=0.
  IF Keyword_Set(lat_center) THEN lat0=lat_center ELSE lat0=0.
  
  rot=[lon0,lat0]
  
  hsize_cm=26.
  IF Keyword_Set(png_write) THEN png_filename=file_path+'.png' ELSE png_filename=0
  IF Keyword_Set(ps_write) THEN ps_filename=file_path+'.ps' ELSE ps_filename=0
  
  
  
  loadsky                         ; cgis package routine, define rotation matrices
  do_fullsky = ~keyword_set(half_sky)
  if N_elements(coord_out) EQ 0 then coord_out = coord_in
  
  if coord_in  EQ 'C' then coord_in_use =  'Q'  ; cgis skyconv coding convention for celestial/equatorial
  if coord_out EQ 'C' then coord_out_use = 'Q'
  if (~keyword_set(silent)) then print,'input file : ',decode_coord(coord_in_use)+' coordinates'
  if (~keyword_set(silent)) then print,'plot coord : ',decode_coord(coord_out_use)+' coordinates'
  do_conv = (coord_in_use NE coord_out_use)
  
  
  rot_ang = ([rot,0.])
  eul_mat = euler_matrix_new(rot_ang(0), -rot_ang(1), rot_ang(2), /Deg, /ZYX)
  do_rot = (TOTAL(ABS(rot_ang)) GT 1.e-5)
  
  data2orth, $
    data, 0, pix_type, nside, do_conv, do_rot, coord_in_use, coord_out_use, eul_mat, $
    planmap, Tmax, Tmin, color_bar, $
    PXSIZE=pxsize, LOG=log, HIST_EQUAL=hist_equal, MAX=max, MIN=min, $
    UNITS='Jy', DATA_plot = data_plot, GAL_CUT=gal_cut, $
    POLARIZATION=polarization, HALF_SKY=half_sky, SILENT=silent, PIXEL_LIST=pixels, ASINH=logplot, $
    DO_SHADE=do_shade, SHADEMAP=shademap, $
    TRUECOLORS=truecolors, DATA_TC=data_tc, MAP_OUT=map_out, ROT=rot, FITS=fits, STAGGER=stagger
    
  
  
;  Call_procedure,proj_routine,file_path+'.fits',_Extra=extra,max=max,min=min,png=png_filename,ps=ps_filename,$
;    retain=1,silent=silent,transparent=1,title=title,asinh=logplot,hist_equal=hist_equal,preview=0,$
;    rot=rot,graticule=20.,charsize=charsize,pxsize=pxsize,glsize=1.,window=-1,units='Jy',colt=color_table;,Coord=['C'];,hxsize=hsize_cm
    
    proj2out, $
  planmap, Tmax, Tmin, color_bar, 0., 'test', $
  'Jy', coord_out, do_rot, eul_mat, $
  CHARSIZE=charsize, COLT=color_table, CROP=crop, GIF = gif, GRATICULE = 20., $
  HXSIZE=hxsize, NOBAR = nobar, NOLABELS = nolabels, PNG = png_filename, PREVIEW = 0, PS=ps_filename, PXSIZE=pxsize, $
  SUBTITLE = subtitle, TITLEPLOT = titleplot, XPOS = xpos, YPOS = ypos, $
  POLARIZATION=polarization, OUTLINE=outline, /ORTH, FLIP=flip, HALF_SKY=half_sky, COORD_IN=coord_in, $
  IGRATICULE=igraticule, HBOUND = hbound, WINDOW = -1, SILENT=silent, GLSIZE=1, IGLSIZE=iglsize, $
  SHADEMAP=shademap, EXECUTE=execute, RETAIN=1, TRUECOLORS=truecolors, TRANSPARENT=1, $
  CHARTHICK=charthick, STAGGER=stagger, JPEG=jpeg
stop
    
end