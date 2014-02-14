;; set log = 1 to get a log color scale
;; for a log color scale that is symmetric around zero, set log=1 and color_profile='sym_log'

pro healpix_quickimage, data, pixels, nside, slice_ind = slice_ind, ordering=ordering, map_out = map_out, noplot = noplot, $
    lat_range = lat_range, lon_range = lon_range, noerase = noerase, savefile = savefile, png = png, eps = eps, $
    map = map, $;mollwiede=mollwiede,cartesian=cartesian,gnomic=gnomic,orthographic=orthographic,_Extra=extra,$
    data_range = data_range, max=max,min=min, silent=silent, title=title, charsize = charsize, degpix=degpix, hist_equal=hist_equal,$
    lon_center=lon_center, lat_center=lat_center, projection=projection, coord_in=coord_in, coord_out = coord_out, $
    color_profile = color_profile, log = log
    
    
  IF N_Elements(silent) EQ 0 THEN silent=1
  IF N_Elements(nside) NE 1 THEN begin
    print, 'nside must be specified'
    return
  endif
  
  if n_elements(data_range) eq 0 then data_range = minmax(data)
  
  data_dims = size(data, /dimension)
  if n_elements(data_dims) gt 1 then begin
    if n_elements(slice_ind) eq 0 then slice_ind = 0 else if slice_ind ge data_dims[1] then begin
      print, 'slice_ind is too large for cube, Defaulting to max value (' + number_formatter(data_dims[1]-1) + ').'
      slice_ind = data_dims[1] - 1
    endif
    
    if n_elements(title) eq 0 then title = 'slice: ' + number_formatter(slice_ind)
    data_plot = data[*,slice_ind]
  endif else begin
    data_plot = data
    if n_elements(title) eq 0 then title = ''
  endelse
  
  if n_elements(ordering) eq 0 then ordering='ring'
  case strlowcase(ordering) of
    'ring': pix_type='R'
    'nested': pix_type='N'
  endcase
  
  if n_elements(coord_in) eq 0 then coord_in='C'
  
  if not keyword_set(map) then projection = 'orthographic'
  
  IF N_Elements(projection) EQ 0 THEN BEGIN
    CASE 1 OF
      Keyword_Set(orthographic):projection='orthographic'
      Keyword_Set(mollwiede):projection='mollwiede'
      Keyword_Set(gnomic):projection='gnomic'
      Keyword_Set(cartesian):projection='cartesian'
      ELSE:projection='orthographic'
    ENDCASE
  ENDIF
  
  case projection of
    'orthographic': proj_routine = 'data2orth'
    'mollwiede': proj_routine = 'data2moll'
    'gnomic': proj_routine = 'data2gnom'
    'cartesian': proj_routine = 'data2cart'
  endcase
  
  hpx_res = sqrt(3./!pi)*(60./nside)
  
  IF Keyword_Set(degpix) THEN resolution=degpix ELSE resolution=.1 ;output resolution in degrees
  
  if keyword_set(map) then begin
    half_sky = 1
    pxsize=360./resolution
    charsize=Ceil(pxsize/800.)
    IF projection eq 'orthographic' THEN if keyword_set(half_sky) then pxsize/=4. else pxsize/=2.
  endif else pxsize = 360.*2/resolution
  
  case strlowcase(ordering) of
    'ring': pix2vec_ring, nside, pixels, pix_center_vec
    'nested': pix2vec_nest, nside, pixels, pix_center_vec
  endcase
  
  ;; find mid point (work in x/y because of possible jumps in phi)
  vec_mid = [mean(pix_center_vec[*,0]), mean(pix_center_vec[*,1]), mean(pix_center_vec[*,2])]
  theta_vals = acos(pix_center_vec[*,2])
  phi_vals = atan(pix_center_vec[*,1], pix_center_vec[*,0])
  theta0 = acos(vec_mid[2])
  phi0 = atan(vec_mid[1], vec_mid[0])
  
  IF Keyword_Set(lon_center) THEN lon0=lon_center ELSE lon0=phi0*180./!pi
  IF Keyword_Set(lat_center) THEN lat0=lat_center ELSE begin
    lat0=theta0*180./!pi
    if lat0 le 90 then lat0 = lat0 + 90
    if lat0 gt 90 then lat0 = lat0*(-1)+90
  end
  
  rot=[lon0,lat0]
  
  hsize_cm=26.
  
  
  
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
  
  Call_procedure,proj_routine, $
    data_plot, 0, pix_type, nside, do_conv, do_rot, coord_in_use, coord_out_use, eul_mat, $
    planmap, Tmax, Tmin, color_bar, $
    PXSIZE=pxsize, HIST_EQUAL=hist_equal, MAX=max, MIN=min, $
    UNITS='Jy', DATA_plot = data_plot, GAL_CUT=gal_cut, $
    POLARIZATION=polarization, HALF_SKY=half_sky, SILENT=silent, PIXEL_LIST=pixels, $
    TRUECOLORS=truecolors, DATA_TC=data_tc, MAP_OUT=map_out, ROT=rot, FITS=fits, STAGGER=stagger
    
  if not keyword_set(map) then begin
    mask = fix(map_out*0)
    mask[where(map_out gt min(map_out))]=1
    
    xrange_map = minmax(where(total(mask,2) gt 0))
    yrange_map = minmax(where(total(mask,1) gt 0))
    
    bad_val = min(map_out)
    map_out = map_out[xrange_map[0]:xrange_map[1], yrange_map[0]:yrange_map[1]]
    
    lat_vals = (theta_vals*180./!pi)*(-1)+90.
    lat_range = minmax(lat_vals)
    
    lon_vals = (phi_vals*180./!pi)*(-1)
    wh_west = where(lon_vals le -180., count_west)
    if count_west gt 0 then lon_vals[wh_west] = lon_vals[wh_west] + 360
    lon_range = reverse(minmax(lon_vals)) ;; astro convention
    
    lon_hist = histogram(lon_vals, binsize = 10, min=-180, max=180, locations=locs)
    wh_lon_0 = where(lon_hist eq 0, count_lon_0, ncomplement = count_lon_inc)
    if count_lon_0 eq 0 then full_lon = 1 else full_lon = 0
    
    if lon_range[0] - lon_range[1] gt count_lon_inc*10 then begin
      ;; longitude branch cut
      lon_range = reverse(lon_range)
      lon_range[1] = lon_range[1] - 360.
    endif
    
    if full_lon eq 1 then begin
      ;; pole or full sky
      if lat_range[1] - lat_range[0] lt 90. then begin
        ;; pole
        print, 'map wraps pole, lat/lon ranges are likely to be wrong'
      endif else begin
        ;; full sky
        print, 'map covers more than one hemisphere, lat/lon ranges are likely to be wrong'
      endelse
    endif
    
    if not keyword_set(noplot) then begin
      quick_image, map_out, missing_val = bad_val, data_range = data_range, xtitle = 'longitude (degrees)', ytitle = 'latitude (degrees)', $
        title = title, charsize = charsize, noerase = noerase, xrange = lon_range, yrange = lat_range, savefile = savefile, png = png, eps = eps, $
        log = log, color_profile = color_profile
    endif
  endif else begin
  
    ;  Call_procedure,proj_routine,file_path+'.fits',_Extra=extra,max=max,min=min,png=png_filename,ps=ps_filename,$
    ;    retain=1,silent=silent,transparent=1,title=title,asinh=logplot,hist_equal=hist_equal,preview=0,$
    ;    rot=rot,graticule=20.,charsize=charsize,pxsize=pxsize,glsize=1.,window=-1,units='Jy',colt=color_table;,Coord=['C'];,hxsize=hsize_cm
  
    if not keyword_set(noplot) then begin
      IF Keyword_Set(png) THEN png_filename=file_path+'.png' ELSE png_filename=0
      IF Keyword_Set(eps) THEN ps_filename=file_path+'.ps' ELSE ps_filename=0
      
      proj2out, $
        planmap, Tmax, Tmin, color_bar, 0., title, $
        'Jy', coord_out, do_rot, eul_mat, $
        CHARSIZE=charsize, COLT=colt, CROP=crop, GIF = gif, GRATICULE = 20., $
        HXSIZE=hxsize, NOBAR = nobar, NOLABELS = nolabels, PNG = png_filename, PREVIEW = 0, PS=ps_filename, PXSIZE=pxsize, $
        SUBTITLE = subtitle, TITLEPLOT = titleplot, XPOS = xpos, YPOS = ypos, $
        POLARIZATION=polarization, OUTLINE=outline, PROJECTION=projection, FLIP=flip, HALF_SKY=half_sky, COORD_IN=coord_in, $
        IGRATICULE=igraticule, HBOUND = hbound, WINDOW = window, SILENT=silent, GLSIZE=.5, IGLSIZE=iglsize, $
        SHADEMAP=shademap, EXECUTE=execute, RETAIN=retain, TRUECOLORS=truecolors, TRANSPARENT=transparent, $
        CHARTHICK=charthick, STAGGER=stagger, JPEG=jpeg
    endif
  endelse
end