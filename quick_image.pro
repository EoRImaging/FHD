pro quick_image, image, xvals, yvals, data_range = data_range, xrange = xrange, yrange = yrange, $
    log=log, color_profile = color_profile, xtitle = xtitle, ytitle = ytitle, title = title, $
    note = note, charsize = charsize, xlog = xlog, ylog = ylog, window_num = window_num, $
    missing_value = missing_value, noerase = noerase, savefile = savefile, png = png, eps = eps
    
  if n_elements(window_num) eq 0 then window_num = 1
  
  if n_elements(savefile) gt 0 or keyword_set(png) or keyword_set(eps) then pub = 1 else pub = 0
  if pub eq 1 then begin
    if not (keyword_set(png) or keyword_set(eps)) then begin
      basename = cgRootName(savefile, directory=directory, extension=extension)
      
      case extension of
        'eps': eps=1
        'png': png=1
        '': png = 1
        else: begin
          print, 'Unrecognized extension, using png'
          png = 1
        end
      endcase
      
    endif
    if n_elements(savefile) eq 0 then begin
      if keyword_set(eps) then savefile = 'idl_quick_image.eps' else savefile = 'idl_quick_image'
      cd, current = current_dir
      print, 'no filename specified for quick_image output. Using ' + current_dir + path_sep() + savefile
    endif
    
    if keyword_set(png) and keyword_set(eps) then begin
      print, 'both eps and png cannot be set, using png'
      eps = 0
    endif
    
    if keyword_set(eps) then delete_ps = 0 else delete_ps = 1
  endif
  
  ;; precaution in case a slice is passed in but it still appears > 2d (ie shallow dimension)
  image = reform(image)
  if n_elements(size(image, /dim)) ne 2 then begin
    print, 'image must be 2 dimensional'
    return
  endif
  
  if max(abs(imaginary(image))) gt 0 then begin
    print, 'image is complex, showing real part'
    image = real_part(image)
  endif
  
  if keyword_set(missing_value) then begin
    good_locs = where(image ne missing_value, count_good, complement = wh_missing, ncomplement = count_missing)
    erase = 1
  endif else good_locs = indgen(n_elements(image))
  
  tvlct, r, g, b, /get
  
  if keyword_set(log) then begin
  
    log_color_calc, image, plot_image, cb_ticks, cb_ticknames, color_range, n_colors, data_range = data_range, $
      color_profile = color_profile, log_cut_val = log_cut_val, oob_low = oob_low, $
      missing_value = missing_value, missing_color = missing_color
      
      
  endif else begin
    if n_elements(data_range) eq 0 then data_range = minmax(image[good_locs])
    
    cgloadct, 25, /brewer, /reverse, BOTTOM = 0, NCOLORS = 256, clip = [0, 235]
    
    color_range = [0, 255]
    if n_elements(missing_value) ne 0 then begin
      if count_missing gt 0 then begin
        missing_color = 255
        data_color_range = [0, 254]
      endif else data_color_range = color_range
    endif else data_color_range = color_range
    n_colors = color_range[1] - color_range[0] + 1
    data_n_colors = data_color_range[1] - data_color_range[0] + 1
    
    plot_image = (image-data_range[0])*data_n_colors/(data_range[1]-data_range[0]) + data_color_range[0]
    
    wh_low = where(image lt data_range[0], count_low)
    if count_low gt 0 then plot_image[wh_low] = data_color_range[0]
    wh_high = where(image gt data_range[1], count_high)
    if count_high gt 0 then plot_image[wh_high] = data_color_range[1]
    
    if n_elements(missing_value) ne 0 then if count_missing gt 0 then plot_image[wh_missing] = missing_color
    
    tickinterval = float(number_formatter((data_range[1]-data_range[0])/6., format = '(e13.0)'))
  endelse
  
  if n_elements(xvals) gt 1 then xrange = minmax(xvals)
  if n_elements(yvals) gt 1 then yrange = minmax(yvals)
  
  if n_elements(xlog) ne 0 then axkeywords = create_struct('xlog', 1, 'xtickformat', 'exponent')
  if n_elements(ylog) ne 0 then $
    if n_elements(axkeywords) ne 0 then axkeywords = create_struct(axkeywords, 'ylog', 1, 'ytickformat', 'exponent') $
  else axkeywords = create_struct('ylog', 1, 'ytickformat', 'exponent')
  
  plot_pos = [.15,.15,.8,.92]
  cb_pos = [.92, .15,.95,.92]
  
  plot_len = [plot_pos[2]-plot_pos[0], plot_pos[3] - plot_pos[1]]
  if min(plot_len) le 0 then stop
  
  plot_aspect = (plot_pos[3] - plot_pos[1]) / (plot_pos[2] - plot_pos[0])
  
  if n_elements(xvals) gt 0 and n_elements(yvals) gt 0 then begin
    xlength = xrange[1] - xrange[0]
    ylength = yrange[1]-yrange[0]
    data_aspect = float(ylength / xlength)
  endif else data_aspect=1
  
  aspect_ratio =  data_aspect /plot_aspect
  if aspect_ratio gt 1 then begin
    y_factor = aspect_ratio
    x_factor = 1.
  endif else begin
    y_factor = 1.
    x_factor = 1./aspect_ratio
  endelse
  
  max_ysize = 1000
  max_xsize = 1200
  base_size = 600
  
  if keyword_set(pub) then begin
    ps_aspect = y_factor / x_factor
    
    if ps_aspect lt 1 then landscape = 1 else landscape = 0
    IF Keyword_Set(eps) THEN landscape = 0
    sizes = cgpswindow(LANDSCAPE=landscape, aspectRatio = ps_aspect, /sane_offsets)
    
    if n_elements(missing_value) gt 0 then begin
      if not keyword_set(noerase) then begin
        if windowavailable(window_num) then begin
          wset, window_num
          if !d.x_size ne xsize or !d.y_size ne ysize then make_win = 1 else make_win = 0
        endif else make_win = 1
        if make_win eq 1 then window, window_num, xsize = xsize, ysize = ysize
        
        cgerase
      endif
      
      alphabackgroundimage = cgsnapshot()
    endif
    
    cgps_open, savefile, /font, encapsulated=eps, /nomatch, inches=sizes.inches, xsize=sizes.xsize, ysize=sizes.ysize, $
      xoffset=sizes.xoffset, yoffset=sizes.yoffset, landscape = landscape
      
  endif else begin
    xsize = round(base_size * x_factor)
    ysize = round(base_size * y_factor)
    while (ysize gt max_ysize) or (xsize gt max_xsize) do begin
      base_size = base_size - 100
      xsize = round(base_size * x_factor)
      ysize = round(base_size * y_factor)
    endwhile
    
    if windowavailable(window_num) then begin
      wset, window_num
      if !d.x_size ne xsize or !d.y_size ne ysize then make_win = 1 else make_win = 0
    endif else make_win = 1
    if make_win eq 1 then window, window_num, xsize = xsize, ysize = ysize
    
    if n_elements(missing_value) gt 0 and not keyword_set(noerase) then cgerase
    
  endelse
  
  cgimage, plot_image, position = plot_pos, /axes, xrange = xrange, $
    yrange = yrange, xtitle = xtitle, ytitle = ytitle, title = title, axkeywords = axkeywords, missing_value = missing_color, noerase = noerase, $
    alphabackgroundimage = alphabackgroundimage, charsize = charsize
    
  if keyword_set(log) then begin
    cgcolorbar, /vertical, position = cb_pos, bottom = color_range[0], ncolors = n_colors, minor = 0, $
      ticknames = cb_ticknames, ytickv = cb_ticks, yticks = n_elements(cb_ticks) -1, $
      charsize = charsize, font = font, oob_low = oob_low
      
  endif else begin
    cgcolorbar, range=data_range, position = cb_pos, /vertical, format='exponent', charsize = charsize, font = font
  endelse
  
  if n_elements(note) ne 0 then begin
    if keyword_set(pub) then char_factor = 0.75 else char_factor = 1
    cgtext, .99, 0.02, note, /normal, alignment=1, charsize = char_factor*charsize, color = annotate_color, font = font
  endif
  
  
  if keyword_set(pub) then begin
    cgps_close, png = png, delete_ps = delete_ps, density=600
    if make_win eq 1 then wdelete, window_num
  endif
  
  tvlct, r, g, b
  
  
end
