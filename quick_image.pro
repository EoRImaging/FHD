pro quick_image, image, xvals, yvals, data_range = data_range, xrange = xrange, yrange = yrange, data_aspect=data_aspect, $
    log=log, color_profile = color_profile, xtitle = xtitle, ytitle = ytitle, title = title, $
    note = note, charsize = charsize_in, xlog = xlog, ylog = ylog, window_num = window_num, $
    multi_pos = multi_pos, start_multi_params = start_multi_params, no_ps_close = no_ps_close, $
    alphabackgroundimage = alphabackgroundimage, missing_value = missing_value, $
    noerase = noerase, savefile = savefile, png = png, eps = eps, pdf = pdf
    
  if n_elements(window_num) eq 0 then window_num = 1
  
  if n_elements(savefile) gt 0 or keyword_set(png) or keyword_set(eps) or keyword_set(pdf) then pub = 1 else pub = 0
  if pub eq 1 then begin
    if not (keyword_set(png) or keyword_set(eps) or keyword_set(pdf)) then begin
      basename = cgRootName(savefile, directory=directory, extension=extension)
      
      case extension of
        'eps': eps=1
        'png': png=1
        'pdf': pdf=1
        '': png = 1
        else: begin
          print, 'Unrecognized extension, using png'
          png = 1
        end
      endcase
      
    endif
    if n_elements(savefile) eq 0 and n_elements(multi_pos) eq 0 then begin
      savefile = 'idl_quick_image'
      cd, current = current_dir
      print, 'no filename specified for quick_image output. Using ' + current_dir + path_sep() + savefile
    endif
    
    n_ext_set = 0
    if keyword_set(png) then n_ext_set +=1
    if keyword_set(pdf) then n_ext_set +=1
    if keyword_set(eps) then n_ext_set +=1
    if n_ext_set gt 1 then begin
      print, 'only one of eps, png, pdf keywords can be set, using png'
      eps = 0
      pdf = 0
    endif
    
    if keyword_set(png) then begin
      plot_exten = '.png'
      delete_ps = 1
    endif else if keyword_set(pdf) then begin
      plot_exten = '.pdf'
      delete_ps = 1
    endif else if keyword_set(eps) then begin
      plot_exten = '.eps'
      delete_ps = 0
    endif
    
    savefile = savefile + plot_exten
  endif
  
  if n_elements(start_multi_params) gt 0 and n_elements(multi_pos) gt 0 then message, 'If start_multi_params are passed, ' + $
    'multi_pos cannot be passed because then it is used as an output to pass back the positions for future plots.'
    
  if n_elements(multi_pos) gt 0 then begin
    if n_elements(multi_pos) ne 4 then message, 'multi_pos must be a 4 element plot position vector'
    if max(multi_pos) gt 1 or min(multi_pos) lt 0 then message, 'multi_pos must be in normalized coordinates (between 0 & 1)'
    if multi_pos[2] le multi_pos[0] or multi_pos[3] le multi_pos[1] then $
      message, 'In multi_pos, x1 must be greater than x0 and y1 must be greater than y0 '
  endif
  
  if n_elements(image) eq 0 then begin
    print, 'image is undefined'
    return
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
  endif
  
  tvlct, r, g, b, /get
  
  if keyword_set(log) then begin
  
    log_color_calc, image, plot_image, cb_ticks, cb_ticknames, color_range, n_colors, data_range = data_range, $
      color_profile = color_profile, log_cut_val = log_cut_val, oob_low = oob_low, $
      missing_value = missing_value, missing_color = missing_color
      
      
  endif else begin
    if n_elements(data_range) eq 0 then $
      if keyword_set(missing_value) then data_range = minmax(image[good_locs]) else data_range = minmax(image)
      
    if abs(data_range[0] - data_range[1]) lt 1e-15 and data_range[0] ne 0 then data_range = minmax([data_range[0], 0])
    
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
    
    plot_image = (image-data_range[0])*(data_n_colors-1)/(data_range[1]-data_range[0]) + data_color_range[0]
    
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
  
  
  
  ;; Work out plot & colorbar positions
  ;; in units of plot area (incl. margins)
  if n_elements(cb_size_in) eq 0 then cb_size = 0.025 else cb_size = cb_size_in
  if n_elements(margin_in) lt 4 then begin
    margin = [0.2, 0.2, 0.02, 0.1]
  endif else margin = margin_in
  
  if n_elements(cb_margin_in) lt 2 then begin
    cb_margin = [0.2, 0.02]
  ;; if units_str ne '' then begin
  ;;    cb_margin = [0.2, 0.02]
  ;; endif else begin
  ;;    ;; no label on colorbar in this case
  ;;    cb_margin = [0.1, 0.02]
  ;; endelse
  endif else cb_margin = cb_margin_in
  
  plot_pos = [margin[0], margin[1], (1-cb_margin[1]-cb_size-cb_margin[0]-margin[2]), (1-margin[3])]
  cb_pos = [(1-cb_margin[1]-cb_size), margin[1], (1-cb_margin[1]), (1-margin[3])]
  
  plot_len = [plot_pos[2]-plot_pos[0], plot_pos[3] - plot_pos[1]]
  if min(plot_len) le 0 then stop
  
  plot_aspect = (plot_pos[3] - plot_pos[1]) / (plot_pos[2] - plot_pos[0])
  
  ;  plot_pos = [.15,.15,.8,.92]
  ;  cb_pos = [.92, .15,.95,.92]
  
  if n_elements(data_aspect) eq 0 then begin
    if n_elements(xvals) gt 0 and n_elements(yvals) gt 0 then begin
      xlength = xrange[1] - xrange[0]
      ylength = yrange[1]-yrange[0]
      data_aspect = ylength / float(xlength)
      max_data_aspect = 3.
      if data_aspect gt max_data_aspect or data_aspect lt 1./max_data_aspect then begin
        print, 'Calculated data_aspect exceeds reasonable plotting aspect ratios, limiting to reasonable values. (data_aspect can also be set as a keyword)'
        if data_aspect gt max_data_aspect then data_aspect = max_data_aspect
        if data_aspect lt 1./max_data_aspect then data_aspect = 1./max_data_aspect
      endif
    endif else data_aspect=1
  endif
  
  aspect_ratio =  data_aspect /plot_aspect
  if aspect_ratio gt 1 then begin
    y_factor = aspect_ratio
    x_factor = 1.
  endif else begin
    y_factor = 1.
    x_factor = 1./aspect_ratio
  endelse
  
  screen_size = get_screen_size()
  max_xsize = screen_size[0]
  max_ysize = screen_size[1]
  base_size = 600
  
  if n_elements(multi_pos) eq 4 then begin
    ;; work out positions scaled to the area allowed in multi_pos with proper aspect ratio
    multi_xlen = (multi_pos[2]-multi_pos[0])
    multi_ylen = (multi_pos[3]-multi_pos[1])
    multi_center = [multi_pos[0] + multi_xlen/2d, multi_pos[1] + multi_ylen/2d]
    
    multi_size = [!d.x_vsize*multi_xlen, !d.y_vsize*multi_ylen]
  endif
  
  
  if n_elements(multi_pos) eq 4 or n_elements(start_multi_params) gt 0 then begin
    if n_elements(start_multi_params) gt 0 then begin
      ;; calculate desired window size and positions for all plots
      ncol = start_multi_params.ncol
      nrow = start_multi_params.nrow
      
      multi_pos = fltarr(4, ncol*nrow)
      
      if tag_exist(start_multi_params, 'ordering') eq 0 then ordering = 'row' $
      else ordering = start_multi_params.ordering
      
      case ordering of
        'col': begin
          ;; col-major values
          col_val = reform(rebin(reform(indgen(ncol), 1, ncol), nrow, ncol), ncol*nrow)
          row_val = reverse(reform(rebin(indgen(nrow), nrow, ncol), ncol*nrow))
        end
        'row': begin
          ;; row-major values
          col_val = reform(rebin(indgen(ncol), ncol, nrow), ncol*nrow)
          row_val = reverse(reform(rebin(reform(indgen(nrow), 1, nrow), ncol, nrow), ncol*nrow))
        end
        else: message, 'unrecognized ordering value in start_multi_params, use "col" or "row" '
      endcase
      
      multi_pos[0,*] = col_val/double(ncol)
      multi_pos[1,*] = row_val/double(nrow)
      multi_pos[2,*] = (col_val+1)/double(ncol)
      multi_pos[3,*] = (row_val+1)/double(nrow)
      
      ;; define window size based on aspect ratio
      base_size_use = base_size
      xsize = round(base_size * x_factor * double(ncol))
      ysize = round(base_size * y_factor * double(nrow))
      if not keyword_set(pub) then begin
        while (ysize gt max_ysize) or (xsize gt max_xsize) do begin
          if base_size_use gt 100 then base_size_use = base_size_use - 100 else base_size_use = base_size_use * .75
          xsize = round(base_size_use * x_factor * double(ncol))
          ysize = round(base_size_use * y_factor * double(nrow))
        endwhile
      endif
      
      ;; if pub is set, start ps output
      if keyword_set(pub) then begin
        ps_aspect = (y_factor * float(nrow)) / (x_factor * float(ncol))
        
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
        ;; make or set window
        if windowavailable(window_num) then begin
          wset, window_num
          if !d.x_size ne xsize or !d.y_size ne ysize then make_win = 1 else make_win = 0
        endif else make_win = 1
        if make_win eq 1 then window, window_num, xsize = xsize, ysize = ysize
        
        if n_elements(missing_value) gt 0 and not keyword_set(noerase) then cgerase
      endelse
      
      ;; calculate multi_size & multi x/ylen not calculated earlier
      multi_xlen = (multi_pos[2,0]-multi_pos[0,0])
      multi_ylen = (multi_pos[3,0]-multi_pos[1,0])
      multi_center = [multi_pos[0,0] + multi_xlen/2d, multi_pos[1,0] + multi_ylen/2d]
      
      multi_size = [!d.x_vsize*multi_xlen, !d.y_vsize*multi_ylen]
      
      multi_pos_use = multi_pos[*,0]
    endif else multi_pos_use = multi_pos
    
    multi_aspect = multi_size[1]/float(multi_size[0])
    
    new_aspect = aspect_ratio/multi_aspect
    if new_aspect gt 1 then begin
      y_factor = 1.
      x_factor = 1/new_aspect
    endif else begin
      y_factor = new_aspect
      x_factor = 1.
    endelse
    
    new_xlen = multi_xlen*x_factor
    new_ylen = multi_ylen*y_factor
    new_multi = [multi_center[0] - new_xlen/2d, multi_center[1] - new_ylen*y_factor/2d, $
      multi_center[0] + new_xlen/2d, multi_center[1] + new_ylen*y_factor/2d]
      
    new_pos = [new_xlen * plot_pos[0] + new_multi[0], new_ylen * plot_pos[1] + new_multi[1], $
      new_xlen * plot_pos[2] + new_multi[0], new_ylen * plot_pos[3] + new_multi[1]]
      
    new_cb_pos = [new_xlen * cb_pos[0] + new_multi[0], new_ylen * cb_pos[1] + new_multi[1], $
      new_xlen * cb_pos[2] + new_multi[0], new_ylen * cb_pos[3] + new_multi[1]]
      
    plot_pos = new_pos
    cb_pos = new_cb_pos
    
    no_erase = 1
  endif else begin
    base_size_use = base_size
    xsize = round(base_size_use * x_factor)
    ysize = round(base_size_use * y_factor)
    
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
      while (ysize gt max_ysize) or (xsize gt max_xsize) do begin
        if base_size_use gt 100 then base_size_use = base_size_use - 100 else base_size_use = base_size_use * .75
        xsize = round(base_size_use * x_factor)
        ysize = round(base_size_use * y_factor)
      endwhile
      
      
      if windowavailable(window_num) then begin
        wset, window_num
        if !d.x_size ne xsize or !d.y_size ne ysize then make_win = 1 else make_win = 0
      endif else make_win = 1
      if make_win eq 1 then window, window_num, xsize = xsize, ysize = ysize
      
      if n_elements(missing_value) gt 0 and not keyword_set(noerase) then cgerase
    endelse
    
    no_erase = 0
  endelse
  
  if keyword_set(pub) then begin
    font = 1
    if n_elements(charsize_in) eq 0 then begin
      if n_elements(multi_pos) gt 0 then begin
        charsize = 1d * (mean(multi_size)/10000.)
      endif else charsize = 2
    endif else charsize = charsize_in
    
  endif else begin
    font = -1
    if n_elements(charsize_in) eq 0 then begin
      if n_elements(multi_pos) gt 0 then begin
        charsize = 1.7d * (multi_size[0]/float(base_size_use))
      endif else charsize = 2
    endif else charsize = charsize_in
    
  endelse
  
  cgimage, plot_image, position = plot_pos, /axes, xrange = xrange, $
    yrange = yrange, xtitle = xtitle, ytitle = ytitle, title = title, axkeywords = axkeywords, missing_value = missing_color, noerase = noerase, $
    alphabackgroundimage = alphabackgroundimage, charsize = charsize, font = font
    
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
    if n_elements(multi_pos) eq 0 and not keyword_set(no_ps_close) then cgps_close, png = png, pdf = pdf, delete_ps = delete_ps, density=600
    if n_elements(make_win) gt 0 then if make_win eq 1 then wdelete, window_num
  endif
  
  tvlct, r, g, b
  
  
end
