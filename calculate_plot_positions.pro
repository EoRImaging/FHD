function calculate_plot_positions, data_aspect, nplots = nplots, plot_margin = plot_margin, $
                                   no_colorbar = no_colorbar, cb_size = cb_size, cb_margin = cb_margin, $
                                   ncol = ncol, nrow = nrow, ordering = ordering

  if n_elements(data_aspect) eq 0 then message, 'data aspect must be provided'

  ;; set defaults
  if n_elements(plot_margin) eq 0 then plot_margin = [.1, .1, .02, .1] $
  else if n_elements(plot_margin) ne 4 then message, 'plot_margin must have 4 elements [x0,y0,x1,y1]'

  if not keyword_set(no_colorbar) then begin
     if n_elements(cb_size) eq 0 then cb_size = 0.03 $
     else if n_elements(cb_size) gt 1 then message, 'cb_size must be a scalar'

     if n_elements(cb_margin) eq 0 then cb_margin = [.1, .05] $
     else if n_elements(cb_margin) ne 2 then message, 'cb_margins must have 2 elements [x0,x1]'
  endif

  if n_elements(nplots) eq 0 then nplots = 1 $
  else if n_elements(nplots) gt 1 then message, 'nplots must be a scalar' $
  else if nplots eq 0 then message, 'nplots must be greater than 0'

  if nplots gt 1 then begin
     if (n_elements(ncol) ne 0) or (n_elements(nrow) ne 0) then begin       
        if n_elements(ncol) eq 0 then ncol = ceil(nplots/float(nrow))
        if n_elements(nrow) eq 0 then nrow = ceil(nplots/float(ncol))

        if nrow*ncol lt nplots then message, 'nplots is greater than ncol*nrow'
     endif else begin
        nrow = ceil(sqrt(nplots))
        ncol = ceil(nplots/float(nrow))
     endelse

     if n_elements(ordering) eq 0 then ordering = 'row'
  endif

  if not keyword_set(no_colorbar) then begin
     plot_pos = [plot_margin[0], plot_margin[1], (1-cb_margin[1]-cb_size-cb_margin[0]-plot_margin[2]), (1-plot_margin[3])]
     cb_pos = [(1-cb_margin[1]-cb_size), plot_margin[1], (1-cb_margin[1]), (1-plot_margin[3])]
     
     if min([plot_pos, cb_pos]) lt 0 or max([plot_pos, cb_pos]) gt 1 then $
        message, 'bad margins and/or cb_size -- positions are outside window'
     
     plot_len = [plot_pos[2]-plot_pos[0], plot_pos[3] - plot_pos[1], cb_pos[2]-cb_pos[0]]
     if min(plot_len) le 0 then message, 'bad margins and/or cb_size -- negative or zero lengths'
     
  endif else begin
     plot_pos = [plot_margin[0], plot_margin[1], (1-plot_margin[2]), (1-plot_margin[3])]
     cb_pos = fltarr(4)

     if min(plot_pos) lt 0 or max(plot_pos) gt 1 then $
        message, 'bad margins and/or cb_size -- positions are outside window'
     
     plot_len = [plot_pos[2]-plot_pos[0], plot_pos[3] - plot_pos[1]]
     if min(plot_len) le 0 then message, 'bad margins -- negative or zero lengths'
  endelse

  
  plot_aspect = (plot_pos[3] - plot_pos[1]) / (plot_pos[2] - plot_pos[0])

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

  if nplots gt 1 then begin
     multi_pos = fltarr(4, ncol*nrow)
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
        else: message, 'unrecognized ordering, use "col" or "row" '
     endcase

     multi_pos[0,*] = col_val/double(ncol)
     multi_pos[1,*] = row_val/double(nrow)
     multi_pos[2,*] = (col_val+1)/double(ncol)
     multi_pos[3,*] = (row_val+1)/double(nrow)

     ;; define window size based on aspect ratio
     base_size_use = base_size
     xsize = round(base_size * x_factor * ncol)
     ysize = round(base_size * y_factor * nrow)
     while (ysize gt max_ysize) or (xsize gt max_xsize) do begin
        if base_size_use gt 200 then base_size_use = base_size_use - 100 else base_size_use = round(base_size_use / 1.25)
        xsize = round(base_size_use * x_factor * ncol)
        ysize = round(base_size_use * y_factor * nrow)
     endwhile
     if xsize eq 0 or ysize eq 0 then stop

     multi_xlen = (multi_pos[2,0]-multi_pos[0,0])
     multi_ylen = (multi_pos[3,0]-multi_pos[1,0])
     multi_center = [[reform(multi_pos[0,*] + multi_xlen/2d)], [reform(multi_pos[1,*] + multi_ylen/2d)]]



     multi_aspect = xsize*multi_xlen/float(ysize*multi_ylen)

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
     new_multi = [[multi_center[*,0] - new_xlen/2d], [multi_center[*,1] - new_ylen*y_factor/2d], $
                  [multi_center[*,0] + new_xlen/2d], [multi_center[*,1] + new_ylen*y_factor/2d]]
          
     new_pos = [[new_xlen * plot_pos[0] + new_multi[*,0]], [new_ylen * plot_pos[1] + new_multi[*,1]], $
                [new_xlen * plot_pos[2] + new_multi[*,0]], [new_ylen * plot_pos[3] + new_multi[*,1]]]
     
     plot_pos = new_pos

     if not keyword_set(no_colorbar) then begin
        new_cb_pos = [[new_xlen * cb_pos[0] + new_multi[*,0]], [new_ylen * cb_pos[1] + new_multi[*,1]], $
                      [new_xlen * cb_pos[2] + new_multi[*,0]], [new_ylen * cb_pos[3] + new_multi[*,1]]]   

        cb_pos = new_cb_pos
     endif
  endif else begin
     xsize = round(base_size * x_factor)
     ysize = round(base_size * y_factor)
     
     if not keyword_set(pub) then begin
        while (ysize gt max_ysize) or (xsize gt max_xsize) do begin
           base_size = base_size - 100
           xsize = round(base_size * x_factor)
           ysize = round(base_size * y_factor)
        endwhile
     endif
  endelse
  
  position_struct = {plot_pos:plot_pos, cb_pos: cb_pos, wsize:[xsize, ysize]}
  return, position_struct

end
