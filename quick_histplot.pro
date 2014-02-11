pro quick_histplot, data, binsize = binsize, logdata = logdata, loghist = loghist, min = min_val, max = max_val, range = range, $
    ytitle = ytitle, _REF_EXTRA=extra
    
    
  if n_elements(range) gt 1 then begin
    min_val = min(range)
    max_val = max(range)
  endif
  
  if keyword_set(logdata) then begin
    if max(data) le 0 then begin
      print, "logdata cannot be set because data is all 0 or negative"
      return
    endif
    if min(data) le 0 then begin
      print, 'data has some 0 or negative values, removing those values because logdata is set'
      wh_le0 = where(data le 0, count_le0, complement = wh_good)
      if count_le0 gt 0 then data_use = data[wh_good] else stop
      data_use = alog10(data_use)
    endif else data_use = alog10(data)
  endif else data_use = data
  
  if n_elements(binsize) eq 0 then binsize = (3.5 * stdev(data_use)) / n_elements(data_use)^(0.3333)
  if n_elements(ytitle) eq 0 then ytitle = 'Histogram count'
  
  hist = histogram(data_use, binsize = binsize, locations = locs, max = max_val, min = min_val, omin = min_val, omax = max_val)
  nbins = n_elements(hist)
  
  locs_plot = [locs[0]-binsize/2., locs[0]-binsize/2., locs, locs[nbins-1]+binsize/2.,locs[nbins-1]+binsize/2.]
  if keyword_set(logdata) then locs_plot = 10.^locs_plot
  hist_plot = [0, hist[0], hist, hist[nbins-1], 0]
  
  if keyword_set(plot_range) then xstyle=1
  cgplot, locs_plot, hist_plot, xlog=logdata, ylog=loghist, psym=10, ytitle = ytitle, _EXTRA=extra
  
end
