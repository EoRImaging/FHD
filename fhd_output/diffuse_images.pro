pro diffuse_images, filenames, pols=pols, ratio = ratio, diff_ratio = diff_ratio, $
    log = log, color_profile = color_profile, data_range = data_range, window_num = window_num, $
    png = png, eps = eps, pdf = pdf, plot_path = plot_path, plotfile = plotfile
    
  if n_elements(filenames) eq 0 then message, 'filenames must be specfied'
  if n_elements(filenames) gt 2 then message, 'no more than 2 filenames can be specified'
  
  if n_elements(pols) eq 0 then pols = 'I'
  if n_elements(pols) gt 2 then message, 'no more than 2 pols can be specified'
  
  n_images = max([n_elements(pols), n_elements(filenames)])
  max_file = n_elements(filenames)-1
  max_pol = n_elements(pols)-1
  
  ;; check to see if filename has the full path
  test = file_test(filenames)
  if min(test) eq 0 then begin
  
    files = strarr(max_file+1)
    for i=0, max_file do begin
      if test[i] eq 1 then begin
        files[i] = filenames[i]
        continue
      endif
      
      case strlowcase(!version.os_family) OF
        'windows': split_delim = ';'
        'unix':    split_delim = ':'
      endcase
      path_dirs = strsplit(!path, split_delim, /extract)
      
      fhd_catalog_loc = strpos(path_dirs, 'catalog_data')
      wh_catalog = where(fhd_catalog_loc gt 0, count_catalog)
      if count_catalog gt 0 then begin
        file_path = path_dirs[wh_catalog[0]]
        ;; make sure file_path has a path separator at the end
        pos = strpos(file_path, path_sep(), /reverse_search)
        if pos+1-strlen(file_path) lt 0 then file_path = file_path + path_sep()
        
        temp = file_path + filenames[i]
        if file_test(temp) eq 1 then files[i] = temp else message, 'file not found in FHD catalog_data: ' + filenames[i]
        
      endif else print, 'Could not locate catalog_data directory in !path variable'
    endfor
  endif else files = filenames
  
  if keyword_set(png) or keyword_set(eps) or keyword_set(pdf) then pub = 1 else pub = 0
  
  if pub and (n_elements(plotfile) eq 0 or n_elements(plot_path) eq 0) then begin
    if n_elements(plot_path) eq 0 then begin
      cd, current = temp
      plot_path = temp + path_sep()
    endif
    if n_elements(plotfile) eq 0 then begin
      fname = strarr(max_file+1)
      for i=0, max_file do fname[i] = cgrootname(files[i])
      if max_file+1 eq 2 then plotfile = fname[0] + cube_op_str + fname[1] else plotfile = fname[0]
    endif
  endif
  if pub then plot_filename = plot_path + plotfile
   
  pol_list = ['I', 'Q']
  pol_inds = intarr(max_pol+1)
  for i=0, max_pol do begin
    wh_pol = where(pol_list eq pols[i], count)
    if count eq 0 then message, 'specified pol is not allowed: ' + pols[i]
    pol_inds[i] = wh_pol[0]
  endfor
  
  hpx_inds1 = getvar_savefile(files[0], 'hpx_inds')
  if n_elements(files) gt 1 then begin
    hpx_inds2 = getvar_savefile(files[1], 'hpx_inds')
    
    match, hpx_inds1, hpx_inds2, sub1, sub2, count = count_hpx_match
    if n_elements(hpx_inds1) eq n_elements(hpx_inds2) and count_hpx_match eq n_elements(hpx_inds1) then begin
      ;; everything matches
      pixels_use = hpx_inds1
      undefine, hpx_inds1, hpx_inds2
    endif else begin
      print, 'healpix pixels do not match between the 2 files, will use overlap'
      if count_hpx_match eq 0 then message, 'No overlapping healpix pixels between the 2 files'
      if count_hpx_match ne n_elements(hpx_inds1) then pixels_use = hpx_inds1[sub1]
      undefine, hpx_inds1, hpx_inds2
    endelse
  endif else pixels_use = temporary(hpx_inds1)
  
  nside1 = getvar_savefile(files[0], 'nside')
  if n_elements(files) gt 1 then begin
    nside2 = getvar_savefile(files[1], 'nside')
    if nside1 ne nside2 gt 0 then message, 'nsides do not match between the 2 files'
  endif
  
  image_varname = strarr(max_file+1)
  for i=0, max_file do begin
    void = getvar_savefile(files[i], names=varnames)
    wh_model = where(stregex(varnames, 'model', /boolean, /fold_case), count)
    if count gt 1 then print, 'more than one variable name containing model, using the first one'
    if count gt 0 then image_varname[i] = varnames[wh_model[0]] else begin
      wh_map = where(stregex(varnames, 'map', /boolean, /fold_case), count)
      if count gt 1 then print, 'more than one variable name containing map, using the first one'
      if count gt 0 then image_varname[i] = varnames[wh_map[0]] $
      else message, 'no variable name containing model or map'
    endelse
  endfor
  
  temp = getvar_savefile(files[0], image_varname[0])
  data_type = size(temp, /type)
  if data_type eq 10 then image1 = *temp[pol_inds[0]] else begin
    data_dims = size(temp,/dimension)
    if n_elements(data_dims) gt 2 then message, 'healpix image has more than 2 dimensions'
    if n_elements(data_dims) eq 1 then begin
      if pol_inds[0] eq 0 then image1 = temp else message, 'file only contains one pol (I)'
    endif else begin
      pol_dim = where(data_dims eq min(data_dims))
      if pol_dim eq 0 then image1 = temp[pol_inds[0], *] else image1 = temp[*,pol_inds[0]]
    endelse
  endelse
  
  if n_images eq 2 then begin
    if n_elements(files) eq 2 then begin
      temp  = getvar_savefile(files[max_file], image_varname[max_file])
      data_type = size(temp, /type)
      if data_type eq 10 then image2 = *temp[pol_inds[max_pol]] else begin
        data_dims = size(temp,/dimension)
        if n_elements(data_dims) gt 2 then message, 'healpix image has more than 2 dimensions'
        if n_elements(data_dims) eq 1 then begin
          if pol_inds[max_pol] eq 0 then image2 = temp else message, 'file only contains one pol (I)'
        endif else begin
          pol_dim = where(data_dims eq min(data_dims))
          if pol_dim eq 0 then image2 = temp[pol_inds[max_pol], *] else image2 = temp[*,pol_inds[max_pol]]
        endelse
      endelse
      if count_hpx_match ne n_elements(hpx_inds1) then begin
        image1 = image1[sub1]
        image2 = image2[sub2]
      endif
    endif else begin
      if data_type eq 10 then image2 = *temp[pol_inds[max_pol]] else begin
        data_dims = size(temp,/dimension)
        if n_elements(data_dims) gt 2 then message, 'healpix image has more than 2 dimensions'
        if n_elements(data_dims) eq 1 then begin
          if pol_inds[max_pol] eq 0 then image2 = temp else message, 'file only contains one pol (I)'
        endif else begin
          pol_dim = where(data_dims eq min(data_dims))
          if pol_dim eq 0 then image2 = temp[pol_inds[max_pol], *] else image2 = temp[*,pol_inds[max_pol]]
        endelse
      endelse
    endelse
  endif
  undefine_fhd, temp
  
  print, 'nside, n pixels: ' + number_formatter(nside1) + ', ' + number_formatter(n_elements(pixels_use))
  
  ;; title to use:
  if n_images gt 1 then begin
    if keyword_set(ratio) then begin
      cube_op = '/'
      cube_op_str = '_over_'
    endif else begin
      cube_op = '-'
      cube_op_str = '_minus_'
    endelse
    diff_title = pols[0] + cube_op + pols[max_pol]
    
    if max_file+1 eq 2 then note = filenames[0] + cube_op + filenames[1] else note = filenames[0]
  endif else begin
    diff_title = pols[0]
    note = filenames[0]
  endelse
  
  
  if keyword_set(png) or keyword_set(eps) or keyword_set(pdf) then pub = 1 else pub = 0
  
  if n_images gt 1 then begin
  
    if max(abs(image1-image2)) eq 0 then message, 'images are identical.'
    
    if keyword_set(diff_ratio) then begin
      print, max(image1), max(image2), max(image1)/max(image2)
      temp = (image1/max(image1) - image2/max(image2)) * mean([max(image1), max(image2)])
      note = note + ', peak ratio = ' + number_formatter(max(image1)/max(image2), format = '(f5.2)')
    endif else if keyword_set(ratio) then temp = image1/image2 else temp = image1-image2
    
  endif else temp = image1
  
  if keyword_set(sym_color) and not keyword_set(log) then begin
    if n_elements(data_range) eq 0 then data_range = [-1,1]*max(abs(temp)) $
    else data_range = [-1,1]*max(abs(data_range))
  endif
  if keyword_set(diff_ratio) then title = diff_title + ', peak norm., ' else title = diff_title
  
  healpix_quickimage, temp, pixels_use, nside1, title = title, savefile = plot_filename, note=note, $
    log = log, color_profile = color_profile, data_range = data_range, window_num = window_num, plot_as_map = plot_as_map, $
    png = png, eps = eps, pdf = pdf
    
end