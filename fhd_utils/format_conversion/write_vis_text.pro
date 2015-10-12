pro write_vis_text, save_file, text_file = text_file

  if n_elements(save_file) eq 0 then message, 'save_file must be passed'
  
  if n_elements(text_file) eq 0 then begin
    filebase = cgrootname(save_file, directory = dir, extension = exten)
    new_dir = (strsplit(dir, 'vis_data',/extract, /regex))[0] + 'text_files/'
    
    text_file = new_dir + filebase + '.txt'
  endif
  
  flag_file = dir + (strsplit(filebase, 'vis', /extract))[0] + 'flags.sav'
  
  metadata_dir = (strsplit(dir, 'vis_data',/extract, /regex))[0] + 'metadata/'
  params_file = metadata_dir + (strsplit(filebase, 'vis', /extract))[0] + 'params.sav'
  
  restore, save_file
  obsname = obs.obsname
  fhd_version = obs.code_version
  
  if n_elements(pol_i) eq 0 then begin
    pol_name = stregex(filebase, '[xy][xy]', /fold_case,/extract)
    pol_index = (where(obs.pol_names eq pol_name, count_pol))[0]
    if count_pol eq 0 then message, 'pol_name not recognized'
  endif else begin
    pol_index=pol_i
    pol_name = obs.pol_names[pol_index]
  endelse
  
  
  freq = (*obs.baseline_info).freq
  n_freq = n_elements(freq)
  
  tile_names = long((*obs.baseline_info).tile_names)
  tile_a = (*obs.baseline_info).tile_a - 1 ;; 1 indexed
  tile_b = (*obs.baseline_info).tile_b - 1 ;; 1 indexed
  n_baselines = n_elements(tile_a)
  
  tile_a = rebin(reform(tile_names[tile_a], 1, n_baselines), n_freq, n_baselines, /sample)
  tile_b = rebin(reform(tile_names[tile_b], 1, n_baselines), n_freq, n_baselines, /sample)
  
  freq_arr = rebin(freq, n_freq, n_baselines, /sample)
  times = (*obs.baseline_info).jdate
  times = rebin(reform(times, 1, n_elements(times)), n_freq, n_baselines, /sample)
  
  vis = *vis_ptr
  undefine_fhd, vis_ptr
  undefine_fhd, obs
  
  restore, flag_file
  flags = 0>*flag_arr[pol_index]<1
  undefine_fhd, flag_arr
  
  restore, params_file
  uu = params.uu * 3.8e6
  vv = params.vv * 3.8e6
  ww = params.ww * 3.8e6
  undefine_fhd, params
  
  ncol = 10
  header = strarr(ncol, 3)
  header[0,0] = obsname
  header[1,0] = pol_name
  header[0,1] = 'fhd version: ' + fhd_version
  header[*,2] = ['mJD', 'Tile A', 'Tile B', 'Frequency (Hz)', 'visibility amplitude', 'visibility phase', 'Flag', 'U (m)', 'V (m)', 'W (m)']
  
  nrow = n_freq*n_baselines
  data = dblarr(ncol, nrow)
  data[0,*] = reform(temporary(times), nrow)
  data[1,*] = reform(temporary(tile_a), nrow)
  data[2,*] = reform(temporary(tile_b), nrow)
  data[3,*] = reform(temporary(freq_arr), nrow)
  data[4,*] = reform(abs(vis), nrow)
  data[5,*] = reform(atan(temporary(vis), /phase), nrow)
  data[6,*] = reform(temporary(flags), nrow)
  data[7,*] = reform(rebin(reform(temporary(uu), 1, n_baselines), n_freq, n_baselines), nrow)
  data[8,*] = reform(rebin(reform(temporary(vv), 1, n_baselines), n_freq, n_baselines), nrow)
  data[9,*] = reform(rebin(reform(temporary(ww), 1, n_baselines), n_freq, n_baselines), nrow)
  
  openw,unit,text_file,/Get_LUN
  
  printf, unit, header
  Printf,unit,data, format = '(d, " ", i3.3, " ", i3.3, " ", e12.6, " ", f, " ", f, " ", i, " ", f, " ", f, " ", f)'
  Free_lun,unit
end