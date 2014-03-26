pro integrate_healpix_cubes, filenames, save_file = save_file, save_path = save_path, $
    discard_unmatched_pix = discard_unmatched_pix, discard_unmatched_freq = discard_unmatched_freq
    
  compile_opt strictarr
  args = Command_Line_Args(count=nargs)
  if nargs gt 0 then begin
    filename_path=args[0]
    starting_index=UINT(args[1])
    ending_index=UINT(args[2])
 
    print,filename_path 
    filenames= FILE_SEARCH(filename_path)
    filenames = filenames[starting_index:ending_index]    
  endif
  
  nfiles = n_elements(filenames)
  
  if n_elements(discard_unmatched_pix) eq 0 then discard_unmatched_pix = 1
  if n_elements(discard_unmatched_freq) eq 0 then discard_unmatched_freq = 1
  
  obsids = long(stregex(file_basename(filenames), '[0-9]+', /extract))
  obs_range = minmax(obsids)
  
  even_mask = stregex(filenames, 'even', /boolean)
  n_even = total(even_mask)
  odd_mask = stregex(filenames, 'odd', /boolean)
  n_odd = total(odd_mask)
  
  if n_even eq nfiles then type = 'even' else if n_odd eq nfiles then type = 'odd' else begin
    if n_even gt 0 and n_odd gt 0 then begin
      if n_even + n_odd eq nfiles then type = 'evenoddmix' else ype = 'evenoddallmix'
    endif else if n_even eq 0 and n_odd eq 0 then type = 'all' else begin
      if n_even gt 0 then type = 'evenallmix' else type = 'oddallmix'
    endelse
  endelse
  
  case type of
    'even':
    'odd':
    'all':
    'evenoddmix': print, 'Warning: Combining a mixture of even and odd cubes'
    'evenallmix': print, 'Warning: Combining a mixture of even and unsplit cubes'
    'oddallmix': print, 'Warning: Combining a mixture of odd and unsplit cubes'
    'evenoddallmix': print, 'Warning: Combining a mixture of even, odd and unsplit cubes'
  endcase
  
  if n_elements(save_file) ne 0 and n_elements(save_path) eq 0 then begin
    save_path = file_dirname(save_file, /mark_directory)
    if save_path eq '.' then undefine, save_dir
    save_base = file_basename(save_file)
  endif
  
  if n_elements(save_file) eq 0 or n_elements(save_path) eq 0 then begin
    if n_elements(save_path) eq 0 then save_path = file_dirname(filenames[0], /mark_directory) + 'Healpix/'
    if n_elements(save_file) eq 0 then save_base = 'Combined_obs_' + number_formatter(obs_range[0]) + '-' + number_formatter(obs_range[1]) + '_' + type + '_cube.sav'
    
    save_file = save_path + save_base
  endif
  
  freq_match_arr = intarr(nfiles)
  pix_match_arr = intarr(nfiles)
  for i=0, nfiles-1 do begin
    void = getvar_savefile(filenames[i], names = this_varnames)
    print,'Working on file '+filenames[i]+'   ('+number_formatter(i+1)+'/'+number_formatter(nfiles)+')'
    if i eq 0 then varnames = this_varnames else begin
      match, varnames, this_varnames, suba, subb, count = count_var
      if count_var ne n_elements(varnames) then begin
        print, 'file ' + filenames[i] + 'does not have the same variables as the other files. It will not be included in the integration.'
        continue
      endif
    endelse
    
    restore, filenames[i]
    
    if n_elements(dirty_xx_cube) ne 0 then cube_struct = create_struct('dirty_xx_cube', ptr_new(temporary(dirty_xx_cube)))
    if n_elements(dirty_yy_cube) ne 0 then if n_elements(cube_struct) eq 0 then $
      cube_struct = create_struct('dirty_yy_cube', ptr_new(temporary(dirty_yy_cube))) else $
      cube_struct = create_struct(cube_struct, 'dirty_yy_cube', ptr_new(temporary(dirty_yy_cube)))
    if n_elements(model_xx_cube) ne 0 then if n_elements(cube_struct) eq 0 then $
      cube_struct = create_struct('model_xx_cube', ptr_new(temporary(model_xx_cube))) else $
      cube_struct = create_struct(cube_struct, 'model_xx_cube', ptr_new(temporary(model_xx_cube)))
    if n_elements(model_yy_cube) ne 0 then if n_elements(cube_struct) eq 0 then $
      cube_struct = create_struct('model_yy_cube', ptr_new(temporary(model_yy_cube))) else $
      cube_struct = create_struct(cube_struct, 'model_yy_cube', ptr_new(temporary(model_yy_cube)))
    if n_elements(res_xx_cube) ne 0 then if n_elements(cube_struct) eq 0 then $
      cube_struct = create_struct('res_xx_cube', ptr_new(temporary(res_xx_cube))) else $
      cube_struct = create_struct(cube_struct, 'res_xx_cube', ptr_new(temporary(res_xx_cube)))
    if n_elements(res_yy_cube) ne 0 then if n_elements(cube_struct) eq 0 then $
      cube_struct = create_struct('res_yy_cube', ptr_new(temporary(res_yy_cube))) else $
      cube_struct = create_struct(cube_struct, 'res_yy_cube', ptr_new(temporary(res_yy_cube)))
    if n_elements(weights_xx_cube) ne 0 then if n_elements(cube_struct) eq 0 then $
      cube_struct = create_struct('weights_xx_cube', ptr_new(temporary(weights_xx_cube))) else $
      cube_struct = create_struct(cube_struct, 'weights_xx_cube', ptr_new(temporary(weights_xx_cube)))
    if n_elements(weights_yy_cube) ne 0 then if n_elements(cube_struct) eq 0 then $
      cube_struct = create_struct('weights_yy_cube', ptr_new(temporary(weights_yy_cube))) else $
      cube_struct = create_struct(cube_struct, 'weights_yy_cube', ptr_new(temporary(weights_yy_cube)))
    if n_elements(variance_xx_cube) ne 0 then if n_elements(cube_struct) eq 0 then $
      cube_struct = create_struct('variance_xx_cube', ptr_new(temporary(variance_xx_cube))) else $
      cube_struct = create_struct(cube_struct, 'variance_xx_cube', ptr_new(temporary(variance_xx_cube)))
    if n_elements(variance_yy_cube) ne 0 then if n_elements(cube_struct) eq 0 then $
      cube_struct = create_struct('variance_yy_cube', ptr_new(temporary(variance_yy_cube))) else $
      cube_struct = create_struct(cube_struct, 'variance_yy_cube', ptr_new(temporary(variance_yy_cube)))
    if n_elements(beam_xx_cube) ne 0 then if n_elements(cube_struct) eq 0 then $
      cube_struct = create_struct('beam_xx_cube', ptr_new(temporary(beam_xx_cube))) else $
      cube_struct = create_struct(cube_struct, 'beam_xx_cube', ptr_new(temporary(beam_xx_cube)))
    if n_elements(beam_yy_cube) ne 0 then if n_elements(cube_struct) eq 0 then $
      cube_struct = create_struct('beam_yy_cube', ptr_new(temporary(beam_yy_cube))) else $
      cube_struct = create_struct(cube_struct, 'beam_yy_cube', ptr_new(temporary(beam_yy_cube)))
      
      
    if i eq 0 then begin
      nside_use = nside
      n_avg_use = n_avg
      pixels_use = temporary(hpx_inds)
      frequencies = (*obs.baseline_info).freq
      obs_arr = temporary(obs)
      
      nfile_contrib_pix = intarr(n_elements(pixels_use)) + 1
      nfile_contrib_freq =  intarr(n_elements(frequencies)) + 1
      int_struct = temporary(cube_struct)
    endif else begin
      if nside ne nside_use then begin
        print, 'file ' + filenames[i] + 'does not have the same nside as the other files. It will not be included in the integration.'
        continue
      endif
      if n_avg ne n_avg_use then begin
        print, 'file ' + filenames[i] + 'does not have the same n_avg as the other files. It will not be included in the integration.'
        continue
      endif
      
      match, pixels_use, hpx_inds, subpix, subhpx, count = count_pix_match
      if n_elements(pixels_use) eq n_elements(hpx_inds) and count_pix_match eq n_elements(pixels_use) then begin
        undefine, hpx_inds
        pix_match = 1
      endif else begin
        if keyword_set(discard_unmatched_pix) then begin
          if count_pix_match lt n_elements(pixels_use)*0.1 then begin
            print, 'file ' + filenames[i] + 'has less than 10% of the same pixels as the other files and discard_unmatched_pix is set. This file will not be included in the integration.'
            continue
          endif
          if count_pix_match ne n_elements(pixels_use) then begin
            npix_ch = 1
            pixels_use = pixels_use[subpix]
            nfile_contrib_pix = nfile_contrib_pix[subpix]
          endif else npix_ch = 0
          nfile_contrib_pix += 1
          undefine, hpx_inds
        endif else begin
          combined_pix = [pixels_use, hpx_inds]
          combined_pix = combined_pix[sort(combined_pix)]
          combined_pix = combined_pix[uniq(combined_pix)]
          
          match, pixels_use, combined_pix, subpix, subcmbpix
          match, hpx_inds, combined_pix, subhpx, subcmbhpx
          
          if n_elements(combined_pix) ne n_elements(pixels_use) then begin
            npix_ch = 1
            pixels_use = temporary(combined_pix)
            temp = intarr(n_elements(pixels_use))
            temp[subcmbpix] = nfile_contrib_pix
            nfile_contrib_pix = temporary(temp)
          endif else npix_ch = 0
          nfile_contrib_pix[subcmbhpx] += 1
          undefine, hpx_inds
        endelse
        pix_match = 0
      endelse
      
      this_freq = (*obs.baseline_info).freq
      match, frequencies, this_freq, subfreq, subthisf, count = count_freq_match
      if n_elements(frequencies) eq n_elements(this_freq) and count_freq_match eq n_elements(frequencies) then begin
        undefine, this_freq
        freq_match = 1
      endif else begin
        if keyword_set(discard_unmatched_freq) then begin
          if count_freq_match lt n_elements(frequencies)*0.1 then begin
            print, 'file ' + filenames[i] + 'has less than 10% of the same frequencies as the other files and discard_unmatched_freq is set. This file will not be included in the integration.'
            continue
          endif
          if count_freq_match ne n_elements(frequencies) then begin
            nfreq_ch = 1
            frequencies = frequencies[subfreq]
            nfile_contrib_freq = nfile_contrib_freq[subfreq]
          endif else nfreq_ch = 0
          nfile_contrib_freq += 1
          undefine, this_freq
        endif else begin
          combined_freq = [frequencies, this_freq]
          combined_freq = combined_freq[sort(combined_freq)]
          combined_freq = combined_freq[uniq(combined_freq)]
          
          match, frequencies, combined_freq, subfreq, subcmbfreq
          match, this_freq, combined_freq, subthisf, subcmbtf
          
          if n_elements(combined_freq) ne n_elements(frequencies) then begin
            nfreq_ch = 1
            frequencies = temporary(combined_freq)
            temp = intarr(n_elements(pixels_use))
            temp[subcmbfreq] = nfile_contrib_freq
            nfile_contrib_freq = temporary(temp)
          endif else nfreq_ch = 0
          nfile_contrib_freq[subcmbfreq] += 1
          undefine, this_freq
        endelse
        freq_match = 0
      endelse
      
      obs_arr = [obs_arr, temporary(obs)]
      
      for j=0, n_tags(int_struct)-1 do begin
        this_int_cube = *(int_struct.(j))
        this_cube = *(cube_struct.(j))
        ptr_free, int_struct.(j)
        ptr_free, cube_struct.(j)
        
        if pix_match eq 1 and freq_match eq 1 then begin
          this_int_cube += this_cube
          
        endif else begin
          if freq_match eq 1 then begin
            ;; pixels don't match but frequencies do
            if keyword_set(discard_unmatched_pix) then begin
              if npix_ch then this_int_cube = this_int_cube[subpix,*]
              this_int_cube += this_cube[subhpx, *]
            endif else begin
              if npix_ch then begin
                temp = fltarr(n_elements(pixels_use), n_elements(frequencies))
                temp[subcmbpix, *] = this_int_cube
                this_int_cube = temporary(temp)
              endif
              this_int_cube[subcmbhpx, *] += this_cube
            endelse
          endif else begin
            if pix_match eq 1 then begin
              ;; pixels match but frequencies don't
              if keyword_set(discard_unmatched_freq) then begin
                if nfreq_ch then this_int_cube = this_int_cube[*, sub_freq]
                this_int_cube += this_cube[*, sub_thisf]
              endif else begin
                if nfreq_ch then begin
                  temp = fltarr(n_elements(pixels_use), n_elements(frequencies))
                  temp[*, subcmbfreq] = this_int_cube
                  this_int_cube = temporary(temp)
                endif
                this_int_cube[*, subcmbtf] += this_cube
              endelse
            endif else begin
              ;; neither match
              if nfreq_ch eq 1 or npix_ch eq 1 then begin
                temp = fltarr(n_elements(pixels_use), n_elements(frequencies))
                inds = rebin(subcmbpix, n_elements(subcmbpix), n_elements(subcmbfreq)) + $
                  n_elements(pixels_use) * rebin(reform(subcmbfreq, 1, n_elements(subcmbpix)), n_elements(subcmbpix), n_elements(subcmbfreq))
                temp[inds] = this_int_cube
                this_int_cube = temporary(temp)
              endif
              
              if keyword_set(discard_unmatched_freq) and keyword_set(discard_unmatched_pix) then begin
                ;; discard all
                inds = rebin(subhpx, n_elements(subhpx), n_elements(sub_thisf)) + $
                  (size(this_cube,/dimensions))[0]* rebin(reform(sub_thisf, 1, n_elements(subhpx)), n_elements(subhpx), n_elements(sub_thisf))
                this_int_cube += this_cube[inds]
              endif else begin
                if keyword_set(discard_unmatched_freq) then this_int_cube[subcmbhpx, *] += this_cube[*, sub_thisf] else $ ;; discard freq, expand pixels
                  if keyword_set(discard_unmatched_pix) then this_int_cube[*, subcmbtf] += this_cube[subhpx, *] else begin ;; discard pixels, expand freqs
                  ;; expand all
                  inds = rebin(subcmbhpx, n_elements(subcmbhpx), n_elements(subcmbtf)) + $
                    n_elements(pixels_use) * rebin(reform(subcmbtf, 1, n_elements(subcmbhpx)), n_elements(subcmbhpx), n_elements(subcmbtf))
                  this_int_cube[inds] += this_cube
                endelse ;; expand all
              endelse ;; ~discard all
            endelse ;;neither match
          endelse ;; ~freq_match eq 1
        endelse ;; ~both match
        
        undefine, this_cube
        int_struct.(j) = ptr_new(temporary(this_int_cube))
      endfor ;; loop over cubes
      undefine, subpix, subhpx, subfreq, subthisf
      if not keyword_set(discard_unmatched_pix) then undefine, combined_pix, subcmbpix, subcmbhpx
      if not keyword_set(discard_unmatched_freq) then undefine, combined_freq, subcmbfreq, subcmbtf
    endelse ;; ~first file
  endfor ;; loop over files
  
  nside = nside_use
  n_avg = n_avg_use
  hpx_inds = temporary(pixels_use)
  
  if tag_exist(int_struct, 'dirty_xx_cube') then begin
    dirty_xx_cube = *int_struct.dirty_xx_cube
    ptr_free, int_struct.dirty_xx_cube
  endif
  if tag_exist(int_struct, 'dirty_yy_cube') then begin
    dirty_yy_cube = *int_struct.dirty_yy_cube
    ptr_free, int_struct.dirty_yy_cube
  endif
  if tag_exist(int_struct, 'model_xx_cube') then begin
    model_xx_cube = *int_struct.model_xx_cube
    ptr_free, int_struct.model_xx_cube
  endif
  if tag_exist(int_struct, 'model_yy_cube') then begin
    model_yy_cube = *int_struct.model_yy_cube
    ptr_free, int_struct.model_yy_cube
  endif
  if tag_exist(int_struct, 'res_xx_cube') then begin
    res_xx_cube = *int_struct.res_xx_cube
    ptr_free, int_struct.res_xx_cube
  endif
  if tag_exist(int_struct, 'res_yy_cube') then begin
    res_yy_cube = *int_struct.res_yy_cube
    ptr_free, int_struct.res_yy_cube
  endif
  if tag_exist(int_struct, 'weights_xx_cube') then begin
    weights_xx_cube = *int_struct.weights_xx_cube
    ptr_free, int_struct.weights_xx_cube
  endif
  if tag_exist(int_struct, 'weights_yy_cube') then begin
    weights_yy_cube = *int_struct.weights_yy_cube
    ptr_free, int_struct.weights_yy_cube
  endif
  if tag_exist(int_struct, 'variance_xx_cube') then begin
    variance_xx_cube = *int_struct.variance_xx_cube
    ptr_free, int_struct.variance_xx_cube
  endif
  if tag_exist(int_struct, 'variance_yy_cube') then begin
    variance_yy_cube = *int_struct.variance_yy_cube
    ptr_free, int_struct.variance_yy_cube
  endif
  if tag_exist(int_struct, 'beam_xx_cube') then begin
    beam_xx_cube = *int_struct.beam_xx_cube
    ptr_free, int_struct.beam_xx_cube
  endif
  if tag_exist(int_struct, 'beam_yy_cube') then begin
    beam_yy_cube = *int_struct.beam_yy_cube
    ptr_free, int_struct.beam_yy_cube
  endif
  
  save, file = save_file, dirty_xx_cube,  dirty_yy_cube,  model_xx_cube,  model_yy_cube,  res_xx_cube,  res_yy_cube, $
    variance_xx_cube,  variance_yy_cube, weights_xx_cube,  weights_yy_cube, beam_xx_cube, beam_yy_cube, $
    nside,  n_avg,  obs_arr,  hpx_inds, frequencies, nfile_contrib_pix, nfile_contrib_freq
    
end
