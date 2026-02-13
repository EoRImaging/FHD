;;
;; Calculate the histogram of baseline grid locations in units of pixels whilst also
;; returning the minimum pixel number that an unflagged baseline contributes to (depending on the 
;; size of the kernel). Optionally return the 2D derivatives for bilinear interpolation and the
;; indices of the unflagged baselines/frequencies.
;;

Function baseline_grid_locations,obs,psf,params,n_bin_use=n_bin_use,bin_i=bin_i,ri=ri,$
  xmin=xmin,ymin=ymin,vis_weight_ptr=vis_weight_ptr,$
  fill_model_visibilities=fill_model_visibilities,bi_use=bi_use,fi_use=fi_use,$
  vis_inds_use=vis_inds_use,interp_flag=interp_flag,dx0dy0_arr=dx0dy0_arr,dx0dy1_arr=dx0dy1_arr,$
  dx1dy0_arr=dx1dy0_arr,dx1dy1_arr=dx1dy1_arr,x_offset=x_offset,y_offset=y_offset,$
  preserve_visibilities=preserve_visibilities,mask_mirror_indices=mask_mirror_indices,$
  wstacking=wstacking,aw_projection=aw_projection,w_info=w_info,polarization=polarization

  ; Extract information from the structures
  n_tile=obs.n_tile
  n_freq=obs.n_freq
  dimension=Long(obs.dimension)
  elements=Long(obs.elements)
  kbinsize=obs.kpix
  min_baseline=obs.min_baseline
  max_baseline=obs.max_baseline
  b_info=*obs.baseline_info
  psf_dim=psf.dim
  psf_resolution=psf.resolution

  ; Unless explicitly stated, do not free visibility array memory once used
  IF N_elements(preserve_visibilities) EQ 0 THEN preserve_visibilities=1

  ; Frequency information of the visibilities
  IF N_Elements(fi_use) EQ 0 THEN fi_use=where(b_info.freq_use)
  IF Keyword_Set(fill_model_visibilities) THEN fi_use=lindgen(n_freq)
  frequency_array=b_info.freq
  frequency_array=frequency_array[fi_use]
  n_freq_use=N_Elements(frequency_array)

  ; Careful treatment to avoid overwriting the weights pointer
  weight_type = size(vis_weight_ptr,/type)
  IF weight_type EQ 10 THEN BEGIN
    ; If the weights are pointers
    vis_weight_switch=Ptr_valid(vis_weight_ptr)
    IF vis_weight_switch THEN BEGIN
      IF Keyword_Set(preserve_visibilities) THEN vis_weights=*vis_weight_ptr ELSE BEGIN
        vis_weights=Temporary(*vis_weight_ptr)
        Ptr_free,vis_weight_ptr
      ENDELSE
    ENDIF
  ENDIF ELSE BEGIN
    ; If the weights are not pointers
    vis_weight_switch=1
    vis_weights = vis_weight_ptr
    vis_weight_ptr=0
  ENDELSE

  ; Baselines to use
  IF N_Elements(bi_use) EQ 0 THEN BEGIN
    ; If the data is being gridded separatedly for the even/odd time samples, then force
    ; flagging to be consistent across even/odd sets
    IF vis_weight_switch AND ~Keyword_set(fill_model_visibilities) THEN BEGIN
      flag_test=Total(vis_weights>0,1)
      bi_use=where((flag_test GT 0))
    ENDIF ELSE BEGIN
      b_info=(*obs.baseline_info)
      tile_use=where(b_info.tile_use)+1
      IF Keyword_Set(fill_model_visibilities) THEN tile_use=lindgen(n_tile)+1
      bi_use=array_match(b_info.tile_A,b_info.tile_B,value_match=tile_use)
    ENDELSE
  ENDIF

  ; Calculate indices of visibilities to grid during this call (i.e. specific freqs, time sets)
  ; and initialize output arrays
  n_b_use=N_Elements(bi_use)
  n_f_use=N_Elements(fi_use)
  vis_inds_use=matrix_multiply(fi_use,replicate(1L,n_b_use))+matrix_multiply(replicate(1L,n_f_use),bi_use)*n_freq
  IF vis_weight_switch THEN vis_weights=vis_weights[vis_inds_use]

  ; Units in pixel/Hz
  kx_arr=params.uu[bi_use]/kbinsize
  ky_arr=params.vv[bi_use]/kbinsize

  IF ~Keyword_Set(fill_model_visibilities) THEN BEGIN
    ; Flag baselines on their maximum and minimum extent in the full frequency range of the observation.
    ; This prevents the sudden disappearance of baselines along frequency
    dist_test=Sqrt((kx_arr)^2.+(ky_arr)^2.)*kbinsize
    dist_test_max=max((*obs.baseline_info).freq)*dist_test
    dist_test_min=min((*obs.baseline_info).freq)*dist_test
    flag_dist_baseline=where((dist_test_min LT min_baseline) $
      OR (dist_test_max GT max_baseline),n_dist_flag)
  ENDIF
  dist_test=0
  dist_test_min=0
  dist_test_max=0

  ; Create the other half of the uv plane via negating the locations
  conj_i=where(ky_arr GT 0,n_conj)
  IF n_conj GT 0 THEN BEGIN
    kx_arr[conj_i]=-kx_arr[conj_i]
    ky_arr[conj_i]=-ky_arr[conj_i]
  ENDIF

  ; Center of baselines for x and y in units of pixels
  xcen=frequency_array#Temporary(kx_arr)
  ycen=frequency_array#Temporary(ky_arr)

  ; Pixel number offet per baseline for each uv-box subset 
  x_offset=Fix(Floor((xcen-Floor(xcen))*psf_resolution) mod psf_resolution, type=12) ; type=12 is unsigned int
  y_offset=Fix(Floor((ycen-Floor(ycen))*psf_resolution) mod psf_resolution, type=12) ; type=12 is unsigned int
  
  IF keyword_set(interp_flag) THEN BEGIN
    ; Derivatives from pixel edge to baseline center for use in interpolation
    dx_arr = (xcen-Floor(xcen))*psf_resolution - Floor((xcen-Floor(xcen))*psf_resolution)
    dy_arr = (ycen-Floor(ycen))*psf_resolution - Floor((ycen-Floor(ycen))*psf_resolution)
    dx0dy0_arr = (1-dx_arr)*(1-dy_arr)
    dx0dy1_arr = (1-dx_arr)*dy_arr
    dx1dy0_arr = dx_arr*(1-dy_arr)
    dx1dy1_arr = Temporary(dx_arr) * Temporary(dy_arr)
  ENDIF

  ; The minimum pixel in the uv-grid (bottom left of the kernel) that each baseline contributes to
  xmin=Long(Floor(Temporary(xcen))+dimension/2-(psf_dim/2-1))
  ymin=Long(Floor(Temporary(ycen))+elements/2-(psf_dim/2-1))

  ; Set the minimum pixel value of baselines which fall outside of the uv-grid to -1 to exclude them
  range_test_x_i=where((xmin LE 0) OR ((xmin+psf_dim-1) GE dimension-1),n_test_x)
  range_test_y_i=where((ymin LE 0) OR ((ymin+psf_dim-1) GE elements-1),n_test_y)
  IF n_test_x GT 0 THEN xmin[range_test_x_i]=(ymin[range_test_x_i]=-1)
  IF n_test_y GT 0 THEN xmin[range_test_y_i]=(ymin[range_test_y_i]=-1)
  
  ; Flag baselines which fall outside the uv plane
  IF ~keyword_set(fill_model_visibilities) THEN BEGIN
    IF n_dist_flag GT 0 THEN BEGIN
      ; If baselines fall outside the desired min/max baseline range at all during the frequency range, 
      ; then set their minimum pixel value to -1 to exlude them 
      xmin[*,flag_dist_baseline]=-1
      ymin[*,flag_dist_baseline]=-1
      flag_dist_baseline=0
    ENDIF
  ENDIF 

  IF vis_weight_switch THEN BEGIN
    ; If baselines are flagged via the weights, then set their minimum pixel value to -1 to exclude them
    flag_i=where(vis_weights LE 0,n_flag)
    IF Keyword_Set(fill_model_visibilities) THEN n_flag=0L
    IF n_flag GT 0 THEN BEGIN
      xmin[flag_i]=-1
      ymin[flag_i]=-1
    ENDIF
    vis_weights=0
    flag_i=0
  ENDIF

  IF Keyword_Set(mask_mirror_indices) THEN BEGIN
    ; Option to exlude v-axis mirrored baselines
    IF n_conj GT 0 THEN BEGIN
      xmin[*,conj_i]=-1
      ymin[*,conj_i]=-1
    ENDIF
  ENDIF

  IF max(xmin)<max(ymin) LT 0 THEN BEGIN
      ; Return if all baselines have been flagged
      print,'WARNING: All data flagged or cut!'
      bin_n=0
      n_bin_use=0
      bin_i=-1
      RETURN,bin_n
  ENDIF

  if keyword_set(wstacking) OR keyword_set(aw_projection) then begin

    if keyword_set(aw_projection) then begin
      ;; Recreate the power beam in image space

      ; Dimension and resolution of the PSF
      psf_dim = psf.dim
      psf_resolution = psf.resolution

      ; Pixels to horizon
      pix_hor = psf.pix_horizon         
      pix_hor_pad = Ceil(pix_hor*1.3/2)*2 

      ; intermediate resolutions
      psf_intermediate_res=(Ceil(Sqrt(psf_resolution)/2)*2.)<psf_resolution

      ; dimensions of the super-res image
      psf_image_dim = ((*psf.image_info).psf_image_dim)  
      dimension_super = psf.dim*psf_resolution
      res_super = 1/(Double(psf_resolution)/Double(psf_intermediate_res))
      fft_norm = 1/double(res_super)^2*psf_intermediate_res^2
      psf_scale=obs.dimension*psf_intermediate_res/psf_image_dim
      res_super = 1/(Double(psf_resolution)/Double(psf_intermediate_res))

      ; Create dummy obs for creating proper astrometry and directional cosines
      obs_copy = obs
      obs_copy.astr.cdelt[*] = psf_scale
      obs_copy.astr.naxis[*] = dimension_super
      obs_copy.astr.crpix[*] = dimension_super/2 + 1

      ; Astrometry of the power beam image
      apply_astrometry, obs_copy, x_arr=meshgrid(dimension_super,dimension_super,1), y_arr=meshgrid(dimension_super,dimension_super,2),ra_arr=ra_arr, dec_arr=dec_arr, /xy2ad
      ; Directional cosines of the power beam image
      n_tracked = l_m_n(obs_copy, psf, l_mode=l_mode, m_mode=m_mode, ra_arr=ra_arr, dec_arr=dec_arr)

      ; Calculate the lm term to the measurement equation    
      lm = sqrt(1 - l_mode*l_mode - m_mode*m_mode)

      ; Determine the indices inside of the horizon, easiest on ra or dec
      inside_horizon_inds=where(finite(shift(dec_arr,dimension_super/2,dimension_super/2)))
      
      image_power_beam_arr = DBLARR(dimension_super,dimension_super,n_freq_use)
      max_amp_freq = DBLARR(n_freq_use)

      if ptr_valid((*psf.image_info).image_power_beam_arr[0]) then begin
        ; If the image power beam exists in the structure, use that directly.
        for fi=0, n_freq_use-1 do begin
          image_power_beam_arr[*,*,fi] = *(*psf.image_info).image_power_beam_arr[polarization,psf.fbin_i[fi_use[fi]]] * lm
        endfor
      endif else begin
        if tag_exist(psf, 'beam_decomp_info') then begin
          ; If there is beam decomposition information, recreate the image power beam.
          ; This is vastly preferable to a stored image, which can drastically increase memory usage. 
          for fi=0, n_freq_use-1 do begin
            image_power_beam_arr[*,*,fi] =  call_function((*psf.beam_decomp_info).decomp_type + '_decomp', $
              FINDGEN(dimension_super),FINDGEN(dimension_super), $
              (*(*psf.beam_decomp_info).beam_params[polarization])[*,fi_use[fi]],ftransform=0,model_npix=(*psf.beam_decomp_info).model_npix,$
              model_res=(*psf.image_info).image_res_scale/(*psf.beam_decomp_info).model_res,over_res=res_super,max_amp=max_amp)
            max_amp_freq[fi] = max_amp
            image_power_beam_arr[*,*,fi] *= lm
            image_power_beam_arr[*,*,fi] = shift(image_power_beam_arr[*,*,fi],dimension_super/2,dimension_super/2)
          endfor
        endif else message, 'Either image_power_beam_arr pointer or decomp_type must be set in the psf structure for w-stacking.'
      endelse
      lm = shift(lm,dimension_super/2,dimension_super/2)
      image_power_beam_arr *= fft_norm
      max_amp_freq *= fft_norm

    endif else begin
    ; Astrometry of the image
      apply_astrometry, obs, x_arr=meshgrid(dimension,elements,1), y_arr=meshgrid(dimension,elements,2), $
        ra_arr=ra_arr, dec_arr=dec_arr, /xy2ad

      ; Directional cosines of the image
      n_tracked = l_m_n(obs, psf, l_mode=l_mode, m_mode=m_mode, ra_arr=ra_arr, dec_arr=dec_arr)

      ; Determine the indices inside of the horizon, easiest on ra or dec
      inside_horizon_inds = where(finite(dec_arr))

      ; Calculate the lm term to the measurement equation    
      lm = sqrt(1 - l_mode^2. - m_mode^2.)

      image_power_beam_arr=0
      max_amp_freq=0
    endelse

    ; Select unflagged baselines for finding correct w-stacks
    ; Baselines are grouped by chunks in frequency -- if any baseline in a chunk is unflagged,
    ;  then that baseline chunk is included in w-stacking 
    baselines_ind = where(total(xmin GE 0, 1) GE 1 and total(ymin GE 0, 1) GE 1)

    ; Calculate the w for each unflagged baseline group in radians
    ww_rad = (2*!DPi) * (params.ww[bi_use[baselines_ind]] * mean(frequency_array))

    ; Create the other half of the uv plane via negating the locations
    conj_i=where(ww_rad LT 0,n_conj)
    IF n_conj GT 0 THEN BEGIN
      ww_rad[conj_i]=-ww_rad[conj_i]
    ENDIF

    ; Calculate the absolute number of w-stacks required.
    n_w_stack = ceil(2 * !DPi * (max(ww_rad,/nan) - min(ww_rad,/nan)) * max(1. - sqrt(1 - l_mode^2 - m_mode^2)))

    ; Calculate the histogram and reverse indices for each w-stack
    w_bin_n = histogram(ww_rad, nbins=n_w_stack, reverse_indices=w_ri, omin=w_omin)

    ; Flag w-stacks with less than 50% of the number of visibilities per bin 
    ; (if evenly distributed) for removal. Check if over 75% of the remaining 
    ; bins are bad, and if so, remove all subsequent bins. If not, keep that bin
    ; and continue looking for the cutoff point.
    ; This keeps a contiguous set of w-stacks whilst increasing efficiency during gridding.
    low_n_baselines_mask = w_bin_n lt N_elements(ww_rad)/n_w_stack*.50
    low_inds = where(low_n_baselines_mask,n_count)
    if n_count GT 0 then begin
      for low_i=0,n_elements(low_inds)-1 do begin
        if total(low_n_baselines_mask[low_inds[low_i]:-1]) gt (n_w_stack - low_inds[low_i])*.75 then begin
          low_n_baselines_mask[low_inds[low_i]:-1] = 1
          continue
        endif else low_n_baselines_mask[low_inds[low_i]] = 0
      endfor
      ; Only apply the mask if not all w-stacks are removed. Otherwise, keep all w-stacks
      ; because this simple filter did not work well in this case.
      if total(low_n_baselines_mask) NE n_w_stack then w_bin_n[where(low_n_baselines_mask)] = 0.0
    endif

    ; Indices of non-empty w-stacks
    w_bin_i=Long(where(w_bin_n))

    ; Also capture the size of the w bins in radians
    w_binsize = (max(ww_rad,/nan) - min(ww_rad,/nan)) / n_w_stack

    ; Reset the number of w-stacks because some may be empty. w_bin_i holds the number of the non-empty w-stacks 
    n_w_stack = N_elements(w_bin_i)

    ; Initialize arrays for the w-stacking
    w_bin = DBLARR(n_w_stack) + w_omin - w_binsize/2
    n_freq_vis_w = LONARR(n_freq_use, n_w_stack)

    xmin_w_i = xmin
    ymin_w_i = ymin

    wstack_hist = PTRARR(n_w_stack,/allocate)
    n_vis=0

    for w_i = 0 , n_w_stack - 1 do begin

      ; Reinitialize the minimum pixel values for each w-stack to be not included
      replicate_inplace, xmin_w_i, -1
      replicate_inplace, ymin_w_i, -1

      ; Calculate the center w of the w-stack in radians
      w_bin[w_i] += w_binsize * (w_bin_i[w_i]+1)

      ; Find the indices for each w-stack where flagged baselines are not included
      inds_i = w_ri[w_ri[w_bin_i[w_i]]:w_ri[w_bin_i[w_i]+1]-1]
      ; Convert these indicies to where flagged are included
      w_stack_inds = baselines_ind[inds_i]

      ; Include baselines in the w-stack
      xmin_w_i[*,w_stack_inds] = xmin[*,w_stack_inds]
      ymin_w_i[*,w_stack_inds] = ymin[*,w_stack_inds]
 
      ; Match all visibilities that map from and to exactly the same pixels and store them as a histogram in bin_n
      ; with their respective index ri. Setting min equal to 0 excludes flagged (i.e. (xmin,ymin)=(-1,-1)) data
      ; Store in pointers due to changing size in each w-stack
      bin_n=Long(histogram(xmin_w_i+ymin_w_i*dimension,binsize=1,reverse_indices=ri,min=0))
      bin_i = Long(where(bin_n,n_bin_use_i))
      *wstack_hist[w_i] = {bin_n:bin_n, ri:ri, bin_i:bin_i}

      if w_i EQ 0 then n_bin_use = n_bin_use_i else n_bin_use = [n_bin_use, n_bin_use_i]
      n_vis += Total(bin_n)

      ; ; Get the number of visibilities in the w-stack per frequency
      freq_inds_per_stack = ri[N_elements(bin_n)+1:*] mod n_freq_use
      for f_i=0, n_freq_use-1 do begin
        temp = where(freq_inds_per_stack EQ f_i,n_count)
        if n_count GT 0 then begin
          n_freq_vis_w[f_i,w_i] = n_count
        endif else n_freq_vis_w[f_i,w_i] = 0
      endfor

    endfor

    w_info = {n_w_stack:n_w_stack,n_vis:n_vis,w_bin:w_bin,wstack_hist:wstack_hist,n_freq_vis_w:n_freq_vis_w,$
      lm:lm,image_power_beam_arr:dcomplex(image_power_beam_arr),inside_horizon_inds:inside_horizon_inds,$
      max_amp:max_amp_freq}

  endif else begin

    ; Match all visibilities that map from and to exactly the same pixels and store them as a histogram in bin_n
    ; with their respective index ri. Setting min equal to 0 excludes flagged (i.e. (xmin,ymin)=(-1,-1)) data
    bin_n=Long(histogram(xmin+ymin*dimension,binsize=1,reverse_indices=ri,min=0))
    bin_i=Long(where(bin_n,n_bin_use))
  
  endelse

  return, bin_n

END
