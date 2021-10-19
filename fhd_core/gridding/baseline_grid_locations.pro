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
  preserve_visibilities=preserve_visibilities,mask_mirror_indices=mask_mirror_indices

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
  xcen=Float(frequency_array#Temporary(kx_arr))
  ycen=Float(frequency_array#Temporary(ky_arr))

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

  ; Match all visibilities that map from and to exactly the same pixels and store them as a histogram in bin_n
  ; with their respective index ri. Setting min equal to 0 excludes flagged (i.e. (xmin,ymin)=(-1,-1)) data
  bin_n=Long(histogram(xmin+ymin*dimension,binsize=1,reverse_indices=ri,min=0))
  bin_i=Long(where(bin_n,n_bin_use))

  return, bin_n

END
