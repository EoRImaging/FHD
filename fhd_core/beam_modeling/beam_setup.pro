FUNCTION beam_setup,obs,status_str,antenna,file_path_fhd=file_path_fhd,restore_last=restore_last,timing=timing,$
  beam_mask_threshold=beam_mask_threshold,silent=silent,psf_dim=psf_dim,psf_resolution=psf_resolution,$
  psf_image_resolution=psf_image_resolution,swap_pol=swap_pol,no_save=no_save,$
  beam_model_version=beam_model_version,beam_dim_fit=beam_dim_fit,save_antenna_model=save_antenna_model,$
  interpolate_kernel=interpolate_kernel,transfer_psf=transfer_psf,beam_per_baseline=beam_per_baseline,$
  beam_gaussian_decomp=beam_gaussian_decomp,save_beam_metadata_only=save_beam_metadata_only,_Extra=extra

  compile_opt idl2,strictarrsubs
  t00=Systime(1)

  antenna_flag=Arg_present(antenna)
  IF N_Elements(save_antenna_model) EQ 0 THEN save_antenna_model=0
  IF Keyword_Set(no_save) THEN save_antenna_model=0
  IF N_Elements(file_path_fhd) EQ 0 THEN file_path_fhd=''
  IF Keyword_Set(restore_last) THEN BEGIN
    fhd_save_io,status_str,psf,var='psf',/restore,file_path_fhd=file_path_fhd
    IF antenna_flag THEN fhd_save_io,status_str,antenna,var='antenna',/restore,file_path_fhd=file_path_fhd
    IF Keyword_Set(psf) THEN RETURN,psf $
    ELSE IF not Keyword_Set(silent) THEN print,"Saved beam model not found. Recalculating."
  ENDIF
  IF Keyword_set(transfer_psf) then begin
    ;
    ; Transfer a psf/obs structure from previous run or from a specified file
    if file_test(transfer_psf) then begin
      psf = getvar_savefile(transfer_psf,'psf')
      obs_restore = getvar_savefile(transfer_psf,'obs')
    endif else begin
      obs_id = obs.obsname

      ; Test for <transfer_psf>/<obs_id>_obs.sav and <transfer_psf>/<obs_id>_beams.sav
      ; Test for  <transfer_psf>/metadata/<obs_id>_obs.sav and <transfer_psf>/beams/<obs_id>_beams.sav
      if file_test(transfer_psf+'/'+obs_id+'_obs.sav') AND file_test(transfer_psf+'/'+obs_id+'_beams.sav') then begin
        file_name_obs = transfer_psf+'/'+obs_id+'_obs.sav'
        file_name_psf = transfer_psf+'/'+obs_id+'_beams.sav'
      endif else begin
        if file_test(transfer_psf+'/metadata/'+obs_id+'_obs.sav') AND file_test(transfer_psf+'/beams/'+obs_id+'_beams.sav') then begin 
          file_name_obs = transfer_psf+'/metadata/'+obs_id+'_obs.sav'
          file_name_psf = transfer_psf+'/beams/'+obs_id+'_beams.sav'
        endif else begin
          print, transfer_psf+' or'
          print, transfer_psf+'/'+obs_id+'_obs.sav and '+transfer_psf+'/'+obs_id+'_beams.sav or '
          print, transfer_psf+'/metadata/'+obs_id+'_obs.sav and '+transfer_psf+'/beams/'+obs_id+'_beams.sav'
          message, 'not found during psf transfer.'
        endelse
      endelse      
      psf = getvar_savefile(file_name_psf,'psf')
      obs_restore = getvar_savefile(file_name_obs,'obs')
    endelse
    ; Transfer the observation volumes to the current obs structure
    obs.primary_beam_sq_area = pointer_copy(obs_restore.primary_beam_sq_area)
    obs.primary_beam_area = pointer_copy(obs_restore.primary_beam_area)

    RETURN,psf
  ENDIF

  IF N_Elements(obs) EQ 0 THEN fhd_save_io,status_str,obs,var='obs',/restore,file_path_fhd=file_path_fhd
  ;Fixed parameters
  ;extract information from the structures
  n_tiles=obs.n_tile
  n_freq=obs.n_freq
  n_pol=obs.n_pol
  double_flag=obs.double_precision

  freq_bin_i=(*obs.baseline_info).fbin_i
  nfreq_bin=Max(freq_bin_i)+1

  nbaselines=obs.nbaselines
  dimension=obs.dimension
  antenna=fhd_struct_init_antenna(obs,beam_model_version=beam_model_version,psf_resolution=psf_resolution,psf_dim=psf_dim,$
    psf_intermediate_res=psf_intermediate_res,psf_image_resolution=psf_image_resolution,timing=t_ant,$
    ra_arr=ra_arr,dec_arr=dec_arr,beam_per_baseline=beam_per_baseline,beam_gaussian_decomp=beam_gaussian_decomp,_Extra=extra)

  IF Keyword_Set(swap_pol) THEN pol_arr=[[1,1],[0,0],[1,0],[0,1]] ELSE pol_arr=[[0,0],[1,1],[0,1],[1,0]]

  kbinsize=obs.kpix
  primary_beam_area=Ptrarr(n_pol,/allocate)
  primary_beam_sq_area=Ptrarr(n_pol,/allocate)
  if keyword_set(beam_gaussian_decomp) then beam_gaussian_params_arr=Ptrarr(n_pol,/allocate)
  IF N_Elements(beam_mask_threshold) EQ 0 THEN beam_mask_threshold=1E2

  ;;begin forming psf
  psf_xvals=Ptrarr(psf_resolution,psf_resolution,/allocate)
  psf_yvals=Ptrarr(psf_resolution,psf_resolution,/allocate)
  xvals_i=Reform(meshgrid(psf_dim,psf_dim,1)*psf_resolution,psf_dim^2.)
  yvals_i=Reform(meshgrid(psf_dim,psf_dim,2)*psf_resolution,psf_dim^2.)
  FOR i=0,psf_resolution-1 DO FOR j=0,psf_resolution-1 DO BEGIN
    *psf_xvals[i,j]=meshgrid(psf_dim,psf_dim,1)-psf_dim/2.+Float(i)/psf_resolution
    *psf_yvals[i,j]=meshgrid(psf_dim,psf_dim,2)-psf_dim/2.+Float(j)/psf_resolution
  ENDFOR

  ;;set up coordinates to generate the high uv resolution model.
  ;;Remember that field of view = uv resolution, image pixel scale = uv span.
  ;;So, the cropped uv span (psf_dim) means we do not need to calculate at full image resolution,
  ;;   while the increased uv resolution can correspond to super-horizon scales. We construct the beam model in 
  ;;   image space, and while we don't need the full image resolution we need to avoid quantization errors that 
  ;;   come in if we make too small an image and then take the FFT
  psf_intermediate_res=(Ceil(Sqrt(psf_resolution)/2)*2.)<psf_resolution
  ;use a larger box to build the model than will ultimately be used, to allow higher resolution in the initial 
  ;    image space beam model
  psf_image_dim=psf_dim*psf_image_resolution*psf_intermediate_res 
  image_res_scale=dimension*psf_intermediate_res/psf_image_dim
  zen_int_x=(obs.zenx-obs.obsx)/image_res_scale+psf_image_dim/2
  zen_int_y=(obs.zeny-obs.obsy)/image_res_scale+psf_image_dim/2
  psf_superres_dim=psf_dim*psf_resolution
  ; Calculate the hyperresolved uv-vals of the beam kernel at highest precision prior to cast to
  ;  be accurate yet small
  res_super = 1/(Double(psf_resolution)/Double(psf_intermediate_res))
  ; Calculate the hyperresolved uv-pixel coords to reduce FFT artifacts if the gauss decomposition is not used
  if ~keyword_set(beam_gaussian_decomp) then begin
    xvals_uv_superres=Float(meshgrid(psf_superres_dim,psf_superres_dim,1)*res_super-$
      Floor(psf_dim/2)*psf_intermediate_res+Floor(psf_image_dim/2))
    yvals_uv_superres=Float(meshgrid(psf_superres_dim,psf_superres_dim,2)*res_super-$
      Floor(psf_dim/2)*psf_intermediate_res+Floor(psf_image_dim/2))
  endif

  beam_arr=Ptrarr(n_pol,nfreq_bin,nbaselines)
  if keyword_set(beam_per_baseline) then image_power_beam_arr=PTRARR(n_pol,nfreq_bin)

  ant_A_list=(*obs.baseline_info).tile_A[0:nbaselines-1]
  ant_B_list=(*obs.baseline_info).tile_B[0:nbaselines-1]
  baseline_mod=(2.^(Ceil(Alog(Sqrt(nbaselines*2.-n_tiles))/Alog(2.)))>(Max(ant_A_list)>Max(ant_B_list)))>256.
  bi_list=ant_B_list+ant_A_list*baseline_mod
  bi_hist0=histogram(bi_list,min=0,omax=bi_max,/binsize,reverse_indices=ri_bi)

  group_arr=Lonarr(n_pol,nfreq_bin,nbaselines)-1
  FOR pol_i=0,n_pol-1 DO BEGIN
    *primary_beam_area[pol_i]=Fltarr(n_freq)
    *primary_beam_sq_area[pol_i]=DBLarr(n_freq)
    ant_pol1=pol_arr[0,pol_i]
    ant_pol2=pol_arr[1,pol_i]

    ;Group IDs label unique beams across the array
    group1=antenna.group_id[ant_pol1,*]
    group2=antenna.group_id[ant_pol2,*]

    ;Histogram group IDs, get reverse indicies, and calculate number of unique beams
    hgroup1=histogram(group1,min=0,/binsize,reverse=gri1)
    hgroup2=histogram(group2,min=0,/binsize,reverse=gri2)
    ng1=N_Elements(hgroup1)
    ng2=N_Elements(hgroup2)
    
    ;Histogram matrix between all separate groups of different beams 
    group_matrix=hgroup1#hgroup2
    
    ;Unset lower half of group matrix if they are redundant with upper half
    ;i.e. beam1 * conj(beam2) == beam2 * conj(beam1) when pols are the same
    IF ant_pol1 EQ ant_pol2 THEN BEGIN
      for ind_i = 0,ng1 - 2 Do BEGIN
        group_matrix[ind_i+1:*,ind_i] = 0
      endfor
    ENDIF
    ;Only use groups which are defined for efficiency
    gi_use=where(group_matrix,n_group)
    freq_center=antenna[0].freq ;all antennas need to have the same frequency coverage, so just take the first

    if keyword_set(beam_gaussian_decomp) then gaussian_params=PTRARR(n_group)

    FOR freq_i=0,nfreq_bin-1 DO BEGIN
      t2_a=Systime(1)

      beam_int=0.
      beam2_int=0.
      n_grp_use=0.
      ;Loop over all unique beams
      FOR g_i=0L,n_group-1 DO BEGIN
        g_i1=gi_use[g_i] mod ng1
        g_i2=Floor(gi_use[g_i]/ng1)

        baseline_group_n=group_matrix[g_i1,g_i2]
        IF baseline_group_n LE 0 THEN CONTINUE

        ;Get antenna indices which use this group's unique beam
        ant_1_arr=gri1[gri1[g_i1]:gri1[g_i1+1]-1]
        ant_2_arr=gri2[gri2[g_i2]:gri2[g_i2+1]-1]
        ;Select first antenna index for beam calculation as representative
        ant_1=ant_1_arr[0]
        ant_2=ant_2_arr[0]
        ;Number of antennas in this group
        ant_1_n=hgroup1[g_i1]
        ant_2_n=hgroup2[g_i2]
        
        ;Get the baseline index
        bi_use=Reform(rebin((ant_1_arr+1),ant_1_n,ant_2_n)*baseline_mod+$
          Rebin(Transpose(ant_2_arr+1),ant_1_n,ant_2_n),baseline_group_n)
        bi_use2=Reform(rebin((ant_1_arr+1),ant_1_n,ant_2_n)+$
          Rebin(Transpose(ant_2_arr+1),ant_1_n,ant_2_n)*baseline_mod,baseline_group_n)
        bi_use = [bi_use, bi_use2]
        IF Max(bi_use) GT bi_max THEN bi_use=bi_use[where(bi_use LE bi_max)]
        bi_use_i=where(bi_hist0[bi_use],n_use)
        IF n_use GT 0 THEN bi_use=bi_use[bi_use_i]
        baseline_group_n=N_Elements(bi_use)
        ;use these indices to index the reverse indices of the original baseline index histogram
        bi_inds=ri_bi[ri_bi[bi_use]] 
        group_arr[pol_i,freq_i,bi_inds]=g_i

        ;If the pols are equal, then only calculating one of the beams in the unique group matrix for efficiency.
        ;Set the redundant beam product to the one that is being calculated and redefine the element numbers.
        IF ant_pol1 EQ ant_pol2 THEN BEGIN
          ;Use temp arrays to avoid overwriting issues
          ant_1_arr_temp = [ant_1_arr, ant_2_arr]
          ant_2_arr_temp = [ant_2_arr, ant_1_arr]
          ant_1_arr = Temporary(ant_1_arr_temp)
          ant_2_arr = Temporary(ant_2_arr_temp)
          ant_1_n *= 2
          ant_2_n *= 2
        ENDIF

        ;Calculate power beam from antenna beams
        psf_base_superres=beam_power(antenna[ant_1],antenna[ant_2],obs=obs,ant_pol1=ant_pol1,ant_pol2=ant_pol2,$
          psf_dim=psf_dim,freq_i=freq_i,psf_intermediate_res=psf_intermediate_res,$
          psf_resolution=psf_resolution,xvals_uv_superres=xvals_uv_superres,yvals_uv_superres=yvals_uv_superres,$
          beam_mask_threshold=beam_mask_threshold,zen_int_x=zen_int_x,zen_int_y=zen_int_y, $
          image_power_beam=image_power_beam,pol_i=pol_i,beam_gaussian_params=beam_gaussian_params,$
          volume_beam=volume_beam,beam_gaussian_decomp=beam_gaussian_decomp,$
          sq_volume_beam=sq_volume_beam,res_super=res_super,psf_superres_dim=psf_superres_dim,_Extra=extra)

        if keyword_set(beam_gaussian_decomp) then begin
          beam_int+=baseline_group_n*volume_beam
          beam2_int+=baseline_group_n*sq_volume_beam
          gaussian_params[g_i]=ptr_new(beam_gaussian_params) 
        endif else begin
          ; divide by psf_resolution^2 since the FFT is done at a different resolution and requires a different normalization
          beam_int+=baseline_group_n*Total(psf_base_superres,/double)/psf_resolution^2.
          beam2_int+=baseline_group_n*Total(Abs(psf_base_superres)^2,/double)/psf_resolution^2.
        endelse
        n_grp_use+=baseline_group_n

        IF ~double_flag THEN psf_base_superres=Complex(psf_base_superres)
        ;NOTE: The extra element at the end of each dimension of psf_single contains the same beam as
        ;  the first element, shifted by one pixel. This allows efficient subscripting for interpolation during gridding
        psf_single=Ptrarr(psf_resolution+1,psf_resolution+1)
  
        FOR i=0,psf_resolution-1 DO BEGIN
          FOR j=0,psf_resolution-1 DO $
            psf_single[psf_resolution-1-i,psf_resolution-1-j]=Ptr_new(psf_base_superres[xvals_i+i,yvals_i+j])
        ENDFOR
        FOR i=0,psf_resolution-1 DO BEGIN
          psf_single[psf_resolution-1-i,psf_resolution]=Ptr_new(reform(shift(reform($
            psf_base_superres[xvals_i+i,yvals_i+psf_resolution-1],psf_dim,psf_dim),0,1),psf_dim^2.))
        ENDFOR
        FOR j=0,psf_resolution-1 DO BEGIN
          psf_single[psf_resolution,psf_resolution-1-j]=Ptr_new(reform(shift(reform($
            psf_base_superres[xvals_i+psf_resolution-1,yvals_i+j],psf_dim,psf_dim),1,0),psf_dim^2.))
        ENDFOR
        psf_single[psf_resolution,psf_resolution]=Ptr_new(reform(shift(reform($
          psf_base_superres[xvals_i+psf_resolution-1,yvals_i+psf_resolution-1],psf_dim,psf_dim),1,1),psf_dim^2.))
        psf_single=Ptr_new(psf_single)
        
        FOR bii=0L,baseline_group_n-1 DO beam_arr[pol_i,freq_i,bi_inds[bii]]=psf_single

      ENDFOR
      if keyword_set(beam_per_baseline) then image_power_beam_arr[pol_i,freq_i]=ptr_new(image_power_beam)
      beam2_int*=weight_invert(n_grp_use)/kbinsize^2. ;factor of kbinsize^2 is fourier units normalization
      beam_int*=weight_invert(n_grp_use)/kbinsize^2.
    
      fi_use=where(freq_bin_i EQ freq_i,nf_use)
      FOR fi1=0L,nf_use-1 DO (*primary_beam_sq_area[pol_i])[fi_use[fi1]]=beam2_int
      FOR fi1=0L,nf_use-1 DO (*primary_beam_area[pol_i])[fi_use[fi1]]=beam_int
    
    ENDFOR
    ;ordered by pol, group, with arrays of coeffs, freq
    if keyword_set(beam_gaussian_decomp) then beam_gaussian_params_arr[pol_i] = Pointer_copy(gaussian_params)
  ENDFOR

  FOR pol_i=0,n_pol-1 DO obs.primary_beam_area[pol_i]=primary_beam_area[pol_i]
  FOR pol_i=0,n_pol-1 DO obs.primary_beam_sq_area[pol_i]=primary_beam_sq_area[pol_i]

  ;higher than necessary psf_dim is VERY computationally expensive, but we also don't want to crop the beam if there 
  ;   is real signal. So, in case a larger than necessary psf_dim was specified above, reduce it now if that is safe
  IF Keyword_Set(beam_dim_fit) THEN beam_dim_fit,beam_arr,psf_dim=psf_dim,psf_resolution=psf_resolution,$
    beam_mask_threshold=beam_mask_threshold,psf_xvals=psf_xvals,psf_yvals=psf_yvals,_Extra=extra

  beam_ptr=Ptr_new(beam_arr)
  psf=fhd_struct_init_psf(beam_ptr=beam_ptr,xvals=psf_xvals,yvals=psf_yvals,fbin_i=freq_bin_i,$
    psf_resolution=psf_resolution,psf_dim=psf_dim,complex_flag=1,pol_norm=pol_norm,freq_norm=freq_norm,$
    n_pol=n_pol,n_freq=nfreq_bin,freq_cen=freq_center,group_arr=group_arr,interpolate_kernel=interpolate_kernel,$
    image_power_beam_arr=image_power_beam_arr,ra_arr=ra_arr,dec_arr=dec_arr,psf_image_dim=psf_image_dim,$
    psf_image_resolution=psf_image_resolution,beam_mask_threshold=beam_mask_threshold,$
    beam_gaussian_params_arr=beam_gaussian_params_arr,pix_horizon=obs.dimension/antenna[0].psf_scale)

  ; Briefly create a new psf struct with the beam ptr set to 0 if only metadata is to be saved to disk,
  ; otherwise save the full psf struct
  if keyword_set(save_beam_metadata_only) then begin
    psf_metadata = fhd_struct_init_psf(beam_ptr=0,xvals=psf_xvals,yvals=psf_yvals,fbin_i=freq_bin_i,$
      psf_resolution=psf_resolution,psf_dim=psf_dim,complex_flag=1,pol_norm=pol_norm,freq_norm=freq_norm,$
      n_pol=n_pol,n_freq=nfreq_bin,freq_cen=freq_center,group_arr=group_arr,interpolate_kernel=interpolate_kernel,$
      image_power_beam_arr=image_power_beam_arr,ra_arr=ra_arr,dec_arr=dec_arr,psf_image_dim=psf_image_dim,$
      psf_image_resolution=psf_image_resolution,beam_mask_threshold=beam_mask_threshold,$
      beam_gaussian_params_arr=beam_gaussian_params_arr,pix_horizon=obs.dimension/antenna[0].psf_scale)
    fhd_save_io,status_str,psf_metadata,var='psf',/compress,file_path_fhd=file_path_fhd,no_save=no_save
  ENDIF ELSE fhd_save_io,status_str,psf,var='psf',/compress,file_path_fhd=file_path_fhd,no_save=no_save

  fhd_save_io,status_str,antenna,var='antenna',/compress,file_path_fhd=file_path_fhd,no_save=~save_antenna_model
  IF not antenna_flag THEN undefine_fhd,antenna
  timing=Systime(1)-t00

  RETURN,psf
END
