FUNCTION vis_model_freq_split,obs,psf,params,flag_arr,model_uv_arr=model_uv_arr,vis_data_arr=vis_data_arr,vis_model_arr=vis_model_arr,$
    weights_arr=weights_arr,variance_arr=variance_arr,model_arr=model_arr,n_avg=n_avg,timing=timing,fft=fft,source_list=source_list,$
    file_path_fhd=file_path_fhd,even_only=even_only,odd_only=odd_only,rephase_weights=rephase_weights,silent=silent,$
    vis_n_arr=vis_n_arr,x_range=x_range,y_range=y_range,preserve_visibilities=preserve_visibilities,$
    obs_out=obs_out,psf_out=psf_out,save_uvf=save_uvf, uvf_name=uvf_name,_Extra=extra
  ext='.UVFITS'
  t0=Systime(1)
  
  IF N_Elements(silent) EQ 0 THEN silent=0
  pol_names=['xx','yy','xy','yx']
  flags_filepath=file_path_fhd+'_flags.sav'
  params_filepath=file_path_fhd+'_params.sav'
  psf_filepath=file_path_fhd+'_beams.sav'
  obs_filepath=file_path_fhd+'_obs.sav'
  vis_filepath=file_path_fhd+'_vis_'
  
  if keyword_set(save_uvf) then begin
    if n_elements(uvf_name) ne 0 then uvf_filepath = file_path_fhd+'_'+uvf_name+'_gridded_uvf.sav' $
    else uvf_filepath = file_path_fhd+'_gridded_uvf.sav'
  endif
  
  IF N_Elements(obs) EQ 0 THEN obs=getvar_savefile(obs_filepath,'obs')
  IF N_Elements(psf) EQ 0 THEN psf=getvar_savefile(psf_filepath,'psf')
  IF N_Elements(params) EQ 0 THEN params=getvar_savefile(params_filepath,'params')
  IF N_Elements(flag_arr) EQ 0 THEN flag_arr=getvar_savefile(flags_filepath,'flag_arr')
  
  n_freq=obs.n_freq
  n_pol=obs.n_pol
  dimension=obs.dimension
  degpix=obs.degpix
  residual_flag=obs.residual
  
  IF Min(Ptr_valid(vis_data_arr)) EQ 0 THEN BEGIN
    vis_data_arr=Ptrarr(n_pol)
    FOR pol_i=0,n_pol-1 DO vis_data_arr[pol_i]=$
      getvar_savefile(vis_filepath+pol_names[pol_i]+'.sav','vis_ptr',verbose=~silent)
  ENDIF
  IF Min(Ptr_valid(vis_model_arr)) EQ 0 THEN BEGIN
    model_flag=1
    FOR pol_i=0,n_pol-1 DO model_flag*=file_test(vis_filepath+'model_'+pol_names[pol_i]+'.sav')
    IF model_flag THEN BEGIN
      vis_model_arr=Ptrarr(n_pol)
      FOR pol_i=0,n_pol-1 DO vis_model_arr[pol_i]=$
        getvar_savefile(vis_filepath+'model_'+pol_names[pol_i]+'.sav','vis_ptr',verbose=~silent)
    ENDIF
  ENDIF ELSE model_flag=1
  
  IF Keyword_Set(even_only) OR Keyword_Set(odd_only) THEN flag_arr_use=split_vis_flags(obs,flag_arr,even_only=even_only,odd_only=odd_only,/preserve_flags) $
  ELSE flag_arr_use=pointer_copy(flag_arr)
  IF n_pol GT 1 THEN flag_test=Total(*flag_arr_use[1]>*flag_arr_use[0]>0,1) ELSE flag_test=Total(*flag_arr_use[0]>0,1)
  bi_use=where(flag_test)
  
  IF N_Elements(n_avg) EQ 0 THEN BEGIN
    freq_bin_i2=(*obs.baseline_info).fbin_i
    n_avg=Round(n_freq/Max(freq_bin_i2+1))
  ENDIF ELSE BEGIN
    freq_bin_i2=Floor(lindgen(n_freq)/n_avg)
  ENDELSE
  
  nf=Max(freq_bin_i2)+1L
  IF Keyword_Set(source_list) OR Keyword_Set(model_uv_arr) THEN model_flag=1; ELSE model_flag=0 ;now set above
  IF Keyword_Set(residual_flag) THEN model_flag=0
  IF Min(Ptr_valid(vis_model_arr)) EQ 0 THEN BEGIN
    IF model_flag THEN BEGIN
      vis_model_arr=vis_source_model(source_list,obs,psf,params,flag_arr_use,model_uv_arr=model_uv_arr,$
        file_path_fhd=file_path_fhd,timing=t_model,silent=silent,_Extra=extra)
      IF ~Keyword_Set(silent) THEN print,"Vis modeling and degridding: ", strn(t_model)
    ENDIF ELSE vis_model_arr=Ptrarr(n_pol)
  ENDIF ELSE IF ~Keyword_Set(residual_flag) THEN model_flag=1
  
  IF Keyword_Set(obs_out) THEN BEGIN
    n_freq=obs_out.n_freq
    n_pol=obs_out.n_pol
    dimension=obs_out.dimension
    degpix=obs_out.degpix
    residual_flag=obs_out.residual
  ENDIF ELSE obs_out=obs
  IF N_Elements(psf_out) EQ 0 THEN psf_out=psf
  
  if keyword_set(save_uvf) then begin
    dirty_uv_arr=Ptrarr(n_pol,nf,/allocate)
    weights_uv_arr=Ptrarr(n_pol,nf,/allocate)
    variance_uv_arr=Ptrarr(n_pol,nf,/allocate)
    model_uv_arr=Ptrarr(n_pol,nf,/allocate)
  endif
  
  dirty_arr=Ptrarr(n_pol,nf,/allocate)
  weights_arr=Ptrarr(n_pol,nf,/allocate)
  variance_arr=Ptrarr(n_pol,nf,/allocate)
  model_arr=Ptrarr(n_pol,nf,/allocate)
  vis_n_arr=Fltarr(n_pol,nf)
  
  IF Keyword_Set(rephase_weights) THEN rephase_use=phase_shift_uv_image(obs_out,/to_orig_phase) ELSE rephase_use=1.
  t_grid=0
  FOR pol_i=0,n_pol-1 DO BEGIN
    vis_ptr=vis_data_arr[pol_i]
    model_ptr=vis_model_arr[pol_i]
    freq_use=(*obs_out.baseline_info).freq_use
    n_vis_use=0.
    IF Keyword_Set(fft) THEN init_arr=Fltarr(dimension,dimension) ELSE init_arr=Complexarr(dimension,dimension)
    IF N_Elements(x_range)<N_Elements(y_range) GT 0 THEN init_arr=extract_subarray(init_arr,x_range,y_range)
    FOR fi=0L,nf-1 DO BEGIN
      fi_use=where((freq_bin_i2 EQ fi) AND (freq_use GT 0),nf_use)
      IF Keyword_Set(model_flag) THEN model_return=1
      variance_holo=1 ;initialize
      weights_holo=1 ;initialize
      IF nf_use EQ 0 THEN n_vis=0 ELSE $
        dirty_UV=visibility_grid(vis_ptr,flag_arr_use[pol_i],obs_out,psf_out,params,timing=t_grid0,fi_use=fi_use,bi_use=bi_use,$
        polarization=pol_i,weights=weights_holo,variance=variance_holo,silent=1,mapfn_recalculate=0,$
        model_ptr=model_ptr,n_vis=n_vis,/preserve_visibilities,model_return=model_return)
      ;        IF nf_use EQ 0 THEN n_vis=0 ELSE IF Keyword_Set(inds_patch) THEN $
      ;            dirty_UV=visibility_patch_grid(vis_ptr,flag_arr_use[pol_i],obs_out,psf_out,params,timing=t_grid0,fi_use=fi_use,bi_use=bi_use,$
      ;                polarization=pol_i,weights=weights_holo,variance=variance_holo,silent=1,mapfn_recalculate=0,$
      ;                model_ptr=model_ptr,n_vis=n_vis,/preserve_visibilities,model_return=model_return,inds_patch=inds_patch,$
      ;                obs_patch=obs_patch,psf_patch=psf_patch,rephase_vis_flag=rephase_vis_flag,_Extra=extra) $
      ;        ELSE $
      ;            dirty_UV=visibility_grid(vis_ptr,flag_arr_use[pol_i],obs_out,psf_out,params,timing=t_grid0,fi_use=fi_use,bi_use=bi_use,$
      ;                polarization=pol_i,weights=weights_holo,variance=variance_holo,silent=1,mapfn_recalculate=0,$
      ;                model_ptr=model_ptr,n_vis=n_vis,/preserve_visibilities,model_return=model_return)
      IF n_vis EQ 0 THEN BEGIN
        *dirty_arr[pol_i,fi]=init_arr
        *weights_arr[pol_i,fi]=init_arr
        *variance_arr[pol_i,fi]=init_arr
        IF Keyword_Set(model_flag) THEN *model_arr[pol_i,fi]=init_arr
        CONTINUE
      ENDIF
      n_vis_use+=n_vis
      vis_n_arr[pol_i,fi]=n_vis
      
      if keyword_set(save_uvf) then begin
        *dirty_uv_arr[pol_i,fi]=dirty_uv*n_vis
        *weights_uv_arr[pol_i,fi]=weights_holo*rephase_use*n_vis
        *variance_uv_arr[pol_i,fi]=variance_holo*rephase_use*n_vis
        *model_uv_arr[pol_i,fi]=model_return*n_vis
      endif
      
      IF Keyword_Set(fft) THEN BEGIN
        IF N_Elements(x_range)<N_Elements(y_range) GT 0 THEN BEGIN
          *dirty_arr[pol_i,fi]=extract_subarray(dirty_image_generate(dirty_uv,degpix=degpix)*n_vis,x_range,y_range)
          *weights_arr[pol_i,fi]=extract_subarray(dirty_image_generate(weights_holo*rephase_use,degpix=degpix)*n_vis,x_range,y_range)
          *variance_arr[pol_i,fi]=extract_subarray(dirty_image_generate(variance_holo*rephase_use,degpix=degpix)*n_vis,x_range,y_range)
          IF N_Elements(model_return) GT 1 THEN $
            *model_arr[pol_i,fi]=extract_subarray(dirty_image_generate(model_return,degpix=degpix)*n_vis,x_range,y_range) $
          ELSE IF Keyword_Set(model_flag) THEN *model_arr[pol_i,fi]=init_arr
        ENDIF ELSE BEGIN
          *dirty_arr[pol_i,fi]=dirty_image_generate(dirty_uv,degpix=degpix)*n_vis
          *weights_arr[pol_i,fi]=dirty_image_generate(weights_holo*rephase_use,degpix=degpix)*n_vis
          *variance_arr[pol_i,fi]=dirty_image_generate(variance_holo*rephase_use,degpix=degpix)*n_vis
          IF N_Elements(model_return) GT 1 THEN $
            *model_arr[pol_i,fi]=dirty_image_generate(model_return,degpix=degpix)*n_vis $
          ELSE IF Keyword_Set(model_flag) THEN *model_arr[pol_i,fi]=init_arr
        ENDELSE
      ENDIF ELSE BEGIN
        *dirty_arr[pol_i,fi]=dirty_uv*n_vis
        *weights_arr[pol_i,fi]=weights_holo*rephase_use*n_vis
        *variance_arr[pol_i,fi]=variance_holo*rephase_use*n_vis
        IF N_Elements(model_return) GT 1 THEN *model_arr[pol_i,fi]=model_return*n_vis
      ENDELSE
      IF Keyword_Set(t_grid0) THEN t_grid+=t_grid0
    ENDFOR
    IF ~Keyword_Set(preserve_visibilities) THEN ptr_free,vis_ptr,model_ptr
  ENDFOR
  obs_out.n_vis=n_vis_use
  IF ~Arg_present(obs_out) THEN  obs.n_vis=n_vis_use
  
  if keyword_set(save_uvf) then save, filename = uvf_filepath, dirty_uv_arr, weights_uv_arr, variance_uv_arr, model_uv_arr, obs_out, /compress
  
  IF ~Keyword_Set(silent) THEN print,"Gridding timing: ",strn(t_grid)
  timing=Systime(1)-t0
  RETURN,dirty_arr
END