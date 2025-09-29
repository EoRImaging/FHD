function vis_model_transfer,obs,params,model_transfer

  data_uniq_time_inds = [0,uniq(params.time)+1]
  ;data_uniq_time_inds = data_uniq_time_inds[0:n_elements(data_uniq_time_inds)-2]
  data_n_time = obs.n_time

  if strlowcase(strmid(model_transfer, strlen(model_transfer)-7, 7)) eq '.uvfits' then begin
    ; model_transfer is a UVFITS file
    if file_test(model_transfer) then begin
      uvfits_read,hdr,params_model,layout,vis_model_arr,vis_weights,file_path_vis=model_transfer,n_pol=obs.n_pol,silent=silent,error=error
    endif else message, model_transfer + ' not found during model transfer.'

  endif else begin
    ; model_transfer is a directory of saved visibility model files
    vis_model_arr=PTRARR(obs.n_pol,/allocate)

    for pol_i=0, obs.n_pol-1 do begin
      transfer_name = model_transfer + '/' + obs.obsname + '_vis_model_'+obs.pol_names[pol_i]+'.sav'
      if ~file_test(transfer_name) then $
        message, transfer_name + ' not found during model transfer.'
      vis_model_arr[pol_i] = getvar_savefile(transfer_name,'vis_model_ptr')
      print, "Model visibilities transferred from " + transfer_name
    endfor

    ;; Get the params file associated with the model visibilities
    if file_test(model_transfer+'/'+obs.obsname+'_params.sav') then begin
      params_model = getvar_savefile(model_transfer+'/'+obs.obsname+'_params.sav','params')
    endif else begin
      if file_test(file_dirname(model_transfer)+'/metadata/'+obs.obsname+'_params.sav') then begin
        params_model = getvar_savefile(file_dirname(model_transfer)+'/metadata/'+obs.obsname+'_params.sav','params')
      endif else message, 'No params file found in model transfer directory.'
    endelse 
  endelse

  model_uniq_time_inds = [0,uniq(params_model.time)+1]
  ;model_uniq_time_inds = model_uniq_time_inds[0:n_elements(model_uniq_time_inds)-2]
  model_n_time = n_elements(model_uniq_time_inds)

  if model_n_time NE data_n_time then begin
  ;; Exclude flagged times from the model visibilities if the number of time steps do not match

    data_Jdate = (*obs.baseline_info).Jdate
    time_res_Jdate = (double(obs.time_res)/2.) / (24.*3600.) ;offset to the center of a time step
    model_time = params_model.time[uniq(params_model.time)]
    data_time = params.time[uniq(params.time)]
    tolerance = 1E-5

    ; Match times betweeen model and data
    ; Option 1: You are working with a model made by FHD. Thus the timing convention in the params is the same
    ; Option 2: You are working with a model made by WODEN above version 1.4. This is different from FHD convention
    ;           by half a timestep.
    ; Option 3: You are working with a model made by WODEN below version 1.4. Thus the timing convention in the 
    ;           params is different and you need to convert the model Jdate to the data Jdate within precision.

    ; Option 1
    matched_times_opt1 = intarr(data_n_time)-1
    for data_time_i=0, data_n_time-1 do begin
      min_val= min(abs(model_time - data_time[data_time_i]), min_ind)
      if min_val LT tolerance then matched_times_opt1[data_time_i] = min_ind
    endfor
  
    ; Option 2
    matched_times_opt2 = intarr(data_n_time)-1
    for data_time_i=0, data_n_time-1 do begin
      min_val= min(abs(model_time - (data_time[data_time_i]-time_res_Jdate)), min_ind)
      if min_val LT tolerance then matched_times_opt2[data_time_i] = min_ind
    endfor

    ; Option 3
    matched_times_opt3 = intarr(data_n_time)-1
    for data_time_i=0, data_n_time-1 do begin
      min_val = min(abs(model_time - (data_Jdate[data_time_i]-time_res_Jdate)), min_ind)
      if min_val LT tolerance then matched_times_opt3[data_time_i] = min_ind
    endfor

    ; Find which option was more successful in matching the times
    ; Initialization of matched_times arrays above to -1 allows up to check if all data times are matched (including the 0th index)
    ; Error if no matching times found or if matching times does not obviously cover all data times
    temp = where(matched_times_opt1 NE -1, n_matched_opt1)
    temp = where(matched_times_opt2 NE -1, n_matched_opt2)
    temp = where(matched_times_opt3 NE -1, n_matched_opt3)
    if (n_matched_opt1 EQ 0) and (n_matched_opt2 EQ 0) and (n_matched_opt3 EQ 0) then $
      message, 'ERROR: No matching times found between transferred model and data.'

    max_matched = max([n_matched_opt1, n_matched_opt2, n_matched_opt3],max_matched_ind)
    CASE max_matched_ind OF
      0: matched_times = matched_times_opt1
      1: matched_times = matched_times_opt2
      2: matched_times = matched_times_opt3
    ENDCASE

    temp = where(matched_times NE -1, n_matched)
    if n_matched NE data_n_time then $
      message, 'ERROR: Number of matching times found between transferred model and data does not equal the number of timesteps in data.'

  endif else matched_times = indgen(model_n_time)

  ;An arbitrary larger number than tiles to make an index array
  baseline_mod = ULONG(obs.n_tile*2)
  ;Create a baseline array. Could use params.baseline_arr, but there is no guarentee of model gen origin
  data_baseline_index = params.antenna1*baseline_mod + params.antenna2
  model_baseline_index = params_model.antenna1*baseline_mod + params_model.antenna2

  ;Initialize matched model pointer array
  matched_model = PTRARR(obs.n_pol,/allocate)
  for pol_i=0, obs.n_pol-1 do begin
    if size(*vis_model_arr[pol_i], /type) eq 6 then *matched_model[pol_i] = complex(FLTARR(obs.n_freq, obs.nbaselines * data_n_time))
    if size(*vis_model_arr[pol_i], /type) eq 9 then *matched_model[pol_i] = dcomplex(DBLARR(obs.n_freq, obs.nbaselines * data_n_time))
  endfor

  ; In each matched timestep, match the baselines and fill a new, matched model array 
  ; Unmatched times in the transferred model are not included in the matched model array 
  for time_i=0, data_n_time-1 do begin
    ;suba is the subset of indices in the first array that are also in the second.
    ;subb is the subset of indices in the second array that are also in the first.
    match, data_baseline_index[data_uniq_time_inds[time_i]:data_uniq_time_inds[time_i+1]-1], $
      model_baseline_index[model_uniq_time_inds[matched_times[time_i]]:model_uniq_time_inds[matched_times[time_i]+1]-1], $
      suba, subb
    for pol_i=0, obs.n_pol-1 do (*matched_model[pol_i])[*,suba+data_uniq_time_inds[time_i]] = (*vis_model_arr[pol_i])[*,subb+model_uniq_time_inds[matched_times[time_i]]]
  endfor

  for pol_i=0, obs.n_pol-1 do vis_model_arr[pol_i] = Pointer_copy(matched_model[pol_i])

  return, vis_model_arr

end
