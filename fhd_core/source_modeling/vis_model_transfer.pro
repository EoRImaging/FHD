function vis_model_transfer,obs,params,model_transfer

  data_nbaselines = obs.nbaselines
  data_n_time = obs.n_time

  ;; Option to transfer pre-made and unflagged model visbilities
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
  model_n_time = n_elements(uniq(params_model.time))
  model_nbaselines = (uniq(params_model.time))[0]+1

  if model_n_time NE data_n_time then begin
  ;; Exclude flagged times from the model visibilities if the number of time steps do not match

    data_Jdate = (*obs.baseline_info).Jdate
    time_res_Jdate = (double(obs.time_res)/2.) / (24.*3600.) ;offset to the center of a time step
    model_time = params_model.time[uniq(params_model.time)]
    data_time = params.time[uniq(params.time)]
    tolerance = 1E-5

    ; Match times betweeen model and data
    ; Option 1: You are working with a model made by FHD. Thus the timing convention in the params is the same
    ; Option 2: You are working with a model made by WODEN. Thus the timing convention in the params is different
    ;           and you need to convert the model Jdate to the data Jdate within precision.

    ; Option 1
    matched_times_opt1 = intarr(data_n_time)
    for data_time_i=0, data_n_time-1 do begin
      min_val= min(abs(model_time - data_time[data_time_i]), min_ind)
      if min_val LT tolerance then matched_times_opt1[data_time_i] = min_ind
    endfor

    ; Option 2
    matched_times_opt2 = intarr(data_n_time)
    for data_time_i=0, data_n_time-1 do begin
      min_val = min(abs(model_time - (data_Jdate[data_time_i]-time_res_Jdate)), min_ind)
      if min_val LT tolerance then matched_times_opt2[data_time_i] = min_ind
    endfor

    ; Find which option was more successful in matching the times
    if total(matched_times_opt1 < 1) GT total(matched_times_opt2 < 1) then matched_times = matched_times_opt1
    if total(matched_times_opt1 < 1) LT total(matched_times_opt2 < 1) then matched_times = matched_times_opt2

    endif else matched_times = indgen(model_n_time)

  ;An arbitrary larger number than tiles to make an index array
  baseline_mod = ULONG(obs.n_tile*2)
  ;Create a baseline array. Could use params.baseline_arr, but there is no guarentee of model gen origin
  data_baseline_index = params.antenna1*baseline_mod + params.antenna2
  model_baseline_index = params_model.antenna1*baseline_mod + params_model.antenna2

  ;Initialize matched model pointer array
  matched_model = PTRARR(obs.n_pol,/allocate)
  for pol_i=0, obs.n_pol-1 do begin
    if size(*vis_model_arr[pol_i], /type) eq 6 then *matched_model[pol_i] = complex(FLTARR(obs.n_freq, data_nbaselines * data_n_time))
    if size(*vis_model_arr[pol_i], /type) eq 9 then *matched_model[pol_i] = dcomplex(DBLARR(obs.n_freq, data_nbaselines * data_n_time))
  endfor

  ; In each matched timestep, match the baselines and fill a new, matched model array 
  for time_i=0, data_n_time-1 do begin
    ;suba is the subset of indices in the first array that are also in the second.
    ;subb is the subset of indices in the second array that are also in the first.
    match, data_baseline_index[time_i*data_nbaselines:(time_i+1)*data_nbaselines-1], $
      model_baseline_index[matched_times[time_i]*model_nbaselines:(matched_times[time_i]+1)*model_nbaselines-1], $
      suba, subb
    for pol_i=0, obs.n_pol-1 do (*matched_model[pol_i])[*,suba+time_i*(data_nbaselines)] = (*vis_model_arr[pol_i])[*,subb+matched_times[time_i]*model_nbaselines]
  endfor

  for pol_i=0, obs.n_pol-1 do vis_model_arr[pol_i] = Pointer_copy(matched_model[pol_i])

  return, vis_model_arr

end
