function vis_model_transfer,obs,model_transfer

  ;; Option to transfer pre-made and unflagged model visbilities
  vis_model_arr=PTRARR(obs.n_pol,/allocate)

  for pol_i=0, obs.n_pol-1 do begin
    transfer_name = model_transfer + '/' + obs.obsname + '_vis_model_'+obs.pol_names[pol_i]+'.sav'
    if ~file_test(transfer_name) then $
      message, transfer_name + ' not found during model transfer.'
    vis_model_arr[pol_i] = getvar_savefile(transfer_name,'vis_model_ptr')
    print, "Model visibilities transferred from " + transfer_name
  endfor

  return, vis_model_arr

end
