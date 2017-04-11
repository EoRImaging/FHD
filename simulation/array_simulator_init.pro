PRO array_simulator_init,obs,params,layout,error=error,sim_from_uvfits_filepath=sim_from_uvfits_filepath,$
    sim_from_fhd_filepath=sim_from_fhd_filepath,simulate_header=simulate_header,$
    simulate_baselines=simulate_baselines, sim_baseline_time_inds=sim_baseline_time_inds, $
    instrument=instrument,n_pol=n_pol,_Extra=extra
    
  CASE 1 OF
    Keyword_Set(sim_from_fhd_filepath): BEGIN
      file_path_fhd=sim_from_fhd_filepath
      fhd_save_io,status_str,file_path_fhd=file_path_fhd,var='status_str',_Extra=extra
      IF status_str.obs EQ 0 OR status_str.params EQ 0 THEN BEGIN
        error=1
        print,'Specified path is not a valid directory of FHD output: '+sim_from_fhd_filepath
      ENDIF ELSE BEGIN
        fhd_save_io,status_str,obs,var='obs',/restore,file_path_fhd=file_path_fhd,_Extra=extra
        fhd_save_io,status_str,params,var='params',/restore,file_path_fhd=file_path_fhd,_Extra=extra
      ENDELSE
      RETURN
    END
    Keyword_Set(sim_from_uvfits_filepath): BEGIN
      file_path_vis=sim_from_uvfits_filepath
      uvfits_read,hdr_in,params_in,layout,file_path_vis=file_path_vis,n_pol=0,silent=silent,_Extra=extra
    END
    ELSE: ;do nothing
  ENDCASE
  
  IF N_Elements(instrument) EQ 0 THEN instrument='mwa'
  IF Keyword_Set(simulate_header) OR ~Keyword_Set(hdr_in) THEN $
    hdr=uvfits_header_simulate(hdr_in,n_pol=n_pol, instrument=instrument,_Extra=extra) $
  ELSE hdr=hdr_in
  
  IF Keyword_Set(simulate_baselines) OR ~Keyword_Set(params_in) THEN BEGIN
    if n_elements(sim_baseline_time_inds) gt 0 then begin
      default_time = params_in.time
      unique_times = default_time[Uniq(default_time)]
      n_time_in=N_Elements(unique_times)
      if max(sim_baseline_time_inds) gt n_time_in-1 then begin
        n_time_new = max(sim_baseline_time_inds)+1
        ;; need to make slots for more times than exist in original uvfits file
        delta_t = unique_times[1] - unique_times[0]
        time_arr = [unique_times, (dindgen(n_time_new-n_time_in)+1.)*delta_t+max(unique_times)]
        sim_baseline_time = time_arr[sim_baseline_time_inds]
      endif else sim_baseline_time = unique_times[sim_baseline_time_inds]
    endif
    params=uvfits_params_simulate(hdr,params_in,sim_baseline_time=sim_baseline_time,_Extra=extra)
  ENDIF ELSE params=params_in

  IF N_Elements(file_path_vis) EQ 0 THEN file_path_vis='simulation'
  obs=fhd_struct_init_obs(file_path_vis,hdr,params,n_pol=n_pol,instrument=instrument,_Extra=extra)
END