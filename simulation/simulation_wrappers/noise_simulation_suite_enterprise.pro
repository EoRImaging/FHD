pro noise_simulation_suite_enterprise, uvf_input = uvf_input,  $
    recalc_sim = recalc_sim, recalc_baselines = recalc_baselines, $
    refresh_ps = refresh_ps
    
  ;sample_factors = [.0002,.0005,.001,.005,.01,.05,.1,.5,1]
  sample_factors = [0.0002]
  obsids = ['1061316176', '1061316296']
  
  version = make_array(n_elements(sample_factors), n_elements(obsids), /string)
  for sample=0, n_elements(sample_factors)-1 do begin
    for obs=0, n_elements(obsids)-1 do begin
      version[sample, obs] = 'rlb_noise_sim_flatUV_' + obsids[obs] + '_' + number_formatter(sample_factors[sample])
    endfor
  endfor
  
  folder_names = 'fhd_' + version
  folder_path = '/data4/MWA/FHD_Aug23/'
  
  for sample=0, n_elements(sample_factors)-1 do begin
    ;for obs=0, n_elements(obsids)-1 do begin
    for obs=1, n_elements(obsids)-1 do begin
    
      if keyword_set(recalc_sim) then begin
        sim_file_test = 0
        refresh_ps = 1
        print, '***RECALC_SIM KEYWORD SET***: simulating density ' + number_formatter(sample_factors[sample]) + ' in folder ' + version[sample,obs]
      endif else begin
        ;; check to see if simulation exists
        if keyword_set(uvf_input) then sim_file_test = file_test(folder_path + folder_names[sample, obs]+'/*gridded_uvf.sav') $
        else sim_file_test = file_test(folder_path + folder_names[sample, obs]+'/Healpix/*cube*.sav')
        if sim_file_test eq 0 then begin
          print, '***NO SIMULATION FOUND***: simulating density ' + number_formatter(sample_factors[sample]) + ' in folder ' + version[sample,obs]
          refresh_ps = 1
        endif
      endelse
      
      if sim_file_test eq 0 then begin
      
        use_saved_baselines = 0
        if obs ne 0 then begin
          if ~keyword_set(recalc_baselines) then use_saved_baselines = file_test(folder_path + folder_names[sample, obs] + '/baselines_sim.sav')
          if use_saved_baselines eq 1 then print, '***USING SAVED BASELINE SIMULATION***' else begin
            use_saved_baselines = file_test(folder_path + folder_names[sample, 0] + '/baselines_sim.sav')
            if use_saved_baselines eq 1 then begin
              file_mkdir, folder_path + folder_names[sample, obs]
              restore, folder_path + folder_names[sample, 0] + '/baselines_sim.sav'
              save, filename = folder_path + folder_names[sample, obs] + '/baselines_sim.sav', sim_baseline_uu, sim_baseline_vv, sim_baseline_time
              print, '***USING SAVED BASELINE SIMULATION*** from ' + version[sample, 0]
            endif else print, '***GENERATING BASELINE SIMULATION***: with density ' + number_formatter(sample_factors[sample])
          endelse
        endif else begin
          if ~keyword_set(recalc_baselines) then use_saved_baselines = file_test(folder_path + folder_names[sample, obs] + '/baselines_sim.sav')
          if use_saved_baselines eq 1 then print, '***USING SAVED BASELINE SIMULATION***' else $
            print, '***GENERATING BASELINE SIMULATION***: with density ' + number_formatter(sample_factors[sample])
        endelse
        
        if obsids[obs] eq '1061316176' then obsnum = 36
        if obsids[obs] eq '1061316296' then obsnum = 37
        print, '***CALCULATING SIMULATION***: for '  + folder_names[sample, obs]
        noise_simulation_enterprise, start=obsnum, end=obsnum, version=version[sample, obs], sim_baseline_density=sample_factors[sample], $
          use_saved_baselines = use_saved_baselines,$
          /recalculate_all
          
      endif
      
      print, '***CALCULATING PS***: for ' + folder_names[sample, obs]
      
      enterprise_wrapper, folder_names[sample, obs], obsids[obs], uvf_input = uvf_input, /sim,$
        refresh_dft = refresh_ps, refresh_ps = refresh_ps
        
    endfor
  endfor
end