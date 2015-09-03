pro noise_simulation_suite_enterprise, uvf_input = uvf_input,  $
    recalc_baselines = recalc_baselines, $
    recalc_sim = recalc_sim, refresh_ps = refresh_ps, refresh_binning = refresh_binning
    
  ;sample_factors = [.0001,.0002,.0005,.001,.005,.01,.05,.1,.5, 1]
  ;sample_factors = [.0001,.0005,.001,.005,.01,.05,.1,.5, 1, 5]
  sample_factors = [.01]
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
    
      if keyword_set(recalc_sim) then sim_file_test = 0 else begin
        ;; check to see if simulation exists
        if keyword_set(uvf_input) then sim_file_test = file_test(folder_path + folder_names[sample, obs]+'/*gridded_uvf.sav') $
        else sim_file_test = file_test(folder_path + folder_names[sample, obs]+'/Healpix/*cube*.sav')
      endelse
      
      if sim_file_test eq 0 then begin
        print, 'simulating density: ' + number_formatter(sample_factors[sample]) + ' in folder: ' + version[sample,obs]
        
        if obs eq 0 or keyword_set(recalc_baselines) then use_saved_baselines = 0 else begin
          use_saved_baselines = file_test(folder_path + folder_names[sample, obs-1] + '/baselines_sim.sav')
          if use_saved_baselines eq 1 then begin
            print, 'using saved baseline simulation from ' + version[sample, obs-1]
            restore, folder_path + folder_names[sample, obs-1] + '/baselines_sim.sav'
            save, filename = folder_path + folder_names[sample, obs] + '/baselines_sim.sav', sim_baseline_uu, sim_baseline_vv, sim_baseline_time
          endif else print, 'no saved baselines found, regenerating baselines with simulated density ' + number_formatter(sample_factors[sample])
        endelse
        
        if obsids[obs] eq '1061316176' then obsnum = 36
        if obsids[obs] eq '1061316296' then obsnum = 37
        noise_simulation_enterprise, start=obsnum, end=obsnum, version=version[sample, obs], sim_baseline_density=sample_factors[sample], $
          use_saved_baselines = use_saved_baselines,$
          /recalculate_all
          
      endif
      
      print, 'calculating ps for ' + folder_names[sample, obs]
      
      enterprise_wrapper, folder_names[sample, obs], obsids[obs], uvf_input = uvf_input, /sim,$
        refresh_ps = refresh_ps, refresh_binning = refresh_binning ;what do these keywords do?
        
    endfor
  endfor
end