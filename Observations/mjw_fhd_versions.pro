pro mjw_fhd_versions
  except=!except
  !except=0
  heap_gc
  
  ; parse command line args
  compile_opt strictarr
  args = Command_Line_Args(count=nargs)
  obs_id = args[0]
  output_directory = args[1]
  version = args[2]
  if nargs gt 3 then platform = args[3] else platform = '' ;indicates if running on AWS
  
  cmd_args={version:version}
  
  case version of
    
    'mjw_Aug23_Jan2018': begin
      uvfits_version = 4
      uvfits_subversion = 1
      transfer_calibration = '/nfs/mwa-04/r1/EoRuvfits/DiffuseSurvey2015/fhd_mjw_Aug23_Jan2018/calibration/1061313128_f181.2_f187.5_t30_t36_cal.sav'
      cal_bp_transfer = 1
      bandpass_calibrate = 1
      ;calibration_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
      ;calibration_catalog_file_path = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
      filter_background = 1
      diffuse_calibrate = 0
      diffuse_model = 0
      subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
      dft_threshold = 0
      ring_radius = 0
      debug_region_grow = 0
      recalculate_all = 1
      vis_file_list = "/nfs/eor-11/r1/EoRuvfits/jd2456528v4_1/1061313128/TV_Cuts/1061313128_f181.2_f187.5_t18_t24.uvfits"
    end
    
    'rlb_Aug23_full_pol_branch_Dec2017': begin
      uvfits_version = 4
      uvfits_subversion = 1
      ;calibration_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
      calibration_catalog_file_path = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
      filter_background = 1
      diffuse_calibrate = 0
      diffuse_model = 0
      subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
      dft_threshold = 0
      ring_radius = 0
      debug_region_grow = 0
      recalculate_all = 1
    end
    
    'rlb_4pol_sim_Jan2018': begin
      recalculate_all = 1
      max_sources = 200000
      calibration_catalog_file_path = '/nfs/eor-00/h1/rbyrne/catalogs/sim_cal_catalog.sav'
      gain_factor = 0.1
      deconvolve = 1
      return_decon_visibilities = 1
      deconvolution_filter = 'filter_uv_uniform'
      filter_background = 1
      return_cal_visibilities = 1 ;required for 4pol runs
      diffuse_calibrate = 0
      diffuse_model = 0
      cal_bp_transfer = 0
      rephase_weights = 0
      restrict_hpx_inds = 0
      hpx_radius = 20
      return_sidelobe_catalog = 1
      dft_threshold = 0
      ring_radius = 0
      write_healpix_fits = 1
      debug_region_grow = 0
      n_pol = 4
      vis_file_list = '/nfs/eor-00/h1/rbyrne/stokes_I_sim.uvfits'
      remove_sim_flags = 1 ;should be used for simulation
    end
    
    'rlb_4pol_sim_nocal_Jan2018': begin
      recalculate_all = 1
      max_sources = 200000
      calibration_catalog_file_path = '/nfs/eor-00/h1/rbyrne/catalogs/sim_cal_catalog.sav'
      model_catalog_file_path = '/nfs/eor-00/h1/rbyrne/catalogs/sim_cal_catalog.sav'
      gain_factor = 0.1
      deconvolve = 1
      return_decon_visibilities = 1
      deconvolution_filter = 'filter_uv_uniform'
      filter_background = 1
      return_cal_visibilities = 0  ; required to be turned on for 4pol normally, is ok to be off in sim
      diffuse_calibrate = 0
      diffuse_model = 0
      cal_bp_transfer = 0
      rephase_weights = 0
      restrict_hpx_inds = 0
      hpx_radius = 20
      return_sidelobe_catalog = 1
      dft_threshold = 0
      ring_radius = 0
      write_healpix_fits = 1
      debug_region_grow = 0
      n_pol = 4
      vis_file_list = '/nfs/eor-00/h1/rbyrne/stokes_I_sim.uvfits'
      remove_sim_flags = 1 ;should be used for simulation
      calibrate_visibilities = 0
      model_visibilities = '/nfs/eor-00/h1/rbyrne/catalogs/sim_cal_catalog.sav'
    end
    
    'rlb_GLEAM+Fornax_cal_decon_4pol_Jan2018': begin
      uvfits_version = 5
      uvfits_subversion = 1
      max_sources = 200000
      calibration_catalog_file_path = filepath('GLEAM_plus_rlb2017.sav',root=rootdir('FHD'),subdir='catalog_data')
      gain_factor = 0.1
      deconvolve = 1
      return_decon_visibilities = 1
      deconvolution_filter = 'filter_uv_uniform'
      filter_background = 1
      return_cal_visibilities = 1  ; Fixed this
      diffuse_calibrate = 0
      diffuse_model = 0
      cal_bp_transfer = 0
      rephase_weights = 0
      restrict_hpx_inds = 0
      hpx_radius = 10
      subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
      return_sidelobe_catalog = 1
      dft_threshold = 0
      ring_radius = 0
      write_healpix_fits = 1
      debug_region_grow = 0
      n_pol = 4
    end
    
    'rlb_2pol_sim_nocal_Jan2018': begin
      recalculate_all = 1
      max_sources = 200000
      calibration_catalog_file_path = '/nfs/eor-00/h1/rbyrne/catalogs/sim_cal_catalog.sav'
      model_catalog_file_path = '/nfs/eor-00/h1/rbyrne/catalogs/sim_cal_catalog.sav'
      gain_factor = 0.1
      deconvolve = 1
      return_decon_visibilities = 1
      deconvolution_filter = 'filter_uv_uniform'
      filter_background = 1
      return_cal_visibilities = 0  ; required to be turned on for 4pol normally, is ok to be off in sim
      diffuse_calibrate = 0
      diffuse_model = 0
      cal_bp_transfer = 0
      rephase_weights = 0
      restrict_hpx_inds = 0
      hpx_radius = 20
      return_sidelobe_catalog = 1
      dft_threshold = 0
      ring_radius = 0
      write_healpix_fits = 1
      debug_region_grow = 0
      n_pol = 2
      vis_file_list = '/nfs/eor-00/h1/rbyrne/stokes_I_4pol_sim.uvfits'
      remove_sim_flags = 1 ;should be used for simulation
      calibrate_visibilities = 0
      model_visibilities = '/nfs/eor-00/h1/rbyrne/catalogs/sim_cal_catalog.sav'
    end
    
    'rlb_2pol_sim_nocal_master_Jan2018': begin
      recalculate_all = 0
      max_sources = 200000
      calibration_catalog_file_path = '/nfs/eor-00/h1/rbyrne/catalogs/sim_cal_catalog.sav'
      model_catalog_file_path = '/nfs/eor-00/h1/rbyrne/catalogs/sim_cal_catalog.sav'
      gain_factor = 0.1
      deconvolve = 1
      return_decon_visibilities = 1
      deconvolution_filter = 'filter_uv_uniform'
      filter_background = 1
      return_cal_visibilities = 0  ; required to be turned on for 4pol normally, is ok to be off in sim
      diffuse_calibrate = 0
      diffuse_model = 0
      cal_bp_transfer = 0
      rephase_weights = 0
      restrict_hpx_inds = 0
      hpx_radius = 20
      return_sidelobe_catalog = 1
      dft_threshold = 0
      ring_radius = 0
      write_healpix_fits = 1
      debug_region_grow = 0
      n_pol = 2
      vis_file_list = '/nfs/eor-00/h1/rbyrne/sim_visibilities/stokes_I_sim_master_2pol.uvfits'
      remove_sim_flags = 1 ;should be used for simulation
      calibrate_visibilities = 0
      model_visibilities = '/nfs/eor-00/h1/rbyrne/catalogs/sim_cal_catalog.sav'
    end
    
  endcase
  
  if ~keyword_set(vis_file_list) and keyword_set(instrument) then begin
    if instrument eq 'hera' then begin
      vis_file_list = '/nfs/eor-00/h1/rbyrne/HERA_analysis/zen.2458042.'+obs_id+'.xx.HH.uvR.uvfits'
      if obs_id eq '38650' then begin
        vis_file_list = '/nfs/eor-00/h1/rbyrne/HERA_analysis/zen.2458042.'+obs_id+'.yy.HH.uvR.uvfits'
      endif
    endif
  endif
  
  if ~keyword_set(vis_file_list) then begin
    if platform eq 'aws' then begin
      vis_file_list = '/uvfits/' + STRING(obs_id) + '.uvfits'
    endif else begin
      SPAWN, 'read_uvfits_loc.py -v ' + STRING(uvfits_version) + ' -s ' + $
        STRING(uvfits_subversion) + ' -o ' + STRING(obs_id), vis_file_list
    endelse
  endif
  
  undefine, uvfits_subversion, uvfits_version
  
  fhd_file_list=fhd_path_setup(vis_file_list,version=version,output_directory=output_directory)
  healpix_path=fhd_path_setup(output_dir=output_directory,subdir='Healpix',output_filename='Combined_obs',version=version)
  
  
  ; Set global defaults and bundle all the variables into a structure.
  ; Any keywords set on the command line or in the top-level wrapper will supercede these defaults
  eor_wrapper_defaults,extra
  fhd_depreciation_test, _Extra=extra
  
  print,""
  print,"Keywords set in wrapper:"
  print,structure_to_text(extra)
  print,""
  
  general_obs,_Extra=extra
  
end
