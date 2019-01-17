pro rlb_fhd_versions
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
  
    'rlb_master_catalog_cal_Sept2016': begin
      recalculate_all = 1
      mapfn_recalculate = 0
      uvfits_version = 5
      uvfits_subversion = 1
      calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
      rephase_weights = 0
      diffuse_calibrate = 0
      diffuse_model = 0
    end
    
    'rlb_GLEAM_cal_Sept2016': begin
      recalculate_all = 1
      mapfn_recalculate = 1
      uvfits_version = 5
      uvfits_subversion = 1
      calibration_catalog_file_path=filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
      rephase_weights = 0
      restrict_hpx_inds = 0
      hpx_radius = 10
      diffuse_calibrate = 0
      diffuse_model = 0
      ring_radius = 0
    end
    
    'rlb_GLEAM_cal_decon_Nov2016': begin
      uvfits_version = 5
      uvfits_subversion = 1
      max_sources = 200000
      calibration_catalog_file_path = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
      gain_factor = 0.1
      deconvolve = 1
      return_decon_visibilities = 1
      deconvolution_filter = 'filter_uv_uniform'
      filter_background = 1
      return_cal_visibilities = 0
      snapshot_recalculate = 1
      recalculate_all = 0
      diffuse_calibrate = 0
      diffuse_model = 0
      rephase_weights = 0
      restrict_hpx_inds = 0
      hpx_radius = 10
      subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
      return_sidelobe_catalog = 1
      dft_threshold = 0
      ring_radius = 0
      write_healpix_fits = 1
      debug_region_grow = 0
    end
    
    'rlb_1130789944_run1_cal_Dec2016': begin
      recalculate_all = 0
      mapfn_recalculate = 0
      uvfits_version = 5
      uvfits_subversion = 1
      calibration_catalog_file_path = '/nfs/mwa-08/d1/DiffuseSurvey2015/1130789944_run1_catalog.sav'
      calibration_subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
      rephase_weights = 0
      restrict_hpx_inds = 0
      hpx_radius = 10
      diffuse_calibrate = 0
      diffuse_model = 0
      ring_radius = 0
    end
    
    'rlb_1130781304_run1_cal_Dec2016': begin
      recalculate_all = 0
      mapfn_recalculate = 0
      uvfits_version = 5
      uvfits_subversion = 1
      calibration_catalog_file_path = '/nfs/mwa-08/d1/DiffuseSurvey2015/1130781304_run1_catalog.sav'
      calibration_subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
      rephase_weights = 0
      restrict_hpx_inds = 0
      hpx_radius = 10
      diffuse_calibrate = 0
      diffuse_model = 0
      ring_radius = 0
    end
    
    'rlb_1130789944_run1_cal_decon_Jan2017': begin
      uvfits_version = 5
      uvfits_subversion = 1
      max_sources = 200000
      calibration_catalog_file_path = '/nfs/mwa-08/d1/DiffuseSurvey2015/1130789944_run1_catalog.sav'
      gain_factor = 0.1
      deconvolve = 1
      return_decon_visibilities = 1
      deconvolution_filter = 'filter_uv_uniform'
      filter_background = 1
      return_cal_visibilities = 0
      snapshot_recalculate = 1
      recalculate_all = 1
      diffuse_calibrate = 0
      diffuse_model = 0
      rephase_weights = 0
      restrict_hpx_inds = 0
      hpx_radius = 10
      subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
      return_sidelobe_catalog = 1
      dft_threshold = 0
      ring_radius = 0
    end
    
    'rlb_1130781304_run1_cal_decon_Jan2017': begin
      uvfits_version = 5
      uvfits_subversion = 1
      max_sources = 200000
      calibration_catalog_file_path = '/nfs/mwa-08/d1/DiffuseSurvey2015/1130781304_run1_catalog.sav'
      gain_factor = 0.1
      deconvolve = 1
      return_decon_visibilities = 1
      deconvolution_filter = 'filter_uv_uniform'
      filter_background = 1
      return_cal_visibilities = 0
      snapshot_recalculate = 1
      recalculate_all = 1
      diffuse_calibrate = 0
      diffuse_model = 0
      rephase_weights = 0
      restrict_hpx_inds = 0
      hpx_radius = 10
      subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
      return_sidelobe_catalog = 1
      dft_threshold = 0
      ring_radius = 0
    end
    
    'rlb_1130789944_run2_cal_Feb2017': begin
      recalculate_all = 1
      mapfn_recalculate = 1
      uvfits_version = 5
      uvfits_subversion = 1
      calibration_catalog_file_path = '/nfs/mwa-08/d1/DiffuseSurvey2015/1130789944_run2_catalog.sav'
      calibration_subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
      rephase_weights = 0
      restrict_hpx_inds = 0
      hpx_radius = 10
      diffuse_calibrate = 0
      diffuse_model = 0
      ring_radius = 0
    end
    
    'rlb_1130781304_run2_cal_Feb2017': begin
      recalculate_all = 1
      mapfn_recalculate = 1
      uvfits_version = 5
      uvfits_subversion = 1
      calibration_catalog_file_path = '/nfs/mwa-08/d1/DiffuseSurvey2015/1130781304_run2_catalog.sav'
      calibration_subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
      rephase_weights = 0
      restrict_hpx_inds = 0
      hpx_radius = 10
      diffuse_calibrate = 0
      diffuse_model = 0
      ring_radius = 0
    end
    
    'rlb_GLEAM_cal_decon_4pol_Apr2017': begin
      uvfits_version = 5
      uvfits_subversion = 1
      max_sources = 200000
      calibration_catalog_file_path = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
      gain_factor = 0.1
      deconvolve = 1
      return_decon_visibilities = 1
      deconvolution_filter = 'filter_uv_uniform'
      filter_background = 1
      return_cal_visibilities = 0
      snapshot_recalculate = 1
      recalculate_all = 0
      diffuse_calibrate = 0
      diffuse_model = 0
      rephase_weights = 0
      restrict_hpx_inds = 0
      hpx_radius = 10
      subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
      return_sidelobe_catalog = 1
      dft_threshold = 0
      ring_radius = 0
      n_pol = 4
    end
    
    'rlb_GLEAM+PicA_cal_decon_Jun2017': begin
      uvfits_version = 5
      uvfits_subversion = 1
      max_sources = 200000
      calibration_catalog_file_path = '/nfs/eor-00/h1/rbyrne/catalogs/GLEAM+PicA-88comp.sav'
      gain_factor = 0.1
      deconvolve = 1
      return_decon_visibilities = 1
      deconvolution_filter = 'filter_uv_uniform'
      filter_background = 1
      return_cal_visibilities = 0
      snapshot_recalculate = 1
      recalculate_all = 1
      diffuse_calibrate = 0
      diffuse_model = 0
      rephase_weights = 0
      restrict_hpx_inds = 0
      hpx_radius = 10
      subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
      return_sidelobe_catalog = 1
      dft_threshold = 0
      ring_radius = 0
      write_healpix_fits = 1
    end
    
    'rlb_GLEAM_plus_cal_decon_Jul2017': begin
      uvfits_version = 5
      uvfits_subversion = 1
      max_sources = 200000
      calibration_catalog_file_path = filepath('GLEAM_plus_rlb2017.sav',root=rootdir('FHD'),subdir='catalog_data')
      gain_factor = 0.1
      deconvolve = 1
      return_decon_visibilities = 1
      deconvolution_filter = 'filter_uv_uniform'
      filter_background = 1
      return_cal_visibilities = 0
      snapshot_recalculate = 1
      recalculate_all = 0
      diffuse_calibrate = 0
      diffuse_model = 0
      rephase_weights = 0
      restrict_hpx_inds = 0
      hpx_radius = 10
      subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
      return_sidelobe_catalog = 1
      dft_threshold = 0
      ring_radius = 0
      write_healpix_fits = 1
      debug_region_grow = 0
    end
    
    'rlb_eor0_GLEAM_cal_norephase_Aug2017': begin
      recalculate_all = 0
      uvfits_version = 5
      uvfits_subversion = 1
      calibration_catalog_file_path = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
      calibration_subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
      rephase_weights = 0
      restrict_hpx_inds = 0
      hpx_radius = 10
      diffuse_calibrate = 0
      diffuse_model = 0
      ring_radius = 0
    end
    
    'rlb_eor0_run1_cal_norephase_Aug2017': begin
      recalculate_all = 0
      uvfits_version = 5
      uvfits_subversion = 1
      calibration_catalog_file_path = '/nfs/mwa-04/r1/EoRuvfits/DiffuseSurvey2015/1131454296_run1_catalog.sav'
      calibration_subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
      rephase_weights = 0
      restrict_hpx_inds = 0
      hpx_radius = 10
      diffuse_calibrate = 0
      diffuse_model = 0
      ring_radius = 0
    end
    
    'rlb_GLEAM+Fornax_cal_decon_Nov2016': begin
      uvfits_version = 5
      uvfits_subversion = 1
      max_sources = 200000
      calibration_catalog_file_path = filepath('GLEAM_plus_rlb2017.sav',root=rootdir('FHD'),subdir='catalog_data')
      gain_factor = 0.1
      deconvolve = 1
      return_decon_visibilities = 1
      deconvolution_filter = 'filter_uv_uniform'
      filter_background = 1
      return_cal_visibilities = 0
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
    end
    
    'rlb_1130776744_run1_cal_decon_Sept2017': begin
      uvfits_version = 5
      uvfits_subversion = 1
      max_sources = 200000
      calibration_catalog_file_path = filepath('GLEAM_plus_rlb2017.sav',root=rootdir('FHD'),subdir='catalog_data')
      gain_factor = 0.1
      deconvolve = 1
      return_decon_visibilities = 1
      deconvolution_filter = 'filter_uv_uniform'
      filter_background = 1
      return_cal_visibilities = 0
      snapshot_recalculate = 1
      recalculate_all = 1
      diffuse_calibrate = 0
      diffuse_model = 0
      rephase_weights = 0
      restrict_hpx_inds = 0
      hpx_radius = 10
      subtract_sidelobe_catalog = '/nfs/mwa-04/r1/EoRuvfits/DiffuseSurvey2015/1130776744_run1_catalog.sav'
      return_sidelobe_catalog = 1
      dft_threshold = 0
      ring_radius = 0
      write_healpix_fits = 1
      debug_region_grow = 0
    end
    
    'rlb_1130776864_run1_cal_decon_Sept2017': begin
      uvfits_version = 5
      uvfits_subversion = 1
      max_sources = 200000
      calibration_catalog_file_path = '/nfs/mwa-04/r1/EoRuvfits/DiffuseSurvey2015/1130776864_run1_catalog.sav'
      gain_factor = 0.1
      deconvolve = 1
      return_decon_visibilities = 1
      deconvolution_filter = 'filter_uv_uniform'
      filter_background = 1
      return_cal_visibilities = 0
      snapshot_recalculate = 1
      recalculate_all = 1
      diffuse_calibrate = 0
      diffuse_model = 0
      rephase_weights = 0
      restrict_hpx_inds = 0
      hpx_radius = 10
      subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
      return_sidelobe_catalog = 1
      dft_threshold = 0
      ring_radius = 0
      write_healpix_fits = 1
      debug_region_grow = 0
    end
    
    'rlb_PAPER_Sept2017': begin
      ;following Nichole's example exactly
      instrument = 'paper'
      calibration_auto_initialize = 1
      ref_antenna = 1
      time_offset=5.*60.
      hera_inds = [80,104,96,64,53,31,65,88,9,20,89,43,105,22,81,10,72,112,97]+1
      paper_inds = [1,3,4,13,15,16,23,26,37,38,41,42,46,47,49,50,56,54,58,59,61,63,66,67,70,71,73,74,82,83,87,90,98,99,103,106,124,123,122,121,120,119,118,117,0,14,44,113,126,127]+1
      paper_hex = [2,21,45,17,68,62,116,125,84,100,85,57,69,40,101,102,114,115,86]+1
      paper_pol = [25,19,48,29,24,28,55,34,27,51,35,75,18,76,5,77,32,78,30,79,33,91,6,92,52,93,7,94,12,95,8,107,11,108,36,109,60,110,39,111]+1
      tile_flag_list = [paper_hex,paper_pol,hera_inds] ;flag all but PAPER imaging
      cal_time_average = 0
      nfreq_average = 1024
      calibration_catalog_file_path=filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
      cable_bandpass_fit = 0
      cal_mode_fit = 0
      max_calibration_sources = 500
      cal_reflection_mode_theory = 0
      cal_reflection_hyperresolve = 0
      cal_reflection_mode_file = 0
      diffuse_calibrate = 0
      diffuse_model = 0
      beam_offset_time = 300
      flag_calibration = 0
      min_cal_baseline = 10
      calibration_polyfit = 0
      bandpass_calibrate = 0
      flag_visibilities = 1
      elements = 4096
      vis_file_list = '/nfs/eor-00/h1/rbyrne/HERA_analysis/zen.2457458.16694.xx.uvUR.uvfits'
    end
    
    'rlb_GLEAM+Fornax_cal_Sept2017': begin
      recalculate_all = 1
      mapfn_recalculate = 1
      uvfits_version = 5
      uvfits_subversion = 1
      calibration_catalog_file_path=filepath('GLEAM_plus_rlb2017.sav',root=rootdir('FHD'),subdir='catalog_data')
      rephase_weights = 0
      restrict_hpx_inds = 0
      hpx_radius = 10
      diffuse_calibrate = 0
      diffuse_model = 0
      ring_radius = 0
    end
    
    'rlb_HERA_only_Oct2017_2': begin ; Use only a 10 MHz interval
      instrument = 'hera'
      calibration_auto_initialize = 1
      ref_antenna = 80
      time_offset=5.*60.
      hera_inds = [80,104,96,64,53,31,65,88,9,20,89,43,105,22,81,10,72,112,97]+1
      paper_inds = [1,3,4,13,15,16,23,26,37,38,41,42,46,47,49,50,56,54,58,59,61,63,66,67,70,71,73,74,82,83,87,90,98,99,103,106,124,123,122,121,120,119,118,117,0,14,44,113,126,127]+1
      paper_hex = [2,21,45,17,68,62,116,125,84,100,85,57,69,40,101,102,114,115,86]+1
      paper_pol = [25,19,48,29,24,28,55,34,27,51,35,75,18,76,5,77,32,78,30,79,33,91,6,92,52,93,7,94,12,95,8,107,11,108,36,109,60,110,39,111]+1
      tile_flag_list = [paper_hex,paper_pol,paper_inds] ;flag all but HERA
      cal_time_average = 0
      nfreq_average = 1024
      calibration_catalog_file_path=filepath('GLEAM_plus_rlb2017.sav',root=rootdir('FHD'),subdir='catalog_data')
      cable_bandpass_fit = 0
      cal_bp_transfer = 0
      cal_mode_fit = 0
      max_calibration_sources = 500
      cal_reflection_mode_theory = 0
      cal_reflection_hyperresolve = 0
      cal_reflection_mode_file = 0
      diffuse_calibrate = 0
      diffuse_model = 0
      beam_offset_time = 300
      flag_calibration = 0
      min_cal_baseline = 0
      calibration_polyfit = 0
      bandpass_calibrate = 0
      flag_visibilities = 0
      elements = 4096
      freq_start = 145
      freq_end = 155
      vis_file_list = '/nfs/eor-00/h1/rbyrne/HERA_analysis/zen.2458042.49835.xx.HH.uvOR.uvfits'
      recalculate_all = 1
      n_pol = 1
    end
    
    'rlb_HERA_only_Nov2017': begin ; Use only a 10 MHz interval
      instrument = 'hera'
      calibration_auto_initialize = 1
      ref_antenna = 80
      time_offset=5.*60.
      hera_inds = [80,104,96,64,53,31,65,88,9,20,89,43,105,22,81,10,72,112,97]+1
      paper_inds = [1,3,4,13,15,16,23,26,37,38,41,42,46,47,49,50,56,54,58,59,61,63,66,67,70,71,73,74,82,83,87,90,98,99,103,106,124,123,122,121,120,119,118,117,0,14,44,113,126,127]+1
      paper_hex = [2,21,45,17,68,62,116,125,84,100,85,57,69,40,101,102,114,115,86]+1
      paper_pol = [25,19,48,29,24,28,55,34,27,51,35,75,18,76,5,77,32,78,30,79,33,91,6,92,52,93,7,94,12,95,8,107,11,108,36,109,60,110,39,111]+1
      tile_flag_list = [paper_hex,paper_pol,paper_inds] ;flag all but HERA
      cal_time_average = 0
      nfreq_average = 1024
      calibration_catalog_file_path=filepath('GLEAM_plus_rlb2017.sav',root=rootdir('FHD'),subdir='catalog_data')
      cable_bandpass_fit = 0
      cal_bp_transfer = 0
      cal_mode_fit = 0
      max_calibration_sources = 500
      cal_reflection_mode_theory = 0
      cal_reflection_hyperresolve = 0
      cal_reflection_mode_file = 0
      diffuse_calibrate = 0
      diffuse_model = 0
      beam_offset_time = 300
      flag_calibration = 0
      min_cal_baseline = 0
      calibration_polyfit = 0
      bandpass_calibrate = 0
      flag_visibilities = 0
      elements = 4096
      freq_start = 145
      freq_end = 155
      recalculate_all = 1
      n_pol = 1
    end
    
    'rlb_4pol_firstpass_Nov2017': begin
      recalculate_all = 0
      mapfn_recalculate = 0
      uvfits_version = 4
      uvfits_subversion = 1
      calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
      rephase_weights = 0
      diffuse_calibrate = 0
      diffuse_model = 0
      cal_bp_transfer = 0
      n_pol = 4
    end
    
    'rlb_GLEAM+Fornax_cal_decon_4pol_Dec2017': begin
      uvfits_version = 5
      uvfits_subversion = 1
      max_sources = 200000
      calibration_catalog_file_path = filepath('GLEAM_plus_rlb2017.sav',root=rootdir('FHD'),subdir='catalog_data')
      gain_factor = 0.1
      deconvolve = 1
      return_decon_visibilities = 1
      deconvolution_filter = 'filter_uv_uniform'
      filter_background = 1
      return_cal_visibilities = 0
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
    
    'rlb_Aug23_Dec2017': begin
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
    
    'rlb_2pol_sim_nocal_master_Jan2018': begin
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
      vis_file_list = '/nfs/eor-00/h1/rbyrne/sim_visibilities/stokes_I_sim_master_2pol.uvfits'
      remove_sim_flags = 1 ;should be used for simulation
      calibrate_visibilities = 0
      model_visibilities = '/nfs/eor-00/h1/rbyrne/catalogs/sim_cal_catalog.sav'
      uvfits_version = 4
      uvfits_subversion = 1
    end
    
    'rlb_4pol_sim_nocal_fullpol_Jan2018': begin
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
      vis_file_list = '/nfs/eor-00/h1/rbyrne/sim_visibilities/stokes_I_sim_fullpol_4pol.uvfits'
      remove_sim_flags = 1 ;should be used for simulation
      calibrate_visibilities = 0
      model_visibilities = '/nfs/eor-00/h1/rbyrne/catalogs/sim_cal_catalog.sav'
      uvfits_version = 4
      uvfits_subversion = 1
    end
    
    'rlb_2pol_sim_nocal_fullpol_Jan2018': begin
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
      vis_file_list = '/nfs/eor-00/h1/rbyrne/sim_visibilities/stokes_I_sim_fullpol_4pol.uvfits'
      remove_sim_flags = 1 ;should be used for simulation
      calibrate_visibilities = 0
      model_visibilities = '/nfs/eor-00/h1/rbyrne/catalogs/sim_cal_catalog.sav'
      uvfits_version = 4
      uvfits_subversion = 1
    end
    
    'rlb_4pol_sim_bright_fullpol_Jan2018': begin
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
      vis_file_list = '/nfs/eor-00/h1/rbyrne/sim_visibilities/stokes_I_sim_bright_fullpol_4pol.uvfits'
      remove_sim_flags = 1 ;should be used for simulation
      calibrate_visibilities = 0
      model_visibilities = '/nfs/eor-00/h1/rbyrne/catalogs/sim_cal_catalog.sav'
      uvfits_version = 4
      uvfits_subversion = 1
    end
    
    'rlb_4pol_sim_fornax_nocal_fullpol_Feb2018': begin
      recalculate_all = 1
      max_sources = 200000
      calibration_catalog_file_path = filepath('GLEAM_plus_rlb2017.sav',root=rootdir('FHD'),subdir='catalog_data')
      model_catalog_file_path = filepath('GLEAM_plus_rlb2017.sav',root=rootdir('FHD'),subdir='catalog_data')
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
      vis_file_list = '/nfs/eor-00/h1/rbyrne/sim_visibilities/fornax_sim_fullpol_4pol.uvfits'
      remove_sim_flags = 1 ;should be used for simulation
      calibrate_visibilities = 0
      model_visibilities = filepath('GLEAM_plus_rlb2017.sav',root=rootdir('FHD'),subdir='catalog_data')
      uvfits_version = 4
      uvfits_subversion = 1
    end
    
    'rlb_4pol_sim_fullpol_Jan2018': begin
      recalculate_all = 1
      max_iter = 300
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
      vis_file_list = '/nfs/eor-00/h1/rbyrne/sim_visibilities/stokes_I_sim_fullpol_4pol.uvfits'
      remove_sim_flags = 1 ;should be used for simulation
    end
    
    'rlb_4pol_sim_fornax_fullpol_Feb2018': begin
      recalculate_all = 1
      max_iter = 300
      max_sources = 200000
      calibration_catalog_file_path = filepath('GLEAM_plus_rlb2017.sav',root=rootdir('FHD'),subdir='catalog_data')
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
      vis_file_list = '/nfs/eor-00/h1/rbyrne/sim_visibilities/fornax_sim_fullpol_4pol.uvfits'
      remove_sim_flags = 1 ;should be used for simulation
    end
    
    'rlb_PAPER_Feb2018': begin
      hera_inds = [80,104,96,64,53,31,65,88,9,20,89,43,105,22,81,10,72,112,97]+1
      paper_inds = [1,3,4,13,15,16,23,26,37,38,41,42,46,47,49,50,56,54,58,59,61,63,66,67,70,71,73,74,82,83,87,90,98,99,103,106,124,123,122,121,120,119,118,117,0,14,44,113,126,127]+1
      paper_hex = [2,21,45,17,68,62,116,125,84,100,85,57,69,40,101,102,114,115,86]+1
      paper_pol = [25,19,48,29,24,28,55,34,27,51,35,75,18,76,5,77,32,78,30,79,33,91,6,92,52,93,7,94,12,95,8,107,11,108,36,109,60,110,39,111]+1
      tile_flag_list = [paper_hex,paper_pol,paper_inds] ;flag all but HERA
      diffuse_calibrate=0
      diffuse_model=0
      recalculate_all=1
      dimension=1024.
      cleanup=0
      ps_export=1 ; changed
      pad_uv_image=1
      split_ps_export=1
      combine_healpix=0
      mapfn_recalculate=0
      healpix_recalculate=0
      vis_baseline_hist=1
      silent=1
      save_visibilities=1
      calibration_visibilities_subtract=0
      snapshot_healpix_export=1
      n_avg=16
      kbinsize=1.
      kspan=1000.
      ps_kbinsize=1.
      ps_kspan=1000.
      image_filter_fn='filter_uv_uniform'
      export_images=1
      instrument='hera'
      verbose=0
      complex_beam=0
      cal_convergence_threshold=1E-2
      max_model_sources=100000
      freq_start=120.
      freq_end=180.
      calibration_polyfit=1
      cal_amp_degree_fit=2
      cal_phase_degree_fit=1
      dft_threshold=1
      no_ps=0
      bandpass_calibrate=1
      n_pol=4
      vis_file_list = '/nfs/eor-00/h1/rbyrne/HERA_analysis/zen.2457458.16694.xx.uvUR.uvfits'
    end
    
    'rlb_2pol_sim_fornax_fullpol_Feb2018': begin
      recalculate_all = 1
      max_iter = 300
      max_sources = 200000
      calibration_catalog_file_path = filepath('GLEAM_plus_rlb2017.sav',root=rootdir('FHD'),subdir='catalog_data')
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
      n_pol = 2
      vis_file_list = '/nfs/eor-00/h1/rbyrne/sim_visibilities/fornax_sim_fullpol_4pol.uvfits'
      remove_sim_flags = 1 ;should be used for simulation
    end
    
    'rlb_stokes_I_master_2pol_decon_Jan2018': begin
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
      uvfits_version = 4
      uvfits_subversion = 1
    end
    
    'rlb_4pol_sim_nocrossphase_fullpol_Feb2018': begin
      recalculate_all = 1
      max_iter = 300
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
      vis_file_list = '/nfs/eor-00/h1/rbyrne/sim_visibilities/stokes_I_sim_fullpol_4pol.uvfits'
      remove_sim_flags = 1 ;should be used for simulation
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
  print,"Keywords set ain wrapper:"
  print,structure_to_text(extra)
  print,""
  
  general_obs,_Extra=extra
  
end
