PRO rlb_fhd_sim_versions

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
  
  
  
  ;Set defaults
  sources_file_name="simple_I_sources"  ; Not included in eor_wrapper_defaults
  model_visibilities=1  ; Differs from eor_wrapper_defaults
  make_regular_bls=0  ; Not included in eor_wrapper_defaults
  save_antenna_model=1  ; Not included in eor_wrapper_defaults
  ps_export=1  ; Differs from eor_wrapper_defaults
  save_imagecube=0  ; Not included in eor_wrapper_defaults
  save_uvf=0  ; Not included in eor_wrapper_defaults
  dft_threshold=0   ; Approximate the DFT (1) or not (0)  Not included in eor_wrapper_defaults
    
  unflag_all=1  ; Not included in eor_wrapper_defaults
  
  n_pol = 2  ; Not included in eor_wrapper_defaults
  
  grid_recalculate = 1 ; This is needed in array_simulator to enter the gridding/imaging sections  Not included in eor_wrapper_defaults
  include_catalog_sources = 1 ; toggles the use of catalog sources in the simulation source list.  Not included in eor_wrapper_defaults
  eor_sim = 0; toggles eor simulation in vis_simulate  Not included in eor_wrapper_defaults
  
  ; To work with Bryna's changes for noise simulations, include_noise must be explicitly off. Should be investigated and fixed!
  include_noise=0  ;  Not included in eor_wrapper_defaults
  noise_sigma_freq=0  ;  Not included in eor_wrapper_defaults
  
  instrument = 'mwa'
  ;Convoluted way of setting up 'instrument' for use here, while still leaving it to be passed in Extra
  ;IF N_Elements(extra) GT 0 THEN IF Tag_exist(extra,'instrument') THEN instrument=extra.instrument
  ;IF N_Elements(instrument) EQ 0 THEN instrument='hera'
  ;IF N_Elements(double_precison_beam) EQ 0 THEN double_precison_beam=0 
  
  case version of
    
    'rlb_haslam_sim_Jan2018': begin
      instrument="mwa"
      recalculate_all = 0
      uvfits_version = 5
      uvfits_subversion = 1
      max_sources = 200000
      calibration_catalog_file_path = 0
      catalog_path = ''
      include_catalog_sources = 0
      gain_factor = 0.1
      deconvolve = 1
      return_decon_visibilities = 1
      smooth_width = 32
      filter_background = 1
      return_cal_visibilities = 0
      pad_uv_image = 1
      diffuse_calibrate = 0
      diffuse_model = 0
      cal_bp_transfer = 0
      rephase_weights = 0
      restrict_hpx_inds = 0
      hpx_radius = 10
      return_sidelobe_catalog = 1
      dft_threshold = 0
      ring_radius = 0
      write_healpix_fits = 1
      debug_region_grow = 0
      sources_file_path = 0
      source_list = 0
      n_pol = 2
      eor_sim = 0
      galaxy_model = 1
    end
    
    'rlb_stokes_I_master_2pol_sim_Jan2018': begin
      instrument="mwa"
      recalculate_all = 1
      uvfits_version = 4
      uvfits_subversion = 1
      max_sources = 200000
      calibration_catalog_file_path = 0
      catalog_file_path = '/nfs/eor-00/h1/rbyrne/catalogs/sources_I_for_sim.sav'
      catalog_path = '/nfs/eor-00/h1/rbyrne/catalogs/sources_I_for_sim.sav'
      include_catalog_sources = 1
      gain_factor = 0.1
      deconvolve = 1
      return_decon_visibilities = 1
      smooth_width = 32
      filter_background = 1
      return_cal_visibilities = 0
      pad_uv_image = 1
      diffuse_calibrate = 0
      diffuse_model = 0
      cal_bp_transfer = 0
      rephase_weights = 0
      restrict_hpx_inds = 0
      hpx_radius = 10
      return_sidelobe_catalog = 1
      dft_threshold = 0
      ring_radius = 0
      write_healpix_fits = 1
      debug_region_grow = 0
      sources_file_path = 0
      source_list = 0
      n_pol = 2
      eor_sim = 0
    end
    
    'rlb_stokes_I_fullpol_2pol_sim_Jan2018': begin
      instrument="mwa"
      recalculate_all = 1
      uvfits_version = 4
      uvfits_subversion = 1
      max_sources = 200000
      calibration_catalog_file_path = 0
      catalog_file_path = '/nfs/eor-00/h1/rbyrne/catalogs/sources_I_for_sim.sav'
      catalog_path = '/nfs/eor-00/h1/rbyrne/catalogs/sources_I_for_sim.sav'
      include_catalog_sources = 1
      gain_factor = 0.1
      deconvolve = 1
      return_decon_visibilities = 1
      smooth_width = 32
      filter_background = 1
      return_cal_visibilities = 0
      pad_uv_image = 1
      diffuse_calibrate = 0
      diffuse_model = 0
      cal_bp_transfer = 0
      rephase_weights = 0
      restrict_hpx_inds = 0
      hpx_radius = 10
      return_sidelobe_catalog = 1
      dft_threshold = 0
      ring_radius = 0
      write_healpix_fits = 1
      debug_region_grow = 0
      sources_file_path = 0
      source_list = 0
      n_pol = 2
      eor_sim = 0
    end
    
    'rlb_stokes_I_fullpol_4pol_sim_Jan2018': begin
      instrument="mwa"
      recalculate_all = 1
      uvfits_version = 4
      uvfits_subversion = 1
      max_sources = 200000
      calibration_catalog_file_path = 0
      catalog_file_path = '/nfs/eor-00/h1/rbyrne/catalogs/sources_I_for_sim.sav'
      catalog_path = '/nfs/eor-00/h1/rbyrne/catalogs/sources_I_for_sim.sav'
      include_catalog_sources = 1
      gain_factor = 0.1
      deconvolve = 1
      return_decon_visibilities = 1
      smooth_width = 32
      filter_background = 1
      return_cal_visibilities = 0
      pad_uv_image = 1
      diffuse_calibrate = 0
      diffuse_model = 0
      cal_bp_transfer = 0
      rephase_weights = 0
      restrict_hpx_inds = 0
      hpx_radius = 10
      return_sidelobe_catalog = 1
      dft_threshold = 0
      ring_radius = 0
      write_healpix_fits = 1
      debug_region_grow = 0
      sources_file_path = 0
      source_list = 0
      n_pol = 4
      eor_sim = 0
    end
    
   'rlb_simulation_bright_fullpol_4pol_Feb2018': begin
      instrument="mwa"
      recalculate_all = 1
      uvfits_version = 4
      uvfits_subversion = 1
      max_sources = 200000
      calibration_catalog_file_path = 0
      catalog_file_path = '/nfs/eor-00/h1/rbyrne/catalogs/sources_I_for_sim_bright.sav'
      catalog_path = '/nfs/eor-00/h1/rbyrne/catalogs/sources_I_for_sim_bright.sav'
      include_catalog_sources = 1
      gain_factor = 0.1
      deconvolve = 1
      return_decon_visibilities = 1
      smooth_width = 32
      filter_background = 1
      return_cal_visibilities = 0
      pad_uv_image = 1
      diffuse_calibrate = 0
      diffuse_model = 0
      cal_bp_transfer = 0
      rephase_weights = 0
      restrict_hpx_inds = 0
      hpx_radius = 10
      return_sidelobe_catalog = 1
      dft_threshold = 0
      ring_radius = 0
      write_healpix_fits = 1
      debug_region_grow = 0
      sources_file_path = 0
      source_list = 0
      n_pol = 4
      eor_sim = 0
    end
    
    'rlb_fornax_sim_fullpol_4pol_Feb2018': begin
      instrument="mwa"
      recalculate_all = 1
      uvfits_version = 5
      uvfits_subversion = 1
      max_sources = 200000
      calibration_catalog_file_path = 0
      catalog_file_path = filepath('GLEAM_plus_rlb2017.sav',root=rootdir('FHD'),subdir='catalog_data')
      catalog_path = filepath('GLEAM_plus_rlb2017.sav',root=rootdir('FHD'),subdir='catalog_data')
      include_catalog_sources = 1
      gain_factor = 0.1
      deconvolve = 1
      return_decon_visibilities = 1
      smooth_width = 32
      filter_background = 1
      return_cal_visibilities = 0
      pad_uv_image = 1
      diffuse_calibrate = 0
      diffuse_model = 0
      cal_bp_transfer = 0
      rephase_weights = 0
      restrict_hpx_inds = 0
      hpx_radius = 10
      return_sidelobe_catalog = 1
      dft_threshold = 0
      ring_radius = 0
      write_healpix_fits = 1
      debug_region_grow = 0
      sources_file_path = 0
      source_list = 0
      n_pol = 4
      eor_sim = 0
    end
    
  endcase
  
  
  SPAWN, 'read_uvfits_loc.py -v ' + STRING(uvfits_version) + ' -s ' + $
    STRING(uvfits_subversion) + ' -o ' + STRING(obs_id), vis_file_list
    
  temp_path=vis_file_list[0] ; vis_file_list needs needs to be a scalar each time it is passed to array_simulator. For now, we are only using one file.:
  undefine, vis_file_list
  sim_from_uvfits_filepath = temp_path ; used in array_simulator_init.pro
  
  print, 'Reading from file: ' + sim_from_uvfits_filepath
  
  file_path_fhd = fhd_path_setup(sim_from_uvfits_filepath,output_directory=output_directory,version=version,_Extra=extra) ;Creates output directories for each obsid
  temp_path2 = file_path_fhd[0]
  undefine, file_path_fhd
  file_path_fhd = temp_path2

  
  undefine,uvfits_version ; don't need these passed further
  undefine,uvfits_subversion
  undefine,obs_id
  
  
  if n_elements(set_sidelobe_keywords) eq 0 then set_sidelobe_keywords=0
  
  allow_sidelobe_image_output=set_sidelobe_keywords
  allow_sidelobe_sources=set_sidelobe_keywords
  allow_sidelobe_model_sources=set_sidelobe_keywords
  allow_sidelobe_cal_sources=set_sidelobe_keywords
  
  
  IF N_Elements(complex_beam) EQ 0 THEN complex_beam=1
  IF N_Elements(precess) EQ 0 THEN precess=0 ;set to 1 ONLY for X16 PXX scans (i.e. Drift_X16.pro)
  
  if make_regular_bls eq 1 THEN BEGIN
    ps_kspan=600
    simulate_baselines=1
    nsample=100
    sim_uu = findgen(nsample)*ps_kspan/nsample - ps_kspan/2
    sim_vv = findgen(nsample)*ps_kspan/nsample - ps_kspan/2
    f_use = 150e6
    sim_uu = sim_uu / f_use
    sim_vv = sim_vv / f_use
    
    max_n_baseline = 8000
    n_time = 2*ceil(nsample/float(max_n_baseline))
    n_per_time = floor(nsample/(n_time/2.))
    if n_per_time*n_time ne nsample then begin
      nsample = n_per_time*n_time/2.
      sim_uu = sim_uu[0:nsample-1]
      sim_vv = sim_vv[0:nsample-1]
    endif
    sim_uu = reform(sim_uu, n_per_time, 1, n_time/2)
    sim_vv = reform(sim_vv, n_per_time, 1, n_time/2)
    
    sim_baseline_uu = reform([[sim_uu], [sim_uu]], n_per_time*n_time)
    sim_baseline_vv = reform([[sim_vv], [sim_vv]], n_per_time*n_time)
    
    sim_baseline_time = [intarr(n_per_time), intarr(n_per_time)+1]
    if n_time gt 2 then for i=1, n_time/2-1 do sim_baseline_time = [sim_baseline_time, intarr(n_per_time)+2*i, intarr(n_per_time)+2*i+1]
  endif
  
  catalog_file_path=sources_file_path
  
  ;extra=var_bundle()
  
  ; Set global defaults and bundle all the variables into a structure.
  ; Any keywords set on the command line or in the top-level wrapper will supercede these defaults
  eor_wrapper_defaults,extra
  fhd_depreciation_test, _Extra=extra
  
  array_simulator, vis_arr, flag_arr, obs, status_str, psf, params, jones, _Extra=extra
  
  heap_gc
  
  !except=except
  
end