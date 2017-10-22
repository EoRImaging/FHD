PRO eor_wrapper_defaults,extra
  ; Set the default settings for all EOR observations.
  ; This file can be copied and modified for other types of observations, under a new name.
  ; The defaults set in this file will be overridden 


  ; Set default values for everything
  calibrate_visibilities=1
  recalculate_all=0
  cleanup=0
  ps_export=0
  split_ps_export=1
  combine_healpix=0
  deconvolve=0
  mapfn_recalculate=0
  healpix_recalculate=0
  flag_visibilities=0
  vis_baseline_hist=1
  silent=0
  save_visibilities=1
  calibration_visibilities_subtract=0
  snapshot_healpix_export=1
  n_avg=2
  ps_kbinsize=0.5
  ps_kspan=600.
  image_filter_fn='filter_uv_uniform'
  deconvolution_filter='filter_uv_uniform'
  
  uvfits_version=5 ;updated by RB, 12/16
  uvfits_subversion=1
  
  catalog_file_path=filepath('MRC_full_radio_catalog.fits',root=rootdir('FHD'),subdir='catalog_data')
  calibration_catalog_file_path=filepath('mwa_calibration_source_list.sav',root=rootdir('FHD'),subdir='catalog_data')
  
  dimension=2048
  max_sources=20000
  pad_uv_image=1.
  FoV=0
  no_ps=1
  min_baseline=1.
  min_cal_baseline=50.
  ring_radius=10.*pad_uv_image
  nfreq_avg=16
  no_rephase=1
  combine_obs=0
  smooth_width=32.
  bandpass_calibrate=1
  calibration_polyfit=2
  no_restrict_cal_sources=1
  cal_reflection_mode_theory=150
  restrict_hpx_inds=1
  
  kbinsize=0.5
  psf_resolution=100
  
  ; some new defaults (possibly temporary)
  beam_model_version=2
  dipole_mutual_coupling_factor=1
  calibration_flag_iterate = 0
  
  no_calibration_frequency_flagging=1
  
  ; even newer defaults
  export_images=1
  cal_reflection_hyperresolve=150
  model_catalog_file_path=filepath('mwa_calibration_source_list.sav',root=rootdir('FHD'),subdir='catalog_data')
  model_visibilities=0
  return_cal_visibilities=1
  allow_sidelobe_cal_sources=1
  allow_sidelobe_model_sources=1
  
  beam_offset_time=56 ; make this a default. But it won't compound with setting it directly in a version so I think it's ok.
  
  ;New defaults - July2015
  diffuse_calibrate=filepath('EoR0_diffuse_model_94.sav',root=rootdir('FHD'),subdir='catalog_data')
  cable_bandpass_fit=1
  cal_bp_transfer=1
  
  ;Defaults added - July2016
  cal_amp_degree_fit=2
  cal_phase_degree_fit=1
  
  ;Defaults added - Nov2016
  calibration_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  
  extra=var_bundle(level=0) ; first gather all variables set in the top-level wrapper
  extra=var_bundle(level=1) ; next gather all variables set in this file, removing any duplicates.
END
