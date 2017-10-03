pro eor_firstpass_versions
  except=!except
  !except=0
  heap_gc
  
  ; wrapper to contain all the parameters for various runs we might do
  ; using firstpass.
  
  ; parse command line args
  compile_opt strictarr
  args = Command_Line_Args(count=nargs)
  IF keyword_set(args) then begin
    obs_id = args[0]
    ;obs_id = '1061316296'
    output_directory = args[1]
    ;output_directory = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/'
    version = args[2]
    if nargs gt 3 then platform = args[3] else platform = '' ;indicates if running on AWS
  ;version = 'nb_temp'
  endif else begin
    obs_id = '1061316296'
    output_directory = '/nfs/mwa-04/r1/EoRuvfits/analysis/'
    version = 'nb_test'
    platform=''
  endelse
  cmd_args={version:version}
  
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
  cal_cable_reflection_fit=150
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
  ;cal_cable_reflection_correct=150
  cal_cable_reflection_mode_fit=150
  model_catalog_file_path=filepath('mwa_calibration_source_list.sav',root=rootdir('FHD'),subdir='catalog_data')
  model_visibilities=0
  return_cal_visibilities=1
  allow_sidelobe_cal_sources=1
  allow_sidelobe_model_sources=1
  
  beam_offset_time=56 ; make this a default. But it won't compound with setting it directly in a version so I think it's ok.
  
  ;New defaults - July2015
  diffuse_calibrate=filepath('EoR0_diffuse_model_94.sav',root=rootdir('FHD'),subdir='catalog_data')
  cable_bandpass_fit=1
  saved_run_bp=1
  
  ;Defaults added - July2016
  cal_amp_degree_fit=2
  cal_phase_degree_fit=1
  
  ;Defaults added - Nov2016
  calibration_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  
  case version of
  
    ;;; Adam's versions!!! Only Adam may edit this section!!!
    'apb_aarons_crazy_obs': begin
      FoV=120.
      dimension=4096.
      snapshot_healpix_export=0
      uvfits_version=0 ; just testing
      uvfits_subversion=0
      tile_flag_list=[111,128,121,131,132,133,141,142,151,152,163,164,165,107,108,118]
      save_visibilities=0
      calibration_catalog_file_path=filepath('mwa_calibration_source_list.sav',root=rootdir('FHD'),subdir='catalog_data')
      
    end
    'apb_aarons_crazy_obs_2': begin
      FoV=120.
      dimension=3072.
      snapshot_healpix_export=0
      uvfits_version=0 ; just testing
      uvfits_subversion=0
      tile_flag_list=[18,81,96,89,97,98,99,105,106,113,114,123,124,125,79,80,88,77]
      save_visibilities=0
      calibration_catalog_file_path=filepath('mwa_calibration_source_list.sav',root=rootdir('FHD'),subdir='catalog_data')
      min_cal_baseline=30
      max_cal_baseline=150
    end
    'apb_aarons_crazy_obs_3': begin
      FoV=120.
      dimension=3072.
      snapshot_healpix_export=0
      uvfits_version=0 ; just testing
      uvfits_subversion=0
      tile_flag_list=[18,81,96,89,97,98,99,105,106,113,114,123,124,125,79,80,88,77]
      save_visibilities=0
      calibration_catalog_file_path=filepath('mwa_calibration_source_list.sav',root=rootdir('FHD'),subdir='catalog_data')
      min_cal_baseline=30
      max_cal_baseline=150
      cal_cable_reflection_fit=150
    end
    'apb_test_fhd_2': begin
      deconvolve=0 ; temporarily to bypass fhd
      return_decon_visibilities=1
      max_sources=30000.
      pad_uv_image=1.
      gain_factor=.2
      uvfits_version=3
      uvfits_subversion=1
      time_cut=[2,-2]
      vis_freq_average=2
      
      snapshot_healpix_export=0
      dimension=3072
      FoV=80.
    end
    'apb_sidelobe_subtract_2': begin
      allow_sidelobe_cal_sources=1
      FoV=160
      dimension=4096
      snapshot_healpix_export=0
      recalculate_all=0
      allow_sidelobe_image_output=1
      beam_output_threshold=0.005
      ;ring_radius=30.*pad_uv_image
      show_beam_contour=1
      contour_levels=[0.01]
    end
    
    ;;; Patti's versions!!! Only Patti may edit this section!!!
    'pac_test_fhd':begin
    deconvolve=1
    return_decon_visibilities=0
    max_sources=30000.
    pad_uv_image=1.
    gain_factor=.2
    uvfits_version=3
    uvfits_subversion=1
    time_cut=[2,-2]
    vis_freq_average=2
    snapshot_healpix_export=0
    dimension=3072
    FoV=80.
    filter_background=0
  end
  
  ;;; Aaron's versions!!! Only Aaron may edit this section!!!
  'aew_mwacs_plus_ben_fornax_and_vla_pic_ultralow_sept5': begin
    calibration_catalog_file_path=filepath('mwa_commissioning_source_list_add_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
    deconvolve=0
    FoV=120.
    dimension=4096
    tile_flag_list=[77,18,89,113,114,115,116,117,118,119,120]
  end
  
  ;;; Abraham's versions!!! Only Abraham may edit this section!!!
  'arn_mwacs_plus_ben_fornax_and_vla_pic': begin
    calibration_catalog_file_path=filepath('mwa_commissioning_source_list_add_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
    snapshot_healpix_export=0
    ;	min_cal_baseline=50
    ;	max_cal_baseline=400
    deconvolve=0
  end
  
  'arn_firstpass_eor1_with_fhd_fornax': begin
    calibration_catalog_file_path=filepath('mwa_commissioning_source_list_add_FHDaug23deconvolve_fornax_and_VLA_pic.sav',root=rootdir('FHD'),subdir='catalog_data')
    snapshot_healpix_export=1
    deconvolve=0
  end
  
  'mwa_cal_ben_fornax_vla_pic': begin
    calibration_catalog_file_path=filepath('mwa_calibration_source_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  
  'transfer_mean_oct24_eor0cal': begin
    transfer_calibration='/nfs/mwa-09/r1/abrahamn/128T/eor1_fhd/low/eor0/fhd_mwa_cal/calibration/cal_avg.sav'
    model_catalog_file_path=filepath('mwa_calibration_source_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_visibilities=1
  end
  
  'transfer_mean_oct24_eor1cal': begin
    transfer_calibration='/nfs/mwa-09/r1/abrahamn/128T/eor1_fhd/low/eor1/fhd_mwa_cal_ben_fornax_vla_pic/calibration/cal_avg.sav'
    model_catalog_file_path=filepath('mwa_calibration_source_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_visibilities=1
  end
  
  'transfer_mean_oct24_eor0cal_rescale': begin
    transfer_calibration='/nfs/mwa-09/r1/abrahamn/128T/eor1_fhd/low/eor0/fhd_mwa_cal/calibration/cal_avg_times1.08.sav'
    model_catalog_file_path=filepath('mwa_calibration_source_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_visibilities=1
  end
  
  'mwa_cal_ben_fornax_vla_pic_rephaseeor1': begin
    override_target_phasera=59.78
    override_target_phasedec=-26.74
    calibration_catalog_file_path=filepath('mwa_calibration_source_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  
  'transfer_mean_oct24_eor1cal_rephaseeor1': begin
    override_target_phasera=59.78
    override_target_phasedec=-26.74
    transfer_calibration='/nfs/mwa-09/r1/abrahamn/128T/eor1_fhd/low/eor1/fhd_mwa_cal_ben_fornax_vla_pic/calibration/cal_avg.sav'
    model_catalog_file_path=filepath('mwa_calibration_source_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_visibilities=1
  end
  
  'arn_caltest_basemin50_basemax1500': begin
    calibration_catalog_file_path=filepath('mwa_calibration_source_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
    snapshot_healpix_export=0
    min_cal_baseline=50
    max_cal_baseline=1500
    deconvolve=1
  end
  
  'arn_caltest_basemin50_basemax1000': begin
    calibration_catalog_file_path=filepath('mwa_calibration_source_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
    snapshot_healpix_export=0
    min_cal_baseline=50
    max_cal_baseline=1000
    deconvolve=1
  end
  
  'arn_caltest_basemin100_basemax1500': begin
    calibration_catalog_file_path=filepath('mwa_calibration_source_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
    snapshot_healpix_export=0
    min_cal_baseline=100
    max_cal_baseline=1500
    deconvolve=1
  end
  
  'arn_caltest_basemin100_basemax1000': begin
    calibration_catalog_file_path=filepath('mwa_calibration_source_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
    snapshot_healpix_export=0
    min_cal_baseline=100
    max_cal_baseline=1000
    deconvolve=1
  end
  
  'arn_caltest_basemin50_basemax500': begin
    calibration_catalog_file_path=filepath('mwa_calibration_source_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
    snapshot_healpix_export=0
    min_cal_baseline=50
    max_cal_baseline=500
    deconvolve=1
  end
  
  'arn_caltest_basemin50_basemax300': begin
    calibration_catalog_file_pith=filepath('mwa_calibration_source_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
    snapshot_healpix_export=0
    min_cal_baseline=50
    max_cal_baseline=300
    deconvolve=1
  end
  
  'arn_eor1_deconvtest_uniform': begin
    model_catalog_file_path=filepath('mwa_calibration_source_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
    dimension=3072.
    FoV=80.
    no_condense_sources=1
    max_sources=100000.
    gain_factor=0.1
    snapshot_healpix_export=0
    deconvolve=1
    decon_filter='filter_uv_uniform'
  end
  
  'arn_eor1_deconvtest_new': begin
    deconvolve=1
    snapshot_healpix_export=0
    FoV=80.
    dimension=3072.
    model_catalog_file_path=filepath('mwa_calibration_source_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
    decon_filter='filter_uv_uniform'
    
    deconvolution_filter='filter_uv_uniform'
    filter_background=1
    smooth_width=32
    max_sources=100000.
    gain_factor=0.1
  end
  
  'arn_export_uncal_vis': begin
    snapshot_healpix_export=0
    calibrate_visibilities=0
  end
  
  'arn_patti_new_catalog': begin
    model_catalog_file_path=filepath('mwa_calibration_source_list_gleam_kgs_no_fornax.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  
  'arn_new_cube_defaults': begin
    cable_bandpass_fit=1
  end
  
  'arn_cable_bandpass_fit_and_patti_eor0low_cat': begin
    cable_bandpass_fit=1
    return_cal_visibilities=1
    model_visibilities=0
    mcalibration_catalog_file_path=filepath('arn_eor0_low_cat_patti_catalog.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  
  'arn_percablebandpass_patticatalog': begin
    cable_bandpass_fit=1
    calibration_catalog_file_path=filepath('arn_eor0_low_cat_patti_catalog.sav',root=rootdir('FHD'),subdir='catalog_data')
    saved_run_bp=0
    return_calibration_visibilities=1
  end
  
  'arn_fittedbandpass_patticatalog': begin
    cable_bandpass_fit=1
    calibration_catalog_file_path=filepath('arn_eor0_low_cat_patti_catalog.sav',root=rootdir('FHD'),subdir='catalog_data')
    saved_run_bp=1
    return_calibration_visibilities=1
  end
  
  'arn_fittedbandpass_patticatalog_nodiffuse': begin
    cable_bandpass_fit=1
    calibration_catalog_file_path=filepath('arn_eor0_low_cat_patti_catalog.sav',root=rootdir('FHD'),subdir='catalog_data')
    saved_run_bp=1
    return_calibration_visibilities=1
    undefine,diffuse_calibrate,diffuse_model
  end
  
  'arn_test_one_jy_source_at_eor0_center': begin
    ;return_cal_visibilities=1
    return_calibration_visibilities=1
    undefine,diffuse_calibrate,diffuse_model
    ;calibrate_visibilities=0
    ;catalag_file_path =          filepath('one_jy_source_at_eor0_center.sav',root=rootdir('FHD'),subdir='catalog_data')
    ;model_visibilities = 1
    calibration_catalog_file_path=filepath('one_jy_source_at_eor0_center.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  
  
  ;;; NEW VERSIONS AFTER 2-10-2014 (Devel merge) - note new defaults!
  
  ; Adam's versions. only Adam can make versions here.
  'apb_test_diffuse_subtract_1': begin
    diffuse_model='/nfs/mwa-09/r1/djc/EoR2013/Aug23/old/fhd_apb_std_Nov2014/Healpix/diffuse_model_1.sav'
    model_visibilities=1
    calibration_visibilities_subtract=0
    return_cal_visibilities=1
    snapshot_healpix_export=1
    export_images=1
    image_filter_fn='filter_uv_natural'
    undefine,model_catalog_file_path
  end
  'apb_test_diffuse_subtract_94': begin
    diffuse_model=filepath('EoR0_diffuse_model_94.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_visibilities=1
    calibration_visibilities_subtract=0
    return_cal_visibilities=1
    snapshot_healpix_export=1
    export_images=1
    image_filter_fn='filter_uv_natural'
    undefine,model_catalog_file_path
    beam_offset_time=56
  end
  'apb_EoR0_high_sem1_1': begin
    production=1
    diffuse_model=filepath('EoR0_diffuse_model_94.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_visibilities=1
    calibration_visibilities_subtract=0
    return_cal_visibilities=1
    undefine,model_catalog_file_path
  end
  
  ;;;;; Nichole's versions
  'nb_no_long_tiles': begin
    diffuse_calibrate=filepath('EoR0_diffuse_model_94.sav',root=rootdir('FHD'),subdir='catalog_data')
    cable_bandpass_fit=1
    tile_flag_list=[78,79,87,88,95,96,97,98,104,112,113,122,123,124]
  end
  'nb_autogainsonly_May2015': begin
    diffuse_calibrate=filepath('EoR0_diffuse_model_94.sav',root=rootdir('FHD'),subdir='catalog_data')
    ;cable_bandpass_fit=1
    undefine,export_images
    calibration_auto_fit=1
  end
  'nb_spec_indices': begin
    calibration_catalog_file_path=filepath('mwa_calibration_source_list_gleam_kgs_fhd_fornax.sav',root=rootdir('FHD'),subdir='catalog_data')
    degrid_spectral=1
    flatten_spectrum=1
    diffuse_spectral_index=-0.5
  end
  'nb_decon_March2016': begin
    max_sources=200000
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    dft_threshold=1
    gain_factor=0.1
    deconvolve=1
    return_decon_visibilities=1
    smooth_width=32
    deconvolution_filter='filter_uv_uniform'
    filter_background=1
    dimension=3072
    return_cal_visibilities=0
    FoV=0
    pad_uv_image=1
    conserve_memory=1
    ;time_cut=[2,-2]
    snapshot_healpix_export=1
    snapshot_recalculate=1
    recalculate_all=0
    
    undefine, diffuse_calibrate, diffuse_model
    saved_run_bp=0
  ;double memory, time
  end
  'nb_decon_March2016_small_through_firstpass': begin
    ;max_calibration_sources=1000
    undefine, diffuse_calibrate, diffuse_model
    calibration_catalog_file_path='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_decon_March2016_small/output_data/'+obs_id+'_source_array2.sav'
    saved_run_bp=0
    recalculate_all=1
    mapfn_recalculate=0
  end
  'nb_decon_July2016_presidelobe': begin
    max_sources=200000
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    ;dft_threshold=1
    gain_factor=0.1
    deconvolve=1
    return_decon_visibilities=1
    smooth_width=32
    deconvolution_filter='filter_uv_uniform'
    filter_background=1
    dimension=2048
    return_cal_visibilities=0
    FoV=0
    pad_uv_image=1
    ;time_cut=[2,-2]
    snapshot_healpix_export=1
    snapshot_recalculate=1
    recalculate_all=1
    
    undefine, diffuse_calibrate, diffuse_model
    saved_run_bp=0
  ;double memory, time
  end
  'nb_decon_July2016_presidelobe_Aug27': begin
    max_sources=200000
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    ;dft_threshold=1
    gain_factor=0.1
    deconvolve=1
    return_decon_visibilities=1
    smooth_width=32
    deconvolution_filter='filter_uv_uniform'
    filter_background=1
    dimension=2048
    return_cal_visibilities=0
    FoV=0
    pad_uv_image=1
    ;time_cut=[2,-2]
    snapshot_healpix_export=1
    ;snapshot_recalculate=1
    recalculate_all=1
    subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAMIDR4_181_consistent.sav'
    ALLOW_SIDELOBE_MODEL_SOURCES =1
    ALLOW_SIDELOBE_CAL_SOURCES =1
    return_sidelobe_catalog=1
    undefine, diffuse_calibrate, diffuse_model
    debug_region_grow=1
  ;saved_run_bp=0
  ;double memory, time
  end
  'nb_decon_southsidelobe': begin
    max_sources=200000
    calibration_catalog_file_path='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog.sav'
    ;dft_threshold=1
    gain_factor=0.1
    deconvolve=1
    return_decon_visibilities=1
    smooth_width=32
    deconvolution_filter='filter_uv_uniform'
    filter_background=1
    dimension=2048
    return_cal_visibilities=0
    FoV=0
    pad_uv_image=1
    ;time_cut=[2,-2]
    snapshot_healpix_export=1
    ;snapshot_recalculate=1
    ;recalculate_all=1
    subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog.sav'
    ALLOW_SIDELOBE_MODEL_SOURCES =1
    ALLOW_SIDELOBE_CAL_SOURCES =1
    return_sidelobe_catalog=1
    undefine, diffuse_calibrate, diffuse_model
    debug_region_grow=1
    rephase_weights = 0
  saved_run_bp=0
  ;double memory, time
  end
  'nb_decon_gleamcal': begin
    max_sources=200000
    calibration_catalog_file_path='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog.sav'
    ;dft_threshold=1
    gain_factor=0.1
    deconvolve=1
    return_decon_visibilities=1
    smooth_width=32
    deconvolution_filter='filter_uv_uniform'
    filter_background=1
    dimension=2048
    return_cal_visibilities=0
    FoV=0
    pad_uv_image=1
    ;time_cut=[2,-2]
    snapshot_healpix_export=1
    ;snapshot_recalculate=1
    ;recalculate_all=1
    subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog.sav'
    ALLOW_SIDELOBE_MODEL_SOURCES =1
    ALLOW_SIDELOBE_CAL_SOURCES =1
    return_sidelobe_catalog=1
    undefine, diffuse_calibrate, diffuse_model
    debug_region_grow=1
  ;saved_run_bp=0
  ;double memory, time
  end
  'nb_decon_gleamcal_sidelobe_3072': begin
    max_sources=200000
    calibration_catalog_file_path='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog.sav'
    ;dft_threshold=1
    gain_factor=0.1
    deconvolve=1
    return_decon_visibilities=1
    smooth_width=32
    deconvolution_filter='filter_uv_uniform'
    filter_background=1
    dimension=3072
    return_cal_visibilities=0
    FoV=0
    pad_uv_image=1
    ;time_cut=[2,-2]
    snapshot_healpix_export=1
    ;snapshot_recalculate=1
    ;recalculate_all=1
    ;subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog.sav'
    ALLOW_SIDELOBE_MODEL_SOURCES =1
    ALLOW_SIDELOBE_CAL_SOURCES =1
    beam_threshold=0.01
    ;return_sidelobe_catalog=1
    undefine, diffuse_calibrate, diffuse_model
    debug_region_grow=1
  ;saved_run_bp=0
  ;double memory, time
  end
    'nb_decon_gleamcal_sidelobe_skip': begin
    max_sources=200000
    calibration_catalog_file_path='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog.sav'
    ;dft_threshold=1
    gain_factor=0.1
    deconvolve=1
    return_decon_visibilities=1
    smooth_width=32
    deconvolution_filter='filter_uv_uniform'
    filter_background=1
    dimension=2048
    return_cal_visibilities=0
    FoV=0
    pad_uv_image=1
    ;time_cut=[2,-2]
    snapshot_healpix_export=1
    ;snapshot_recalculate=1
    recalculate_all=1
    ;subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog.sav'
    ALLOW_SIDELOBE_MODEL_SOURCES =1
    ALLOW_SIDELOBE_CAL_SOURCES =1
    beam_threshold=0.02
    ;return_sidelobe_catalog=1
    undefine, diffuse_calibrate, diffuse_model
    debug_region_grow=1
    skip_beam_alias=1
  ;saved_run_bp=0
  ;double memory, time
  end
  'nb_decon_July2016_presidelobe_Aug26low': begin
    max_sources=200000
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    ;dft_threshold=1
    gain_factor=0.1
    deconvolve=1
    return_decon_visibilities=1
    smooth_width=32
    deconvolution_filter='filter_uv_uniform'
    filter_background=1
    dimension=2048
    return_cal_visibilities=0
    FoV=0
    pad_uv_image=1
    ;time_cut=[2,-2]
    snapshot_healpix_export=1
    ;snapshot_recalculate=1
    recalculate_all=1
    subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAMIDR4_181_consistent.sav'
    ALLOW_SIDELOBE_MODEL_SOURCES =1
    ALLOW_SIDELOBE_CAL_SOURCES =1
    return_sidelobe_catalog=1
    undefine, diffuse_calibrate, diffuse_model
    saved_run_bp=1
  ;double memory, time
  end
  'nb_decon_July2016_presidelobe_Oct23_EoR1': begin
    max_sources=200000
    calibration_catalog_file_path=filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    ;dft_threshold=1
    gain_factor=0.1
    deconvolve=1
    return_decon_visibilities=1
    smooth_width=32
    deconvolution_filter='filter_uv_uniform'
    filter_background=1
    dimension=2048
    return_cal_visibilities=0
    FoV=0
    pad_uv_image=1
    ;time_cut=[2,-2]
    snapshot_healpix_export=1
    ;snapshot_recalculate=1
    ;recalculate_all=1
    subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAMIDR4_181_consistent.sav'
    ALLOW_SIDELOBE_MODEL_SOURCES =1
    ALLOW_SIDELOBE_CAL_SOURCES =1
    return_sidelobe_catalog=1
    undefine, diffuse_calibrate, diffuse_model
    saved_run_bp=1
    grid_recalculate=1
  ;double memory, time
  end
  'nb_decon_July2016_presidelobe_CenA': begin
    max_sources=200000
    calibration_catalog_file_path=filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    ;dft_threshold=1
    gain_factor=0.1
    deconvolve=1
    return_decon_visibilities=1
    smooth_width=32
    deconvolution_filter='filter_uv_uniform'
    filter_background=1
    dimension=2048
    return_cal_visibilities=0
    FoV=0
    pad_uv_image=1
    ;time_cut=[2,-2]
    snapshot_healpix_export=1
    ;snapshot_recalculate=1
    ;recalculate_all=1
    subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAMIDR4_181_consistent.sav'
    ALLOW_SIDELOBE_MODEL_SOURCES =1
    ALLOW_SIDELOBE_CAL_SOURCES =1
    return_sidelobe_catalog=1
    undefine, diffuse_calibrate, diffuse_model
    saved_run_bp=0
    calibration_flux_threshold = .1
    grid_recalculate=1
    uvfits_version=5
    uvfits_subversion=1
  ;double memory, time
  end
  'nb_decon_July2016_presidelobe_Oct24low': begin
    max_sources=200000
    calibration_catalog_file_path=filepath('GLEAMIDR4_158_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    ;dft_threshold=1
    gain_factor=0.1
    deconvolve=1
    return_decon_visibilities=1
    smooth_width=32
    deconvolution_filter='filter_uv_uniform'
    filter_background=1
    dimension=2048
    return_cal_visibilities=0
    FoV=0
    pad_uv_image=1
    ;time_cut=[2,-2]
    snapshot_healpix_export=1
    ;snapshot_recalculate=1
    recalculate_all=1
    subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAMIDR4_158_consistent.sav'
    ALLOW_SIDELOBE_MODEL_SOURCES =1
    ALLOW_SIDELOBE_CAL_SOURCES =1
    return_sidelobe_catalog=1
    undefine, diffuse_calibrate, diffuse_model
    calibration_flux_threshold = .05
    saved_run_bp=1
  ;double memory, time
  end
  
  'nb_bubble_test': begin
    in_situ_sim_input = '/nfs/mwa-00/h1/nbarry/'
    calibrate_visibilities=0
    model_visibilities=1
    max_model_sources=100
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
  end
  'nb_model_large_bandwidth_fullsource_eor': begin
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    enhance_eor=1
    nfreq_avg=384
    eor_savefile = '/nfs/eor-00/h1/nbarry/'
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_model_large_bandwidth_fullsource_2017': begin
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    nfreq_avg=384
    cal_time_average=0
    model_delay_filter=1
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_model_phase2': begin
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    nfreq_avg=384
    cal_time_average=1
    eor_savefile = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/calibration_sim/bubbles/'
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_model_large_bandwidth_fullsource_2017_4096': begin
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    dimension=4096
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    nfreq_avg=384
    cal_time_average=0
    model_delay_filter=1
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_model_large_bandwidth_fullsource_2017_gauss_norm': begin
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    nfreq_avg=384
    cal_time_average=0
    debug_gauss=1
    ;psf_dim=1
    model_delay_filter=1
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_model_large_bandwidth_fullsource_2017_psfdim20_v2': begin
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    nfreq_avg=384
    psf_dim=20.
    debug_dim=1
    cal_time_average=0
    model_delay_filter=1
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  
  'nb_model_large_bandwidth_fullsource_2017_nosidelobes': begin
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    nfreq_avg=384
    cal_time_average=0
    model_delay_filter=1
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    allow_sidelobe_cal_sources=0
    allow_sidelobe_model_sources=0
  end
  'nb_model_beam_ref': begin
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    cal_time_average=0
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_beam_ref': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_beam_ref'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    ;model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_model_beam_debug_beam_clip_grow': begin
    debug_beam_clip_grow=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    cal_time_average=0
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_model_beam_debug_beam_clip_grow_thresh': begin
    debug_beam_clip_grow=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    beam_mask_threshold=1e3
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    cal_time_average=0
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_debug_beam_clip_grow_thresh': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_beam_debug_beam_clip_grow_thresh'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_grow=1
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    beam_mask_threshold=1e3
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    ;model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_debug_beam_clip_grow': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_beam_debug_beam_clip_grow'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_grow=1
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    ;model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_model_beam_debug_clip_beam_mask': begin
    debug_clip_beam_mask=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    cal_time_average=0
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_model_beam_debug_beam_clip_floor': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    cal_time_average=0
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_model_beam_debug_beam_clip_floor_thresh': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    cal_time_average=0
    beam_mask_threshold=1e3
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_model_beam_debug_beam_clip_floor_384': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    cal_time_average=0
    nfreq_avg=384
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_model_beam_debug_beam_clip_floor_1': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    cal_time_average=0
    nfreq_avg=1
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end

  'nb_model_beam_debug_beam_clip_floor_384_delay': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    cal_time_average=0
    nfreq_avg=384
    model_delay_filter=1
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_floor_384_delay': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_beam_debug_beam_clip_floor_384_delay'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
       nfreq_avg=384
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_floor_1_delay': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_beam_debug_beam_clip_floor_1_delay'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
       nfreq_avg=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_model_floor_384_delay_full': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    cal_time_average=0
    nfreq_avg=384
    model_delay_filter=1
    model_catalog_file_path = filepath('uniform_full.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_model_floor_384_delay_full_nobt': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    cal_time_average=0
    nfreq_avg=384
    model_delay_filter=1
    beam_threshold=0
    model_catalog_file_path = filepath('uniform_full.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
    'nb_sim_floor_384_delay_primary': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_floor_384_delay_full'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    debug_beam_clip_floor=1
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        nfreq_avg=384
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('uniform_primary.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
    'nb_sim_floor_384_delay_nulls': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_floor_384_delay_full'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    debug_beam_clip_floor=1
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        nfreq_avg=384
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('uniform_nulls.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
    'nb_sim_floor_384_delay_sidelobe': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_floor_384_delay_full'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    debug_beam_clip_floor=1
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        nfreq_avg=384
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('uniform_sidelobe.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
    'nb_sim_floor_384_delay_negative_primary': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_floor_384_delay_full'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    debug_beam_clip_floor=1
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        nfreq_avg=384
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('uniform_negative_primary.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
    'nb_sim_floor_384_delay_negative_nulls': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_floor_384_delay_full'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    debug_beam_clip_floor=1
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        nfreq_avg=384
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('uniform_negative_nulls.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
    'nb_sim_floor_384_delay_negative_sidelobe': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_floor_384_delay_full'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    debug_beam_clip_floor=1
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        nfreq_avg=384
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('uniform_negative_sidelobe.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  
  
    'nb_sim_floor_384_delay_negative_primary_nobt': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_floor_384_delay_full_nobt'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    debug_beam_clip_floor=1
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        nfreq_avg=384
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
        beam_threshold=0
    calibration_catalog_file_path=filepath('uniform_negative_primary.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
    'nb_sim_floor_384_delay_negative_nulls_nobt': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_floor_384_delay_full_nobt'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    debug_beam_clip_floor=1
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        nfreq_avg=384
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
        beam_threshold=0
    calibration_catalog_file_path=filepath('uniform_negative_nulls.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
    'nb_sim_floor_384_delay_negative_sidelobe_nobt': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_floor_384_delay_full_nobt'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    debug_beam_clip_floor=1
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        nfreq_avg=384
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    beam_threshold=0
    calibration_catalog_file_path=filepath('uniform_negative_sidelobe.sav',root=rootdir('FHD'),subdir='catalog_data')
  end

  'nb_model_beam_debug_beam_clip_floor_1_delay': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    ;recalculate_all=1
    ;mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    cal_time_average=0
    model_delay_filter=1
    nfreq_avg=1
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_test': begin
    instrument = ['hera','paper']
    inst_tile_ptr = PTRARR(2,/allocate)
    *inst_tile_ptr[0] = INDGEN(126)
    *inst_tile_ptr[1] = [126,127]
    undefine, diffuse_model, diffuse_calibrate
  end
  'nb_model_beam_debug_beam_clip_floor_delay': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    cal_time_average=0
    model_delay_filter=1
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_model_beam_flagged_best2': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    ;unflag_all=1
    ;recalculate_all=1
    ;mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    cal_time_average=0
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=2
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
 'nb_sim_beam_flagged_best2': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_beam_flagged_best2'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=2
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    ;model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_model_beam_flagged_best4': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    ;unflag_all=1
    ;recalculate_all=1
    ;mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    cal_time_average=0
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=4
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
 'nb_sim_beam_flagged_best4': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_beam_flagged_best4'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=4
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    ;model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_model_beam_flagged_best8': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    ;unflag_all=1
    ;recalculate_all=1
    ;mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    cal_time_average=0
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
 'nb_sim_beam_flagged_best8': begin
    in_situ_sim_input = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_model_beam_flagged_best8'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=8
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    ;model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_model_beam_flagged_best16': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    ;unflag_all=1
    ;recalculate_all=1
    ;mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    cal_time_average=0
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=16
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
 'nb_sim_beam_flagged_best16_navg1': begin
    in_situ_sim_input = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_model_beam_flagged_best16'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=16
    n_avg=1
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    ;model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_model_beam_best1': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    ;recalculate_all=1
    ;mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    cal_time_average=0
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=1
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
 'nb_sim_beam_best1': begin
    in_situ_sim_input = '/nfs/eor-10/r1/EoRuvfits/analysis/fhd_nb_model_beam_best1'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=1
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    ;model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
    'nb_model_beam_best2': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    cal_time_average=0
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=2
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
 'nb_sim_beam_best2': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_beam_best2'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=2
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    ;model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
    'nb_model_beam_best4': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    cal_time_average=0
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=4
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_beam_best4': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_beam_best4'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=4
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    ;model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  
   'nb_model_beam_best6': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    ;recalculate_all=1
    ;mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    cal_time_average=0
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=6
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end

  'nb_sim_beam_best6': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_beam_best6'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=6
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    ;model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  
   'nb_model_beam_best8': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    ;recalculate_all=1
    ;mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    cal_time_average=0
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  
  'nb_sim_beam_best8': begin
    in_situ_sim_input = '/nfs/eor-11/r1/EoRuvfits/analysis/fhd_nb_model_beam_best8'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=8
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    ;model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  
   'nb_model_beam_best12': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    ;recalculate_all=1
    ;mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    cal_time_average=0
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=12
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  
  'nb_sim_beam_best12': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_beam_best12'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=12
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    ;model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  
  
   'nb_model_beam_softedge': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    cal_time_average=0
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=384
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_beam_softedge': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_beam_softedge'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=384
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    ;model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
     
  
   'nb_model_beam_best16': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    ;recalculate_all=1
    ;mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    cal_time_average=0
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=16
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  
   'nb_model_beam_best8_thresh': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    interpolate_threshold=1
    unflag_all=1
    ;recalculate_all=1
    ;mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    cal_time_average=0
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  
  'nb_sim_beam_best8_thresh': begin
    in_situ_sim_input = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_model_beam_best8_thresh'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=8
    interpolate_threshold=1
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    ;model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end  
      
  'nb_sim_beam_best16': begin
    in_situ_sim_input = '/nfs/mwa-11/r1/EoRuvfits/analysis/fhd_nb_model_beam_best16'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=16
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    ;model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
      
  'nb_model_beam_debug_beam_clip_floor_thresh_res200': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    cal_time_average=0
    beam_mask_threshold=1e3
    psf_resolution=200
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_debug_beam_clip_floor_thresh_res200': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_beam_debug_beam_clip_floor_thresh_res200'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    beam_mask_threshold=1e3
    psf_resolution=200
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    ;model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_debug_beam_clip_floor': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_beam_debug_beam_clip_floor'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    ;model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_debug_beam_clip_floor_1': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_beam_debug_beam_clip_floor_1'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=1
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    ;model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_debug_beam_clip_floor_384': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_beam_debug_beam_clip_floor_384'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=384
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    ;model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_debug_beam_clip_floor_thresh': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_beam_debug_beam_clip_floor_thresh'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    beam_mask_threshold=1e3
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    ;model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
    'nb_sim_debug_beam_clip_floor_delay': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_beam_debug_beam_clip_floor_delay'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  
  'nb_sim_delay_4000_2017_nosidelobes': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_large_bandwidth_fullsource_2017_nosidelobes'
    allow_sidelobe_cal_sources=0
    allow_sidelobe_model_sources=0
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    nfreq_avg=384
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_delay_4000_2017_4096': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_large_bandwidth_fullsource_2017_4096'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    dimension=4096
    nfreq_avg=384
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  
  'nb_sim_delay_4000_2017': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_large_bandwidth_fullsource_2017'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    nfreq_avg=384
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  
  'nb_sim_phase2_all_perf': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_phase2'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    ;max_calibration_sources=4000
    nfreq_avg=384
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    cal_time_average=1 ;reseting the gains makes this unnecessary
    ;model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    eor_savefile = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/calibration_sim/bubbles/'
  end
  'nb_sim_phase2_4000_perf': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_phase2'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    nfreq_avg=384
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    cal_time_average=1 ;reseting the gains makes this unnecessary
    ;model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    eor_savefile = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/calibration_sim/bubbles/'
  end
  'nb_sim_phase2_4000_over': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_phase2'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    nfreq_avg=384
    over_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    cal_time_average=1 ;reseting the gains makes this unnecessary
    ;model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    eor_savefile = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/calibration_sim/bubbles/'
  end
  
  'nb_sim_delay_4000_2017_psfdim20_v2': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_large_bandwidth_fullsource_2017_psfdim20_v2'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    nfreq_avg=384
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    psf_dim=20.
    debug_dim=1
    healpix_recalculate=1
    cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_delay_4000_2017_gauss': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_large_bandwidth_fullsource_2017_gauss'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    nfreq_avg=384
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    debug_gauss=1
    ;psf_dim=1
    healpix_recalculate=1
    cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_delay_4000_2017_gauss_norm': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_large_bandwidth_fullsource_2017_gauss_norm'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    nfreq_avg=384
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    debug_gauss=1
    ;psf_dim=1
    healpix_recalculate=1
    cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_model_large_bandwidth_fullsource_2017_400psf': begin
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    nfreq_avg=384
    cal_time_average=0
    model_delay_filter=1
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    psf_resolution=400
  end
  'nb_sim_delay_4000_2017_400psf': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_large_bandwidth_fullsource_2017_400psf'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    nfreq_avg=384
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    psf_resolution=400
  end
  'nb_model_large_bandwidth_fullsource_2017_600psf': begin
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    nfreq_avg=384
    cal_time_average=0
    model_delay_filter=1
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    psf_resolution=600
  end
  
  'nb_sim_delay_4000_2017_600psf': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_large_bandwidth_fullsource_2017_600psf'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    nfreq_avg=384
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    psf_resolution=600
  end
  'nb_sim_delay_4000_2017_largecube': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_large_bandwidth_fullsource_2017'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    nfreq_avg=384
    perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
  end
  
  'nb_sim_delay_all_eor': begin
    in_situ_sim_input = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_model_large_bandwidth_fullsource_eor'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    ;max_calibration_sources=4000
    nfreq_avg=384
    undefine, diffuse_model, diffuse_calibrate
    enhance_eor=1
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  
  'nb_tenth_beam_threshold': begin
    undefine, diffuse_calibrate, diffuse_model
    saved_run_bp=0
    beam_cal_threshold=0.001 ;tenth of a percent
    cal_time_average=0
    recalculate_all=1
    mapfn_recalculate=0
        calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_241brighter_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_241brighter_ssextended.sav'
  end
  'nb_four_tenths_beam_threshold': begin
    undefine, diffuse_calibrate, diffuse_model
    saved_run_bp=0
    beam_cal_threshold=0.004 ;five tenths of a percent
    cal_time_average=0
        recalculate_all=1
    mapfn_recalculate=0
        calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_241brighter_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_241brighter_ssextended.sav'
  end
    'nb_eight_tenths_beam_threshold': begin
    undefine, diffuse_calibrate, diffuse_model
    saved_run_bp=0
    beam_cal_threshold=0.008 ;five tenths of a percent
    cal_time_average=0
        recalculate_all=1
    mapfn_recalculate=0
        calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_241brighter_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_241brighter_ssextended.sav'
  end

  'nb_baseline_cut_50_2017_beam': begin
    ;transfer_calibration = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_pre_baseline_cut/calibration/1061316296_cal.sav'
    undefine, diffuse_calibrate, diffuse_model
    saved_run_bp=1
    max_baseline=50.
    calibration_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    save_uvf=1
    save_imagecube=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
  end
  'nb_baseline_cut_80_2017': begin
    transfer_calibration = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_2013longrun/calibration/1061316296_cal.sav'
    model_visibilities=1
    undefine, diffuse_calibrate, diffuse_model
    max_baseline=80.
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    save_uvf=1
    save_imagecube=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
  end
  'nb_fhd_amp_cal': begin
    ;rts_cal=1
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
  end
  'nb_rts_amp_cal': begin
    rts_cal=1
    saved_run_bp=0
    ;recalculate_all=1
    ;mapfn_recalculate=0
    undefine, diffuse_calibrate, diffuse_model
  end
  'nb_rts_amp_cal_fit2': begin
    fit_rts_cal=1
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
  ;   recalculate_all=1
  ;mapfn_recalculate=0
  end
  'nb_2013longrun': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
  end
  'nb_2013longrun_savedbp': begin
    saved_run_bp=1
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    healpix_recalculate=1
  end
  'nb_notimeavg': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    snapshot_recalculate=1
    mapfn_recalculate=0
  end
  'nb_notimeavg_gleam_sidelobe_16': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_16perbrighter.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_16perbrighter.sav'
    ;calibration_flux_threshold = .1
    recalculate_all=1
    mapfn_recalculate=0
  end

  'nb_notimeavg_gleam_sidelobe_16_ssextended': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_16percentbrighter_ssextended_20.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_16percentbrighter_ssextended_20.sav'
    ;calibration_flux_threshold = .1
    recalculate_all=1
    mapfn_recalculate=0
  end
  'nb_notimeavg_gleam_sidelobe_20_ssextended': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_20percentbrighter_ssextended_20.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_20percentbrighter_ssextended_20.sav'
    ;calibration_flux_threshold = .1
    recalculate_all=1
    mapfn_recalculate=0
  end
  'nb_notimeavg_241_ssextended': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=5
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/KGS_ssextended_241.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/KGS_ssextended_241.sav'
    ;calibration_flux_threshold = .1
    recalculate_all=1
    mapfn_recalculate=0
  end
  'nb_notimeavg_241_all': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=5
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_241brighter.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_241brighter.sav'
    ;calibration_flux_threshold = .1
    recalculate_all=1
    mapfn_recalculate=0
  end
  'nb_notimeavg_241_ssextended_all': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=5
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_241brighter_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_241brighter_ssextended.sav'
    ;calibration_flux_threshold = .1
    recalculate_all=1
    mapfn_recalculate=0
  end
  'nb_notimeavg_241_ssextended_all_deaddipoles': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=5
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_241brighter_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_241brighter_ssextended.sav'
    ;calibration_flux_threshold = .1
    recalculate_all=1
    mapfn_recalculate=0
    flag_dead_dipoles=1
  end
    'nb_notimeavg_kgs': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=5
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    ;calibration_flux_threshold = .1
    recalculate_all=1
    mapfn_recalculate=0
  end
  'nb_2014zenith_calonly': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=5
    uvfits_subversion=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    ;phase_longrun=1 ;add to github
    jump_longrun=1 
    no_ref_tile=1
    cal_stop=1
    time_cut=-4
  end
  'nb_2013zenith_calonly': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    save_antenna_model=1
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    ;phase_longrun=1 ;add to github
    jump_longrun=1 
    no_ref_tile=1
    cal_stop=1
    time_cut=-4
  end
  'nb_2013zenith_test_nostop': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=5
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    ;phase_longrun=1 ;add to github
    jump_longrun=1 
    no_ref_tile=1
    ;cal_stop=1
    time_cut=-4
  end
  'nb_2013zenith_test_stop': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=5
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/cal_prerun/vis_data'
    transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/beams'
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    ;phase_longrun=1 ;add to github
    jump_longrun=1 
    no_ref_tile=1
    ;cal_stop=1
    time_cut=-4
  end
  'nb_longrun_timecuttest': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=5
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_241brighter_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_241brighter_ssextended.sav'
    ;calibration_flux_threshold = .1
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    ;phase_longrun=1 ;add to github
    jump_longrun=1 
    no_ref_tile=1
    time_cut=-4
    ;cal_stop=1
  end
    'nb_longrun_notimecuttest': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=5
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_241brighter_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_241brighter_ssextended.sav'
    ;calibration_flux_threshold = .1
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    ;phase_longrun=1 ;add to github
    jump_longrun=1 
    no_ref_tile=1
    ;time_cut=[2,-2]
    ;cal_stop=1
  end
    'nb_autos': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_241brighter_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_241brighter_ssextended.sav'
    ;calibration_flux_threshold = .1
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    ;nfreq_avg=8
    ;phase_longrun=1 ;add to github
    jump_longrun=1 
    ;no_ref_tile=1
    calibration_auto_fit=1
    ;time_cut=[2,-2]
    cal_stop=1
  end
    'nb_autos5': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=5
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_241brighter_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_241brighter_ssextended.sav'
    ;calibration_flux_threshold = .1
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    ;nfreq_avg=8
    ;phase_longrun=1 ;add to github
    jump_longrun=1 
    ;no_ref_tile=1
    calibration_auto_fit=1
    ;time_cut=[2,-2]
    cal_stop=1
  end
      'nb_autos_2': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_241brighter_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_241brighter_ssextended.sav'
       model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_autos/cal_prerun/vis_data'
    transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_autos/beams'
    ;calibration_flux_threshold = .1
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    ;nfreq_avg=8
    ;phase_longrun=1 ;add to github
    jump_longrun=1 
    ;no_ref_tile=1
    calibration_auto_fit=1
    ;time_cut=[2,-2]
    cal_stop=1
  end
      'nb_autos_3': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_241brighter_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_241brighter_ssextended.sav'
    ;   model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_autos/cal_prerun/vis_data'
    ;transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_autos/beams'
    ;calibration_flux_threshold = .1
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    ;model_delay_filter=1
    beam_mask_threshold=1e3
    ;nfreq_avg=8
    ;phase_longrun=1 ;add to github
    jump_longrun=1 
    ;no_ref_tile=1
    ;calibration_auto_fit=1
    ;time_cut=[2,-2]
    cal_stop=1
  end  
    'nb_autos5_2': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=5
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_241brighter_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_241brighter_ssextended.sav'
       model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_autos5/cal_prerun/vis_data'
    transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_autos5/beams'
    ;calibration_flux_threshold = .1
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    ;nfreq_avg=8
    ;phase_longrun=1 ;add to github
    jump_longrun=1 
    ;no_ref_tile=1
    calibration_auto_fit=1
    ;time_cut=[2,-2]
    cal_stop=1
  end
    'nb_bart_test': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=5
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_241brighter_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_241brighter_ssextended.sav'
    ;model_transfer='/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_bart_test/vis_data'
    transfer_psf='/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_bart_test/beams'
    ;calibration_flux_threshold = .1
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=16
    ;phase_longrun=1 ;add to github
    ;jump_longrun=1 
    ;no_ref_tile=1
    ;time_cut=[2,-2]
    ;cal_stop=1
  end
  'nb_notimeavg_ssextended': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_catalog_file_path='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/master_sgal_cat_ssextended.sav'
    ;model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_16perbrighter.sav'
    ;calibration_flux_threshold = .1
    recalculate_all=1
    mapfn_recalculate=0
  end
  'nb_notimeavg_ssextended_16': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_catalog_file_path='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/master_sgal_cat_ssextended_16.sav'
    ;model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_16perbrighter.sav'
    ;calibration_flux_threshold = .1
    recalculate_all=1
    mapfn_recalculate=0
  end
  'nb_notimeavg_ssextended_20': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_catalog_file_path='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/master_sgal_cat_ssextended_20.sav'
    ;model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_16perbrighter.sav'
    ;calibration_flux_threshold = .1
    recalculate_all=1
    mapfn_recalculate=0
  end
  
  'nb_notimeavg_delaymodel': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    ;recalculate_all=1
    mapfn_recalculate=0
    model_delay_filter=1
    snapshot_recalculate=1
  end
  'nb_notimeavg_delaymodel_quarter': begin
    saved_run_bp=0
    kbinsize=.25
    dimension=4096
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    ;recalculate_all=1
    mapfn_recalculate=0
    model_delay_filter=1
    snapshot_recalculate=1
    ps_kbinsize=.25
  end
  'nb_notimeavg_selected_cal_cableave': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    flag_calibration=0
    transfer_weights = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_2013longrun/vis_data/1061316296_flags.sav'
    debug_selected_cal=1
  end
  'nb_notimeavg_selected_cal_amp': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    flag_calibration=0
    transfer_weights = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_2013longrun/vis_data/'+strtrim(obs_id,2)+'_flags.sav'
    debug_selected_cal=1
    cable_bandpass_fit=0
    recalculate_all=1
    mapfn_recalculate=0
  end
  'nb_notimeavg_selected_cal': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    flag_calibration=0
    transfer_weights = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_2013longrun/vis_data/'+strtrim(obs_id,2)+'_flags.sav'
    debug_selected_cal=2
    cable_bandpass_fit=0
    recalculate_all=1
    mapfn_recalculate=0
    bandpass_calibrate=0
  end
  'nb_notimeavg_cal': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    flag_calibration=0
    transfer_weights = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_2013longrun/vis_data/'+strtrim(obs_id,2)+'_flags.sav'
    cable_bandpass_fit=0
    bandpass_calibrate=0
    recalculate_all=1
    mapfn_recalculate=0
  end
  'nb_notimeavg_cal_cableave': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    flag_calibration=0
    transfer_weights ='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_2013longrun/vis_data/'+strtrim(obs_id,2)+'_flags.sav'
  end
  'nb_notimeavg_cal_cableave_flippedbeam_calconstrained': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    debug_flip=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;cal_time_average=0
    recalculate_all=1
    mapfn_recalculate=0
    flag_calibration=0
    transfer_weights ='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_2013longrun/vis_data/'+strtrim(obs_id,2)+'_flags.sav'
    transfer_calibration = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_notimeavg_cal_cableave/calibration/'+strtrim(obs_id,2)+'_cal.sav'
    model_visibilities=1
    model_catalog_file_path = filepath('mwa_calibration_source_list.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_notimeavg_selected_cal_cableave_zero': begin
    zero_debug_cal=2
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    flag_calibration=0
    transfer_weights = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_2013longrun/vis_data/1061316296_flags.sav'
  end
  'nb_notimeavg_selected_cal_zero': begin
    zero_debug_cal=1
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    flag_calibration=0
    transfer_weights = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_2013longrun/vis_data/1061316296_flags.sav'
    cable_bandpass_fit=0
  end
  'nb_selected_cal_zero2': begin
    debug_selected_cal=2
    over_calibrate=1
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=1
    flag_calibration=0
    ;transfer_weights = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_2013longrun/vis_data/1061316296_flags.sav'
    cable_bandpass_fit=0
    undefine, cal_cable_reflection_correct,cal_cable_reflection_fit,cal_cable_reflection_mode_fit
    bandpass_calibrate=1
    healpix_recalculate=1
  ;recalculate_all=1
  ;mapfn_recalculate=0
  end
  'nb_longrun_cal_amponly3': begin
    longrun_cal=1
    ;cal_time_average=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    recalculate_all=0
    mapfn_recalculate=0
    undefine, cal_cable_reflection_correct,cal_cable_reflection_fit,cal_cable_reflection_mode_fit
  end
  'nb_longrun_cal_ampphase2': begin
    longrun_cal=1
    ;cal_time_average=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    recalculate_all=1
    mapfn_recalculate=0
    undefine, cal_cable_reflection_correct,cal_cable_reflection_fit,cal_cable_reflection_mode_fit
    phase_longrun=1
  end
  'nb_longrun_cal_ampphase2_dig': begin
    longrun_cal=1
    ;cal_time_average=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    recalculate_all=1
    mapfn_recalculate=0
    undefine, cal_cable_reflection_correct,cal_cable_reflection_fit,cal_cable_reflection_mode_fit
    phase_longrun=1
    jump_longrun=1
  end
  'nb_longrun_cal_phaseaveref': begin
    longrun_cal=1
    ;cal_time_average=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    recalculate_all=1
    mapfn_recalculate=0
    undefine, cal_cable_reflection_correct,cal_cable_reflection_fit,cal_cable_reflection_mode_fit
    phase_longrun=1
    jump_longrun=1
  end
  'nb_phaseaveref_full': begin
    ;longrun_cal=1
    ;cal_time_average=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    recalculate_all=1
    mapfn_recalculate=0
    undefine, cal_cable_reflection_correct,cal_cable_reflection_fit,cal_cable_reflection_mode_fit
    phase_longrun=1
    ;jump_longrun=1
    saved_run=0
    ave_ref=1
  end
  
  'nb_longrun_cal_amp_dig': begin
    longrun_cal=1
    ;cal_time_average=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    recalculate_all=1
    mapfn_recalculate=0
    undefine, cal_cable_reflection_correct,cal_cable_reflection_fit,cal_cable_reflection_mode_fit
    ;phase_longrun=1
    jump_longrun=1
  end
  'nb_notileref': begin
    ;longrun_cal=1
    cal_time_average=0 ;to match longrun
    saved_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    recalculate_all=1
    mapfn_recalculate=0
    ;undefine, cal_cable_reflection_correct,cal_cable_reflection_fit,cal_cable_reflection_mode_fit
    ;phase_longrun=1
    no_ref_tile = 1
    cal_stop=1
  ;jump_longrun=1
  end
  'nb_longrun_cal_amp_dig_conv': begin
    longrun_cal=1
    ;cal_time_average=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    recalculate_all=1
    mapfn_recalculate=0
    undefine, cal_cable_reflection_correct,cal_cable_reflection_fit,cal_cable_reflection_mode_fit
    ;phase_longrun=1
    jump_longrun=1
  end
  'nb_longrun_cal_amp_dig_conv_lowerband': begin
    longrun_cal=1
    saved_run_bp=0
    ;cal_time_average=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    recalculate_all=1
    mapfn_recalculate=0
    undefine, cal_cable_reflection_correct,cal_cable_reflection_fit,cal_cable_reflection_mode_fit
    ;phase_longrun=1
    ;jump_longrun=1
    ;max_calibration_sources=2
    freq_start=167.0
    freq_end=187.475
  end
  'nb_longrun_cal_amp_dig_zeroend': begin
    longrun_cal=1
    ;cal_time_average=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    recalculate_all=1
    mapfn_recalculate=0
    undefine, cal_cable_reflection_correct,cal_cable_reflection_fit,cal_cable_reflection_mode_fit
    ;phase_longrun=1
    jump_longrun=1
  end
  'nb_longrun_cal_amp_dig_zeroend3': begin
    longrun_cal=1
    ;cal_time_average=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    recalculate_all=1
    mapfn_recalculate=0
    undefine, cal_cable_reflection_correct,cal_cable_reflection_fit,cal_cable_reflection_mode_fit
    ;phase_longrun=1
    jump_longrun=1
  end
  'nb_longrun_cal_amp_dig_hyperfine': begin
    longrun_cal=1
    ;cal_time_average=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    ;firstpass_model_recalculate = 0
    recalculate_all=1
    mapfn_recalculate=0
    ;undefine, cal_cable_reflection_correct,cal_cable_reflection_fit,cal_cable_reflection_mode_fit
    mode_debug=1
    jump_longrun=1
    cal_cable_reflection_fit=[-400,-524]
  ;force_data=1
  ;restore_last=1
  end
  'nb_channel_7': begin ;run on 1061313496
    save_imagecube=1
    save_uvf=1
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    transfer_weights = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_decon_July2016_presidelobe/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_decon_July2016_presidelobe/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
  end
  'nb_channel_7_midobs': begin ;run on 1061313496
    save_imagecube=1
    save_uvf=1
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[56,-54] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_decon_July2016_presidelobe/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_decon_July2016_presidelobe/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    image_filter_fn='filter_uv_natural'
  end
  'nb_channel_7_midobs2': begin ;run on 1061313496
    save_imagecube=1
    save_uvf=1
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[54,-56] ;2seconds before the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_decon_July2016_presidelobe/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_decon_July2016_presidelobe/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    image_filter_fn='filter_uv_natural'
  end
  'nb_channel_7_midobs4': begin ;run on 1061313496
    save_imagecube=1
    save_uvf=1
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[52,-58] ;4seconds before the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_decon_July2016_presidelobe/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_decon_July2016_presidelobe/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    image_filter_fn='filter_uv_natural'
  end
  'nb_guardlow_channel_7': begin ;run on 1061313496
    save_imagecube=1
    save_uvf=1
    split_ps_export=1
    freq_end = 181
    freq_start = 175
    transfer_weights = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_decon_July2016_presidelobe/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_decon_July2016_presidelobe/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
  end
  'nb_time_cut': begin
    time_cut=[2,-2]
       saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
  end  
  
  
  ;;; Patti's versions!!! Only Patti may edit this section!!!
  
  ; My default full deconvolution parameters
  'pac_full_fhd': begin
    deconvolve=1
    return_decon_visibilities=1
    max_sources=30000.
    pad_uv_image=1.
    gain_factor=.2
    uvfits_version=4
    uvfits_subversion=0
    time_cut=[2,-2]
    vis_freq_average=2
    snapshot_healpix_export=0
    dimension=3072
    FoV=80.
    filter_background=0
    decon_filter='filter_uv_uniform'
  end
  
  ;shallow clean >.5Jy sources
  'pac_shallow_clean': begin
    calibration_catalog_file_path=filepath('patti_v3.7.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  
  ;Standard MWACS for comparison
  'pac_standard_cat': begin
    calibration_catalog_file_path=filepath('standard_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  
  ; NOTE combined_cat.sav here is called combined_cat2.sav in the settings
  ;Updated MWACS with FHD positions where well matched and fluxes where confident
  'pac_combined_cat2': begin
    calibration_catalog_file_path=filepath('combined_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  
  ; NOTE this combined_cat.sav is true to the catalog in catalog_data
  ;Updated combined cat with NVSS and SUMSS  positions where well matched
  'pac_combined_cat_2': begin
    calibration_catalog_file_path=filepath('combined_cat_2.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  
  ;sidelobe deconvolution test
  'pac_sidelobes': begin
    calibration_catalog_file_path=filepath('testmaster_cat.sav',root=rootdir('FHD'),subdir='catalog_data/pattis_tests/')
    deconvolve=1
    return_decon_visibilities=0
    max_sources=30000.
    pad_uv_image=1.
    gain_factor=.2
    time_cut=[2,-2]
    vis_freq_average=2
    snapshot_healpix_export=0
    dimension=3072
    FoV=80.
    filter_background=0
    decon_filter='filter_uv_uniform'
  end
  
  ;;;;;; Jon R's Stuff ;;;;;;;
  'jonr_barebones_aws': begin
  end
  
  
  ;;;;;Ruby's stuff;;;;;
  
  'rlb_devel_nodiffuse_june2015': begin
    model_visibilities=0
    diffuse_model=0
    firstpass=1
    cable_bandpass_fit=1
    saved_run_bp=1
    production=1
  end
  
  'rlb_master_nodiffuse_june2015': begin ;;Deleted 7/27/15
    model_visibilities=0
    diffuse_model=0
    firstpass=1
    cable_bandpass_fit=1
    saved_run_bp=1
    production=1
  end
  
  'rlb_flag_rec15': begin ;;July 2015
    diffuse_calibrate = filepath('EoR0_diffuse_model_94.sav', root = rootdir('FHD'), subdir = 'catalog_data')
    cable_bandpass_fit = 1
    saved_run_bp = 1
    tile_flag_list = [151, 152, 153, 154, 155, 156, 157, 158]
  end
  
  'rlb_pipe_dream': begin ;;July 2015
    dft_threshold = 1
    snapshot_recalculate = 1
  end
  
  'rlb_diffuse_survey_oneobs_nodiffuse': begin ;;July 2016
    recalculate_all = 1
    mapfn_recalculate = 0
    uvfits_version = 5
    uvfits_subversion = 1
    saved_run_bp = 0
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    override_target_phasera = 0.0
    override_target_phasedec = -27.0
    undefine, diffuse_calibrate, diffuse_model
  end
  
  'rlb_diffuse_survey_oneobs': begin ;;July 2016
    recalculate_all = 1
    mapfn_recalculate = 0
    uvfits_version = 5
    uvfits_subversion = 1
    saved_run_bp = 0
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  
  'rlb_diffuse_survey_threeobs_nodiffuse': begin ;;August 2016
    recalculate_all = 1
    mapfn_recalculate = 0
    uvfits_version = 5
    uvfits_subversion = 1
    saved_run_bp = 0
    calibration_catalog_file_path=filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    override_target_phasera = 0.0
    override_target_phasedec = -27.0
    undefine, diffuse_calibrate, diffuse_model
  end
  
  'rlb_golden_set_oneobs_nodiffuse': begin ;;August 2016
    recalculate_all = 1
    mapfn_recalculate = 0
    uvfits_version = 5
    uvfits_subversion = 1
    saved_run_bp = 0
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    undefine, diffuse_calibrate, diffuse_model
  end
  
  'rlb_diffuse_survey_oneobs_nodiffuse_flag_rec14': begin ;;September 2016
    recalculate_all = 1
    mapfn_recalculate = 0
    uvfits_version = 5
    uvfits_subversion = 1
    saved_run_bp = 0
    calibration_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    tile_flag_list = ['141','142','143','144','145','146','147','148']
    override_target_phasera = 0.0
    override_target_phasedec = -27.0
    undefine, diffuse_calibrate, diffuse_model
  end
  
  'rlb_diffuse_survey_threeobs_flag_rec14': begin ;;September 2016
    uvfits_version = 5
    uvfits_subversion = 1
    saved_run_bp = 0
    calibration_catalog_file_path=filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    tile_flag_list = ['141','142','143','144','145','146','147','148']
    override_target_phasera = 0.0
    override_target_phasedec = -27.0
    undefine, diffuse_calibrate, diffuse_model
  end
  
  'rlb_diffuse_survey_decon_Sept2016_sidelobe_subtract': begin ;;September 2016
    uvfits_version = 5
    uvfits_subversion = 1
    override_target_phasera = 0.0
    override_target_phasedec = -27.0
    max_sources = 200000
    calibration_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    dft_threshold = 1
    gain_factor = 0.1
    deconvolve = 1
    return_decon_visibilities = 1
    smooth_width = 32
    deconvolution_filter = 'filter_uv_uniform'
    filter_background = 1
    dimension = 2048
    return_cal_visibilities = 0
    FoV = 0
    pad_uv_image = 1
    snapshot_healpix_export = 1
    snapshot_recalculate = 1
    recalculate_all = 1
    undefine, diffuse_calibrate, diffuse_model
    saved_run_bp = 0
    subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    allow_sidelobe_model_sources = 1
    allow_sidelobe_cal_sources = 1
  end
  
  'rlb_master_catalog_cal_Sept2016': begin
    recalculate_all = 1
    mapfn_recalculate = 0
    uvfits_version = 5
    uvfits_subversion = 1
    saved_run_bp = 0
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    rephase_weights = 0
    undefine, diffuse_calibrate, diffuse_model
  end
  
  'rlb_GLEAM_cal_Sept2016': begin
    recalculate_all = 1
    mapfn_recalculate = 1
    uvfits_version = 5
    uvfits_subversion = 1
    saved_run_bp = 0
    calibration_catalog_file_path=filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    rephase_weights = 0
    restrict_hpx_inds = 0
    hpx_radius = 10
    undefine, diffuse_calibrate, diffuse_model
    ring_radius = 0
  end
  
  'rlb_GLEAM_cal_decon_Oct2016': begin
    uvfits_version = 5
    uvfits_subversion = 1
    max_sources = 200000
    calibration_catalog_file_path = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    dft_threshold = 1
    gain_factor = 0.1
    deconvolve = 1
    return_decon_visibilities = 1
    smooth_width = 32
    deconvolution_filter = 'filter_uv_uniform'
    filter_background = 1
    dimension = 2048
    return_cal_visibilities = 0
    FoV = 0
    pad_uv_image = 1
    snapshot_healpix_export = 1
    snapshot_recalculate = 1
    recalculate_all = 1
    undefine, diffuse_calibrate, diffuse_model
    saved_run_bp = 0
    rephase_weights = 0
    restrict_hpx_inds = 0
    hpx_radius = 10
    subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    allow_sidelobe_model_sources = 1
    allow_sidelobe_cal_sources = 1
  end
  
  'rlb_GLEAM_cal_decon_Nov2016': begin
    uvfits_version = 5
    uvfits_subversion = 1
    max_sources = 200000
    calibration_catalog_file_path = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    gain_factor = 0.1
    deconvolve = 1
    return_decon_visibilities = 1
    smooth_width = 32
    deconvolution_filter = 'filter_uv_uniform'
    filter_background = 1
    dimension = 2048
    return_cal_visibilities = 0
    FoV = 0
    pad_uv_image = 1
    snapshot_healpix_export = 1
    snapshot_recalculate = 1
    recalculate_all = 0
    undefine, diffuse_calibrate, diffuse_model
    saved_run_bp = 0
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
    saved_run_bp = 0
    calibration_catalog_file_path = '/nfs/mwa-08/d1/DiffuseSurvey2015/1130789944_run1_catalog.sav'
    calibration_subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    rephase_weights = 0
    restrict_hpx_inds = 0
    hpx_radius = 10
    undefine, diffuse_calibrate, diffuse_model
    ring_radius = 0
  end
  
  'rlb_1130781304_run1_cal_Dec2016': begin
    recalculate_all = 0
    mapfn_recalculate = 0
    uvfits_version = 5
    uvfits_subversion = 1
    saved_run_bp = 0
    calibration_catalog_file_path = '/nfs/mwa-08/d1/DiffuseSurvey2015/1130781304_run1_catalog.sav'
    calibration_subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    rephase_weights = 0
    restrict_hpx_inds = 0
    hpx_radius = 10
    undefine, diffuse_calibrate, diffuse_model
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
    smooth_width = 32
    deconvolution_filter = 'filter_uv_uniform'
    filter_background = 1
    dimension = 2048
    return_cal_visibilities = 0
    FoV = 0
    pad_uv_image = 1
    snapshot_healpix_export = 1
    snapshot_recalculate = 1
    recalculate_all = 1
    undefine, diffuse_calibrate, diffuse_model
    saved_run_bp = 0
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
    smooth_width = 32
    deconvolution_filter = 'filter_uv_uniform'
    filter_background = 1
    dimension = 2048
    return_cal_visibilities = 0
    FoV = 0
    pad_uv_image = 1
    snapshot_healpix_export = 1
    snapshot_recalculate = 1
    recalculate_all = 1
    undefine, diffuse_calibrate, diffuse_model
    saved_run_bp = 0
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
    saved_run_bp = 0
    calibration_catalog_file_path = '/nfs/mwa-08/d1/DiffuseSurvey2015/1130789944_run2_catalog.sav'
    calibration_subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    rephase_weights = 0
    restrict_hpx_inds = 0
    hpx_radius = 10
    undefine, diffuse_calibrate, diffuse_model
    ring_radius = 0
  end
  
  'rlb_1130781304_run2_cal_Feb2017': begin
    recalculate_all = 1
    mapfn_recalculate = 1
    uvfits_version = 5
    uvfits_subversion = 1
    saved_run_bp = 0
    calibration_catalog_file_path = '/nfs/mwa-08/d1/DiffuseSurvey2015/1130781304_run2_catalog.sav'
    calibration_subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    rephase_weights = 0
    restrict_hpx_inds = 0
    hpx_radius = 10
    undefine, diffuse_calibrate, diffuse_model
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
    smooth_width = 32
    deconvolution_filter = 'filter_uv_uniform'
    filter_background = 1
    dimension = 2048
    return_cal_visibilities = 0
    FoV = 0
    pad_uv_image = 1
    snapshot_healpix_export = 1
    snapshot_recalculate = 1
    recalculate_all = 0
    undefine, diffuse_calibrate, diffuse_model
    saved_run_bp = 0
    rephase_weights = 0
    restrict_hpx_inds = 0
    hpx_radius = 10
    subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    return_sidelobe_catalog = 1
    dft_threshold = 0
    ring_radius = 0
    n_pol = 4
  end
  
  'rlb_HERA_May2017': begin
    recalculate_all = 1
    uvfits_version = 5
    uvfits_subversion = 1
    saved_run_bp = 0
    calibration_catalog_file_path=filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    rephase_weights = 0
    restrict_hpx_inds = 0
    hpx_radius = 10
    undefine, diffuse_calibrate, diffuse_model
    ring_radius = 0
  end
  
  'rlb_GLEAM+PicA_cal_decon_Jun2017': begin
    uvfits_version = 5
    uvfits_subversion = 1
    max_sources = 200000
    calibration_catalog_file_path = '/nfs/eor-00/h1/rbyrne/catalogs/GLEAM+PicA-88comp.sav'
    gain_factor = 0.1
    deconvolve = 1
    return_decon_visibilities = 1
    smooth_width = 32
    deconvolution_filter = 'filter_uv_uniform'
    filter_background = 1
    dimension = 2048
    return_cal_visibilities = 0
    FoV = 0
    pad_uv_image = 1
    snapshot_healpix_export = 1
    snapshot_recalculate = 1
    recalculate_all = 1
    undefine, diffuse_calibrate, diffuse_model
    saved_run_bp = 0
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
    smooth_width = 32
    deconvolution_filter = 'filter_uv_uniform'
    filter_background = 1
    dimension = 2048
    return_cal_visibilities = 0
    FoV = 0
    pad_uv_image = 1
    snapshot_healpix_export = 1
    snapshot_recalculate = 1
    recalculate_all = 0
    undefine, diffuse_calibrate, diffuse_model
    saved_run_bp = 0
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
  
  'rlb_eor0_GLEAM_cal_Aug2017': begin
    recalculate_all = 0
    uvfits_version = 5
    uvfits_subversion = 1
    saved_run_bp = 0
    calibration_catalog_file_path = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    calibration_subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    undefine, diffuse_calibrate, diffuse_model
    ring_radius = 0
  end
  
  'rlb_eor0_run1_cal_Aug2017': begin
    recalculate_all = 0
    uvfits_version = 5
    uvfits_subversion = 1
    saved_run_bp = 0
    calibration_catalog_file_path = '/nfs/mwa-04/r1/EoRuvfits/DiffuseSurvey2015/1131454296_run1_catalog.sav'
    calibration_subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    undefine, diffuse_calibrate, diffuse_model
    ring_radius = 0 
  end
  
  'rlb_eor0_GLEAM_cal_norephase_Aug2017': begin
    recalculate_all = 0
    uvfits_version = 5
    uvfits_subversion = 1
    saved_run_bp = 0
    calibration_catalog_file_path = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    calibration_subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    rephase_weights = 0
    restrict_hpx_inds = 0
    hpx_radius = 10
    undefine, diffuse_calibrate, diffuse_model
    ring_radius = 0
  end
  
  'rlb_eor0_run1_cal_norephase_Aug2017': begin
    recalculate_all = 0
    uvfits_version = 5
    uvfits_subversion = 1
    saved_run_bp = 0
    calibration_catalog_file_path = '/nfs/mwa-04/r1/EoRuvfits/DiffuseSurvey2015/1131454296_run1_catalog.sav'
    calibration_subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    rephase_weights = 0
    restrict_hpx_inds = 0
    hpx_radius = 10
    undefine, diffuse_calibrate, diffuse_model
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
    smooth_width = 32
    deconvolution_filter = 'filter_uv_uniform'
    filter_background = 1
    dimension = 2048
    return_cal_visibilities = 0
    FoV = 0
    pad_uv_image = 1
    snapshot_healpix_export = 1
    snapshot_recalculate = 1
    recalculate_all = 1
    undefine, diffuse_calibrate, diffuse_model
    saved_run_bp = 0
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
  
  'rlb_HERA_Sept2017': begin
    recalculate_all = 1
    uvfits_version = 5
    uvfits_subversion = 1
    saved_run_bp = 0
    calibration_catalog_file_path=filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    rephase_weights = 0
    restrict_hpx_inds = 0
    hpx_radius = 10
    undefine, diffuse_calibrate, diffuse_model
    ring_radius = 0
    n_pol = 1
  end
  
  'rlb_HERA_Sept2017_2': begin
    recalculate_all = 1
    uvfits_version = 5
    uvfits_subversion = 1
    saved_run_bp = 0
    calibration_catalog_file_path=filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    rephase_weights = 0
    restrict_hpx_inds = 0
    hpx_radius = 10
    undefine, diffuse_calibrate, diffuse_model
    ring_radius = 0
    n_pol = 1
    cable_bandpass_fit = 0
  end
  
  'rlb_HERA_Sept2017_3': begin
    recalculate_all = 1
    uvfits_version = 5
    uvfits_subversion = 1
    saved_run_bp = 0
    calibration_catalog_file_path=filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    rephase_weights = 0
    restrict_hpx_inds = 0
    hpx_radius = 10
    undefine, diffuse_calibrate,diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
    ring_radius = 0
    n_pol = 1
    cable_bandpass_fit = 0
    unflag_all = 1
    instrument = ['paper','hera']
    inst_tile_ptr = PTRARR(2,/allocate)
    *inst_tile_ptr[1] = [80,104,96,64,53,31,65,88,9,20,89,43,105,22,81,10,72,112,97]
    *inst_tile_ptr[0] = [1,3,4,13,15,16,23,26,37,38,41,42,46,47,49,50,56,54,58,59,61,63,66,67,70,71,73,74,82,83,87,90,98,99,103,106,124,123,122,121,120,119,118,117,0,14,44,113,126,127]
    calibration_auto_initialize = 1
    nfreq_avg = 1024
    cal_mode_fit = 0
    calibration_polyfit = 0
    bandpass_calibrate = 0
    debug_region_grow = 1
  end
  
  'rlb_1130776744_run1_cal_decon_Sept2017': begin
    uvfits_version = 5
    uvfits_subversion = 1
    max_sources = 200000
    calibration_catalog_file_path = filepath('GLEAM_plus_rlb2017.sav',root=rootdir('FHD'),subdir='catalog_data')
    gain_factor = 0.1
    deconvolve = 1
    return_decon_visibilities = 1
    smooth_width = 32
    deconvolution_filter = 'filter_uv_uniform'
    filter_background = 1
    dimension = 2048
    return_cal_visibilities = 0
    FoV = 0
    pad_uv_image = 1
    snapshot_healpix_export = 1
    snapshot_recalculate = 1
    recalculate_all = 1
    undefine, diffuse_calibrate, diffuse_model
    saved_run_bp = 0
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
    smooth_width = 32
    deconvolution_filter = 'filter_uv_uniform'
    filter_background = 1
    dimension = 2048
    return_cal_visibilities = 0
    FoV = 0
    pad_uv_image = 1
    snapshot_healpix_export = 1
    snapshot_recalculate = 1
    recalculate_all = 1
    undefine, diffuse_calibrate, diffuse_model
    saved_run_bp = 0
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
    saved_run_bp = 0
    cal_mode_fit = 0
    max_calibration_sources = 500
    undefine, diffuse_calibrate,diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
    beam_offset_time = 300
    flag_calibration = 0
    min_cal_baseline = 10
    calibration_polyfit = 0
    bandpass_calibrate = 0
    flag_visibilities = 1
    dimension = 4096
    elements = 4096
  end
  
  'rlb_HERA_only_Sept2017': begin
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
    calibration_catalog_file_path=filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    cable_bandpass_fit = 0
    saved_run_bp = 0
    cal_mode_fit = 0
    max_calibration_sources = 500
    undefine, diffuse_calibrate,diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
    beam_offset_time = 300
    flag_calibration = 0
    min_cal_baseline = 10
    calibration_polyfit = 0
    bandpass_calibrate = 0
    flag_visibilities = 1
    dimension = 4096
    elements = 4096
  end
  
  'rlb_GLEAM+Fornax_cal_Sept2017': begin
    recalculate_all = 1
    mapfn_recalculate = 1
    uvfits_version = 5
    uvfits_subversion = 1
    saved_run_bp = 0
    calibration_catalog_file_path=filepath('GLEAM_plus_rlb2017.sav',root=rootdir('FHD'),subdir='catalog_data')
    rephase_weights = 0
    restrict_hpx_inds = 0
    hpx_radius = 10
    undefine, diffuse_calibrate, diffuse_model
    ring_radius = 0
  end
  
  'rlb_HERA_only_Oct2017': begin
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
    calibration_catalog_file_path=filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    cable_bandpass_fit = 0
    saved_run_bp = 0
    cal_mode_fit = 0
    max_calibration_sources = 500
    undefine, diffuse_calibrate,diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
    beam_offset_time = 300
    flag_calibration = 0
    min_cal_baseline = 0
    calibration_polyfit = 0
    bandpass_calibrate = 0
    flag_visibilities = 1
    dimension = 4096
    elements = 4096
  end
  
  ;;;;;;; Mike Wilensky's Stuff ;;;;;;;;
  'mwilensky_test_3_6_2017' : begin
    recalculate_all = 1
    mapfn_recalculate = 1
    uvfits_version = 5
    uvfits_subversion = 1
    saved_run_bp = 0
    calibration_catalog_file_path=filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    rephase_weights = 0
    restrict_hpx_inds = 0
    hpx_radius = 10
    undefine, diffuse_calibrate, diffuse_model
    ring_radius = 0
  end
  
endcase


case version of 

  'nb_test': begin
    ;vis_file_list = '/nfs/mwa-03/r1/EoR2013/cotter_pyuvfits_test/'+strtrim(string(obs_id),2)+'.uvfits'
    vis_file_list = '/nfs/eor-11/r1/EoRuvfits/jd2456528v4_1/1061316296/1061316296.uvfits'
  end
  
  'rlb_HERA_May2017': begin 
    vis_file_list = '/nfs/eor-00/h1/rbyrne/HERA_analysis/zen.2457458.16694.xx.uvUR.uvfits'
  end
  
  'rlb_HERA_Sept2017': begin 
    vis_file_list = '/nfs/eor-00/h1/rbyrne/HERA_analysis/zen.2457458.16694.xx.uvUR.uvfits'
  end
  
  'rlb_HERA_Sept2017_2': begin 
    vis_file_list = '/nfs/eor-00/h1/rbyrne/HERA_analysis/zen.2457458.16694.xx.uvUR.uvfits'
  end
  
  'rlb_HERA_Sept2017_3': begin 
    vis_file_list = '/nfs/eor-00/h1/rbyrne/HERA_analysis/zen.2457458.16694.xx.uvUR.uvfits'
  end
  
  'rlb_PAPER_Sept2017': begin
    vis_file_list = '/nfs/eor-00/h1/rbyrne/HERA_analysis/zen.2457458.16694.xx.uvUR.uvfits'
  end
  
  'rlb_HERA_only_Sept2017': begin
    vis_file_list = '/nfs/eor-00/h1/rbyrne/HERA_analysis/zen.2457458.16694.xx.uvUR.uvfits'
  end
  
  'rlb_HERA_only_Oct2017': begin
    vis_file_list = '/nfs/eor-00/h1/rbyrne/HERA_analysis/zen.2457458.16694.xx.uvUR.uvfits'
  end
  
  else: begin
    if platform eq 'aws' then vis_file_list = '/uvfits/' + STRING(obs_id) + '.uvfits' else begin
      SPAWN, 'read_uvfits_loc.py -v ' + STRING(uvfits_version) + ' -s ' + $
        STRING(uvfits_subversion) + ' -o ' + STRING(obs_id), vis_file_list
    endelse
  end
  
endcase

;vis_file_list=vis_file_list ; this is silly, but it's so var_bundle sees it.
undefine, uvfits_subversion, uvfits_version

fhd_file_list=fhd_path_setup(vis_file_list,version=version,output_directory=output_directory)
healpix_path=fhd_path_setup(output_dir=output_directory,subdir='Healpix',output_filename='Combined_obs',version=version)

extra=var_bundle() ; bundle all the variables into a structure

print,""
print,"Keywords set in wrapper:"
print,structure_to_text(extra)
print,""
general_obs,_Extra=extra

end
