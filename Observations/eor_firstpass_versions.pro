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
  'nb_spec_indices': begin
    calibration_catalog_file_path=filepath('mwa_calibration_source_list_gleam_kgs_fhd_fornax.sav',root=rootdir('FHD'),subdir='catalog_data')
    degrid_spectral=1
    flatten_spectrum=1
    diffuse_spectral_index=-0.5
  end
  'nb_decon_March2016_small_through_firstpass': begin
    ;max_calibration_sources=1000
    undefine, diffuse_calibrate, diffuse_model
    calibration_catalog_file_path='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_decon_March2016_small/output_data/'+obs_id+'_source_array2.sav'
    saved_run_bp=0
    recalculate_all=1
    mapfn_recalculate=0
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
  'nb_model_goldenset': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    nfreq_avg=384
    model_delay_filter=1
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/'
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_goldenset': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_goldenset'
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
    max_calibration_sources=4000
    nfreq_avg=384
    sim_over_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        model_delay_filter=1
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_goldenset_bubbles': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_goldenset'
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
    max_calibration_sources=4000
    nfreq_avg=384
    sim_over_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    cal_mode_fit=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        model_delay_filter=1
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_goldenset_bubbles_perf': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_goldenset'
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
    max_calibration_sources=4000
    nfreq_avg=384
    sim_perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    cal_mode_fit=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        model_delay_filter=1
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
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
  'nb_test': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    cal_time_average=0
    beam_mask_threshold = 1e3
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
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
 'nb_sim_beam_flagged_best16_bubbles': begin
    in_situ_sim_input = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_model_beam_flagged_best16'
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=16
    ;n_avg=384
    cal_mode_fit=0
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    ;model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
 'nb_sim_beam_flagged_best16_bubbles_flagmodel': begin
    in_situ_sim_input = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_model_beam_flagged_best16'
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=16
    ;n_avg=384
    cal_mode_fit=0
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    ;model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
 'nb_sim_beam_flagged_best16_bubbles_overfit': begin
    in_situ_sim_input = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_model_beam_flagged_best16'
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_over_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=16
    ;n_avg=384
    cal_mode_fit=0
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    ;model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
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
  'nb_Aug2017_std': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
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
    ;no_ref_tile=1
    ;cal_stop=1
    time_cut=-4
  end
  'nb_Aug2017_globalbp': begin
    saved_run_bp=0
    cable_bandpass_fit=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/cal_prerun/vis_data'
    transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/beams'
    ;recalculate_all=1
    ;mapfn_recalculate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    ;phase_longrun=1 ;add to github
    jump_longrun=1 
    ;no_ref_tile=1
    ;cal_stop=1
    time_cut=-4
  end
  'nb_Aug2017_savedbp': begin
    saved_run_bp=1
    cable_bandpass_fit=1
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/cal_prerun/vis_data'
    transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/beams'
    ;recalculate_all=1
    ;mapfn_recalculate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    ;phase_longrun=1 ;add to github
    jump_longrun=1 
    ;no_ref_tile=1
    ;cal_stop=1
    time_cut=-4
  end
  'nb_Aug2017_savedbp_branchrun': begin
    cal_bp_transfer = 1
    cable_bandpass_fit=1
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/cal_prerun/vis_data'
    transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/beams'
    ;recalculate_all=1
    ;mapfn_recalculate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    ;phase_longrun=1 ;add to github
    ;digital_gain_jump_polyfit=1 
    ;no_ref_tile=1
    ;cal_stop=1
    time_cut=-4
    cal_reflection_mode_theory=150
    cal_reflection_hyperresolve=1
  end
  'nb_Aug2017_autocal2': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    ;model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/cal_prerun/vis_data'
    transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/beams'
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    ;model_delay_filter=1
    channel_edge_flag_width=2
    calibration_auto_fit=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    ;phase_longrun=1 ;add to github
    digital_gain_jump_polyfit=1 
    ;no_ref_tile=1
    ;cal_stop=1
    time_cut=-4
  end
  'nb_2013longrun_std': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/cal_prerun/vis_data'
    transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/beams'
    tile_flag_list = ['111','118','121','128','131','132','133','141','142','144','151','152','163','164']
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    ;phase_longrun=1 ;add to github
    jump_longrun=1 
    ;no_ref_tile=1
    ;cal_stop=1
    time_cut=-4
  end
  'nb_Aug2017_aveamp': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/cal_prerun/vis_data'
    transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/beams'
    ;tile_flag_list = ['111','118','121','128','131','132','133','141','142','144','151','152','163','164']
    amp_longrun=1
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    ;phase_longrun=1 ;add to github
    jump_longrun=1 
    ;no_ref_tile=1
    ;cal_stop=1
    time_cut=-4
  end
    'nb_Aug2017_aveampphase_dayref_tileflag': begin
    saved_run_bp=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/cal_prerun/vis_data'
    transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/beams'
    tile_flag_list = ['111','118','121','128','131','132','133','141','142','144','151','152','163','164']
    amp_longrun=1
    phase_longrun=2
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    ;phase_longrun=1 ;add to github
    jump_longrun=1 
    ;no_ref_tile=1
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
  'nb_eor0clusteredall': begin
  uvfits_version = 5
    uvfits_subversion = 1
    saved_run_bp = 0
    calibration_catalog_file_path = filepath('eor0clusteredall.sav',root=rootdir('FHD'),subdir='catalog_data')
    calibration_subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    undefine, diffuse_calibrate, diffuse_model
    ring_radius = 0
    rephase_weights = 0
    restrict_hpx_inds = 0
    hpx_radius = 10
        recalculate_all=1
    mapfn_recalculate=0
  end 
    'nb_eor0clusteredall_clustercal': begin
  uvfits_version = 5
    uvfits_subversion = 1
    saved_run_bp = 0
    calibration_catalog_file_path = filepath('eor0clusteredall.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_catalog_file_path = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_visibilities=1
    calibration_subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    undefine, diffuse_calibrate, diffuse_model
    ring_radius = 0
    rephase_weights = 0
    restrict_hpx_inds = 0
    hpx_radius = 10
        recalculate_all=1
    mapfn_recalculate=0
  end  
      'nb_eor0clusteredall_clustermodel': begin
  uvfits_version = 5
    uvfits_subversion = 1
    saved_run_bp = 0
    calibration_catalog_file_path = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_catalog_file_path = filepath('eor0clusteredall.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_visibilities=1
    calibration_subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    undefine, diffuse_calibrate, diffuse_model
    ring_radius = 0
    rephase_weights = 0
    restrict_hpx_inds = 0
    hpx_radius = 10
        recalculate_all=1
    mapfn_recalculate=0
  end  
  'nb_eor0clustered': begin
  uvfits_version = 5
    uvfits_subversion = 1
    saved_run_bp = 0
    calibration_catalog_file_path = filepath('eor0_clustered_comp.sav',root=rootdir('FHD'),subdir='catalog_data')
    calibration_subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    undefine, diffuse_calibrate, diffuse_model
    ring_radius = 0
    rephase_weights = 0
    restrict_hpx_inds = 0
    hpx_radius = 10
    recalculate_all=1
    mapfn_recalculate=0
  end  
  'nb_eor0clustered_points': begin
  uvfits_version = 5
    uvfits_subversion = 1
    saved_run_bp = 0
    calibration_catalog_file_path = filepath('eor0_clustered_points.sav',root=rootdir('FHD'),subdir='catalog_data')
    calibration_subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    undefine, diffuse_calibrate, diffuse_model
    ring_radius = 0
    rephase_weights = 0
    restrict_hpx_inds = 0
    hpx_radius = 10
        recalculate_all=1
    mapfn_recalculate=0
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
  
    
  'rlb_GLEAM_cal_decon_4pol_May2017': begin
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
    return_cal_visibilities = 1
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
    ;image_filter_fn = 'filter_uv_natural'
    write_healpix_fits = 1 
  end
  
  'rlb_1131454296_run1_cal_decon_4pol_May2017': begin
    uvfits_version = 5
    uvfits_subversion = 1
    max_sources = 200000
    calibration_catalog_file_path = '/nfs/mwa-08/d1/DiffuseSurvey2015/1131454296_run1_catalog.sav'
    gain_factor = 0.1
    deconvolve = 1
    return_decon_visibilities = 1
    smooth_width = 32
    deconvolution_filter = 'filter_uv_uniform'
    filter_background = 1
    dimension = 2048
    return_cal_visibilities = 1
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
  
  'rlb_1131454296_run1_cal_4pol_May2017': begin
    recalculate_all = 0
    mapfn_recalculate = 0
    uvfits_version = 5
    uvfits_subversion = 1
    saved_run_bp = 0
    calibration_catalog_file_path = '/nfs/mwa-08/d1/DiffuseSurvey2015/1131454296_run1_catalog.sav'
    calibration_subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    rephase_weights = 0
    restrict_hpx_inds = 0
    hpx_radius = 10
    undefine, diffuse_calibrate, diffuse_model
    ring_radius = 0
    n_pol = 4
 end
 
 'rlb_1130781304_run1_cal_decon_4pol_May2017': begin
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
    return_cal_visibilities = 1
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
    n_pol = 4
  end
  
  'rlb_1130781304_run1_cal_4pol_May2017': begin
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
    n_pol = 4
 end
 
 'rlb_1130781304_run2_cal_decon_4pol_May2017': begin
    uvfits_version = 5
    uvfits_subversion = 1
    max_sources = 200000
    calibration_catalog_file_path = '/nfs/mwa-08/d1/DiffuseSurvey2015/1130781304_run2_catalog.sav'
    gain_factor = 0.1
    deconvolve = 1
    return_decon_visibilities = 1
    smooth_width = 32
    deconvolution_filter = 'filter_uv_uniform'
    filter_background = 1
    dimension = 2048
    return_cal_visibilities = 1
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
    n_pol = 4
  end
  
  'rlb_1130781304_run2_cal_4pol_May2017': begin
    recalculate_all = 0
    mapfn_recalculate = 0
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
    n_pol = 4
 end
 
 'rlb_Pic_A_run1_cal_decon_May2017': begin
    uvfits_version = 5
    uvfits_subversion = 1
    max_sources = 200000
    calibration_catalog_file_path = '/nfs/mwa-04/r1/EoRuvfits/DiffuseSurvey2015/Pic_A_run1_catalog.sav'
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
 
 'rlb_Orion_run1_cal_decon_May2017': begin
    uvfits_version = 5
    uvfits_subversion = 1
    max_sources = 200000
    calibration_catalog_file_path = '/nfs/mwa-04/r1/EoRuvfits/DiffuseSurvey2015/Orion_run1_catalog.sav'
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
  
  'rlb_GLEAM+Fornax_cal_decon_4pol_Sept2017': begin
    uvfits_version = 5
    uvfits_subversion = 1
    max_sources = 200000
    calibration_catalog_file_path = filepath('GLEAM_plus_rlb2017.sav',root=rootdir('FHD'),subdir='catalog_data')
    gain_factor = 0.1
    deconvolve = 1
    return_decon_visibilities = 1
    smooth_width = 32
    deconvolution_filter = 'filter_uv_natural'
    filter_background = 1
    dimension = 2048
    return_cal_visibilities = 1
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
  
  'rlb_GLEAM+Fornax_cal_4pol_Sept2017': begin
    uvfits_version = 5
    uvfits_subversion = 1
    saved_run_bp = 0
    calibration_catalog_file_path = filepath('GLEAM_plus_rlb2017.sav',root=rootdir('FHD'),subdir='catalog_data')
    calibration_subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
    rephase_weights = 0
    restrict_hpx_inds = 0
    hpx_radius = 10
    undefine, diffuse_calibrate, diffuse_model
    ring_radius = 0
    n_pol = 4
 end
  
  'rlb_GLEAM+Fornax_cal_fullpol_branch_Sept2017': begin
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
