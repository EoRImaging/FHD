pro nb_eor_firstpass_versions
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
    output_directory = args[1]
    version = args[2]
    if nargs gt 3 then platform = args[3] else platform = '' ;indicates if running on AWS
  endif else begin
    ;obs_id = '1141660840'
    ;obs_id = '1061316296'
    obs_id = '1061319472'
    output_directory = '/Users/nabarry/MWA/data/'
    version = 'nb_nvis_test'
    platform=''
  endelse
  cmd_args={version:version}
  
  uvfits_version=5
  uvfits_subversion=1
  
  case version of

  ;;;;; Nichole's versions
  'nb_nvis_test': begin
    cal_bp_transfer=0
    diffuse_calibrate=0
    diffuse_model=0
    uvfits_version=4
    uvfits_subversion=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog=filepath('GLEAM_EGC_catalog_KGSscale_ssextended.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_subtract_sidelobe_catalog=filepath('GLEAM_EGC_catalog_KGSscale_ssextended.sav',root=rootdir('FHD'),subdir='catalog_data')
    ;transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/beams'
    transfer_psf = '/Users/nabarry/MWA/data/sample_data/beams'
        model_transfer = '/Users/nabarry/MWA/data/sample_data/cal_prerun/vis_data'
    ;        model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013cal_redo/cal_prerun/vis_data'
    tile_flag_list = ['111','118','121','128','131','132','133','141','142','144','151','152','163','164']
    ;       debug_gain_transfer='/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013longrun_std/calibration/'
    ;recalculate_all=1
    mapfn_recalculate=0
    grid_recalculate=0
    snapshot_recalculate=1
    save_visibilities=0
    export_images=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    ;beam_mask_threshold=1e3
    digital_gain_jump_polyfit=1
    calibration_auto_fit=1
    nfreq_avg=8
    time_cut=-4
    ;no_ref_tile=1
    ps_kspan=200.
  end
  
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
 'nb_model_goldenset_psf300': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    nfreq_avg=384
    psf_resolution=300
    model_delay_filter=1
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/'
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_model_interpolate': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    interpolate_kernel=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    nfreq_avg=384
    model_delay_filter=1
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/'
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_model_nofilter': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    nfreq_avg=384
    remove_sim_flags=1
    ;model_delay_filter=1
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/'
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_model_nofilter_flagged': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    nfreq_avg=16
    ;remove_sim_flags=1
    ;model_delay_filter=1
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/'
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_model_filter_flagged': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    nfreq_avg=16
    ;remove_sim_flags=1
    model_delay_filter=1
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/'
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_model_noclip_nofilter': begin
    ;debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    nfreq_avg=16
    ;remove_sim_flags=1
    ;model_delay_filter=1
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/'
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_model_clip_nofilter': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    undefine, diffuse_model, diffuse_calibrate
    nfreq_avg=16
    ;remove_sim_flags=1
    ;model_delay_filter=1
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/'
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_model_goldenset_phaseII': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    recalculate_all=1
    mapfn_recalculate=0
    return_cal_visibilities=0
    diffuse_model=0
    diffuse_calibrate=0
    nfreq_avg=384
    model_delay_filter=1
    remove_sim_flags=1
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
    'nb_sim_analytic_beam': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_goldenset'
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
    max_calibration_sources=4000
    nfreq_avg=384
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    recalculate_all=1
    mapfn_recalculate=0
    remove_sim_flags=1
    cal_mode_fit=0
    cal_reflection_hyperresolve=0
    cal_reflection_mode_theory=0
      beam_model_version=1
  dipole_mutual_coupling_factor=0
        model_delay_filter=1
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
    'nb_sim_perfect_beam': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_goldenset'
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
    max_calibration_sources=4000
    nfreq_avg=384
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    recalculate_all=1
    mapfn_recalculate=0
    remove_sim_flags=1
    cal_mode_fit=0
    cal_reflection_hyperresolve=0
    cal_reflection_mode_theory=0
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
  'nb_sim_goldenset_freq1': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_goldenset'
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
    max_calibration_sources=4000
    nfreq_avg=384
    sim_over_calibrate=1
     diffuse_model=0
      diffuse_calibrate=0
    cal_mode_fit=0
    ;freq_end=167.07500
    freq_start=167.15500
    freq_end = 167.31500
    ;freq_start=167.15500; 167.23500 167.31500 167.39500 167.47500 167.55500 167.63500 167.71500 167.79500 167.87500
    ;freq_start=167.23500; 167.31500 167.39500 167.47500 167.55500 167.63500 167.71500 167.79500 167.87500
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        model_delay_filter=1
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_goldenset_freq2': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_goldenset'
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
    max_calibration_sources=4000
    nfreq_avg=384
    sim_over_calibrate=1
     diffuse_model=0
      diffuse_calibrate=0
    cal_mode_fit=0
    ;freq_end=167.07500
    freq_start=167.394
    freq_end = 167.556
    ;freq_start=167.15500; 167.23500 167.31500 167.39500 167.47500 167.55500 167.63500 167.71500 167.79500 167.87500
    ;freq_start=167.23500; 167.31500 167.39500 167.47500 167.55500 167.63500 167.71500 167.79500 167.87500
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        model_delay_filter=1
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
'nb_sim_goldenset_freq3': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_goldenset'
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
    max_calibration_sources=4000
    nfreq_avg=384
    sim_over_calibrate=1
     diffuse_model=0
      diffuse_calibrate=0
    cal_mode_fit=0
    ;freq_end=167.07500
    freq_start=167.554
    freq_end = 167.716
    ;freq_start=167.15500; 167.23500 167.31500 167.39500 167.47500 167.55500 167.63500 167.71500 167.79500 167.87500
    ;freq_start=167.23500; 167.31500 167.39500 167.47500 167.55500 167.63500 167.71500 167.79500 167.87500
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        model_delay_filter=1
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_goldenset_freq4': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_goldenset'
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
    max_calibration_sources=4000
    nfreq_avg=384
    sim_over_calibrate=1
     diffuse_model=0
      diffuse_calibrate=0
    cal_mode_fit=0
    ;freq_end=167.07500
    freq_start=167.714
    freq_end = 167.876
    ;freq_start=167.15500; 167.23500 167.31500 167.39500 167.47500 167.55500 167.63500 167.71500 167.79500 167.87500
    ;freq_start=167.23500; 167.31500 167.39500 167.47500 167.55500 167.63500 167.71500 167.79500 167.87500
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        model_delay_filter=1
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
'nb_sim_goldenset_freq5': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_goldenset'
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
    max_calibration_sources=4000
    nfreq_avg=384
    sim_over_calibrate=1
     diffuse_model=0
      diffuse_calibrate=0
    cal_mode_fit=0
    ;freq_end=167.07500
    freq_start=167.874
    freq_end = 168.036
    ;freq_start=167.15500; 167.23500 167.31500 167.39500 167.47500 167.55500 167.63500 167.71500 167.79500 167.87500 167.95500 168.03500       168.11500       168.19500       168.27500       168.35500       168.43500       168.51500       168.59500       168.67500       168.75500       168.83500       168.91500
    ;freq_start=167.23500; 167.31500 167.39500 167.47500 167.55500 167.63500 167.71500 167.79500 167.87500
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        model_delay_filter=1
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
'nb_sim_goldenset_freq6': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_goldenset'
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
    max_calibration_sources=4000
    nfreq_avg=384
    sim_over_calibrate=1
     diffuse_model=0
      diffuse_calibrate=0
    cal_mode_fit=0
    ;freq_end=167.07500
    freq_start=168.034
    freq_end = 168.196
    ;freq_start=167.15500; 167.23500 167.31500 167.39500 167.47500 167.55500 167.63500 167.71500 167.79500 167.87500 167.95500 168.03500       168.11500       168.19500       168.27500       168.35500       168.43500       168.51500       168.59500       168.67500       168.75500       168.83500       168.91500
    ;freq_start=167.23500; 167.31500 167.39500 167.47500 167.55500 167.63500 167.71500 167.79500 167.87500
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        model_delay_filter=1
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_goldenset_freq7': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_goldenset'
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
    max_calibration_sources=4000
    nfreq_avg=384
    sim_over_calibrate=1
     diffuse_model=0
      diffuse_calibrate=0
    cal_mode_fit=0
    ;freq_end=167.07500
    freq_start=168.194
    freq_end = 168.356
    ;freq_start=167.15500; 167.23500 167.31500 167.39500 167.47500 167.55500 167.63500 167.71500 167.79500 167.87500 167.95500 168.03500       168.11500       168.19500       168.27500       168.35500       168.43500       168.51500       168.59500       168.67500       168.75500       168.83500       168.91500
    ;freq_start=167.23500; 167.31500 167.39500 167.47500 167.55500 167.63500 167.71500 167.79500 167.87500
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        model_delay_filter=1
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_goldenset_bubbles_over_all_2corr': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_goldenset'
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
    ;max_calibration_sources=4000
    nfreq_avg=384
    sim_over_calibrate=1
    undefine, diffuse_model, diffuse_calibrate
    cal_mode_fit=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        model_delay_filter=1
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data_2/'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_zenith_thermalnoise_bubbles_allmodel': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_goldenset'
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
    ;max_calibration_sources=4000
    sim_noise='/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013longrun_std/metadata/'+obs_id+'_obs.sav'
    nfreq_avg=384
    sim_over_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    cal_mode_fit=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        model_delay_filter=1
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_hash_perfect_smooth': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_goldenset'
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
        remove_sim_flags=1
    ;max_calibration_sources=4000
    nfreq_avg=384
    sim_over_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    cal_mode_fit=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        model_delay_filter=1
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_hash_smoothbeam/vis_data/'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
    'nb_sim_hash_perfect_smooth_v1': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_goldenset'
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
        remove_sim_flags=1
    ;max_calibration_sources=4000
    nfreq_avg=384
    sim_over_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    cal_mode_fit=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        model_delay_filter=1
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_hash_smoothbeam/vis_data_v1/'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_hash_perfect_nofilter': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_nofilter'
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
        remove_sim_flags=1
    ;max_calibration_sources=4000
    nfreq_avg=384
    sim_over_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    cal_mode_fit=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        ;model_delay_filter=1
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim//'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_hash_perfect_fullbeamredo': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_goldenset'
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
        remove_sim_flags=1
    ;max_calibration_sources=4000
    nfreq_avg=384
    sim_over_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    cal_mode_fit=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        model_delay_filter=1
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_hash_smoothbeam/vis_data_fullbeamredo/'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
'nb_sim_hash_perfect_psf300': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_goldenset'
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
        remove_sim_flags=1
    ;max_calibration_sources=4000
    nfreq_avg=384
    sim_over_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    cal_mode_fit=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        model_delay_filter=1
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_hash_smoothbeam/vis_data_psf300/'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
'nb_sim_hash_perfect_psf300_full': begin
    in_situ_sim_input = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_model_goldenset_psf300'
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
        remove_sim_flags=1
    ;max_calibration_sources=4000
    nfreq_avg=384
    sim_over_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    cal_mode_fit=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        model_delay_filter=1
        psf_resolution=300.
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_hash_smoothbeam/vis_data_psf300/'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
'nb_sim_bubble_perfect_psf200_double': begin
    ;in_situ_sim_input = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_model_goldenset_psf300'
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
        remove_sim_flags=1
    ;max_calibration_sources=4000
    nfreq_avg=384
    sim_over_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    calibrate_visibilities=0
    cal_mode_fit=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        model_delay_filter=1
        psf_resolution=200.
        double_precision=1
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_bubble_smoothbeam/vis_data_double/'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
'nb_sim_bubble_overfit': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_goldenset'
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
        remove_sim_flags=1
    ;max_calibration_sources=4000
    nfreq_avg=384
    sim_over_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    cal_mode_fit=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        model_delay_filter=1
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_mwa_bubbles/vis_data/'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
'nb_sim_hash_overfit': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_goldenset'
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
        remove_sim_flags=1
    ;max_calibration_sources=4000
    nfreq_avg=384
    sim_over_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    cal_mode_fit=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        model_delay_filter=1
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_hash_removesimflags/vis_data/'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
'nb_sim_hash_overfit_uvf': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_goldenset'
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
        remove_sim_flags=1
    ;max_calibration_sources=4000
    nfreq_avg=384
    sim_over_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    cal_mode_fit=0
    ;recalculate_all=1
    ;mapfn_recalculate=0
    ;healpix_recalculate=1
        model_delay_filter=1
        save_uvf=1
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_mwa_hash/vis_data/'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
'nb_sim_hash_only_pskspan60': begin
    ;in_situ_sim_input = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_model_goldenset_psf300'
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
        remove_sim_flags=1
    ;max_calibration_sources=4000
    nfreq_avg=384
    sim_over_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    ps_kspan=120.
    cal_mode_fit=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        psf_resolution=100.
        calibrate_visibilities=0
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_mwa_hash/vis_data/'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
'nb_sim_bubble_only': begin
    ;in_situ_sim_input = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_model_goldenset_psf300'
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
        remove_sim_flags=1
    ;max_calibration_sources=4000
    nfreq_avg=384
    sim_over_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    cal_mode_fit=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        psf_resolution=100.
        calibrate_visibilities=0
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_bubble_removesimflags/vis_data/'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end

  'nb_sim_hash_perfect_nofilter_noclip': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_nofilter_noclip'
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    ;debug_beam_clip_floor=1
        remove_sim_flags=1
    ;max_calibration_sources=4000
    nfreq_avg=384
    sim_over_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    cal_mode_fit=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        ;model_delay_filter=1
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim//'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_phaseII_bubbles_test': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_goldenset_phaseII'
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
        remove_sim_flags=1
    ;max_calibration_sources=4000
    nfreq_avg=384
    sim_over_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    cal_mode_fit=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        model_delay_filter=1
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
    'nb_sim_phaseII_hash_ave0': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_goldenset_phaseII'
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
        remove_sim_flags=1
    ;max_calibration_sources=4000
    nfreq_avg=384
    sim_over_calibrate=1
    diffuse_model=0
     diffuse_calibrate=0
     cal_bp_transfer=0
    cal_mode_fit=0
    cal_time_average=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        model_delay_filter=1
    eor_savefile =  '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_hash_phaseII/vis_data/'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
    'nb_sim_phaseII_hash_only': begin
    ;in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_goldenset_phaseII'
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    debug_beam_clip_floor=1
        remove_sim_flags=1
    ;max_calibration_sources=4000
    nfreq_avg=384
    sim_over_calibrate=1
    diffuse_model=0
     diffuse_calibrate=0
     calibrate_visibilities=0
     cal_bp_transfer=0
    cal_mode_fit=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
        model_delay_filter=1
    eor_savefile =  '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_hash_phaseII/vis_data/'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
'nb_sim_beam_flagged_best16_bubbles_fullflags': begin
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
    ;recalculate_all=1
    ;mapfn_recalculate=0
    ;healpix_recalculate=1
    nfreq_avg=16
    ;n_avg=384
    cal_mode_fit=0
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
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
  'nb_model_flagged': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    ;unflag_all=1
    ;recalculate_all=1
    ;mapfn_recalculate=0
    return_cal_visibilities=0
    diffuse_model=0
    diffuse_calibrate=0
    cal_time_average=0
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=16
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_model_fullbeam_16': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    ;recalculate_all=1
    ;mapfn_recalculate=0
    return_cal_visibilities=0
    diffuse_model=0
    diffuse_calibrate=0
    cal_time_average=0
    ;model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=16
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
    'nb_model_unflag_384': begin
    debug_beam_clip_floor=1
    calibrate_visibilities=0
    model_visibilities=1
    unflag_all=1
    ;recalculate_all=1
    ;mapfn_recalculate=0
    return_cal_visibilities=0
    diffuse_model=0
    diffuse_calibrate=0
    cal_time_average=0
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=384
    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
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
 'nb_sim_fullbeam_16': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_fullbeam_16'
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_mwa_hash/vis_data/'
        restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=16
    cal_mode_fit=0
        pskspan=200.
    ;model_delay_filter=1
    remove_sim_flags=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
   'nb_sim_simplebeam_16': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_fullbeam_16'
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_mwa_hash/vis_data/'
        restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=16
      beam_model_version=1
  dipole_mutual_coupling_factor=0
    cal_mode_fit=0
    pskspan=200.
    ;model_delay_filter=1
    remove_sim_flags=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  
'nb_sim_uvf_pskspan60_uvfilter': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_flagged'
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=16
    ;n_avg=384
    cal_mode_fit=0
    save_uvf=1
    ps_kspan=120.
    uv_filter='Tukey'
    squared_window=1
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
'nb_sim_uvf_pskspan60_uvfilter_varv2': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_flagged'
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=16
    ;n_avg=384
    cal_mode_fit=0
    save_uvf=1
    ps_kspan=120.
    uv_filter='Tukey'
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
'nb_sim_uvf_pskspan100': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_flagged'
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=16
    ;n_avg=384
    cal_mode_fit=0
    save_uvf=1
    ps_kspan=200.
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
'nb_sim_uvf_pskspan100_v2': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_unflag_16'
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=16
    ;n_avg=384
    cal_mode_fit=0
    save_uvf=1
    ps_kspan=200.
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  
  'nb_sim_uvf_pskspan150': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_flagged'
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=16
    ;n_avg=384
    cal_mode_fit=0
    save_uvf=1
    ps_kspan=300.
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_uvf_pskspan150_v2': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_unflag_16'
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=16
    ;n_avg=384
    cal_mode_fit=0
    save_uvf=1
    ps_kspan=300.
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
'nb_sim_uvf_pskspan300': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_flagged'
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=16
    ;n_avg=384
    cal_mode_fit=0
    save_uvf=1
    ps_kspan=600.
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
 'nb_sim_uvf_pskspan300_v2': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_unflag_16'
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=16
    ;n_avg=384
    cal_mode_fit=0
    ;save_uvf=1
    ps_kspan=600.
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
 'nb_sim_uvf_pskspan300_v2_noflags': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_unflag_16'
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=16
    remove_sim_flags=1
    ;n_avg=384
    cal_mode_fit=0
    ;save_uvf=1
    ps_kspan=600.
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
 'nb_sim_uvf_pskspan300_v2_noflags_large': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_unflag_16'
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=16
    remove_sim_flags=1
    ;n_avg=384
    cal_mode_fit=0
    ;save_uvf=1
    ps_kspan=600.
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
 'nb_sim_large_unflag_eorhash': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_unflag_384'
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_mwa_hash/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=384
    remove_sim_flags=1
    cal_mode_fit=0
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
'nb_sim_large_unflag_eorhash_pskspan100': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_unflag_384'
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_mwa_hash/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=384
    ;n_avg=384
    cal_mode_fit=0
    remove_sim_flags=1
    ps_kspan=200.
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
'nb_sim_large_unflag_eorhash_pskspan200': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_unflag_384'
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_mwa_hash/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=384
    ;n_avg=384
    cal_mode_fit=0
    remove_sim_flags=1
    ps_kspan=400.
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end  

  'nb_sim_large_unflag_eorhash_uvfilter': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_unflag_384
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_mwa_hash/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    ;recalculate_all=1
    snapshot_recalculate=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=384
        remove_sim_flags=1
    ;n_avg=384
    cal_mode_fit=0
    save_uvf=1
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    uv_filter='Blackman-Harris'
    squared_window=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_large_unflag_eorhash_uvfilter_save': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_unflag_384
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_mwa_hash/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    ;recalculate_all=1
    snapshot_recalculate=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=384
        remove_sim_flags=1
    ;n_avg=384
    cal_mode_fit=0
    ;save_uvf=1
    wt_array_sav=1
    pix_window_save=1
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    uv_filter='Blackman-Harris'
    squared_window=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_large_unflag_uvfilter': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_unflag_384
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_mwa_hash/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    ;recalculate_all=1
    snapshot_recalculate=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=384
        remove_sim_flags=1
    ;n_avg=384
    cal_mode_fit=0
    save_uvf=1
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    uv_filter='Blackman-Harris'
    squared_window=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
'nb_sim_large_unflag_eorhash_uvfilter_pskspan100': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_unflag_384'
    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_mwa_hash/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=384
    ;n_avg=384
    cal_mode_fit=0
    remove_sim_flags=1
    ps_kspan=200.
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    uv_filter='Blackman-Harris'
    squared_window=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
 'nb_sim_uvf_pskspan300_v2_noflags_ann100': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_unflag_16'
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=16
    remove_sim_flags=1
    ;n_avg=384
    cal_mode_fit=0
    save_uvf=1
    ps_kspan=600.
    max_baseline=120
    min_baseline=100.
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
 'nb_sim_uvf_pskspan300_v2_noflags_40-50': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_unflag_16'
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=16
    remove_sim_flags=1
    ;n_avg=384
    cal_mode_fit=0
    save_uvf=1
    ps_kspan=600.
    max_baseline=50.
    min_baseline=40.
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
 'nb_sim_uvf_pskspan300_v2_noflags_min20': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_unflag_16'
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=16
    remove_sim_flags=1
    ;n_avg=384
    cal_mode_fit=0
    save_uvf=1
    ps_kspan=600.
    max_baseline=120
    min_baseline=20.
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
   'nb_sim_uvf_pskspan300_v2_noflags_ann200': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_unflag_16'
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=16
    remove_sim_flags=1
    ;n_avg=384
    cal_mode_fit=0
    save_uvf=1
    ps_kspan=600.
    max_baseline=220
    min_baseline=200.
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end


'nb_sim_flagged_best': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_flagged'
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=16
    ;n_avg=384
    cal_mode_fit=0
     calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end

'nb_sim_uvf_pskspan300_uvfilter': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_flagged'
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    ;recalculate_all=1
    snapshot_recalculate=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=16
    ;n_avg=384
    cal_mode_fit=0
    save_uvf=1
    ps_kspan=600.
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    uv_filter='Tukey'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
  'nb_sim_uvf_pskspan300_uvfilter_varv2': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_flagged'
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    ;recalculate_all=1
    snapshot_recalculate=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=16
    ;n_avg=384
    cal_mode_fit=0
    save_uvf=1
    ps_kspan=600.
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    uv_filter='Tukey'
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
'nb_sim_max200': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_flagged'
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=16
    ;n_avg=384
    cal_mode_fit=0
    ;ps_kspan=120.
    max_baseline=200
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
'nb_sim_max150': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_flagged'
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=16
    ;n_avg=384
    cal_mode_fit=0
    ;ps_kspan=120.
    max_baseline=150
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
'nb_sim_max100': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_flagged'
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=16
    ;n_avg=384
    cal_mode_fit=0
    save_uvf=1
    ;ps_kspan=120.
    max_baseline=100
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
  end
'nb_sim_max60': begin
    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_flagged'
    ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_goldenset_bubbles/vis_data/'
    ;calibrate_visibilities=0
    ;model_visibilities=1
    max_calibration_sources=4000
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    sim_perf_calibrate=1
    diffuse_model=0
    diffuse_calibrate=0
    recalculate_all=1
    mapfn_recalculate=0
    healpix_recalculate=1
    nfreq_avg=16
    ;n_avg=384
    cal_mode_fit=0
    save_uvf=1
    ;ps_kspan=120.
    max_baseline=60
    ;cal_time_average=1 ;reseting the gains makes this unnecessary
    model_delay_filter=1
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
  'nb_2014zenith_calonly': begin
    cal_bp_transfer=0
    diffuse_calibrate=0
    diffuse_model=0
    uvfits_version=5
    uvfits_subversion=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    ;transfer_psf = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_2014zenith_calonly/beams_save'
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    ;phase_longrun=1 ;add to github
    digital_gain_jump_polyfit=1 
    no_ref_tile=1
    cal_stop=1
    time_cut=-4
  end
    'nb_2014zenith_calonly_fix': begin
    saved_run_bp=0
    diffuse_calibrate=0
    diffuse_model=0
    uvfits_version=5
    uvfits_subversion=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    ;transfer_psf = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_2014zenith_calonly/beams_save'
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
    debug_fix=1
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
  'nb_2013zenith_calonly_autovis': begin
    cal_bp_transfer=0
    diffuse_calibrate=0
    diffuse_model=0
    uvfits_version=5
    uvfits_subversion=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/beams'
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    ;phase_longrun=1 ;add to github
    cal_stop=1
    time_cut=-4
  end
  'nb_2013cal_largerdim': begin
    cal_bp_transfer=0
    diffuse_calibrate=0
    diffuse_model=0
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
    digital_gain_jump_polyfit=1
    no_ref_tile=1
    cal_stop=1
    time_cut=-4
    debug_dim=1
  end
  'nb_2013cal_largerdim28': begin
    cal_bp_transfer=0
    diffuse_calibrate=0
    diffuse_model=0
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
    digital_gain_jump_polyfit=1
    no_ref_tile=1
    cal_stop=1
    time_cut=-4
    debug_dim=1
  end
  'nb_2013cal_redo': begin
    cal_bp_transfer=0
    diffuse_calibrate=0
    diffuse_model=0
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
    ;beam_mask_threshold=1e3
    nfreq_avg=8
    ;phase_longrun=1 ;add to github
    digital_gain_jump_polyfit=1
    no_ref_tile=1
    cal_stop=1
    time_cut=-4
  end

  'nb_auto_binoffset0': begin
    cal_bp_transfer=0
    diffuse_calibrate=0
    diffuse_model=0
    beam_offset_time=0
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    ;transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/beams'
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    ;phase_longrun=1 ;add to github
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
  'nb_Aug2017_globalbp_wo_cable': begin
    cal_bp_transfer=0
    cable_bandpass_fit=0
    undefine, diffuse_calibrate, diffuse_model
    undefine, cal_reflection_mode_theory,cal_reflection_mode_file,cal_reflection_mode_delay,cal_reflection_hyperresolve, cal_mode_fit
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
    time_cut=-4
  end
  'nb_Aug2017_globalbp_w_cable_w_digjump_extraflagged': begin
    cal_bp_transfer=0
    cable_bandpass_fit=0
    diffuse_calibrate=0
    diffuse_model=0
    cal_reflection_mode_theory = 150
    cal_reflection_hyperresolve=1
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly_autovis/cal_prerun/vis_data'
    transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/beams'
         debug_gain_transfer='/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013longrun_std/calibration/'
                 tile_flag_list = ['111','118','121','128','131','132','133','141','142','144','151','152','163','164']
    ;recalculate_all=1
    ;mapfn_recalculate=0
    digital_gain_jump_polyfit=1 
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    time_cut=-4
  end
  'nb_Aug2017_zeroedbp': begin
    cal_bp_transfer=0
    cable_bandpass_fit=1
    diffuse_calibrate=0
    diffuse_model=0
    cal_reflection_mode_theory=0
    cal_reflection_mode_file=0
    cal_reflection_mode_delay=0
    cal_reflection_hyperresolve=0
     cal_mode_fit=0
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly_autovis/cal_prerun/vis_data'
    transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/beams'
    debug_amp_longrun='/nfs/mwa-10/r1/EoRuvfits/analysis/ave_cals/full_ave_amp_zeroed.sav'
     debug_gain_transfer='/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013longrun_std/calibration/'
                ; tile_flag_list = ['111','118','121','128','131','132','133','141','142','144','151','152','163','164']
    recalculate_all=1
    mapfn_recalculate=0
    digital_gain_jump_polyfit=1 
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    time_cut=-4
  end
  'nb_Aug2017_globalbp_w_cable_w_digjump_nophaseref': begin
    cal_bp_transfer=0
    cable_bandpass_fit=0
    undefine, diffuse_calibrate, diffuse_model
    cal_reflection_mode_theory = 150
    cal_reflection_hyperresolve=1
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
    digital_gain_jump_polyfit=1 
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    time_cut=-4
    no_ref_tile=1
  end
  'nb_Aug2017_globalbp_w_cable_w_digjump_nophaseref_phaseave': begin
    cal_bp_transfer=0
    cable_bandpass_fit=0
    undefine, diffuse_calibrate, diffuse_model
    cal_reflection_mode_theory = 150
    cal_reflection_hyperresolve=1
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
    digital_gain_jump_polyfit=1 
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    time_cut=-4
    no_ref_tile=1
    debug_phase_longrun='/nfs/mwa-10/r1/EoRuvfits/analysis/ave_cals/full_ave_phase_dayref.sav'
  end
  'nb_Aug2017_globalbp_w_cable_w_digjump_phaseave': begin
    cal_bp_transfer=0
    cable_bandpass_fit=0
    diffuse_calibrate=0
     diffuse_model=0
    cal_reflection_mode_theory = 150
    cal_reflection_hyperresolve=1
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly_autovis/cal_prerun/vis_data'
    transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/beams'
    ;recalculate_all=1
    ;mapfn_recalculate=0
    digital_gain_jump_polyfit=1 
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    time_cut=-4
    ;no_ref_tile=1
    debug_phase_longrun='/nfs/mwa-10/r1/EoRuvfits/analysis/ave_cals/full_ave_phase_dayref.sav'
  end  

  'nb_Aug2017_cablebp_w_cable_w_digjump': begin
    cal_bp_transfer=0
    cable_bandpass_fit=1
    undefine, diffuse_calibrate, diffuse_model
    cal_reflection_mode_theory = 150
    cal_reflection_hyperresolve=1
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
    digital_gain_jump_polyfit=1 
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    time_cut=-4
  end
  'nb_Aug2017_savedbp_w_cable_w_digjump': begin
    cal_bp_transfer=1
    cable_bandpass_fit=1
    diffuse_calibrate=0
     diffuse_model=0
    cal_reflection_mode_theory = 150
    rephase_weights=1
    cal_reflection_hyperresolve=1
    uvfits_version=4
    uvfits_subversion=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly_autovis/cal_prerun/vis_data'
    transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/beams'
                debug_gain_transfer='/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_Aug2017_savedbp_w_cable_w_digjump/calibration/'
    ;recalculate_all=1
    ;mapfn_recalculate=0
    snapshot_recalculate=1
    save_visibilities=0
    export_images=0
    grid_recalculate=0
    digital_gain_jump_polyfit=1 
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    time_cut=-4
  end 

  'nb_Aug2017_tilebp_w_cable_w_digjump': begin
    cal_bp_transfer=0
    cable_bandpass_fit=1
    diffuse_calibrate=0
    diffuse_model=0
    cal_reflection_mode_theory = 150
    cal_reflection_hyperresolve=1
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/cal_prerun/vis_data'
    transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/beams'
    debug_amp_longrun='/nfs/mwa-10/r1/EoRuvfits/analysis/ave_cals/full_ave_amp.sav'
    recalculate_all=1
    mapfn_recalculate=0
    digital_gain_jump_polyfit=1 
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    time_cut=-4
  end
  'nb_Aug2017_savedbp': begin
    saved_run_bp=1
    cable_bandpass_fit=1
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
'nb_Aug2017_autocal_wo_cable_w_digjump': begin
    cal_transfer_bp=0

    diffuse_calibrate=0
    diffuse_model=0
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
        cal_reflection_mode_theory = 0
    cal_reflection_hyperresolve=0
    cal_reflection_mode_file=0
    cal_reflection_mode_delay=0
    cal_mode_fit=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly_autovis/cal_prerun/vis_data'
    transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/beams'
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    ;channel_edge_flag_width=2
    calibration_auto_fit=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    ;phase_longrun=1 ;add to github
    digital_gain_jump_polyfit=1 
    ;no_ref_tile=1
    ;cal_stop=1
    time_cut=-4
  end
'nb_Aug2017_autocal1_beam0': begin
    cal_transfer_bp=0

    diffuse_calibrate=0
    diffuse_model=0
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    ;model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/cal_prerun/vis_data'
    ;transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/beams'
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_offset_time=0
    ;channel_edge_flag_width=2
    calibration_auto_fit=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    ;phase_longrun=1 ;add to github
    digital_gain_jump_polyfit=1 
    ;no_ref_tile=1
    cal_stop=1
    time_cut=-4
  end

  'nb_Aug2017_autocal1_delaytest': begin
    cal_transfer_bp=0

    diffuse_calibrate=0
     diffuse_model=0
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
    ;channel_edge_flag_width=2
    calibration_auto_fit=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    ;phase_longrun=1 ;add to github
    digital_gain_jump_polyfit=1 
    ;no_ref_tile=1
    ;cal_stop=1
    time_cut=-4
  end
  'nb_hertzian_dipole_nomutual': begin
    cal_transfer_bp=0

    diffuse_calibrate=0
    diffuse_model=0
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    ;model_delay_filter=1
    ;channel_edge_flag_width=2
    calibration_auto_fit=1
    beam_mask_threshold=1e3
    ;nfreq_avg=8
    beam_model_version=1
    dipole_mutual_coupling_factor=0
    ;phase_longrun=1 ;add to github
    digital_gain_jump_polyfit=1 
    ;no_ref_tile=1
    cal_stop=1
    time_cut=-4
  end
  'nb_hertzian_dipole_nomutual_nocomplex': begin
    cal_transfer_bp=0

    diffuse_calibrate=0
    diffuse_model=0
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    ;model_delay_filter=1
    ;channel_edge_flag_width=2
    calibration_auto_fit=1
    beam_mask_threshold=1e3
    ;nfreq_avg=8
    beam_model_version=1
    dipole_mutual_coupling_factor=0
    ;phase_longrun=1 ;add to github
    digital_gain_jump_polyfit=1 
    ;no_ref_tile=1
    complex_beam=0
    cal_stop=1
    time_cut=-4
  end
    'nb_regbeam_nomutual': begin
    cal_transfer_bp=0

    diffuse_calibrate=0
    diffuse_model=0
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    ;model_delay_filter=1
    ;channel_edge_flag_width=2
    calibration_auto_fit=1
    beam_mask_threshold=1e3
    ;nfreq_avg=8
    beam_model_version=2
    dipole_mutual_coupling_factor=0
    ;phase_longrun=1 ;add to github
    digital_gain_jump_polyfit=1 
    ;no_ref_tile=1
    cal_stop=1
    time_cut=-4
  end
  'nb_2013longrun_std': begin
    cal_bp_transfer=0
    diffuse_calibrate=0
    diffuse_model=0
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
    digital_gain_jump_polyfit=1
    ;no_ref_tile=1
    ;cal_stop=1
    time_cut=-4
  end
  'nb_vis_stats': begin ;run on 1061313496
    cal_bp_transfer=0
    diffuse_calibrate=0
    diffuse_model=0
    uvfits_version=4
    uvfits_subversion=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    ;model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/cal_prerun/vis_data'
    ;transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/beams'
    tile_flag_list = ['111','118','121','128','131','132','133','141','142','144','151','152','163','164']
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    ;phase_longrun=1 ;add to github
    digital_gain_jump_polyfit=1
    ;no_ref_tile=1
    ;cal_stop=1
    time_cut=-4
  end
    'nb_2013longrun_autocal': begin
    cal_bp_transfer=0
    diffuse_calibrate=0
    diffuse_model=0
    uvfits_version=4
    uvfits_subversion=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/beams'
        model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly_autovis/cal_prerun/vis_data'
            tile_flag_list = ['111','118','121','128','131','132','133','141','142','144','151','152','163','164']
            debug_gain_transfer='/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013longrun_std/calibration/'
    ;snapshot_recalculate=1
    ;save_visibilities=0
    ;export_images=0
    ;grid_recalculate=0
    ;rephase_weights=1
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    digital_gain_jump_polyfit=1
    calibration_auto_fit=1
    nfreq_avg=8
    time_cut=-4
  end
  
      'nb_2013longrun_autocal_uvfilter': begin
    cal_bp_transfer=0
    diffuse_calibrate=0
    diffuse_model=0
    uvfits_version=4
    uvfits_subversion=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/beams'
        model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly_autovis/cal_prerun/vis_data'
            tile_flag_list = ['111','118','121','128','131','132','133','141','142','144','151','152','163','164']
            debug_gain_transfer='/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013longrun_std/calibration/'
    ;snapshot_recalculate=1
    ;save_visibilities=0
    ;export_images=0
    grid_recalculate=0
    save_visibilities=0
    export_images=0
    ;rephase_weights=1
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    digital_gain_jump_polyfit=1
    calibration_auto_fit=1
    nfreq_avg=8
    time_cut=-4
    uv_filter='Blackman-Harris'
    squared_window=1
  end
      'nb_2013longrun_autocal_pskspan100': begin
    cal_bp_transfer=0
    diffuse_calibrate=0
    diffuse_model=0
    uvfits_version=4
    uvfits_subversion=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog=filepath('GLEAM_EGC_catalog_KGSscale_ssextended.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_subtract_sidelobe_catalog=filepath('GLEAM_EGC_catalog_KGSscale_ssextended.sav',root=rootdir('FHD'),subdir='catalog_data')
    ;transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/beams'
    ;transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013cal_redo/beams'
    ;    model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly_autovis/cal_prerun/vis_data'
    ;        model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013cal_redo/cal_prerun/vis_data'
            tile_flag_list = ['111','118','121','128','131','132','133','141','142','144','151','152','163','164']
     ;       debug_gain_transfer='/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013longrun_std/calibration/'
     ;recalculate_all=1
     mapfn_recalculate=0
    grid_recalculate=0
    snapshot_recalculate=1
    save_visibilities=1
    export_images=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    ;beam_mask_threshold=1e3
    digital_gain_jump_polyfit=1
    calibration_auto_fit=1
    nfreq_avg=8
    time_cut=-4
    ;no_ref_tile=1
    ps_kspan=200.
  end
      'nb_2013longrun_autocal_pskspan100_horizonhpx': begin
    cal_bp_transfer=0
    diffuse_calibrate=0
    diffuse_model=0
    uvfits_version=4
    uvfits_subversion=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/beams'
        model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly_autovis/cal_prerun/vis_data'
            tile_flag_list = ['111','118','121','128','131','132','133','141','142','144','151','152','163','164']
            debug_gain_transfer='/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013longrun_std/calibration/'
    ;snapshot_recalculate=1
    ;save_visibilities=0
    ;export_images=0
    grid_recalculate=0
    save_visibilities=0
    export_images=0
    ;rephase_weights=1
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    digital_gain_jump_polyfit=1
    calibration_auto_fit=1
    nfreq_avg=8
    time_cut=-4
    ps_kspan=200.
  end
  
   'nb_2013longrun_autocal_pskbinsize2': begin
    cal_bp_transfer=0
    diffuse_calibrate=0
    diffuse_model=0
    uvfits_version=4
    uvfits_subversion=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/beams'
        model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly_autovis/cal_prerun/vis_data'
            tile_flag_list = ['111','118','121','128','131','132','133','141','142','144','151','152','163','164']
            debug_gain_transfer='/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013longrun_std/calibration/'
    ;snapshot_recalculate=1
    ;save_visibilities=0
    ;export_images=0
    ;grid_recalculate=0
    ;rephase_weights=1
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    digital_gain_jump_polyfit=1
    calibration_auto_fit=1
    nfreq_avg=8
    time_cut=-4
    ps_kbinsize=2.
  end
  'nb_2013longrun_autocal_pskspan60_smooth': begin
    cal_bp_transfer=0
    diffuse_calibrate=0
    diffuse_model=0
    uvfits_version=4
    uvfits_subversion=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/beams'
        model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly_autovis/cal_prerun/vis_data'
            tile_flag_list = ['111','118','121','128','131','132','133','141','142','144','151','152','163','164']
            debug_gain_transfer='/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013longrun_std/calibration/'
    snapshot_recalculate=1
    ;save_visibilities=0
    ;export_images=0
    grid_recalculate=1
    recalculate_all=1
    mapfn_recalculate=0
    ;rephase_weights=1
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    digital_gain_jump_polyfit=1
    calibration_auto_fit=1
    nfreq_avg=8
    time_cut=-4
    ps_kspan=120.
    baseline_threshold=-120.
    width_smooth=20.
  end
    'nb_2014longrun_autocal': begin
    cal_bp_transfer=0
    diffuse_calibrate=0
    diffuse_model=0
    uvfits_version=5
    uvfits_subversion=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    transfer_psf = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_2014zenith_calonly/beams'
        model_transfer = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_2014zenith_calonly/cal_prerun/vis_data'
            tile_flag_list = ['111','118','121','128','131','132','133','141','142','144','151','152','163','164']
            debug_gain_transfer='/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_2014zenith_calonly/calibration/'
    ;snapshot_recalculate=1
    ;save_visibilities=0
    ;export_images=0
    ;grid_recalculate=0
    ;rephase_weights=1
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    digital_gain_jump_polyfit=1
    calibration_auto_fit=1
    nfreq_avg=8
    time_cut=-4
  end
 'nb_2014longrun_std': begin
    cal_bp_transfer=0
    diffuse_calibrate=0
    diffuse_model=0
    uvfits_version=5
    uvfits_subversion=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    ;transfer_psf = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_2014zenith_calonly/beams_save'
    ;    model_transfer = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_2014zenith_calonly/cal_prerun/vis_data'
            tile_flag_list = ['111','118','121','128','131','132','133','141','142','144','151','152','163','164']
    ;        debug_gain_transfer='/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_2014zenith_calonly/calibration/'
    ;snapshot_recalculate=1
    ;save_visibilities=0
    ;export_images=0
    ;grid_recalculate=0
    ;rephase_weights=1
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    digital_gain_jump_polyfit=1
    ;calibration_auto_fit=1
    nfreq_avg=8
    time_cut=-4
  end
  'nb_2013longrun_zeroedbp': begin
    cal_bp_transfer=0
    cable_bandpass_fit=1
    diffuse_calibrate=0
    diffuse_model=0
    cal_reflection_mode_theory=0
    cal_reflection_mode_file=0
    cal_reflection_mode_delay=0
    cal_reflection_hyperresolve=0
     cal_mode_fit=0
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly_autovis/cal_prerun/vis_data'
    transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/beams'
    debug_amp_longrun='/nfs/mwa-10/r1/EoRuvfits/analysis/ave_cals/full_ave_amp_zeroed.sav'
     debug_gain_transfer='/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013longrun_std/calibration/'
                 tile_flag_list = ['111','118','121','128','131','132','133','141','142','144','151','152','163','164']
    recalculate_all=1
    mapfn_recalculate=0
    digital_gain_jump_polyfit=1 
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    time_cut=-4
  end
   'nb_2013longrun_autocal_smallcubecompare': begin
    cal_bp_transfer=0
    diffuse_calibrate=0
    diffuse_model=0
    uvfits_version=4
    uvfits_subversion=1
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/beams'
        model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly_autovis/cal_prerun/vis_data'
            tile_flag_list = ['111','118','121','128','131','132','133','141','142','144','151','152','163','164']
            debug_gain_transfer='/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013longrun_std/calibration/'
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    digital_gain_jump_polyfit=1
    calibration_auto_fit=1
    nfreq_avg=8
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
    'nb_2013longrun_aveampphase': begin
    cal_bp_transfer=0
    undefine, diffuse_calibrate, diffuse_model
    uvfits_version=5
    uvfits_subversion=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_transfer = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/cal_prerun/vis_data'
    transfer_psf = '/nfs/mwa-10/r1/EoRuvfits/analysis/fhd_nb_2013zenith_calonly/beams'
    tile_flag_list = ['111','118','121','128','131','132','133','141','142','144','151','152','163','164']
    debug_amp_longrun=1
    debug_phase_longrun=1
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    beam_mask_threshold=1e3
    nfreq_avg=8
    ;phase_longrun=1 ;add to github
    digital_gain_jump_polyfit=1 
    undefine, cal_reflection_mode_theory,cal_reflection_mode_file,cal_reflection_mode_delay,cal_reflection_hyperresolve
    ;no_ref_tile=1
    ;cal_stop=1
    time_cut=-4
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
   'nb_test': begin
    cal_bp_transfer=0
    diffuse_calibrate=0
    diffuse_model=0
    uvfits_version=4
    uvfits_subversion=1
    ;beam_model_version=1
    ;dipole_mutual_coupling_factor=0
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    save_antenna_model=1
    recalculate_all=1
    mapfn_recalculate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    ;beam_mask_threshold=1e3
    nfreq_avg=8
    ;phase_longrun=1 ;add to github
    digital_gain_jump_polyfit=1
    no_ref_tile=1
    cal_stop=1
    time_cut=-4
    debug_dim=1
  end
 'nb_paper_beam_test': begin
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
    diffuse_calibrate=0
    diffuse_model=0
    cal_cable_reflection_fit=0
    cal_cable_reflection_mode_fit=0
    cal_cable_reflection_correct=0
    beam_offset_time = 300
    flag_calibration = 0
    min_cal_baseline = 10
    calibration_polyfit = 0
    bandpass_calibrate = 0
    ;flag_visibilities = 1
    dimension = 4096
    elements = 4096
  end  
'nb_channel_7_001': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[0,-110] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_002': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[2,-108] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_003': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[4,-106] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_004': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[6,-104] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_005': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[8,-102] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_006': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[10,-100] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_007': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[12,-98] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
 'nb_channel_7_008': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[14,-96] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_009': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[16,-94] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_010': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[18,-92] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_011': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[20,-90] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_012': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[22,-88] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_013': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[24,-86] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_014': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[26,-84] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_015': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[28,-82] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_016': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[30,-80] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_017': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[32,-78] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_018': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[34,-76] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_019': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[36,-74] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_020': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[38,-72] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_021': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[40,-70] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_022': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[42,-68] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_023': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[44,-66] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_024': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[46,-64] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_025': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[48,-62] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_026': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[50,-60] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_027': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[52,-58] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_028': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[54,-56] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_029': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[56,-54] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_030': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[58,-52] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_031': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[60,-50] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_032': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[62,-48] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_033': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[64,-46] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_034': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[66,-44] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_035': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[68,-42] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_036': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[70,-40] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_037': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[72,-38] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_038': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[74,-36] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_039': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[76,-34] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_040': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[78,-32] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_041': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[80,-30] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_042': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[82,-28] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_043': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[84,-26] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_044': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[86,-24] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_045': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[88,-22] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_046': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[90,-20] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_048': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[92,-18] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_049': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[94,-16] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_050': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[96,-14] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_051': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[98,-12] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    end
'nb_channel_7_052': begin ;run on 1061313496
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[100,-10] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
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
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    ;image_filter_fn='filter_uv_natural'
    end
  'nb_channel_7_midobs_-3': begin ;run on 1061313496
    save_imagecube=1
    save_uvf=1
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[50,-60] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    ;image_filter_fn='filter_uv_natural'
    end
  'nb_channel_7_midobs_-2': begin ;run on 1061313496
    save_imagecube=1
    save_uvf=1
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[52,-58] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    ;image_filter_fn='filter_uv_natural'
    end
  'nb_channel_7_midobs_-1': begin ;run on 1061313496
    save_imagecube=1
    save_uvf=1
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[54,-56] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    ;image_filter_fn='filter_uv_natural'
    end
  'nb_channel_7_midobs_+1': begin ;run on 1061313496
    save_imagecube=1
    save_uvf=1
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[58,-52] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    ;image_filter_fn='filter_uv_natural'
    end
  'nb_channel_7_midobs_+2': begin ;run on 1061313496
    save_imagecube=1
    save_uvf=1
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[60,-50] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    ;image_filter_fn='filter_uv_natural'
    end
  'nb_channel_7_midobs_+3': begin ;run on 1061313496
    save_imagecube=1
    save_uvf=1
    split_ps_export=1
    freq_end = 187
    freq_start = 181
    time_cut=[62,-48] ;2seconds in the middle
    ;flag_visibilities=1
    transfer_weights = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/vis_data/1061313496_flags.sav'
    transfer_calibration = '/nfs/mwa-01/r1/EoRuvfits/analysis/fhd_nb_channel_7_weights/calibration/1061313496_cal.sav'
    recalculate_all=1
    mapfn_recalculate=0
    ;image_filter_fn='filter_uv_natural'
    end
     'nb_channel_7_weights': begin
    cal_bp_transfer=1
    cable_bandpass_fit=1
    diffuse_calibrate=0
     diffuse_model=0
    cal_reflection_mode_theory = 150
    cal_reflection_hyperresolve=1
    uvfits_version=4
    uvfits_subversion=1
    cal_time_average=0
    calibration_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    calibration_catalog_file_path='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    model_subtract_sidelobe_catalog='/nfs/eor-00/h1/nbarry/MWA/IDL_code/FHD/catalog_data/GLEAM_EGC_catalog_KGSscale_ssextended.sav'
    digital_gain_jump_polyfit=1 
    debug_beam_clip_floor=1
    model_delay_filter=1
  end 
endcase

case version of 

  'nb_nvis_test': begin
    ;vis_file_list = '/nfs/mwa-03/r1/EoR2013/cotter_pyuvfits_test/'+strtrim(string(obs_id),2)+'.uvfits'
    ;vis_file_list = '/nfs/eor-11/r1/EoRuvfits/jd2456528v4_1/1061316296/1061316296.uvfits'
    vis_file_list = '/Users/nabarry/MWA/data/sample_data/uvfits/4.1/1061319472/1061319472.uvfits'
    ;vis_file_list = '/nfs/eor-10/r1/EoRuvfits/jd2456856v5_1/1089664592/1089664592.uvfits'
  end
  'nb_paper_beam_test': begin
    ;vis_file_list = '/nfs/mwa-03/r1/EoR2013/cotter_pyuvfits_test/'+strtrim(string(obs_id),2)+'.uvfits'
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
