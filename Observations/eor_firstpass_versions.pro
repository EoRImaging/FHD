pro eor_firstpass_versions
except=!except
!except=0
heap_gc 

; wrapper to contain all the parameters for various runs we might do
; using firstpass.

; parse command line args
compile_opt strictarr
args = Command_Line_Args(count=nargs)
obs_id = args[0]
;obs_id = '1061321792'
;obs_id = '1061316296'
output_directory = args[1]
;output_directory = '/nfs/mwa-09/r1/djc/EoR2013/Aug23'
version = args[2]
;version = 'nb_small_uvfits'
cmd_args={version:version}

; Set default values for everything
calibrate_visibilities=1
recalculate_all=0
cleanup=0
ps_export=0
split_ps_export=1
combine_healpix=0
deconvolve=0
;mapfn_recalculate=0
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

uvfits_version=4
uvfits_subversion=1

catalog_file_path=filepath('MRC_full_radio_catalog.fits',root=rootdir('FHD'),subdir='catalog_data') ;Depreciated
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

case version of

   'apb_test_restrict_hpx_inds_1': begin
      restrict_hpx_inds='EoR0_high_healpix_inds.idlsave'       ; now graduated to a default
   end
   'apb_test_galaxy_cal_1': begin
      galaxy_calibrate=1
      image_filter_fn='filter_uv_natural'
   end
   'apb_test_pattis_catalog_1': begin
      calibration_catalog_file_path=filepath('pattis_catalog.sav',root=rootdir('FHD'),subdir='catalog_data')
   end 
   'apb_test_pattis_catalog_2': begin
      calibration_catalog_file_path=filepath('pattis_catalog.sav',root=rootdir('FHD'),subdir='catalog_data')
      flag_visibilities=0
   end
   'apb_test_2s_2': begin
      uvfits_subversion=3
      restrict_hpx_inds='EoR1_high_healpix_inds.idlsave'
   end
   'apb_make_EoR1_hpx_list': begin
      undefine,restrict_hpx_inds
   end
   'nb_firstpass_cablefit_no90': begin
      force_no_data=1
      allow_sidelobe_cal_sources=0  ;made before subtracting sidelobes
      ;vis_cal_polyfit.pro: line 86 changed to cable_cut_i=where(cable_len EQ 90,n_cable_cut)
   end
   'apb_test_small_kpix_1': begin
      ps_kbinsize=1.5
   end
   'apb_test_small_kpix_2': begin
      ps_kbinsize=0.75
   end
   'apb_test_small_kpix_3': begin
      ps_kbinsize=0.5
   end
   'apb_test_small_kpix_4': begin
      FoV=0
      kbinsize=0.5
      ps_kbinsize=0.5
   end
   'apb_test_dig_gain_fix_1': begin
      uvfits_version=3
      uvfits_subversion=1
      snapshot_healpix_export=0
   end
   'apb_test_no_flagging_1': begin
      flag_visibilities=0
   end
   'apb_test_no_flagging_2': begin
      flag_visibilities=0
      no_frequency_flagging=1
   end
   'apb_test_psf_resolution_1': begin
     psf_resolution=100.
   end
   'nb_firstpass_memo': begin
      uvfits_version=3
      uvfits_subversion=0
   end
   'apb_test_no_flagging_3': begin
      unflag_all=1
      flag_visibilities=0
      flag_calibration=0
   end
   'apb_test_no_flagging_4': begin
      flag_visibilities=0
      no_frequency_flagging=1
      nfreq_avg=1
   end

   ;;; Versions below this are with new defaults as of 23 May, 2014 !!!

   'apb_test_pattis_catalog_1_0': begin
      calibration_catalog_file_path=filepath('pattis_catalog_1_0.sav',root=rootdir('FHD'),subdir='catalog_data')
   end
   'apb_test_pattis_catalog_1_1': begin
      calibration_catalog_file_path=filepath('pattis_catalog_1_1.sav',root=rootdir('FHD'),subdir='catalog_data')
   end
   'apb_test_pattis_catalog_2_0': begin
      calibration_catalog_file_path=filepath('pattis_catalog_2_0.sav',root=rootdir('FHD'),subdir='catalog_data')
   end
   'apb_test_new_cotter_1': begin
      vis_freq_average=2
      uvfits_version=3
      uvfits_subversion=1
      time_cut=[2,-2]
   end
   'apb_gen_sourcelist_11': begin
      deconvolve=1
      return_decon_visibilities=1
      max_sources=30000.
      pad_uv_image=1.
      gain_factor=.2
      uvfits_version=3
      uvfits_subversion=1
      time_cut=[2,-2]
      vis_freq_average=2
   end
   'jp_EoR0_sem1_2': begin
      export_images=0
      grid_visibilities=0 
      calibration_catalog_file_path=filepath('pattis_catalog_1_1.sav',root=rootdir('FHD'),subdir='catalog_data')
   end
   'apb_test_beam_mask': begin
      beam_mask_electric_field=1
   end
   'nb_no_freq_avg': begin
      n_avg=1
   end

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
   'apb_test_compressed_1': begin
      vis_freq_average=2
      uvfits_version=3
      uvfits_subversion=3
      time_cut=[2,-2]
   end
   'apb_test_uncompressed_1': begin
      vis_freq_average=2
      uvfits_version=3
      uvfits_subversion=4
      time_cut=[2,-2]
   end
   'apb_test_single_compressed': begin
      uvfits_version=3
      uvfits_subversion=5
      n_avg=4
   end
   'apb_test_single_uncompressed': begin
      uvfits_version=3
      uvfits_subversion=6
      n_avg=4
   end
   'apb_test_single_compressed_transfer_cal': begin
      uvfits_version=3
      uvfits_subversion=5
      n_avg=4
      transfer_calibration='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_apb_test_single_uncompressed/1061316296_cal.sav'
   end
   'apb_test_fhd_2':begin
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
   'apb_test_diffuse_subtract':begin
      diffuse_model='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_apb_pp_deep_8/Healpix/diffuse_model.sav'
      model_visibilities=1
      dipole_mutual_coupling_factor=0
   end
   'apb_test_devel_no_coupling':begin
      dipole_mutual_coupling_factor=0
   end
   'apb_test_devel_beam_0_no_coupling':begin
      beam_model_version=0
      dipole_mutual_coupling_factor=0
   end

   'bjh_test_devel_beam_0_no_coupling_2':begin
      beam_model_version=0
      dipole_mutual_coupling_factor=0
      calibration_flag_iterate = 0
   end

  'bjh_test_devel_beam_0_no_coupling_3':begin
      beam_model_version=0
      dipole_mutual_coupling_factor=0
      calibration_flag_iterate = 0
   end


   'apb_sidelobe_subtract_2':begin
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

  ;catalog based on pac_test_fhd run using all 93 observations, fdet>.9
   'pac_test_catalog_3': begin
      calibration_catalog_file_path=filepath('patti_v3.0b.sav',root=rootdir('FHD'),subdir='catalog_data')
   end     
  

   ;catalog based on first 75 obsids
   'pac_test_catalog_3_5': begin
      calibration_catalog_file_path=filepath('patti_v3.5.sav',root=rootdir('FHD'),subdir='catalog_data')
   end   
  
   ;shallow clean >.5Jy sources
   'pac_test_catalog_3_7': begin
      calibration_catalog_file_path=filepath('patti_v3.7.sav',root=rootdir('FHD'),subdir='catalog_data')
   end   

  'pac_test_catalog_3': begin
      calibration_catalog_file_path=filepath('patti_v3.0b.sav',root=rootdir('FHD'),subdir='catalog_data')
   end     

   'pac_test_catalog_3_1': begin
      calibration_catalog_file_path=filepath('patti_v3.1.sav',root=rootdir('FHD'),subdir='catalog_data')
   end  

   'pac_test_catalog_3_2': begin
      calibration_catalog_file_path=filepath('patti_v3.2.sav',root=rootdir('FHD'),subdir='catalog_data')
   end 


   'pac_test_catalog_3_4': begin
      calibration_catalog_file_path=filepath('patti_v3.4.sav',root=rootdir('FHD'),subdir='catalog_data')
   end   

   'pac_test_catalog_3_5': begin
      calibration_catalog_file_path=filepath('patti_v3.5.sav',root=rootdir('FHD'),subdir='catalog_data')
   end   
  
   ;FHD zenith, beam 1a
   'pac_zenith_beam_1a': begin
      beam_model_version=1
      dipole_mutual_coupling_factor=0
   end   

   ;FHD zenith, beam 1b
   'pac_zenith_beam_1a': begin
      beam_model_version=1
      dipole_mutual_coupling_factor=1
   end 

   ;FHD zenith, beam 2a
   'pac_zenith_beam_1a': begin
      beam_model_version=2
      dipole_mutual_coupling_factor=0
   end 

   ;FHD zenith, beam 2b
   'pac_zenith_beam_1a': begin
      beam_model_version=2
      dipole_mutual_coupling_factor=1
   end 

   ;;; Aaron's versions!!! Only Aaron may edit this section!!!
   'aew_mwacs_plus_ben_fornax_and_vla_pic_ultralow_sept5':begin
      calibration_catalog_file_path=filepath('mwa_commissioning_source_list_add_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
      deconvolve=0
      FoV=120.
      dimension=4096
      tile_flag_list=[77,18,89,113,114,115,116,117,118,119,120]
   end

   ;;; Abraham's versions!!! Only Abraham may edit this section!!!
   'arn_mwacs_plus_ben_fornax_and_vla_pic':begin
	calibration_catalog_file_path=filepath('mwa_commissioning_source_list_add_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
	snapshot_healpix_export=0
;	min_cal_baseline=50
;	max_cal_baseline=400
	deconvolve=0
   end

   'arn_firstpass_eor1_with_fhd_fornax':begin
        calibration_catalog_file_path=filepath('mwa_commissioning_source_list_add_FHDaug23deconvolve_fornax_and_VLA_pic.sav',root=rootdir('FHD'),subdir='catalog_data')
        snapshot_healpix_export=1
        deconvolve=0
     end

   'mwa_cal_ben_fornax_vla_pic':begin
        calibration_catalog_file_path=filepath('mwa_calibration_source_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
   end

   'transfer_mean_oct24_eor0cal':begin
	transfer_calibration='/nfs/mwa-09/r1/abrahamn/128T/eor1_fhd/low/eor0/fhd_mwa_cal/calibration/cal_avg.sav'
	model_catalog_file_path=filepath('mwa_calibration_source_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
	model_visibilities=1
   end

   'transfer_mean_oct24_eor1cal':begin
	transfer_calibration='/nfs/mwa-09/r1/abrahamn/128T/eor1_fhd/low/eor1/fhd_mwa_cal_ben_fornax_vla_pic/calibration/cal_avg.sav'
	model_catalog_file_path=filepath('mwa_calibration_source_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
	model_visibilities=1
   end

   'transfer_mean_oct24_eor0cal_rescale':begin
	transfer_calibration='/nfs/mwa-09/r1/abrahamn/128T/eor1_fhd/low/eor0/fhd_mwa_cal/calibration/cal_avg_times1.08.sav'
        model_catalog_file_path=filepath('mwa_calibration_source_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
        model_visibilities=1
    end

   'mwa_cal_ben_fornax_vla_pic_rephaseeor1':begin
	override_target_phasera=59.78
	override_target_phasedec=-26.74
	calibration_catalog_file_path=filepath('mwa_calibration_source_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
   end

   'transfer_mean_oct24_eor1cal_rephaseeor1':begin
	override_target_phasera=59.78
	override_target_phasedec=-26.74
        transfer_calibration='/nfs/mwa-09/r1/abrahamn/128T/eor1_fhd/low/eor1/fhd_mwa_cal_ben_fornax_vla_pic/calibration/cal_avg.sav'
        model_catalog_file_path=filepath('mwa_calibration_source_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
        model_visibilities=1
    end

   'arn_caltest_basemin50_basemax1500':begin
        calibration_catalog_file_path=filepath('mwa_calibration_source_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
        snapshot_healpix_export=0
	min_cal_baseline=50
	max_cal_baseline=1500
	deconvolve=1
     end

   'arn_caltest_basemin50_basemax1000':begin
        calibration_catalog_file_path=filepath('mwa_calibration_source_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
        snapshot_healpix_export=0
        min_cal_baseline=50
        max_cal_baseline=1000
	deconvolve=1
     end

   'arn_caltest_basemin100_basemax1500':begin
        calibration_catalog_file_path=filepath('mwa_calibration_source_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
        snapshot_healpix_export=0
        min_cal_baseline=100
        max_cal_baseline=1500
	deconvolve=1
     end

   'arn_caltest_basemin100_basemax1000':begin
        calibration_catalog_file_path=filepath('mwa_calibration_source_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
        snapshot_healpix_export=0
        min_cal_baseline=100
        max_cal_baseline=1000
	deconvolve=1
     end

   'arn_caltest_basemin50_basemax500':begin
        calibration_catalog_file_path=filepath('mwa_calibration_source_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
        snapshot_healpix_export=0
        min_cal_baseline=50
        max_cal_baseline=500
        deconvolve=1
     end

   'arn_caltest_basemin50_basemax300':begin
        calibration_catalog_file_pith=filepath('mwa_calibration_source_BenMcKinley_fornax_and_VLA_pic_halfpixeloffset.sav',root=rootdir('FHD'),subdir='catalog_data')
        snapshot_healpix_export=0
        min_cal_baseline=50
        max_cal_baseline=300
        deconvolve=1
     end

   'arn_eor1_deconvtest_uniform':begin
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

    'arn_eor1_deconvtest_new':begin
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
 
    'arn_export_uncal_vis':begin
	snapshot_healpix_export=0
	calibrate_visibilities=0
    end
 
    'arn_patti_new_catalog':begin
	model_catalog_file_path=filepath('mwa_calibration_source_list_gleam_kgs_no_fornax.sav',root=rootdir('FHD'),subdir='catalog_data')
    end

    'arn_new_cube_defaults':begin
	cable_bandpass_fit=1
     end

    'arn_cable_bandpass_fit_and_patti_eor0low_cat':begin
	cable_bandpass_fit=1
	return_cal_visibilities=1
	model_visibilities=0
	mcalibration_catalog_file_path=filepath('arn_eor0_low_cat_patti_catalog.sav',root=rootdir('FHD'),subdir='catalog_data')
    end

    'arn_percablebandpass_patticatalog':begin
	cable_bandpass_fit=1
	calibration_catalog_file_path=filepath('arn_eor0_low_cat_patti_catalog.sav',root=rootdir('FHD'),subdir='catalog_data')
	saved_run_bp=0
	return_calibration_visibilities=1
    end

    'arn_fittedbandpass_patticatalog':begin
        cable_bandpass_fit=1
        calibration_catalog_file_path=filepath('arn_eor0_low_cat_patti_catalog.sav',root=rootdir('FHD'),subdir='catalog_data')
        saved_run_bp=1
        return_calibration_visibilities=1
    end

    'arn_fittedbandpass_patticatalog_nodiffuse':begin
	cable_bandpass_fit=1
	calibration_catalog_file_path=filepath('arn_eor0_low_cat_patti_catalog.sav',root=rootdir('FHD'),subdir='catalog_data')
	saved_run_bp=1
	return_calibration_visibilities=1
	undefine,diffuse_calibrate,diffuse_model
    end

    'arn_test_one_jy_source_at_eor0_center':begin
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
   'apb_sidelobe_subtract_3':begin
      allow_sidelobe_cal_sources=1
   end
   'apb_test_reflect_coefficients_1':begin
      cal_cable_reflection_correct=150
   end
   'apb_test_reflect_coefficients_2':begin
      cal_cable_reflection_correct=150
   end      
   'apb_test_max_cal_iter':begin
      max_cal_iter=100
   end
   'apb_test_diffuse_subtract_1':begin
      diffuse_model='/nfs/mwa-09/r1/djc/EoR2013/Aug23/old/fhd_apb_std_Nov2014/Healpix/diffuse_model_1.sav'
      model_visibilities=1
      calibration_visibilities_subtract=0
      return_cal_visibilities=1
      snapshot_healpix_export=1
      export_images=1
	image_filter_fn='filter_uv_natural'
	undefine,model_catalog_file_path
   end
   'apb_cal_sidelobes_N':begin
      calibration_catalog_file_path=filepath('MRC_calibration_catalog.sav',root=rootdir('FHD'),subdir='catalog_data')
      snapshot_healpix_export=0
   end
   'apb_cal_sidelobes_S':begin
      snapshot_healpix_export=0
   end
   'apb_cal_sidelobes_E':begin
      snapshot_healpix_export=0
   end
   'apb_cal_sidelobes_W':begin
      snapshot_healpix_export=0
   end
   'apb_test_reflect_coefficients_3':begin
                                ; don't actually set
                                ; anything. I changed the
                                ; expression for bandwidth in the mode
                                ; fitting code.
   end
   'apb_test_reflect_coefficients_4':begin
      cal_cable_reflection_correct=150
   end
   'apb_test_reflect_coefficients_5':begin
      cal_cable_reflection_correct=0
      cal_cable_reflection_mode_fit=150
   end
   'apb_test_delicate':begin
      calibration_catalog_file_path=filepath('foo.sav',root=rootdir('FHD'),subdir='catalog_data')
      delicate_calibration_catalog=1
   end
   'apb_std_Nov2014':begin
	export_images=1
   end
   'apb_test_beam_1B':begin
	; set the beam
	beam_model_version=1
	dipole_mutual_coupling_factor=1
	; turn fits back on
	export_images=1
   end
   'apb_test_beam_2B':begin
	; set the beam
	beam_model_version=2
	dipole_mutual_coupling_factor=1
	; turn fits back on
	export_images=1
   end
   'apb_test_beam_1A':begin
	; set the beam
	beam_model_version=1
	dipole_mutual_coupling_factor=0
	; turn fits back on
	export_images=1
     end
   'apb_test_beam_2A':begin
	; set the beam
	beam_model_version=2
	dipole_mutual_coupling_factor=0
	; turn fits back on
	export_images=1
   end
   'apb_std_Nov2014b':begin
        image_filter_fn='filter_uv_natural'
	export_images=1
   end
   'apb_compare_rts_catalog':begin
	model_catalog_file_path=filepath('RTS_catalog2.sav',root=rootdir('FHD'),subdir='catalog_data')
	model_visibilities=1
	return_cal_visibilities=0
	allow_sidelobe_model_sources=1
     end
   'apb_std_Dec2014b':begin
      beam_offset_time=56
   end
   'apb_test_diffuse_subtract_94':begin
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
   'apb_test_diffuse_subtract_94_tapered':begin
      diffuse_model=filepath('EoR0_diffuse_model_94_tapered.sav',root=rootdir('FHD'),subdir='catalog_data')
      model_visibilities=1
      calibration_visibilities_subtract=0
      return_cal_visibilities=1
      snapshot_healpix_export=1
      export_images=1
      image_filter_fn='filter_uv_natural'
      undefine,model_catalog_file_path
      beam_offset_time=56
   end
   'apb_test_beam_model_timing':begin
      nfreq_avg=1
   end
   'apb_transfer_rts_cal_1':begin
      transfer_calibration='/nfs/eor-00/h1/beards/rts_cal/cal_rts.sav'
   end
   'apb_std_Dec2014b':begin
	production=1
   end
   'apb_EoR0_high_sem1_1':begin
	production=1
	diffuse_model=filepath('EoR0_diffuse_model_94.sav',root=rootdir('FHD'),subdir='catalog_data')
        model_visibilities=1
        calibration_visibilities_subtract=0
        return_cal_visibilities=1
	undefine,model_catalog_file_path
   end
   'apb_find_moon':begin
      snapshot_healpix_export=0
   end
   'apb_look_for_variable_1':begin
        snapshot_healpix_export=0
   end
   'apb_look_for_variable_2':begin
        split_ps_export=0
        n_avg=2
   end

   ; Abraham's versions

   ; Bryna's versions
   'bjh_test_max_cal_iter':begin
      max_cal_iter=100
   end

   'bjh_test_max_cal_iter_no_cal_freq_flag':begin
      max_cal_iter=100
      no_calibration_frequency_flagging=1
   end

  ;Nichole's versions
   'nb_no_long_tiles':begin
      diffuse_calibrate=filepath('EoR0_diffuse_model_94.sav',root=rootdir('FHD'),subdir='catalog_data')
      cable_bandpass_fit=1
      tile_flag_list=[78,79,87,88,95,96,97,98,104,112,113,122,123,124]
   end
   'nb_autogainsonly_May2015':begin
      diffuse_calibrate=filepath('EoR0_diffuse_model_94.sav',root=rootdir('FHD'),subdir='catalog_data')
      ;cable_bandpass_fit=1
      undefine,export_images
      calibration_auto_fit=1
   end   
   'nb_devel_June2015_diffuseright':begin
      diffuse_calibrate=filepath('EoR0_diffuse_model_94.sav',root=rootdir('FHD'),subdir='catalog_data')
      diffuse_model=diffuse_calibrate
      cable_bandpass_fit=1
      saved_run_bp=1
      ;production=1
      ;no long tiles used in calculating saved bp
      
      allow_sidelobe_image_output=1
      beam_output_threshold=0.002
      ;ring_radius=30.*pad_uv_image
      show_beam_contour=1
      contour_levels=[0.01]
      recalculate_all=0
      stokes_high=1.
   end   
   'nb_devel_June2015_nosidelobes':begin
      diffuse_calibrate=filepath('EoR0_diffuse_model_94.sav',root=rootdir('FHD'),subdir='catalog_data')
      cable_bandpass_fit=1
      saved_run_bp=1
      ;production=1
      allow_sidelobe_cal_sources=0
      allow_sidelobe_model_sources=0
      beam_threshold=0.01
      beam_cal_threshold=0.01
      beam_model_threshold=0.01
      
      ;allow_sidelobe_image_output=1
      beam_output_threshold=0.002
      show_beam_contour=1
      contour_levels=[0.01]
      recalculate_all=1
      ;no long tiles used in calculating saved bp
   end  
   'nb_devel_June2015_nosidelobes_diffuseright':begin
      diffuse_calibrate=filepath('EoR0_diffuse_model_94.sav',root=rootdir('FHD'),subdir='catalog_data')
      diffuse_model=diffuse_calibrate
      cable_bandpass_fit=1
      saved_run_bp=1
      ;production=1
      allow_sidelobe_cal_sources=0
      allow_sidelobe_model_sources=0
      beam_threshold=0.01
      beam_cal_threshold=0.01
      beam_model_threshold=0.01
      
      ;allow_sidelobe_image_output=1
      beam_output_threshold=0.002
      show_beam_contour=1
      contour_levels=[0.01]
      recalculate_all=1
      ;no long tiles used in calculating saved bp
   end   
   'nb_devel_June2015_sidelobecalonly':begin
      diffuse_calibrate=filepath('EoR0_diffuse_model_94.sav',root=rootdir('FHD'),subdir='catalog_data')
      cable_bandpass_fit=1
      saved_run_bp=1
      production=1
      allow_sidelobe_cal_sources=1
      allow_sidelobe_model_sources=0
      ;no long tiles used in calculating saved bp
      
      ;Maybe needs these to? Ask Ian
      ;model_catalog_file_path=filepath('mwa_commissioning_source_list.sav',root=rootdir('FHD'),subdir='catalog_data')
      model_visibilities=1
      return_cal_visibilities=0
      allow_sidelobe_cal_sources=0
      allow_sidelobe_model_sources=1
   end     
   'nb_sidelobe_calibration_july2015': begin
   diffuse_calibrate=filepath('EoR0_diffuse_model_94.sav',root=rootdir('FHD'),subdir='catalog_data')
   cable_bandpass_fit=1
   saved_run_bp=1
   production=1
   allow_sidelobe_cal_sources=1
   allow_sidelobe_model_sources=0
   ;no long tiles used in calculating saved bp
   
   ;model_catalog_file_path=filepath('mwa_commissioning_source_list.sav',root=rootdir('FHD'),subdir='catalog_data')
   model_visibilities=1
   return_cal_visibilities=0
   
   beam_threshold=0.01
   beam_cal_threshold=0.01
   beam_model_threshold=0.01
   beam_output_threshold=0.01
   diffuse_model=diffuse_calibrate
   recalculate_all=1
   end       
    'nb_spec_indices': begin
      ;will shift bp, bp will need 1,2
       calibration_catalog_file_path=filepath('mwa_calibration_source_list_gleam_kgs_fhd_fornax.sav',root=rootdir('FHD'),subdir='catalog_data')
       degrid_spectral=1
       flatten_spectrum=1
       diffuse_spectral_index=-0.5
   end 
   'nb_decon_March2016':begin 
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
   'nb_decon_March2016_presidelobe':begin 
      max_sources=200000
      calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
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
      snapshot_recalculate=1
      recalculate_all=1
      
            undefine, diffuse_calibrate, diffuse_model
      saved_run_bp=0
      ;double memory, time
   end
      'nb_decon_Nov2015_presidelobesubtract':begin 
      max_sources=200000
      dft_threshold=1
      gain_factor=0.1
      deconvolve=1
      return_decon_visibilities=1
      smooth_width=32
      deconvolution_filter='filter_uv_uniform'
      filter_background=1
      dimension=3072
      FoV=80.
      pad_uv_image=1
      time_cut=[2,-2]
      snapshot_healpix_export=1
      subtract_sidelobe_catalog=1
      recalculate_all=1
      ;double memory, time
   end
   
   'nb_decon_July2015_through_firstpass': begin
      ;max_calibration_sources=1000
      undefine, diffuse_calibrate, diffuse_model
      calibration_catalog_file_path='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_decon_March2016_small/output_data/'+obs_id+'_source_array2.sav'
      saved_run_bp=0
      recalculate_all=1
      mapfn_recalculate=0
   end
   'nb_pytest_after_versioncontrol': begin
       recalculate_all=1
       mapfn_recalculate=0
   end
      'nb_pytest': begin
       recalculate_all=1
       mapfn_recalculate=0
   end
      'nb_pytest_before_versioncontrol': begin
       recalculate_all=1
       mapfn_recalculate=0
   end 
   'nb_gleam_firstpass': begin
       saved_run_bp=0
       calibration_catalog_file_path=filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
   end
   'nb_std_test_twopolyquad_extrafancymodeobs_Oct2015':begin 
      saved_run_twopoly_meanmode=1
   end
   'nb_vis_integration_4sec':begin 
      debug_evenoddsplit_integration=4
   end
   'nb_gleam_firstpass_100mJy': begin
       saved_run_bp=0
       calibration_catalog_file_path=filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
       calibration_flux_threshold = .1
   end
   'nb_gleam_firstpass_100mJy_nodiffuse': begin
       saved_run_bp=0
       undefine, diffuse_calibrate, diffuse_model
       calibration_catalog_file_path=filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
       calibration_flux_threshold = .1
   end
   'nb_patti_catalog': begin
       saved_run_bp=0
       calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
   end
   'nb_patti_catalog_nodiffuse': begin
       saved_run_bp=0
       undefine, diffuse_calibrate, diffuse_model
       calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
   end

   'nb_patti_catalog_nocaldiffuse': begin
       saved_run_bp=0
       undefine, diffuse_calibrate
       diffuse_model=filepath('EoR0_diffuse_model_94.sav',root=rootdir('FHD'),subdir='catalog_data')
       calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
       model_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
       model_visibilities=1
      return_cal_visibilities=0
   end
   'nb_patti_catalog_nosidelobes': begin
      allow_sidelobe_cal_sources=0
      allow_sidelobe_model_sources=0
      saved_run_bp=0
      calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
             recalculate_all=1
       mapfn_recalculate=0
   end
      'nb_gleam_firstpass_nosidelobes': begin
      allow_sidelobe_cal_sources=0
      allow_sidelobe_model_sources=0
       saved_run_bp=0
       calibration_catalog_file_path=filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
              recalculate_all=1
       mapfn_recalculate=0
   end 
   'nb_decon_July2016_presidelobe':begin 
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
  'nb_bandpass_division_test': begin
       saved_run_bp=0
       calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
       uvfits_version=5
       uvfits_subversion=1
   end
   'nb_no_bandpass_division_test': begin
       saved_run_bp=0
       calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
       uvfits_version=5
       uvfits_subversion=1
   end

   'nb_Aug24_2014': begin
       saved_run_bp=0
       calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
       uvfits_version=5
       uvfits_subversion=1
   end
   'nb_Sep14_2014': begin
       saved_run_bp=0
       calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
       uvfits_version=5
       uvfits_subversion=1
   end
   'nb_Sep10_2015': begin
       saved_run_bp=0
       calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
       uvfits_version=5
       uvfits_subversion=1
       recalculate_all=1
       mapfn_recalculate=0
   end
   'nb_sim_perfect_cal_noflag':begin 
      calibrate_visibilities=0
      export_images=0
      unflag_all=1
   end   
   'nb_sim_overfit_cal_noflag':begin 
      over_calibrate=1
      unflag_all=1
   end 
     'nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit_2':begin 
      nfreq_avg=384
      no_frequency_flagging=1
      recalculate_all=1
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
      ;turn_off_visflagbasic=1
      unflag_all=1

      nfreq_avg=384
      flag_calibration=0
      over_calibrate=1 
                  recalculate_all=1
      mapfn_recalculate=0
      calibration_catalog_file_path=filepath('confusion3.sav',root=rootdir('FHD'),subdir='catalog_data')
   end 
   'nb_sim_perfect_cal_eor_ones_maxcalsources_nod':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_beamperchannel_unflagged_nodiffuse_onebeam'
      no_frequency_flagging=1
      perfect_cal_ones=1
      max_calibration_sources=4000
      nfreq_avg=384
      save_uvf=1
      save_imagecube=1
      snapshot_recalculate=1
      recalculate_all=1
      ;make_grid_beam=1
      ;make_grid_psf=1
      temp_make_grid_beam=1
      undefine, diffuse_calibrate, diffuse_model
   end
   
      'nb_sim_perfect_cal_eor_ones_maxcalsources_nod_noauto_iter1000':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_beamperchannel_unflagged_nodiffuse_onebeam'
      no_frequency_flagging=1
      perfect_cal_ones=1
      max_calibration_sources=4000
      nfreq_avg=384
      recalculate_all=1
      ;make_grid_beam=1
      ;make_grid_psf=1
      calibration_auto_initialize=0
      max_cal_iter=1000
      undefine, diffuse_calibrate, diffuse_model
   end

   'nb_sim_perfect_cal_eor_ones_maxcalsources_nod_zenithpointing':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing'
      no_frequency_flagging=1
      perfect_cal_ones=1
      max_calibration_sources=4000
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model
   end
   
      'nb_sim_overfit_cal_eor_maxcalsources_nod_zenithpointing':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing'
      no_frequency_flagging=1
      over_calibrate=1
      max_calibration_sources=4000
      nfreq_avg=384  
      transfer_flags='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_sim_perfect_cal_eor_ones_maxcalsources_nod_zenithpointing/vis_data/'+obs_id+'_flags.sav'
      undefine, diffuse_calibrate, diffuse_model
   end
      'nb_sim_perfect_cal_eor_ones_maxcalsources_nod_zenithpointing_dnorm':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing' ;accidently had all sources, not just 10
      no_frequency_flagging=1
            psf_resolution=100
      interpolate_kernel=1
      perfect_cal_ones=1
      max_calibration_sources=4000
            flag_calibration=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model
   end
   
      'nb_sim_overfit_cal_eor_maxcalsources_nod_zenithpointing_dnorm':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_psf100interpol_10source_dnorm' ;accidently had all sources, not just 10
      psf_resolution=100
      interpolate_kernel=1
      no_frequency_flagging=1
      over_calibrate=1
                  flag_calibration=0
      max_calibration_sources=4000
      nfreq_avg=384  
      transfer_flags='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_sim_perfect_cal_eor_ones_maxcalsources_nod_zenithpointing/vis_data/'+obs_id+'_flags.sav'
      undefine, diffuse_calibrate, diffuse_model
   end
   
   
         'nb_sim_perfect_cal_eor_ones_confusion_S5000_100mJy':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_confusion_S5000'
      no_frequency_flagging=1
      perfect_cal_ones=1
      flag_calibration=0
      ;model_flux_threshold = .1
                        recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
            calibration_catalog_file_path=filepath('confusion_completeness3.sav',root=rootdir('FHD'),subdir='catalog_data')
      
      undefine, diffuse_calibrate, diffuse_model
   end
            'nb_sim_perfect_cal_noeor_ones_confusion_S5000_100mJy':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_confusion_S5000'
      no_frequency_flagging=1
      perfect_cal_ones=1
      flag_calibration=0
      remove_eor=1
      ;model_flux_threshold = .1
                        recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
            calibration_catalog_file_path=filepath('confusion_completeness3.sav',root=rootdir('FHD'),subdir='catalog_data')
      
      undefine, diffuse_calibrate, diffuse_model
   end
   
            'nb_sim_perfect_cal_eor_ones_farextent2_nod':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent2_nod'
      no_frequency_flagging=1
      perfect_cal_ones=1
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 4096
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model
   end
               'nb_sim_perfect_cal_eor_ones_farextent3_nod':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent3_nod'
      no_frequency_flagging=1
      perfect_cal_ones=1
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 6144
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model
   end
   
   
      'nb_sim_overfit_cal_eor_maxcalsources_nod_zenithpointing_notileflag':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      over_calibrate=1
            flag_calibration=0
      max_calibration_sources=4000
      nfreq_avg=384  
      ;transfer_flags='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_sim_perfect_cal_eor_ones_maxcalsources_nod_zenithpointing/vis_data/'+obs_id+'_flags.sav'
      undefine, diffuse_calibrate, diffuse_model
   end
      'nb_sim_perfect_cal_eor_ones_maxcalsources_nod_zenithpointing_notileflag':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      perfect_cal_ones=1
      flag_calibration=0
      max_calibration_sources=4000
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model
   end
         'nb_sim_overfit_cal_eor_maxcalsources_nod_zenithpointing_notileflag_noise':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      ;no_frequency_flagging=1
      over_calibrate=1
            ;flag_calibration=0
      max_calibration_sources=4000
      add_sim_noise=1
      nfreq_avg=384  
      recalculate_all=1
      mapfn_recalculate=0
      ;transfer_flags='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_sim_perfect_cal_eor_ones_maxcalsources_nod_zenithpointing/vis_data/'+obs_id+'_flags.sav'
      undefine, diffuse_calibrate, diffuse_model
   end
            'nb_sim_overfit_cal_eor_maxcalsources_nod_zenithpointing_notileflag_noise_nomin':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      over_calibrate=1
            flag_calibration=0
      max_calibration_sources=4000
      min_cal_baseline=1.
      add_sim_noise=1
      nfreq_avg=384  
      recalculate_all=1
      mapfn_recalculate=0
      ;transfer_flags='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_sim_perfect_cal_eor_ones_maxcalsources_nod_zenithpointing/vis_data/'+obs_id+'_flags.sav'
      undefine, diffuse_calibrate, diffuse_model
   end
               'nb_sim_overfit_cal_eor_nod_zenithpointing_notileflag_noise_nomin':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      over_calibrate=1
            flag_calibration=0
      ;max_calibration_sources=4000
      min_cal_baseline=1.
      add_sim_noise=1
      nfreq_avg=384  
      recalculate_all=1
      mapfn_recalculate=0
      ;transfer_flags='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_sim_perfect_cal_eor_ones_maxcalsources_nod_zenithpointing/vis_data/'+obs_id+'_flags.sav'
      undefine, diffuse_calibrate, diffuse_model
   end
   
            'nb_sim_overfit_cal_eor_nod_zenithpointing_notileflag_noise':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      over_calibrate=1
            flag_calibration=0
      ;max_calibration_sources=4000
      add_sim_noise=1
      nfreq_avg=384  
      recalculate_all=1
      mapfn_recalculate=0
      ;transfer_flags='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_sim_perfect_cal_eor_ones_maxcalsources_nod_zenithpointing/vis_data/'+obs_id+'_flags.sav'
      undefine, diffuse_calibrate, diffuse_model
   end
            'nb_sim_overfit_cal_eor_maxcalsources_nod_zenithpointing_notileflag_nonoisecheck':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      over_calibrate=1
            flag_calibration=0
      max_calibration_sources=4000
      ;add_sim_noise=1
      nfreq_avg=384  
      recalculate_all=1
      mapfn_recalculate=0
      ;transfer_flags='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_sim_perfect_cal_eor_ones_maxcalsources_nod_zenithpointing/vis_data/'+obs_id+'_flags.sav'
      undefine, diffuse_calibrate, diffuse_model
   end
      'nb_sim_perfect_cal_eor_ones_maxcalsources_nod_zenithpointing_notileflag_noise':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      perfect_cal_ones=1
      flag_calibration=0
      max_calibration_sources=4000
      add_sim_noise=1
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model
   end
   
   'nb_sim_perfect_cal_noeor_ones_maxcalsources_nod_zenithpointing_notileflag':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=4000
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model
   end
      'nb_sim_perfect_cal_noeor_ones_maxcalsources_nod_zenithpointing_notileflag_round2':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_noeor_ones_maxcalsources_nod_zenithpointing_notileflag'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=2000
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model
   end
         'nb_sim_perfect_cal_noeor_ones_maxcalsources_nod_zenithpointing_notileflag_modelmake10_2':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=10
      nfreq_avg=384  
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
            'nb_sim_perfect_cal_noeor_ones_maxcalsources_nod_zenithpointing_notileflag_modelmake10_Jan':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=10
      nfreq_avg=384  
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
            'nb_sim_perfect_cal_noeor_ones_maxcalsources_nod_zenithpointing_notileflag_modelmake10_2_psf200':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=10
      nfreq_avg=384  
      psf_resolution=200
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
               'nb_sim_perfect_cal_noeor_ones_maxcalsources_nod_zenithpointing_notileflag_modelmake10_2_psf1000':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=10
      nfreq_avg=384  
      psf_resolution=1000
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
                    'nb_sim_model_psf8interpol_4000source':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=4000
      nfreq_avg=384  
      psf_resolution=8
      interpolate_grid_kernel=1
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
                     'nb_sim_model_psf16interpol_4000source':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=4000
      nfreq_avg=384  
      psf_resolution=16
      interpolate_grid_kernel=1
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
                     'nb_sim_model_psf32interpol_4000source':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=4000
      nfreq_avg=384  
      psf_resolution=32
      interpolate_grid_kernel=1
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
                     'nb_sim_model_psf100interpol_4000source':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=4000
      nfreq_avg=384  
      psf_resolution=100
      interpolate_grid_kernel=1
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
                        'nb_sim_model_psf500interpol_4000source':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=4000
      nfreq_avg=384  
      psf_resolution=500
      interpolate_grid_kernel=1
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
        'nb_sim_model_psf8interpol_10source_dnorm':begin 
      nfreq_avg=384
      no_frequency_flagging=1
      recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
      ;turn_off_visflagbasic=1
      unflag_all=1
      flag_calibration=0
      over_calibrate=1 
            psf_resolution=8
      interpolate_kernel=1
   end 
           'nb_sim_model_psf16interpol_10source_dnorm':begin 
      nfreq_avg=384
      no_frequency_flagging=1
      recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
      ;turn_off_visflagbasic=1
      unflag_all=1
      flag_calibration=0
      over_calibrate=1 
            psf_resolution=16
      interpolate_kernel=1
   end 
           'nb_sim_model_psf32interpol_10source_dnorm':begin 
      nfreq_avg=384
      no_frequency_flagging=1
      recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
      ;turn_off_visflagbasic=1
      unflag_all=1
      flag_calibration=0
      over_calibrate=1 
            psf_resolution=32
      interpolate_kernel=1
   end 
           'nb_sim_model_psf100interpol_10source_dnorm':begin 
      nfreq_avg=384
      no_frequency_flagging=1
      recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
      ;turn_off_visflagbasic=1
      unflag_all=1
      flag_calibration=0
      over_calibrate=1 
            psf_resolution=100
      interpolate_kernel=1
   end 
           'nb_sim_model_psf500interpol_10source_dnorm':begin 
      nfreq_avg=384
      no_frequency_flagging=1
      recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
      ;turn_off_visflagbasic=1
      unflag_all=1
      flag_calibration=0
      over_calibrate=1 
            psf_resolution=500
      interpolate_kernel=1
   end 
                     'nb_sim_model_psf1000interpol_windowtest':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='calibration_sim/fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=10
      nfreq_avg=384  
      psf_resolution=1000
      interpolate_kernel=1
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
                       'nb_sim_perfect_psf1000interpol_windowtest':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_psf1000interpol_windowtest'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=5
      nfreq_avg=384  
      psf_resolution=1000
      interpolate_kernel=1
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
                  'nb_sim_model_psf8interpol_10source':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=10
      nfreq_avg=384  
      psf_resolution=8
      interpolate_kernel=1
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
                     'nb_sim_model_psf16interpol_10source':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=10
      nfreq_avg=384  
      psf_resolution=16
      interpolate_kernel=1
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
                     'nb_sim_model_psf32interpol_10source':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=10
      nfreq_avg=384  
      psf_resolution=32
      interpolate_kernel=1
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
                     'nb_sim_model_psf100interpol_10source':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=10
      nfreq_avg=384  
      psf_resolution=100
      interpolate_kernel=1
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
                        'nb_sim_model_psf500interpol_10source':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=10
      nfreq_avg=384  
      psf_resolution=500
      interpolate_kernel=1
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
                     'nb_sim_model_psf8interpol_10source_dnorm_5s':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_psf8interpol_10source_dnorm'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=10
      nfreq_avg=384  
      psf_resolution=8
      interpolate_kernel=1
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
                     'nb_sim_model_psf16interpol_10source_dnorm_5s':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_psf16interpol_10source_dnorm'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=10
      nfreq_avg=384  
      psf_resolution=16
      interpolate_kernel=1
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
                     'nb_sim_model_psf32interpol_10source_dnorm_5s':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_psf32interpol_10source_dnorm'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=10
      nfreq_avg=384  
      psf_resolution=32
      interpolate_kernel=1
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
                     'nb_sim_model_psf100interpol_10source_dnorm_5s':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_psf100interpol_10source_dnorm'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=10
      nfreq_avg=384  
      psf_resolution=100
      interpolate_kernel=1
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
                        'nb_sim_model_psf500interpol_10source_dnorm_5s':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_psf500interpol_10source_dnorm'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=10
      nfreq_avg=384  
      psf_resolution=500
      interpolate_kernel=1
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
                    'nb_sim_perfect_psf8interpol_10source':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_psf8interpol_10source'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=5
      nfreq_avg=384  
      psf_resolution=8
      interpolate_kernel=1
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
                     'nb_sim_perfect_psf16interpol_10source':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_psf16interpol_10source'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=5
      nfreq_avg=384  
      psf_resolution=16
      interpolate_kernel=1
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
                     'nb_sim_perfect_psf32interpol_10source':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_psf32interpol_10source'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=5
      nfreq_avg=384  
      psf_resolution=32
      interpolate_kernel=1
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
                     'nb_sim_perfect_psf100interpol_10source':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_psf100interpol_10source'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=5
      nfreq_avg=384  
      psf_resolution=100
      interpolate_kernel=1
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
                        'nb_sim_perfect_psf500interpol_10source':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_psf500interpol_10source'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=5
      nfreq_avg=384  
      psf_resolution=500
      interpolate_kernel=1
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
   
                       'nb_sim_perfect_psf8interpol_10source_dnorm':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_psf8interpol_10source_dnorm_5s'
      no_frequency_flagging=1
      perfect_cal_ones=1
      ;remove_eor=1
      flag_calibration=0
      max_calibration_sources=5
      nfreq_avg=384  
      psf_resolution=8
      interpolate_kernel=1
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
                     'nb_sim_perfect_psf16interpol_10source_dnorm':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_psf16interpol_10source_dnorm_5s'
      no_frequency_flagging=1
      perfect_cal_ones=1
      ;remove_eor=1
      flag_calibration=0
      max_calibration_sources=5
      nfreq_avg=384  
      psf_resolution=16
      interpolate_kernel=1
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
                     'nb_sim_perfect_psf32interpol_10source_dnorm':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_psf32interpol_10source_dnorm_5s'
      no_frequency_flagging=1
      perfect_cal_ones=1
      ;remove_eor=1
      flag_calibration=0
      max_calibration_sources=5
      nfreq_avg=384  
      psf_resolution=32
      interpolate_kernel=1
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
                     'nb_sim_perfect_psf100interpol_10source_dnorm':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_psf100interpol_10source_dnorm_5s'
      no_frequency_flagging=1
      perfect_cal_ones=1
      ;remove_eor=1
      flag_calibration=0
      max_calibration_sources=5
      nfreq_avg=384  
      psf_resolution=100
      interpolate_kernel=1
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
                        'nb_sim_perfect_psf500interpol_10source_dnorm':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_psf500interpol_10source_dnorm_5s'
      no_frequency_flagging=1
      perfect_cal_ones=1
      ;remove_eor=1
      flag_calibration=0
      max_calibration_sources=5
      nfreq_avg=384  
      psf_resolution=500
      interpolate_kernel=1
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
                           'nb_sim_perfect_psf100interpol_10source_dnorm_noeor':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_psf100interpol_10source_dnorm_5s'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=5
      nfreq_avg=384  
      psf_resolution=100
      interpolate_kernel=1
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
   
            'nb_sim_perfect_cal_noeor_ones_maxcalsources_nod_zenithpointing_notileflag_modelmake10_double_2':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=10
      double=1
      nfreq_avg=384  
            recalculate_all=1
      mapfn_recalculate=0
      undefine, diffuse_calibrate, diffuse_model
   end
            'nb_sim_perfect_cal_noeor_ones_maxcalsources_nod_zenithpointing_notileflag_5outof10_2':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_noeor_ones_maxcalsources_nod_zenithpointing_notileflag_modelmake10_2'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=5
            recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model
   end
               'nb_sim_perfect_cal_noeor_ones_maxcalsources_nod_zenithpointing_notileflag_5outof10_Jan':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_noeor_ones_maxcalsources_nod_zenithpointing_notileflag_modelmake10_Jan'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=5
            recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model
   end
               'nb_sim_perfect_cal_noeor_ones_maxcalsources_nod_zenithpointing_notileflag_5outof10_2_psf200':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_noeor_ones_maxcalsources_nod_zenithpointing_notileflag_modelmake10_2_psf200'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=5
      recalculate_all=1
      psf_resolution=200
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model
   end
                  'nb_sim_perfect_cal_noeor_ones_maxcalsources_nod_zenithpointing_notileflag_5outof10_2_psf1000':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_noeor_ones_maxcalsources_nod_zenithpointing_notileflag_modelmake10_2_psf1000'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=5
      recalculate_all=1
      psf_resolution=1000
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model
   end
               'nb_sim_perfect_cal_noeor_ones_maxcalsources_nod_zenithpointing_notileflag_5outof10_double_2':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_noeor_ones_maxcalsources_nod_zenithpointing_notileflag_modelmake10_double_2'
      no_frequency_flagging=1
      double=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=5
            recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model
   end
               'nb_sim_perfect_cal_noeor_ones_maxcalsources_nod_zenithpointing_notileflag_all10_2':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_noeor_ones_maxcalsources_nod_zenithpointing_notileflag_modelmake10_2'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      ;max_calibration_sources=5
            recalculate_all=1
      mapfn_recalculate=0
      recalculate_all=1
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model
      calibration_catalog_file_path=filepath('10brightsources.sav',root=rootdir('FHD'),subdir='catalog_data')
   end
            'nb_sim_perfect_cal_noeor_ones_maxcalsources_nod_zenithpointing_notileflag_minus5outof10_2':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_noeor_ones_maxcalsources_nod_zenithpointing_notileflag_modelmake10_2'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      ;max_calibration_sources=
            recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      recalculate_all=1
      mapfn_recalculate=0
      calibration_catalog_file_path=filepath('5dimsources.sav',root=rootdir('FHD'),subdir='catalog_data')
      
      undefine, diffuse_calibrate, diffuse_model
   end
               'nb_sim_perfect_cal_noeor_ones_maxcalsources_nod_zenithpointing_notileflag_minus5outof10_double_2':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_noeor_ones_maxcalsources_nod_zenithpointing_notileflag_modelmake10_double_2'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      ;max_calibration_sources=
            recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      recalculate_all=1
      double=1
      mapfn_recalculate=0
      calibration_catalog_file_path=filepath('5dimsources.sav',root=rootdir('FHD'),subdir='catalog_data')
      
      undefine, diffuse_calibrate, diffuse_model
   end
   
        'nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit_onesource':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_bright_source7'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      max_calibration_sources=4000
      nfreq_avg=384  
      recalculate_all=1
      undefine, diffuse_calibrate, diffuse_model
      calibration_catalog_file_path=filepath('bright_source7.sav',root=rootdir('FHD'),subdir='catalog_data')
   end 
        'nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit_onesource_round2':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit_onesource'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      ;max_calibration_sources=4000
      nfreq_avg=384  
      recalculate_all=1
      undefine, diffuse_calibrate, diffuse_model
      calibration_catalog_file_path=filepath('bright_source7.sav',root=rootdir('FHD'),subdir='catalog_data')
   end    
           'nb_sim_model_StokesV':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit_onesource'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      ;max_calibration_sources=4000
      nfreq_avg=384  
      instrument='mwa'
      recalculate_all=1
      mapfn_recalculate=0
      n_pol=4

      undefine, diffuse_calibrate, diffuse_model
      calibration_catalog_file_path=filepath('confusion_StokesV_2_ids.sav',root=rootdir('FHD'),subdir='catalog_data')
   end 
              'nb_sim_model_StokesV_2':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit_onesource'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      ;max_calibration_sources=4000
      nfreq_avg=384  
      instrument='mwa'
      recalculate_all=1
      mapfn_recalculate=0
      n_pol=4

      undefine, diffuse_calibrate, diffuse_model
      calibration_catalog_file_path=filepath('confusion_StokesV_2_ids_200mJy.sav',root=rootdir('FHD'),subdir='catalog_data')
   end 
                 'nb_sim_model_StokesQ':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit_onesource'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      ;max_calibration_sources=4000
      nfreq_avg=384  
      instrument='mwa'
      recalculate_all=1
      mapfn_recalculate=0
      n_pol=4

      undefine, diffuse_calibrate, diffuse_model
      calibration_catalog_file_path=filepath('confusion_StokesQ_ids_10Jy.sav',root=rootdir('FHD'),subdir='catalog_data')
   end 
                    'nb_sim_model_StokesV_West':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit_onesource'
      no_frequency_flagging=1
      perfect_cal_ones=1
      remove_eor=1
      flag_calibration=0
      ;max_calibration_sources=4000
      nfreq_avg=384  
      instrument='mwa'
      recalculate_all=1
      mapfn_recalculate=0
      n_pol=4

      undefine, diffuse_calibrate, diffuse_model
      calibration_catalog_file_path=filepath('confusion_StokesV_ids_10Jy_West.sav',root=rootdir('FHD'),subdir='catalog_data')
   end 
               'nb_sim_perfect_cal_noeor_ones_StokesQ':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      perfect_cal_ones=1
      flag_calibration=0
      remove_eor=1
      cal_sim_input='fhd_nb_sim_model_StokesQ'
      no_frequency_flagging=1
      perfect_cal_ones=1
      nfreq_avg=384
      skip_bp_plots=1
      instrument='mwa'
            recalculate_all=1
      mapfn_recalculate=0
      n_pol=4
      stokesV=1
      calibration_catalog_file_path=filepath('confusion_completeness_StokesQ_ids_10Jy.sav',root=rootdir('FHD'),subdir='catalog_data')
      undefine, diffuse_calibrate, diffuse_model
   end
                  'nb_sim_perfect_cal_noeor_ones_StokesV_West':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      perfect_cal_ones=1
      flag_calibration=0
      remove_eor=1
      cal_sim_input='fhd_nb_sim_model_StokesV_West'
      no_frequency_flagging=1
      perfect_cal_ones=1
      nfreq_avg=384
      skip_bp_plots=1
      instrument='mwa'
            recalculate_all=1
      mapfn_recalculate=0
      n_pol=4
      stokesV=1
      calibration_catalog_file_path=filepath('confusion_completeness_StokesV_ids_10Jy_West.sav',root=rootdir('FHD'),subdir='catalog_data')
      undefine, diffuse_calibrate, diffuse_model
   end
            'nb_sim_perfect_cal_noeor_ones_StokesV':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      perfect_cal_ones=1
      flag_calibration=0
      remove_eor=1
      cal_sim_input='fhd_nb_sim_model_StokesV'
      no_frequency_flagging=1
      perfect_cal_ones=1
      nfreq_avg=384
      skip_bp_plots=1
      instrument='mwa'
            recalculate_all=1
      mapfn_recalculate=0
      n_pol=4
      stokesV=1
      calibration_catalog_file_path=filepath('confusion_completeness_StokesV_2_ids.sav',root=rootdir('FHD'),subdir='catalog_data')
      undefine, diffuse_calibrate, diffuse_model
   end
              'nb_sim_perfect_cal_noeor_ones_StokesV_2':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      perfect_cal_ones=1
      flag_calibration=0
      remove_eor=1
      cal_sim_input='fhd_nb_sim_model_StokesV_2'
      no_frequency_flagging=1
      perfect_cal_ones=1
      nfreq_avg=384
      skip_bp_plots=1
      instrument='mwa'
            recalculate_all=1
      mapfn_recalculate=0
      n_pol=4
      stokesV=1
      calibration_catalog_file_path=filepath('confusion_completeness_StokesV_2_ids_200mJy.sav',root=rootdir('FHD'),subdir='catalog_data')
      undefine, diffuse_calibrate, diffuse_model
   end
         'nb_sim_perfect_cal_noeor_ones_dimcalsources_nod_notileflag':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      perfect_cal_ones=1
      flag_calibration=0
      remove_eor=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      perfect_cal_ones=1
      max_calibration_sources=-4000
      nfreq_avg=384
      recalculate_all=1
      undefine, diffuse_calibrate, diffuse_model
   end
         'nb_sim_perfect_cal_eor_ones_maxcalsources_nod_zenithpointing_notileflag_cable':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      perfect_cal_ones=1
      perfect_add_mode=1
      flag_calibration=0
      max_calibration_sources=4000
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model
   end
         'nb_sim_savedfit_cal_eor_maxcalsources_nod_zenithpointing_notileflag_2':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      saved_calibrate=1
            flag_calibration=0
      
      max_calibration_sources=4000
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
            'nb_sim_savedfit_cal_eor_maxcalsources_nod_zenithpointing_notileflag_noiseoncal_2p':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      noise_calibrate=1
            flag_calibration=0
      
      max_calibration_sources=4000
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
               'nb_sim_savedfit_cal_eor_maxcalsources_nod_zenithpointing_notileflag_noiseonall':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      noise_calibrate=1
            flag_calibration=0
      add_sim_noise=1
      max_calibration_sources=4000
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
            'nb_sim_smoothfit_cal_eor_maxcalsources_nod_zenithpointing_notileflag':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      smooth_calibrate=1
            flag_calibration=0
      recalculate_all=1
      max_calibration_sources=4000
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
   'nb_sim_smoothfit_cal_eor_maxcalsources_nod_zenithpointing_notileflag_perfectphase':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit'
      no_frequency_flagging=1
      smooth_calibrate=1
            flag_calibration=0
      recalculate_all=1
      max_calibration_sources=4000
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
         'nb_sim_savedfit_cal_eor_maxcalsources_nod_zenithpointing':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_unflagged_nodiffuse_onebeam_zenithpointing'
      no_frequency_flagging=1
      saved_calibrate=1
      max_calibration_sources=4000
      nfreq_avg=384  
      transfer_flags='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_sim_perfect_cal_eor_ones_maxcalsources_nod_zenithpointing/vis_data/'+obs_id+'_flags.sav'
      undefine, diffuse_calibrate, diffuse_model
   end
   'nb_sim_perfect_cal_eor_ones_maxcalsources_nod_calvisflag':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_beamperchannel_unflagged_nodiffuse_onebeam'
      no_frequency_flagging=1
      perfect_cal_ones=1
      flag_calibration=0
      no_tile_vis_flagging=1
      max_calibration_sources=4000
      nfreq_avg=384
      recalculate_all=1
      ;make_grid_beam=1
      make_grid_psf=1
      undefine, diffuse_calibrate, diffuse_model
   end
   'nb_sim_perfect_cal_eor_ones_dimcalsources_nod_unflagall':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      unflag_all=1
      cal_sim_input='fhd_nb_sim_beamperchannel_unflagged_nodiffuse_onebeam'
      no_frequency_flagging=1
      perfect_cal_ones=1
      max_calibration_sources=-4000
      nfreq_avg=384
      recalculate_all=1
      undefine, diffuse_calibrate, diffuse_model
   end
      'nb_sim_perfect_cal_eor_ones_dimcalsources_nod_calvisflag':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      perfect_cal_ones=1
      flag_calibration=0
      cal_sim_input='fhd_nb_sim_beamperchannel_unflagged_nodiffuse_onebeam'
      no_frequency_flagging=1
      perfect_cal_ones=1
      max_calibration_sources=-4000
      nfreq_avg=384
      recalculate_all=1
      undefine, diffuse_calibrate, diffuse_model
   end
      'nb_sim_perfect_cal_eor_ones_maxcalsources_nod_5000':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_beamperchannel_unflagged_nodiffuse_onebeam'
      no_frequency_flagging=1
      perfect_cal_ones=1
      max_calibration_sources=5000
      nfreq_avg=384
      save_uvf=1
      save_imagecube=1
      snapshot_recalculate=1
      recalculate_all=1
      ;make_grid_beam=1
      ;make_grid_psf=1
      temp_make_grid_beam=1
      undefine, diffuse_calibrate, diffuse_model
   end
      'nb_sim_perfect_cal_eor_ones_maxcalsources_nod_6000':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_beamperchannel_unflagged_nodiffuse_onebeam'
      no_frequency_flagging=1
      perfect_cal_ones=1
      max_calibration_sources=6000
      nfreq_avg=384
      save_uvf=1
      save_imagecube=1
      snapshot_recalculate=1
      recalculate_all=1
      ;make_grid_beam=1
      ;make_grid_psf=1
      temp_make_grid_beam=1
      undefine, diffuse_calibrate, diffuse_model
   end
      'nb_sim_perfect_cal_eor_ones_maxcalsources_nod_7000':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_beamperchannel_unflagged_nodiffuse_onebeam'
      no_frequency_flagging=1
      perfect_cal_ones=1
      max_calibration_sources=7000
      nfreq_avg=384
      save_uvf=1
      save_imagecube=1
      snapshot_recalculate=1
      recalculate_all=1
      ;make_grid_beam=1
      ;make_grid_psf=1
      temp_make_grid_beam=1
      undefine, diffuse_calibrate, diffuse_model
   end
      'nb_sim_perfect_cal_eor_ones_maxcalsources_nod_3000':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_beamperchannel_unflagged_nodiffuse_onebeam'
      no_frequency_flagging=1
      perfect_cal_ones=1
      max_calibration_sources=3000
      nfreq_avg=384
      save_uvf=1
      save_imagecube=1
      snapshot_recalculate=1
      recalculate_all=1
      ;make_grid_beam=1
      ;make_grid_psf=1
      temp_make_grid_beam=1
      undefine, diffuse_calibrate, diffuse_model
   end
      'nb_sim_perfect_cal_eor_ones_maxcalsources_nod_2000':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_beamperchannel_unflagged_nodiffuse_onebeam'
      no_frequency_flagging=1
      perfect_cal_ones=1
      max_calibration_sources=2000
      nfreq_avg=384
      save_uvf=1
      save_imagecube=1
      snapshot_recalculate=1
      recalculate_all=1
      ;make_grid_beam=1
      ;make_grid_psf=1
      temp_make_grid_beam=1
      undefine, diffuse_calibrate, diffuse_model
   end
      'nb_sim_perfect_cal_eor_ones_maxcalsources_nod_1000':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_beamperchannel_unflagged_nodiffuse_onebeam'
      no_frequency_flagging=1
      perfect_cal_ones=1
      max_calibration_sources=1000
      nfreq_avg=384
      save_uvf=1
      save_imagecube=1
      snapshot_recalculate=1
      recalculate_all=1
      ;make_grid_beam=1
      ;make_grid_psf=1
      temp_make_grid_beam=1
      undefine, diffuse_calibrate, diffuse_model
   end
      'nb_sim_perfect_cal_eor_ones_maxcalsources_nod_double':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_beamperchannel_unflagged_nodiffuse_onebeam_double'
      no_frequency_flagging=1
      perfect_cal_ones=1
      max_calibration_sources=4000
      nfreq_avg=384
      snapshot_recalculate=1
      recalculate_all=1
      double=1
      undefine, diffuse_calibrate, diffuse_model
      recalculate_all=1
   end
        
          'nb_nocablecal':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
   end  
   'nb_twopolyquad_extrafancy_2xfix_phasesolinput': begin
   saved_run_twopoly_meanmode=1
   end

      'nb_decon_Jan2016_nocleanbias':begin 
      max_sources=200000
      dft_threshold=1
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
      recalculate_all = 1 
      deconvolve=1
      export_images = 1
      model_recalculate = 1
      clean_bias_threshold = 0
      ;double memory, time
   end
      'nb_decon_Jan2016_nocondense':begin 
      max_sources=200000
      ;dft_threshold=1
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
      recalculate_all = 1
      deconvolve=1
      export_images = 1
      model_recalculate = 1
      no_condense_sources = 1
      ;double memory, time
   end
      'nb_decon_Jan2016_filterwidth2':begin 
      max_sources=200000
      ;dft_threshold=1
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
      recalculate_all = 1 
      deconvolve=1
      export_images = 1
      model_recalculate = 1
      filter_width = 2
      ;double memory, time
   end
         'nb_decon_Jan2016_filterwidth4':begin 
      max_sources=200000
      ;dft_threshold=1
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
      recalculate_all = 1
      deconvolve=1
      export_images = 1
      model_recalculate = 1
      filter_width = 4
      ;double memory, time
   end
            'nb_decon_Jan2016_filterwidth10':begin 
      max_sources=200000
      ;dft_threshold=1
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
      recalculate_all = 1
      deconvolve=1
      export_images = 1
      model_recalculate = 1
      filter_width = 10
      ;double memory, time
   end

   ;;; Patti's versions!!! Only Patti may edit this section!!!
   
   ; My default full deconvolution parameters
   'pac_full_fhd':begin
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

   'nb_diffuse_I_Jan2016':begin ;remade
      diffuse_calibrate=filepath('EoR0_diffuse_refract.sav',root=rootdir('FHD'),subdir='catalog_data')
      diffuse_model=diffuse_calibrate
   end 
   'nb_diffuse_I_Jan2016_throughfirstpass':begin ;remade
      diffuse_calibrate=filepath('EoR0_diffuse_refract.sav',root=rootdir('FHD'),subdir='catalog_data')
      diffuse_model=diffuse_calibrate
      calibration_catalog_file_path='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_decon_Jan2016/output_data/'+obs_id+'_catalog.sav'
   end
   'nb_nodiffuse_I_Jan2016_throughfirstpass':begin ;remade
      ;diffuse_calibrate=filepath('EoR0_diffuse_refract.sav',root=rootdir('FHD'),subdir='catalog_data')
      ;diffuse_model=diffuse_calibrate
      calibration_catalog_file_path='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_decon_Jan2016/output_data/'+obs_id+'_catalog.sav'
   end
   'nb_nodiffuse_Jan2016':begin ;remade
      undefine,diffuse_model,diffuse_calibrate
   end 
   'nb_sim_model_farextent2_nod':begin 
      FoV=0
      kbinsize=0.5
      dimension= 4096
      no_frequency_flagging=1
      ;turn_off_visflagbasic=1
      unflag_all=1
      nfreq_avg=384
      undefine, diffuse_calibrate, diffuse_model
   end
      'nb_sim_model_farextent3_nod':begin 
      FoV=0
      kbinsize=0.5
      dimension= 6144
      no_frequency_flagging=1
      ;turn_off_visflagbasic=1
      unflag_all=1
      nfreq_avg=384
      undefine, diffuse_calibrate, diffuse_model
   end
         'nb_sim_model_farextent1_nod_Apr':begin 
      FoV=0
      kbinsize=0.5
      dimension= 2048
      no_frequency_flagging=1
      interpolate_kernel=1
      flag_calibration=0
      ;turn_off_visflagbasic=1
      unflag_all=1
      nfreq_avg=384
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
            'nb_sim_model_farextent1_nod_Apr_nointerp':begin 
      FoV=0
      kbinsize=0.5
      dimension= 2048
      no_frequency_flagging=1
      ;interpolate_kernel=1
      flag_calibration=0
      ;turn_off_visflagbasic=1
      unflag_all=1
      nfreq_avg=384
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
               'nb_sim_model_farextent1_nod_Apr_nointerp_zenithpointing':begin 
      FoV=0
      kbinsize=0.5
      dimension= 2048
      no_frequency_flagging=1
      min_cal_baseline=1.
      ;interpolate_kernel=1
      flag_calibration=0
      ;turn_off_visflagbasic=1
      unflag_all=1
      nfreq_avg=384
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                  'nb_sim_perfect_cal_eor_ones_farextent1_nod_Apr':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                     'nb_sim_perfect_cal_eor_ones_farextent1_nod_Apr_run2':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_eor_ones_farextent1_nod_Apr'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                           'nb_sim_perfect_cal_eor_ones_farextent1_nod_Apr_max':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_eor_ones_farextent1_nod_Apr'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      max_calibration_sources=4000
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                     'nb_sim_perfect_cal_eor_ones_farextent1_nod_Apr_nointerp':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                        'nb_sim_perfect_cal_eor_ones_short_baselines_included':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      perfect_cal_ones=1
      max_calibration_sources=4000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                           'nb_sim_perfect_cal_eor_ones_short_baselines_included_beam2a':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      perfect_cal_ones=1
      max_calibration_sources=4000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      beam_model_version=2
      dipole_mutual_coupling_factor=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                              'nb_sim_perfect_cal_eor_ones_short_baselines_included_beam1b':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      perfect_cal_ones=1
      max_calibration_sources=4000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      beam_model_version=1
      dipole_mutual_coupling_factor=1
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                 'nb_sim_perfect_cal_eor_ones_short_baselines_included_beam1a':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      perfect_cal_ones=1
      max_calibration_sources=4000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      beam_model_version=1
      dipole_mutual_coupling_factor=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
   
   
                           'nb_sim_perfect_cal_eor_ones_short_baselines_included_beam2a_allsources':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      perfect_cal_ones=1
      ;max_calibration_sources=4000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      beam_model_version=2
      dipole_mutual_coupling_factor=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                              'nb_sim_perfect_cal_eor_ones_short_baselines_included_beam1b_allsources':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      perfect_cal_ones=1
      ;max_calibration_sources=4000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      beam_model_version=1
      dipole_mutual_coupling_factor=1
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                 'nb_sim_perfect_cal_eor_ones_short_baselines_included_beam1a_allsources':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      perfect_cal_ones=1
      ;max_calibration_sources=4000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      beam_model_version=1
      dipole_mutual_coupling_factor=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
   
   
                           'nb_sim_overfit_cal_eor_ones_short_baselines_included_beam2a':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp'
      no_frequency_flagging=1
      ;interpolate_kernal=1
            over_calibrate=1
      max_calibration_sources=4000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      beam_model_version=2
      dipole_mutual_coupling_factor=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                              'nb_sim_overfit_cal_eor_ones_short_baselines_included_beam1b':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp'
      no_frequency_flagging=1
      ;interpolate_kernal=1
            over_calibrate=1
      max_calibration_sources=4000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      beam_model_version=1
      dipole_mutual_coupling_factor=1
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                 'nb_sim_overfit_cal_eor_ones_short_baselines_included_beam1a':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp'
      no_frequency_flagging=1
      ;interpolate_kernal=1
            over_calibrate=1
      max_calibration_sources=4000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      beam_model_version=1
      dipole_mutual_coupling_factor=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
   
                              'nb_sim_overfit_cal_eor_ones_short_baselines_included_beam2a_matched':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp'
      no_frequency_flagging=1
      ;interpolate_kernal=1
            over_calibrate=1
      max_calibration_sources=4000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      beam_model_version=2
      dipole_mutual_coupling_factor=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
            calibration_catalog_file_path = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_sim_model_farextent1_nod_Apr_nointerp/output_data/catalog.sav'
   end
                              'nb_sim_overfit_cal_eor_ones_short_baselines_included_beam1b_matched':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp'
      no_frequency_flagging=1
      ;interpolate_kernal=1
            over_calibrate=1
      max_calibration_sources=4000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      beam_model_version=1
      dipole_mutual_coupling_factor=1
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
            calibration_catalog_file_path = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_sim_model_farextent1_nod_Apr_nointerp/output_data/catalog.sav'
   end
                                 'nb_sim_overfit_cal_eor_ones_short_baselines_included_beam1a_matched':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp'
      no_frequency_flagging=1
      ;interpolate_kernal=1
            over_calibrate=1
      max_calibration_sources=4000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      beam_model_version=1
      dipole_mutual_coupling_factor=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
      calibration_catalog_file_path = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_sim_model_farextent1_nod_Apr_nointerp/output_data/catalog.sav'

   end
   
   
   
                           'nb_sim_perfect_cal_eor_ones_short_baselines_included_zenithpointing':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp_zenithpointing'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      perfect_cal_ones=1
      max_calibration_sources=4000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                           'nb_sim_overfit_cal_eor_ones_short_baselines_included':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      over_calibrate=1
      max_calibration_sources=4000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                              'nb_sim_overfit_cal_eor_ones_short_baselines_included_zenithpointing':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp_zenithpointing'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      over_calibrate=1
      max_calibration_sources=4000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                       'nb_sim_overfit_cal_eor_ones_short_baselines_included_zenithpointing_1000':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp_zenithpointing'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      over_calibrate=1
      max_calibration_sources=1000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                       'nb_sim_overfit_cal_eor_ones_short_baselines_included_zenithpointing_2000':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp_zenithpointing'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      over_calibrate=1
      max_calibration_sources=2000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                       'nb_sim_overfit_cal_eor_ones_short_baselines_included_zenithpointing_3000':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp_zenithpointing'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      over_calibrate=1
      max_calibration_sources=3000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                 'nb_sim_overfit_cal_eor_ones_short_baselines_included_zenithpointing_5000':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp_zenithpointing'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      over_calibrate=1
      max_calibration_sources=5000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                    'nb_sim_overfit_cal_eor_ones_short_baselines_included_zenithpointing_6000':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp_zenithpointing'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      over_calibrate=1
      max_calibration_sources=6000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                 'nb_sim_savedfit_cal_eor_ones_short_baselines_included_zenithpointing':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp_zenithpointing'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      saved_calibrate=1
      max_calibration_sources=4000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                    'nb_sim_savedfit_cal_eor_ones_short_baselines_included_zenithpointing_1000':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp_zenithpointing'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      one_saved_calibrate=1
      max_calibration_sources=1000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                       'nb_sim_savedfit_cal_eor_ones_short_baselines_included_zenithpointing_2000':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp_zenithpointing'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      two_saved_calibrate=1
      max_calibration_sources=2000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                       'nb_sim_savedfit_cal_eor_ones_short_baselines_included_zenithpointing_3000':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp_zenithpointing'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      three_saved_calibrate=1
      max_calibration_sources=3000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                       'nb_sim_savedfit_cal_eor_ones_short_baselines_included_zenithpointing_5000':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp_zenithpointing'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      five_saved_calibrate=1
      max_calibration_sources=5000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                       'nb_sim_savedfit_cal_eor_ones_short_baselines_included_zenithpointing_6000':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp_zenithpointing'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      six_saved_calibrate=1
      max_calibration_sources=6000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                          'nb_sim_perfect_cal_eor_ones_short_baselines_included_zenithpointing_6000':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp_zenithpointing'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      perfect_cal_ones=1
      max_calibration_sources=6000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                             'nb_sim_perfect_cal_eor_ones_short_baselines_included_zenithpointing_5000':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp_zenithpointing'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      perfect_cal_ones=1
      max_calibration_sources=5000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                             'nb_sim_perfect_cal_eor_ones_short_baselines_included_zenithpointing_3000':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp_zenithpointing'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      perfect_cal_ones=1
      max_calibration_sources=3000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                             'nb_sim_perfect_cal_eor_ones_short_baselines_included_zenithpointing_2000':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp_zenithpointing'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      perfect_cal_ones=1
      max_calibration_sources=2000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                             'nb_sim_perfect_cal_eor_ones_short_baselines_included_zenithpointing_1000':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp_zenithpointing'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      perfect_cal_ones=1
      max_calibration_sources=1000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                              'nb_sim_smoothfit_cal_eor_ones_short_baselines_included':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      smooth_calibrate=1
      max_calibration_sources=4000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                 'nb_sim_cablefit_cal_eor_ones_short_baselines_included':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      saved_calibrate=1
      max_calibration_sources=4000
      min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                              'nb_sim_overfit_cal_eor_ones_short_baselines_excluded':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_nod_Apr_nointerp'
      no_frequency_flagging=1
      ;interpolate_kernal=1
      over_calibrate=1
      max_calibration_sources=4000
      ;min_cal_baseline=1.
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                        'nb_sim_model_farextent2_maxbaseline_512':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      FoV=0
      kbinsize=0.5
      dimension= 4096
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      max_baseline = 512.
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                           'nb_sim_model_farextent2_maxbaseline_512_sourcein':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      FoV=0
      kbinsize=0.5
      dimension= 4096
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      max_baseline = 512.
      calibration_catalog_file_path=filepath('proj_test.sav',root=rootdir('FHD'),subdir='catalog_data')
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                              'nb_sim_perfect_farextent2_maxbaseline_512':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent2_maxbaseline_512'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      max_calibration_sources=4000
      FoV=0
      kbinsize=0.5
      dimension= 4096
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      max_baseline =512.
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                 'nb_sim_perfect_farextent2_maxbaseline_512_sourcein':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent2_maxbaseline_512_sourcein'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      max_calibration_sources=3
      FoV=0
      kbinsize=0.5
      dimension= 4096
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      max_baseline =512.
      calibration_catalog_file_path= filepath('proj_test.sav',root=rootdir('FHD'),subdir='catalog_data')
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                 'nb_sim_perfect_farextent1_maxbaseline_512':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_maxbaseline_512'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      max_calibration_sources=4000
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      max_baseline =512.
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                    'nb_sim_perfect_farextent1_maxbaseline_512_sourcein':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_maxbaseline_512_sourcein'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      max_calibration_sources=3
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      max_baseline =512.
      calibration_catalog_file_path= filepath('proj_test.sav',root=rootdir('FHD'),subdir='catalog_data')
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                           'nb_nb_sim_perfect_farextent2_maxbaseline':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent2_maxbaseline'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      FoV=0
      kbinsize=0.5
      dimension= 4096
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      max_baseline = 700.
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                              'nb_sim_model_farextent1_maxbaseline_512':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_eor_ones_farextent1_nod_Apr'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      max_baseline = 512.
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                 'nb_sim_model_farextent1_maxbaseline_512_sourcein':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_eor_ones_farextent1_nod_Apr'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      max_baseline = 512.
      calibration_catalog_file_path= filepath('proj_test.sav',root=rootdir('FHD'),subdir='catalog_data')
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
   
                        'nb_nb_sim_perfect_farextent1_maxbaseline':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent1_maxbaseline'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      FoV=0
      kbinsize=0.5
      dimension= 2048
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      max_baseline = 700.
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
      'nb_sim_model_farextent2_nod_Apr':begin 
      FoV=0
      kbinsize=0.5
      dimension= 4096
      no_frequency_flagging=1
      interpolate_kernel=1
      flag_calibration=0
      conserve_memory=5E7
      ;turn_off_visflagbasic=1
      unflag_all=1
      nfreq_avg=384
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                     'nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent2_nod_Apr'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      FoV=0
      kbinsize=0.5
      dimension= 4096
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                        'nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr_700':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      FoV=0
      kbinsize=0.5
      dimension= 4096
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      baseline_threshold=700.
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                           'nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr_1000':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      FoV=0
      kbinsize=0.5
      dimension= 4096
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      baseline_threshold=1000.
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                              'nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr_1000_largewidth':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      FoV=0
      kbinsize=0.5
      dimension= 4096
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      baseline_threshold=1000.
      width_smooth=200.
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                 'nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr_1400_largewidth':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      FoV=0
      kbinsize=0.5
      dimension= 4096
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      baseline_threshold=1400.
      width_smooth=400.
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                    'nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr_1400_extralargewidth':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      FoV=0
      kbinsize=0.5
      dimension= 4096
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      baseline_threshold=1400.
      width_smooth=600.
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                    'nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr_1400_largewidth_max':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      FoV=0
      kbinsize=0.5
      dimension= 4096
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      max_calibration_sources=4000
      baseline_threshold=1400.
      width_smooth=400.
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                    'nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr_1400_extralargewidth_max':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      FoV=0
      kbinsize=0.5
      dimension= 4096
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      max_calibration_sources=4000
      baseline_threshold=1400.
      width_smooth=600.
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                        'nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr_max':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      max_calibration_sources=4000
      FoV=0
      kbinsize=0.5
      dimension= 4096
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
      'nb_sim_model_farextent3_nod_Apr':begin 
      FoV=0
      kbinsize=0.5
      dimension= 6144
      no_frequency_flagging=1
      interpolate_kernel=1
      flag_calibration=0
      conserve_memory=5E7
      ;turn_off_visflagbasic=1
      unflag_all=1
      nfreq_avg=384
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                              'nb_sim_perfect_cal_eor_ones_farextent3_nod_Apr_1500':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_eor_ones_farextent3_nod_Apr'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      baseline_threshold=1500.
      FoV=0
      kbinsize=0.5
      dimension= 6144
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                           'nb_sim_perfect_cal_eor_ones_farextent3_nod_Apr_max':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_eor_ones_farextent3_nod_Apr'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      max_calibration_sources=4000
      FoV=0
      kbinsize=0.5
      dimension= 6144
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                     'nb_sim_perfect_cal_eor_ones_farextent3_nod_Apr':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_model_farextent3_nod_Apr'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      FoV=0
      kbinsize=0.5
      dimension= 6144
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
   
   
                              'nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr_-1000':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      FoV=0
      kbinsize=0.5
      dimension= 4096
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      baseline_threshold=-1000.
      max_calibration_sources=4000
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                              'nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr_-1000_largewidth':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      FoV=0
      kbinsize=0.5
      dimension= 4096
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      baseline_threshold=-1000.
      max_calibration_sources=4000
      width_smooth=200.
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                 'nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr_-1400_largewidth':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      FoV=0
      kbinsize=0.5
      dimension= 4096
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      baseline_threshold=-1400.
      max_calibration_sources=4000
      width_smooth=400.
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                    'nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr_-1400_extralargewidth':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      FoV=0
      kbinsize=0.5
      dimension= 4096
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      baseline_threshold=-1400.
      max_calibration_sources=4000
      width_smooth=600.
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
   
                                 'nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr_-1000_thresh':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      FoV=0
      kbinsize=0.5
      dimension= 4096
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      baseline_threshold=-1000.
      calibration_flux_threshold=.4
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                              'nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr_-1000_largewidth_thresh':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      FoV=0
      kbinsize=0.5
      dimension= 4096
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      baseline_threshold=-1000.
      calibration_flux_threshold=.4
      width_smooth=200.
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                 'nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr_-1400_largewidth_thresh':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      FoV=0
      kbinsize=0.5
      dimension= 4096
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      baseline_threshold=-1400.
      calibration_flux_threshold=.4
      width_smooth=400.
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end
                                    'nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr_-1400_extralargewidth_thresh':begin 
      saved_run_bp=0
      cable_bandpass_fit=0
      turn_off_visflagbasic=1
      cal_sim_input='fhd_nb_sim_perfect_cal_eor_ones_farextent2_nod_Apr'
      no_frequency_flagging=1
      interpolate_kernal=1
      perfect_cal_ones=1
      flag_calibration=0
      conserve_memory=5E7
      FoV=0
      kbinsize=0.5
      dimension= 4096
      recalculate_all=1
      mapfn_recalculate=0
      nfreq_avg=384  
      baseline_threshold=-1400.
      calibration_flux_threshold=.4
      width_smooth=600.
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
   end   
   
   'nb_decon_March2016_small_through_firstpass': begin
      ;max_calibration_sources=1000
      undefine, diffuse_calibrate, diffuse_model
      calibration_catalog_file_path='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_decon_March2016_small/output_data/'+obs_id+'_source_array2.sav'
      saved_run_bp=0
      recalculate_all=1
      mapfn_recalculate=0
   end
   
      'nb_decon_March2016_small_through_firstpass_largeind': begin
      ;max_calibration_sources=1000
      undefine, diffuse_calibrate, diffuse_model
      calibration_catalog_file_path='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_decon_March2016_small/output_data/'+obs_id+'_source_array2.sav'
      saved_run_bp=0
      recalculate_all=1
      mapfn_recalculate=0
      restore, '/nfs/mwa-00/h1/nbarry/MWA/IDL_code/FHD/Observations/EoR0_high_healpix_inds_large.idlsave'
      restrict_hpx_inds=hpx_inds
   end
   
   
      'nb_firstpass_map_2048_maxbaseline': begin
      ;max_calibration_sources=1000
      undefine, diffuse_calibrate, diffuse_model
      calibration_catalog_file_path='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_decon_March2016_small/output_data/'+obs_id+'_source_array2.sav'
      saved_run_bp=0
      recalculate_all=1
      mapfn_recalculate=0
      max_cal_baseline=512
      max_baseline=512
      dimension = 2048
            FoV=0
      kbinsize=0.5
   end
   
         'nb_firstpass_map_2048_nocon': begin
      ;max_calibration_sources=1000
      undefine, diffuse_calibrate, diffuse_model
      calibration_catalog_file_path='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_decon_March2016_small/output_data/'+obs_id+'_source_array2.sav'
      saved_run_bp=0
      recalculate_all=1
      mapfn_recalculate=0
      dimension = 2048
            FoV=0
      kbinsize=0.5
      no_conjugate=1
   end
   
         'nb_firstpass_map_1024_maxbaseline': begin
      ;max_calibration_sources=1000
      undefine, diffuse_calibrate, diffuse_model
      calibration_catalog_file_path='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_decon_March2016_small/output_data/'+obs_id+'_source_array2.sav'
      saved_run_bp=0
      recalculate_all=1
      mapfn_recalculate=0
      dimension = 1024
            max_cal_baseline=512
      max_baseline=512
            FoV=0
      kbinsize=0.5
   end
            'nb_firstpass_map_4096_maxbaseline': begin
      ;max_calibration_sources=1000
      undefine, diffuse_calibrate, diffuse_model
      calibration_catalog_file_path='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_decon_March2016_small/output_data/'+obs_id+'_source_array2.sav'
      saved_run_bp=0
      recalculate_all=1
      mapfn_recalculate=0
      dimension = 4096
            max_cal_baseline=512
      max_baseline=512
            FoV=0
      kbinsize=0.5
   end
            'nb_firstpass_map_1024_nocon': begin
      ;max_calibration_sources=1000
      undefine, diffuse_calibrate, diffuse_model
      calibration_catalog_file_path='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_decon_March2016_small/output_data/'+obs_id+'_source_array2.sav'
      saved_run_bp=0
      recalculate_all=1
      mapfn_recalculate=0
      dimension = 1024
            FoV=0
      kbinsize=0.5
      no_conjugate=1
   end
   
   
   'nb_April2016_newdiffuse': begin
      ;max_calibration_sources=1000
      diffuse_calibrate=filepath('EoR0_diffuse_March2016_small_through_firstpass_normfix_415.sav',root=rootdir('FHD'),subdir='catalog_data')
      calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
      saved_run_bp=0
   end

            'nb_decon_March2016_maxcal':begin 
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
      max_cal_baseline = 700.
      
      undefine, diffuse_calibrate, diffuse_model
      saved_run_bp=0
      ;double memory, time
   end
               'nb_decon_March2016_maxall':begin 
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
      max_cal_baseline = 700.
      max_baseline = 700.
      
      
      undefine, diffuse_calibrate, diffuse_model
      saved_run_bp=0
      ;double memory, time
   end
               'nb_decon_March2016_small_maxcal':begin 
      max_sources=200000
      calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
      dft_threshold=1
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
      conserve_memory=1
      ;time_cut=[2,-2]
      snapshot_healpix_export=1
      snapshot_recalculate=1
      recalculate_all=0
      max_cal_baseline = 700.
      
      undefine, diffuse_calibrate, diffuse_model
      saved_run_bp=0
      ;double memory, time
   end
                  'nb_decon_March2016_small_maxall':begin 
      max_sources=200000
      calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
      dft_threshold=1
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
      conserve_memory=1
      ;time_cut=[2,-2]
      snapshot_healpix_export=1
      snapshot_recalculate=1
      recalculate_all=0
      max_cal_baseline = 700.
      max_baseline = 700.
      
      undefine, diffuse_calibrate, diffuse_model
      saved_run_bp=0
      ;double memory, time
   end
   ;;; Patti's versions!!! Only Patti may edit this section!!!
   
   ; My default full deconvolution parameters
   'pac_full_fhd':begin
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
      uvfits_version = 5
      uvfits_subversion = 1
      saved_run_bp = 0
      calibration_catalog_file_path=filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')
      undefine, diffuse_calibrate, diffuse_model
   end
   
   'rlb_diffuse_survey_threeobs': begin ;;August 2016
      uvfits_version = 5
      uvfits_subversion = 1
      saved_run_bp = 0
      calibration_catalog_file_path=filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')

   end
   
   ;;;;;; Jon R's Stuff ;;;;;;;
    'jonr_barebones_aws':begin
     end

endcase

  
  if version EQ 'nb_pytest_after' then begin 
;if version EQ 'nb_whitening' then begin
  ;vis_file_list = '/nfs/mwa-03/r1/EoRuvfits/whitening_change/uvfits/'+strtrim(string(obs_id),2)+'.uvfits'
  ;vis_file_list = '/nfs/mwa-03/r1/EoRuvfits/pyuvfits_test/'+strtrim(string(obs_id),2)+'.uvfits'
  vis_file_list = '/nfs/mwa-03/r1/EoR2013/cotter_pyuvfits_test/'+strtrim(string(obs_id),2)+'.uvfits'
endif else begin
  SPAWN, 'read_uvfits_loc.py -v ' + STRING(uvfits_version) + ' -s ' + $
    STRING(uvfits_subversion) + ' -o ' + STRING(obs_id), vis_file_list
  ;vis_file_list=vis_file_list ; this is silly, but it's so var_bundle sees it.
endelse
undefine, uvfits_subversion, uvfits_version

fhd_file_list=fhd_path_setup(vis_file_list,version=version,output_directory=output_directory)
healpix_path=fhd_path_setup(output_dir=output_directory,subdir='Healpix',output_filename='Combined_obs',version=version)

extra=var_bundle() ; bundle all the variables into a structure

print,""
print,"Keywords set in wrapper:"
print,structure_to_text(extra)
print,""
general_obs,_Extra=extra

IF keyword_set(profile) THEN BEGIN
   PROFILER, FILENAME=STRING(profile_path), /REPORT;, /CODE_COVERAGE
ENDIF

end
