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
output_directory = args[1]
version = args[2]
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

uvfits_version=4
uvfits_subversion=1

catalog_file_path=filepath('MRC_full_radio_catalog.fits',root=rootdir('FHD'),subdir='catalog_data')
calibration_catalog_file_path=filepath('mwa_calibration_sourcelist.sav',root=rootdir('FHD'),subdir='catalog_data')

dimension=2048
max_sources=20000
pad_uv_image=2.
FoV=0
no_ps=1
min_baseline=1.
min_cal_baseline=50.
ring_radius=10.*pad_uv_image
nfreq_avg=16
no_rephase=1
combine_obs=0
smooth_width=11.
bandpass_calibrate=1
calibration_polyfit=2
no_restrict_cal_sources=1
cal_cable_reflection_fit=150
restrict_hpx_inds=1

kbinsize=0.5
psf_resolution=100

; some new defaults (possibly temporary)
beam_model_version=0
dipole_mutual_coupling_factor=0
calibration_flag_iterate = 0

no_calibration_frequency_flagging=1

; even newer defaults
export_images=0
cal_cable_reflection_correct=150
model_catalog_file_path=filepath('mwa_calibration_sourcelist.sav',root=rootdir('FHD'),subdir='catalog_data')
model_visibilities=1
return_cal_visibilities=1
allow_sidelobe_cal_sources=0
allow_sidelobe_model_sources=1

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
   'nb_test_old_cotter_1': begin
      n_avg=1
   end

   ;;; Versions below this are with new defaults as of 23 May, 2014 !!!
   
   'nb_test_new_cotter_1': begin
      vis_freq_average=2
      uvfits_version=3
      uvfits_subversion=1
      time_cut=[2,-2]
   end
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

   ;;;; Nichole's versions!!! Only Nichole may edit this section!!!

   'nb_test_new_cotter_2': begin
      uvfits_version=3
      uvfits_subversion=1
      ;n_avg=4
      ;rerun without averaging for cubes
      n_avg=1
      snapshot_recalculate=1
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
      diffuse_model='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_apb_pp_deep_8/Healpix/diffuse_model.sav'
      model_visibilities=1
      calibration_visibilities_subtract=1
      return_cal_visibilities=0
      snapshot_healpix_export=0
      image_filter_fn='filter_uv_natural'
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

   ; Abraham's versions

   ; Bryna's versions
   'bjh_test_max_cal_iter':begin
      max_cal_iter=100
   end

   'bjh_test_max_cal_iter_no_cal_freq_flag':begin
      max_cal_iter=100
      no_calibration_frequency_flagging=1
   end

   ; Nichole's versions
   'nb_test_debug_direction':begin
      debug_direction=1
   end
   
   'nb_subtract_sidelobes_nocal':begin
      model_catalog_file_path=filepath('mwa_commissioning_source_list.sav',root=rootdir('FHD'),subdir='catalog_data')
      model_visibilities=1
      return_cal_visibilities=0
      allow_sidelobe_cal_sources=0
      allow_sidelobe_model_sources=1
   end

   'nb_test_transfercal_on_devel':begin
      model_catalog_file_path=filepath('mwa_commissioning_source_list.sav',root=rootdir('FHD'),subdir='catalog_data')
      model_visibilities=1
      transfer_calibration='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_apb_pp_deep_9/'
   end

   'nb_std_Oct2014':begin
      max_cal_iter=10
      cal_convergence_threshold=1E-3
      dipole_mutual_coupling=0
      beam_model=0
      calibration_flag_iterate=0
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


   else: print,'Default parameters'
endcase
   
SPAWN, 'read_uvfits_loc.py -v ' + STRING(uvfits_version) + ' -s ' + $
  STRING(uvfits_subversion) + ' -o ' + STRING(obs_id), vis_file_list
;vis_file_list=vis_file_list ; this is silly, but it's so var_bundle sees it.
undefine,uvfits_version ; don't need these passed further
undefine,uvfits_subversion
undefine,obs_id

fhd_file_list=fhd_path_setup(vis_file_list,version=version,output_directory=output_directory)
healpix_path=fhd_path_setup(output_dir=output_directory,subdir='Healpix',output_filename='Combined_obs',version=version)

extra=var_bundle() ; bundle all the variables into a structure

general_obs,_Extra=extra

end
