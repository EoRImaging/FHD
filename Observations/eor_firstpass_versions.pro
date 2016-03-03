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
;obs_id = '1061316176'
output_directory = args[1]
;output_directory = '/nfs/mwa-03/r1/EoR2013/'
version = args[2]
;version = 'nb_sim_model_StokesV_2'
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

case version of


   'nb_sim_unflagged_nodiffuse_onebeam_zenithpointing':begin 
      nfreq_avg=384
      no_frequency_flagging=1
      recalculate_all=1
      undefine, diffuse_calibrate, diffuse_model
      ;turn_off_visflagbasic=1
      unflag_all=1
   end 
     'nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit':begin 
      nfreq_avg=384
      no_frequency_flagging=1
      recalculate_all=1
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
      ;turn_off_visflagbasic=1
      unflag_all=1
      flag_calibration=0
      over_calibrate=1 
   end 
   
     'nb_sim_unflagged_nodiffuse_onebeam_zenithpointing_calvisflag_overfit_onesource':begin 
      nfreq_avg=384
      no_frequency_flagging=1
      recalculate_all=1
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
      ;turn_off_visflagbasic=1
      unflag_all=1
      calibration_auto_initialize=0
      flag_calibration=0
      over_calibrate=1 
      calibration_catalog_file_path=filepath('bright_source7.sav',root=rootdir('FHD'),subdir='catalog_data')
   end 
        'nb_sim_model_confusion_S5000':begin 
      nfreq_avg=384
      no_frequency_flagging=1
      undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct
      ;turn_off_visflagbasic=1
      unflag_all=1
      calibration_auto_initialize=0
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
   'nb_polarized_diffuse_Oct2015':begin
      diffuse_calibrate=filepath('diffuse_maps_polarized_Oct2015.sav',root=rootdir('FHD'),subdir='catalog_data')
      production=1
      ;no long tiles used in calculating saved bp
   end  
   
      'nb_decon_Jan2016':begin 
      max_sources=200000
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
      ;double memory, time
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

print,""
print,"Keywords set in wrapper:"
print,structure_to_text(extra)
print,""
general_obs,_Extra=extra

end
