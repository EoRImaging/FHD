pro wyl_eor_firstpass_versions
except=!except
!except=0
heap_gc 

; wrapper to contain all the parameters for various runs we might do
; using firstpass.

; parse command line args
compile_opt strictarr
args = Command_Line_Args(count=nargs)
obs_id = args[0]
;obs_id = '1061316296'
output_directory = args[1]
;output_directory = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/'
version = args[2]
;version = 'nb_autocal'
cmd_args={version:version}

;Options for using the IDL profiler
profile=0
profile_path='/dev/null'

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
cal_time_average=1


uvfits_version=4
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
decompose_auto=0
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
;debug_beam_clip_floor=1
;cal_cable_reflection_correct=150
cal_reflection_hyperresolve=150  ; set to zero to remove. corrects for reflections on 150m cables. DFT finds largest peak and removes
model_catalog_file_path=filepath('mwa_calibration_source_list.sav',root=rootdir('FHD'),subdir='catalog_data')
model_visibilities=0
return_cal_visibilities=1
allow_sidelobe_cal_sources=1
allow_sidelobe_model_sources=1
beam_offset_time=56 ; make this a default. But it won't compound with setting it directly in a version so I think it's ok.

;New defaults - July2015
diffuse_calibrate=filepath('EoR0_diffuse_model_94.sav',root=rootdir('FHD'),subdir='catalog_data')
cable_bandpass_fit=1  ; modify file in instrument_config directory with list of tiles and cable lengths (for hexes). turn off for sim
;saved_run_bp=1  ; set to zero if changing calibration (such as simulating hexes)
fill_model_visibilities = 1
cal_amp_degree_fit=2
cal_phase_degree_fit=1

case version of
    'MWA_PhaseII_FOA': begin
    calibrate_visibilities=0
    dimension=1024
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    end

    'incalphs2': begin
    ;dimension=1024
    max_calibration_sources=5000
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    ;calibrate_visibilities=0  
    ;debug_beam_clip_floor=1
    ;model_delay_filter=1
    unflag_all=1
    nfreq_avg=384
    saved_run_bp=0
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    end

    'FIL_SIM_Omni': begin
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    calibrate_visibilities=0 
    unflag_all=1
    nfreq_avg=384
    ;ps_tile_flag_list=['11','12','13','14','15','16','17','18','21','22','23','24','25','26','27','28','31','32','33','34','35','36','37','38','41','42','43','44','45','46','47','48','61','62','63','64','65','66','67','68','81','82','83','84','85','86','87','88','91','92','93','94','95','96','97','98']
 ;   saved_run_bp=0
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
    end

   'HEX_SIM_Incal': begin
    ;calibrate_visibilities=0 
    ;model_visibilities=1 
   flag_calibration=0
   unflag_all=1 
    ;return_cal_visibilities=0 
   nfreq_avg=384 
   bandpass_calibrate=0
   cable_bandpass_fit=0
   cal_reflection_hyperresolve=0
   cal_reflection_mode_theory=0
   undefine,cal_reflection_mode_theory
   calibration_polyfit=0
    ;remove_sim_flags=1 
   max_calibration_sources=5000
   model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')  
   calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
   ps_tile_flag_list=['11','12','13','14','15','16','17','18','21','22','23','24','25','26','27','28','31','32','33','34','35','36','37','38','41','42','43','44','45','46','47','48','61','62','63','64','65','66','67','68','81','82','83','84','85','86','87','88','91','92','93','94','95','96','97','98']
   end

   'FIL_SIM_Incal': begin
   ;in_situ_sim_input = '/users/wl42/data/wl42/FHD_out/fhd_SIM_Phase2' 
   ;remove_sim_flags=1
   max_calibration_sources=5000 
   flag_calibration=0
   unflag_all=1
   nfreq_avg=384
   model_delay_filter=1 
   bandpass_calibrate=0
   cable_bandpass_fit=0
   cal_reflection_hyperresolve=0
   cal_reflection_mode_theory=0
   undefine,cal_reflection_mode_theory
   calibration_polyfit=0
   model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
   ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_hash_removesimflags/vis_data/'
   calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
   ;ps_tile_flag_list=['11','12','13','14','15','16','17','18','21','22','23','24','25','26','27','28','31','32','33','34','35','36','37','38','41','42','43','44','45','46','47','48','61','62','63','64','65','66','67','68','81','82','83','84','85','86','87','88','91','92','93','94','95','96','97','98']
   end 

   'EoRsimPhase2': begin
   flag_calibration=0
   unflag_all=1
   nfreq_avg=384
   model_delay_filter=1
   bandpass_calibrate=0
   cable_bandpass_fit=0
   cal_reflection_hyperresolve=0
   cal_reflection_mode_theory=0
   undefine,cal_reflection_mode_theory
   calibration_polyfit=0
   model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
   ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_hash_removesimflags/vis_data/'
   calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
   end

   'RecoverEoR': begin
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    flag_calibration=0      
    unflag_all=1
    nfreq_avg=384
    model_delay_filter=1
    bandpass_calibrate=0
    cable_bandpass_fit=0
    cal_reflection_hyperresolve=0
    cal_reflection_mode_theory=0
    undefine,cal_reflection_mode_theory
    calibration_polyfit=0
    remove_sim_flags=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
   end

   'wavextentdef': begin
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    calibrate_visibilities=0
    unflag_all=1
    nfreq_avg=384 
    remove_sim_flags=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
   end

   'wavextent200': begin
    ps_kspan=200
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    calibrate_visibilities=0
    unflag_all=1
    nfreq_avg=384
    remove_sim_flags=1
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
   end

   'cable_ave_cal': begin
    debug_beam_clip_floor=1
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    ps_kspan=200
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
   end

   'cable_aut_cal': begin
    decompose_auto=1
    debug_beam_clip_floor=1
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    ps_kspan=200
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
   end

   'test_autocal': begin
    decompose_auto=1
    calibrate_visibilities=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    ps_kspan=200
   end

   'Calibration_PhaseII': begin
    decompose_auto=1
    diffuse_calibrate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    cal_stop=1
   end

   'Afterauto2': begin
    diffuse_calibrate=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    cal_stop=1
   end

   'int_PhaseII': begin
    calibrate_visibilities=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    dimension=1024
    ps_kspan=200
    ;restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
   end

   'skycal_PhaseII': begin
    calibrate_visibilities=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    dimension=1024
    ps_kspan=200
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
   end

   'redcal_PhaseII': begin
    calibrate_visibilities=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    dimension=1024
    ps_kspan=200
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
   end

   'uvw2': begin
    calibrate_visibilities=0
    debug_beam_clip_floor=1
   end

   'cath_PhaseII': begin
    calibrate_visibilities=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    dimension=1024
    ps_kspan=200
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
   end

   'test_PhaseII': begin
    calibrate_visibilities=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    dimension=1024
    ps_kspan=200
    end

   'model_PhaseII': begin
    calibrate_visibilities=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    dimension=1024
    ps_kspan=200
    restrict_hpx_inds='EoR0_high_healpix_inds_3x.idlsave'
   end

   'vis_sim_gen': begin
   flag_calibration=0
   dimension=2048
   ;debug_beam_clip_floor=1
   unflag_all=1
   nfreq_avg=192
   ;model_delay_filter=1
   bandpass_calibrate=0
   cable_bandpass_fit=0
   cal_reflection_hyperresolve=0
   cal_reflection_mode_theory=0
   undefine,cal_reflection_mode_theory
   calibration_polyfit=0
   cal_stop=1
   model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
   ;eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_hash_removesimflags/vis_data/'
   calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
   end

  'ps_sim_hex72': begin
    calibrate_visibilities=0
    ;debug_beam_clip_floor=1
    ;model_delay_filter=1
    ps_kspan=200
    nfreq_avg=384
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    dimension=1024
  end

  'pos_sim_gen': begin
    diffuse_calibrate=0
    ;debug_beam_clip_floor=1
    ;model_delay_filter=1
    dimension=1024
    nfreq_avg=384
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    cal_stop=1
  end

  'image_sim': begin
    calibrate_visibilities=0
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data') 
    dimension=1024
    nfreq_avg=384
  end

   'df_PhaseII': begin
    calibrate_visibilities=0
    debug_beam_clip_floor=1
    model_delay_filter=1
    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    model_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data')
    dimension=1024
    ps_kspan=200
    restrict_hpx_inds='EoR0_high_healpix_inds_325.idlsave'
   end

endcase

;old version here -- for MIT
;SPAWN, 'read_uvfits_loc.py -v ' + STRING(uvfits_version) + ' -s ' + $
;  STRING(uvfits_subversion) + ' -o ' + STRING(obs_id), vis_file_list

; On Oscar
SPAWN, 'locate_uvfits_oscar.py -o ' + STRING(obs_id), vis_file_list


IF (profile eq 1) THEN BEGIN
   RESOLVE_ROUTINE, 'general_obs',/QUIET	; Profiler only looks at compiled modules...
   RESOLVE_ROUTINE, 'slurm_ps_job', /QUIET
   RESOLVE_ALL,/CONTINUE_ON_ERROR,/QUIET
   PROFILER
;   PROFILER, /SYSTEM
ENDIF

;vis_file_list=vis_file_list ; this is silly, but it's so var_bundle sees it.
;data_directory= '/users/alanman/data/alanman'
;vis_file_list=[data_directory+'/1061311664.uvfits']
;print, vis_file_list[0]

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

IF (profile eq 1) THEN BEGIN
   PROFILER, FILENAME=STRING(profile_path), /REPORT, /CODE_COVERAGE
ENDIF

end
