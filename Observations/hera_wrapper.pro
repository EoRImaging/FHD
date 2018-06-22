pro hera_wrapper
except=!except
!except=0
heap_gc 

; wrapper to contain all the parameters for various runs we might do
; using firstpass.
; Adapted to run on Oscar (Brown)

; parse command line args
  compile_opt strictarr
  args = Command_Line_Args(count=nargs)
  IF keyword_set(args) then begin
    obs_id = args[0]
    print,obs_id
    output_directory = args[1]
    version = args[2]
  endif; else begin
  cmd_args={version:version}

; Set default values for everything
FoV=90.
dimension=2048
;degpix=.05
recalculate_all=0
deconvolution_over_resolution=1
vis_auto_model=0
vis_baseline_hist=0
calibration_visibilities_subtract=0
cleanup=1
ps_export=1 ; changed
pad_uv_image=1
split_ps_export=1
combine_healpix=0
mapfn_recalculate=0
healpix_recalculate=0
silent=0
save_visibilities=1
snapshot_healpix_export=0
;kbinsize=.3
;kspan=1000. ;820.
;ps_kbinsize=.8
;ps_kspan=500 ;820.
image_filter_fn='filter_uv_uniform'
export_images=1
instrument='hera'

uvfits_version=5
uvfits_subversion=1

catalog_file_path=filepath('mwa_calibration_source_list_gleam_kgs_fhd_fornax.sav',root=rootdir('FHD'),subdir='catalog_data')
calibration_catalog_file_path=filepath('mwa_calibration_source_list_gleam_kgs_fhd_fornax.sav',root=rootdir('FHD'),subdir='catalog_data')
model_catalog_file_path=filepath('mwa_calibration_source_list_gleam_kgs_fhd_fornax.sav',root=rootdir('FHD'),subdir='catalog_data')
; mwa_calibration_source_list_gleam_kgs_fhd_fornax.sav
;dimension=4096.
allow_sidelobe_cal_sources=1
allow_sidelobe_model_sources=1

beam_offset_time=0.;279.17 ;56 ; make this a default. But it won't compound with setting it directly in a version so I think it's ok.

;New defaults - July2015
;diffuse_calibrate=filepath('gsm_150MHz.sav',root=rootdir('FHD'),subdir='catalog_data')
;diffuse_filepath=filepath('gsm_150MHz.sav',root=rootdir('FHD'),subdir='catalog_data')
;diffuse_model=filepath('gsm_150MHz.sav',root=rootdir('FHD'),subdir='catalog_data')
;diffuse_model='/users/jkerriga/scratch/FHD/catalog_data/gsm_150MHz.sav'

cable_bandpass_fit=0
saved_run_bp=0		;Set to 1, because I don't have the highband saves (AEL 11/30/16)

;Defaults added - July2016
;cal_amp_degree_fit=2
;cal_phase_degree_fit=1

case version of
  'Diffuse_test': begin
;        unflag_all=0
        diffuse_model=filepath('gsm_150MHz.sav',root=rootdir('FHD'),subdir='catalog_data')
        verbose=1
        complex_beam=0 
        combine_obs=0
	max_calibration_sources=10000
        max_model_sources=10000
        max_sources=10000
        n_pol=2
        nfreq_avg=1
        freq_start=115. ;115. standard
        freq_end=185. ;185. standard
        ;;;;; Calibration fit params
        calibration_polyfit=1
        cal_amp_degree_fit=1
        cal_phase_degree_fit=1
        ;inverse=1 ;something to do with converting instr. pol to stokes
        ;time_cut=-590. ;cut the final 9 mins of obs
	dft_threshold=0
	no_ps=0
	bandpass_calibrate=1
        firstpass=0
        deconvolve=0
        no_extend=1
        ;no_condense_sources=1
        deconvolution_filter='filter_uv_uniform'
        ;;; New decon params
        ;deconvolution_horizon_threshold=0
        ;subtract_sidelobe_catalog=0
        gain_factor=.1
        ;scale_gain=0
        filter_background=0        
        ;;;
        calibrate_visibilities=1
        return_decon_visibilities=1
        diffuse_calibrate=1
        model_visibilities=1 ;set to 0 for decon/ 1 for firstpass
        cable_bandpass_fit=0
        ;settings for beam
;        beam_model_version=2
        ;beam_cal_threshold=0.01
;        beam_mask_threshold=10
;        beam_model_threshold=0.01
;        beam_threshold=0.01
        ;;; Try not allowing calibration stage to flag antennas
        flag_calibration=1
        flag_visibilities=0
        min_baseline=1.
        min_cal_baseline=1.
        calibration_auto_initialize=0
        calibration_auto_fit=0
        ;time_average=0
        tile_flag_list=['13','14','24','37','39','55','72','83','86','87','122','140','141','144'] ;diffuse test
        ;tile_flag_list=['11','24','41','50','82','87','122']
        ;tile_flag_list=['25','53','55','67','68','69','70','117','124','137','139','141','142','157']
        time_offset=0           ;279.172
        max_iter=100
     end
 'Decon': begin
        verbose=1
	max_calibration_sources=10000
        max_model_sources=10000
        max_sources=10000
        n_pol=2
        nfreq_avg=64
        freq_start=115. ;115. standard
        freq_end=185. ;185. standard
        calibration_polyfit=1
        cal_amp_degree_fit=2
        cal_phase_degree_fit=1
	dft_threshold=1
	no_ps=0
	bandpass_calibrate=1
        firstpass=0
        deconvolve=1
        gain_factor=.01
        filter_background=0        
        calibrate_visibilities=1
        return_decon_visibilities=1
        diffuse_calibrate=0
        model_visibilities=1 ;set to 0 for decon/ 1 for firstpass
        cable_bandpass_fit=0
        flag_calibration=1
        flag_visibilities=0
        min_baseline=1.
        min_cal_baseline=10.
        calibration_auto_initialize=0
        calibration_auto_fit=0
        tile_flag_list=['13','14','24','37','39','55','72','83','86','87','122','140','141','144']
        ;tile_flag_list=['24','55','69','72','83','122','143']
        time_offset=0           ;279.172
        max_iter=1000
        beam_threshold=0.05
        beam_max_threshold=1e-2
;        add_threshold=.001
;        reject_pol_sources=1
   end      
endcase
   

data_directory='/users/jkerriga/data/jkerriga/HERAFHD'
vis_file_list=file_search(data_directory,obs_id,count=n_files)
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
