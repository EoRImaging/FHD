pro eor_firstpass_versions
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
    ;obs_id = '1061316296'
    output_directory = args[1]
    ;output_directory = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/'
    version = args[2]
  ;version = 'nb_temp'
  endif else begin
     obs_id = '1061667176'
     output_directory = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/'
     version = 'nb_decon_July2016_presidelobe_Aug27'
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
  'ael_test_0': begin
	max_model_sources=1000
	dft_threshold=1
	no_ps=0
  end
endcase
   
if version EQ 'nb_pytest' then begin
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

end
