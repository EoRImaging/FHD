pro PRISim_to_FHD
except=!except
!except=0
heap_gc

; wrapper to contain all the parameters for various runs we might do
; using firstpass.

; parse command line args
  compile_opt strictarr
  args = Command_Line_Args(count=nargs)
  IF keyword_set(args) then begin
    output_directory = args[0]
    version = args[1]
    vis_file_list = args[2]
  endif else begin
     output_directory = '/nfs/mwa-09/r1/djc/EoR2013/Aug23/'
     version = 'nb_rts_amp_cal_fit'
  endelse
  cmd_args={version:version}

; Set default values for everything
instrument='hera'
calibrate_visibilities=0
recalculate_all=0
cleanup=0
split_ps_export=1
deconvolve=0
healpix_recalculate=0
flag_visibilities=0
flag_calibration=0
vis_baseline_hist=1
silent=0
snapshot_healpix_export=1
residual_flag=0
model_flag=0
dirty_flag=0
n_avg=1
ps_kbinsize=.5
ps_kspan=600.
kspan=600.
degpix=.1
image_filter_fn='filter_uv_uniform'
deconvolution_filter='filter_uv_uniform'

; here are some power spectrum settings to help hera sims
hpx_radius=10
nside=1024

; uvfits_version=5 ;updated by RB, 12/16
; uvfits_subversion=1

catalog_file_path=filepath('MRC_full_radio_catalog.fits',root=rootdir('FHD'),subdir='catalog_data')
; calibration_catalog_file_path=filepath('mwa_calibration_source_list.sav',root=rootdir('FHD'),subdir='catalog_data')

dimension=0
max_sources=20000
pad_uv_image=1.
; FoV=0
no_ps=1
min_baseline=0.
min_cal_baseline=7
ring_radius=10.*pad_uv_image
nfreq_avg=32
no_rephase=1
combine_obs=0
smooth_width=32.
restrict_hpx_inds=0
bandpass_calibrate=1
calibration_polyfit=2
no_restrict_cal_sources=1
; cal_cable_reflection_fit=150
kbinsize=.5
psf_resolution=100

; some new defaults (possibly temporary)
dipole_mutual_coupling_factor=0
calibration_flag_iterate = 0
no_calibration_frequency_flagging=1

; even newer defaults
export_images=1
model_visibilities=0
return_cal_visibilities=0
cal_amp_degree_fit=2
cal_phase_degree_fit=1

time_offset=330
beam_offset_time=330 ; make this a default. But it won't compound with setting it directly in a version so I think it's ok.

fhd_file_list=fhd_path_setup(vis_file_list,version=version,output_directory=output_directory)
healpix_path=fhd_path_setup(output_dir=output_directory,subdir='Healpix',output_filename='Combined_obs',version=version)

extra=var_bundle() ; bundle all the variables into a structure

print,""
print,"Keywords set in wrapper:"
print,structure_to_text(extra)
print,""
general_obs,_Extra=extra

end
