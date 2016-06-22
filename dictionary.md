# FHD Keyword Dictionary
FHD uses keywords to create unique run-specific settings. This dictionary describes the purpose of each keyword, as well as their logic or applicable ranges. Some keywords can override others, which is also documentated.  This is a work in progress; please add keywords as you find them in alphabetical order with their corresponding definition.

## Beam

*beam_offset_time*: calculate the beam at a specific time within the observation. An observation has 112 seconds, with 0 seconds indicating the start of the observation and 112 indicating the end of the observation.
  -**Default**:56
  -**Range**:0-112

## Calibration

*calibrate_visibilities*: turn on or turn off calibration of the visilibilities. If turned on, calibration of the dirty, modelling, and subtraction to make a residual occurs. Otherwise, none of these occur.
  -**Turn off/on**: 0/1
  -**Default**: 1 
*cable_bandpass_fit*: average the calibration solutions across tiles within a cable grouping.
  -**Dependency**: instrument_config/<instrument>_cable_length.txt
  -**Turn off/on**: 0/1
  -**Default**: 1
*saved_run_bp*
  -**Needs updating**: File name needs more information to descriminate between instruments and bands.
  -**Dependency**: instrument_config/<pointing number>_bandpass.txt
  -**Turn off/on**: 0/1
  -**Default**: 1
*diffuse_calibrate*
  -**Default**: filepath('EoR0_diffuse_model_94.sav',root=rootdir('FHD'),subdir='catalog_data')
*return_cal_visibilities*
  -**Default**: 1
*allow_sidelobe_cal_sources*
  -**Default**: 1

catalog_file_path=filepath('MRC_full_radio_catalog.fits',root=rootdir('FHD'),subdir='catalog_data')
calibration_catalog_file_path=filepath('mwa_calibration_source_list.sav',root=rootdir('FHD'),subdir='catalog_data')
bandpass_calibrate=1
calibration_polyfit=2
no_restrict_cal_sources=1

## Diffuse

## Model

## Recalculation

*recalculate_all*:
  -**Turn off/on**: 0/1
  -**Default**:0

mapfn_recalculate=0
healpix_recalculate=0

## Export

ps_export=0
split_ps_export=1
snapshot_healpix_export=1

cleanup=0
combine_healpix=0
deconvolve=0

flag_visibilities=0
vis_baseline_hist=1
silent=0
save_visibilities=1
calibration_visibilities_subtract=0

n_avg=2
ps_kbinsize=0.5
ps_kspan=600.
image_filter_fn='filter_uv_uniform'
deconvolution_filter='filter_uv_uniform'

uvfits_version=4
uvfits_subversion=1


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

allow_sidelobe_model_sources=1
