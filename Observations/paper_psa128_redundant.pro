PRO paper_psa128_redundant,recalculate_all=recalculate_all,export_images=export_images,version=version,_Extra=extra
except=!except
!except=0 
heap_gc

IF N_Elements(recalculate_all) EQ 0 THEN recalculate_all=1
IF N_Elements(export_images) EQ 0 THEN export_images=1
IF N_Elements(cleanup) EQ 0 THEN cleanup=0
IF N_Elements(ps_export) EQ 0 THEN ps_export=0
IF N_Elements(version) EQ 0 THEN version=0
image_filter_fn='filter_uv_uniform' ;applied ONLY to output images

data_directory=rootdir('mwa')+filepath('',root='PAPER_DATA',subdir=['psa128_redundant'])
vis_file_list=file_search(data_directory,'*.uvfits',count=n_files)
fhd_file_list=fhd_path_setup(vis_file_list,version=version)

healpix_path=fhd_path_setup(output_dir=data_directory,subdir='Healpix',output_filename='Combined_obs',version=version)
catalog_file_path=filepath('MRC_full_radio_catalog.fits',root=rootdir('FHD'),subdir='catalog_data')
calibration_catalog_file_path=filepath('mwa_commissioning_source_list_add_FHDaug23deconvolve_fornax_and_VLA_pic.sav',root=rootdir('FHD'),subdir='catalog_data')
model_catalog_file_path=filepath('mwa_commissioning_source_list_add_FHDaug23deconvolve_fornax_and_VLA_pic.sav',root=rootdir('FHD'),subdir='catalog_data')

FoV=160.
dimension=1024.

max_sources=15000.
pad_uv_image=1.
precess=1 ;At least the new uvfits files from Jonnie need to be precessed to J2000
instrument='paper'
lat=Ten(-30,42,17.5)
lon=Ten(21,25,41)
n_pol=2
;mirror_X=1
;independent_fit=1 ;not sure of polarization calibration for now!
time_offset=5.*60. ;time offset of phase center from start time. PAPER data are phased to 5 minutes after the start time. 
no_ps=1
no_complex_beam=1
nfreq_avg=1.

calibration_flag_iterate=0
gain_factor=0.15
min_baseline=1.
min_cal_baseline=20.
no_fits=1
silent=0
smooth_width=11.
bandpass_calibrate=0
calibration_polyfit=0.
no_restrict_cal_sources=1
no_rephase=1
calibrate_visibilities=1
save_visibilities=0
reorder_visibilities=1
freq_start=124.
freq_end=174.
beam_model_version=2
beam_cal_threshold=0.5

beam_diff_image=1
beam_residual_threshold=0.1
output_residual_histogram=1
show_beam_contour=1
contour_level=[0,0.01,0.05,0.1,0.2,0.5,0.67,0.9]
contour_color='blue'

n_pol=2
firstpass=1
max_cal_iter=10000L
cal_convergence_threshold=1E-4
phase_fit_iter=10L
cal_gain_init=1E-2
tile_flag_list=['14','25','27','38','51','52','108','113','114','115','116','117','118','119','120','121','122','124','125','126','127','128']

IF N_Elements(extra) GT 0 THEN cmd_args=extra

extra=var_bundle()
general_obs,_Extra=extra

!except=except
END