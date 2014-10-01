PRO testcal_128T,_Extra=extra
except=!except
!except=0 
heap_gc

calibrate_visibilities=1
recalculate_all=0
export_images=1
cleanup=0
ps_export=0
version=''
image_filter_fn='filter_uv_uniform' ;applied ONLY to output images

IF N_Elements(data_version) EQ 0 THEN data_version='3'
data_directory=rootdir('mwa')+filepath('',root='DATA3',subdir=['128T','testcal'+data_version])
vis_file_list=file_search(data_directory,'*.uvfits.sav',count=n_files)
fhd_file_list=fhd_path_setup(vis_file_list,version=version,_Extra=extra)
healpix_path=fhd_path_setup(output_dir=data_directory,subdir='Healpix',output_filename='Combined_obs',version=version,_Extra=extra)
catalog_file_path=filepath('MRC_full_radio_catalog.fits',root=rootdir('FHD'),subdir='catalog_data')
calibration_catalog_file_path=filepath('mwa_commissioning_source_list.sav',root=rootdir('FHD'),subdir='catalog_data')
;calibration_catalog_file_path=filepath('mwa_EOR0_source_list_v0.sav',root=rootdir('FHD'),subdir='catalog_data')
;calibration_catalog_file_path=filepath('eor01_calibration_source_list.sav',root=rootdir('FHD'),subdir='catalog_data')

;noise_calibrate=0
;align=0
combine_obs=0
dimension=2048.
max_sources=30000.
pad_uv_image=1.
IF dimension GT 2048 THEN pad_uv_image=1.
precess=0 ;set to 1 ONLY for X16 PXX scans (i.e. Drift_X16.pro)
FoV=80.
no_ps=1 ;don't save postscript copy of images
gain_factor=0.2
min_baseline=1.
min_cal_baseline=50.
no_fits=1
silent=0
smooth_width=11.
nfreq_avg=4.
ps_kbinsize=2.
ps_kspan=600.
split_ps=1
bandpass_calibrate=1
calibration_polyfit=2.
save_vis=0
cal_mode_fit=1
cal_cable_reflection_fit=150.
recalculate_all=0
no_restrict_cal_sources=1
no_rephase=1
calibrate_visibilities=1
mark_zenith=1
psf_resolution=32.
beam_diff_image=1

cmd_args=extra
extra=var_bundle()
general_obs,_Extra=extra
!except=except
END