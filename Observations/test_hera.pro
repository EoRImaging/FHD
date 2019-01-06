PRO test_hera,_Extra=extra
except=!except
!except=0 
heap_gc

instrument = 'hera'

calibrate_visibilities=1
recalculate_all=0
export_images=1
cleanup=0
ps_export=0
version=''
image_filter_fn='filter_uv_uniform' ;applied ONLY to output images

data_directory=rootdir('mwa')+filepath('',root='DATA2',subdir=['HERA_DATA','testcal'])
vis_file_list=file_search(data_directory,'*.uvfits',count=n_files)
IF n_files EQ 0 THEN vis_file_list=file_search(data_directory,'*.uvfits.sav',count=n_files) ;compatibility with my laptop 
fhd_file_list=fhd_path_setup(vis_file_list,version=version,_Extra=extra)
healpix_path=fhd_path_setup(output_dir=data_directory,subdir='Healpix',output_filename='Combined_obs',version=version,_Extra=extra)
catalog_file_path=filepath('MRC_full_radio_catalog.fits',root=rootdir('FHD'),subdir='catalog_data')
calibration_catalog_file_path=filepath('mwa_calibration_source_list_gleam_kgs_fhd_fornax.sav',root=rootdir('FHD'),subdir='catalog_data')

combine_obs=0
dimension=2048.
max_sources=100000.
pad_uv_image=1.
IF dimension GE 2048 THEN pad_uv_image=1.
precess=0 ;set to 1 ONLY for X16 PXX scans (i.e. Drift_X16.pro)
FoV=!Radeg*2.
no_ps=1 ;don't save postscript copy of images
gain_factor=0.1
min_baseline=1.
min_cal_baseline=50.
no_fits=1
silent=0
smooth_width=32.
nfreq_avg=4.
ps_kbinsize=2.
ps_kspan=600.
split_ps=1
bandpass_calibrate=1
calibration_polyfit=1
cal_amp_degree_fit=2
cal_phase_degree_fit=1
save_vis=0
cal_mode_fit=1
cal_cable_reflection_correct=150.
recalculate_all=0
no_restrict_cal_sources=1
no_rephase=1
calibrate_visibilities=1
mark_zenith=1
psf_resolution=32.
beam_diff_image=1
beam_residual_threshold=0.1
output_residual_histogram=1
show_beam_contour=1
contour_level=[0,0.01,0.05,0.1,0.2,0.5,0.67,0.9]
contour_color='blue'

default_diffuse='D:\MWA\IDL_code\FHD\catalog_data\EoR0_polarized_diffuse_2.sav'
IF N_Elements(extra) GT 0 THEN IF Tag_exist(extra,'diffuse_calibrate') THEN IF extra.diffuse_calibrate EQ 1 THEN $
    extra=structure_update(extra,diffuse_calibrate=default_diffuse)
IF N_Elements(extra) GT 0 THEN IF Tag_exist(extra,'diffuse_model') THEN IF extra.diffuse_model EQ 1 THEN BEGIN
    extra=structure_update(extra,diffuse_model=default_diffuse)
    IF ~(Tag_exist(extra,'model_visibilities') OR (N_Elements(model_visibilities) GT 0)) THEN model_visibilities=1
ENDIF
undefine_fhd,default_diffuse
n_pol=2
restore_vis_savefile=0
firstpass=1
max_cal_iter=100L
beam_model_version=2
dft_threshold=1
init_healpix

cmd_args=extra
extra=var_bundle()
general_obs,_Extra=extra
!except=except
END