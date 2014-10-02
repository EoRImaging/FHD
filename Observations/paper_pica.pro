PRO paper_pica,recalculate_all=recalculate_all,export_images=export_images,version=version,_Extra=extra
except=!except
!except=0 
heap_gc

IF N_Elements(recalculate_all) EQ 0 THEN recalculate_all=1
IF N_Elements(export_images) EQ 0 THEN export_images=1
IF N_Elements(cleanup) EQ 0 THEN cleanup=0
IF N_Elements(ps_export) EQ 0 THEN ps_export=0
IF N_Elements(version) EQ 0 THEN version=0
image_filter_fn='filter_uv_uniform' ;applied ONLY to output images

;data_directory=rootdir('mwa')+filepath('',root='PAPER_DATA',subdir=['psa32'])
;data_directory='/data2/PAPER/psa32/'
IF StrLowCase(!version.os_family) EQ 'unix' THEN data_directory='/data2/PAPER/PicA/' $
    ELSE data_directory=rootdir('mwa')+filepath('',root='PAPER_DATA',subdir=['PicA'])
vis_file_list=file_search(data_directory,'*.uvfits',count=n_files)
fhd_file_list=fhd_path_setup(vis_file_list,version=version)

healpix_path=fhd_path_setup(output_dir=data_directory,subdir='Healpix',output_filename='Combined_obs',version=version)
catalog_file_path=filepath('MRC_full_radio_catalog.fits',root=rootdir('FHD'),subdir='catalog_data')
calibration_catalog_file_path=filepath('mwa_calibration_source_list_nofornax.sav',root=rootdir('FHD'),subdir='catalog_data')

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

gain_factor=0.15
min_baseline=1.
min_cal_baseline=50.
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

cmd_args=extra

extra=var_bundle()
general_obs,_Extra=extra

!except=except
END