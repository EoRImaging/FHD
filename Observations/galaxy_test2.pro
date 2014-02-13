PRO galaxy_test2,cleanup=cleanup,ps_export=ps_export,recalculate_all=recalculate_all,export_images=export_images,version=version,$
    channel=channel,_Extra=extra
except=!except
!except=0 
heap_gc

calibrate_visibilities=1
IF N_Elements(recalculate_all) EQ 0 THEN recalculate_all=1
IF N_Elements(export_images) EQ 0 THEN export_images=1
IF N_Elements(cleanup) EQ 0 THEN cleanup=0
IF N_Elements(ps_export) EQ 0 THEN ps_export=0
IF N_Elements(version) EQ 0 THEN version=''

data_directory=rootdir('mwa')+filepath('',root='DATA3',subdir=['128T','Galaxy_test2'])
vis_file_list=file_search(data_directory,'*.uvfits',count=n_files)
fhd_file_list=fhd_path_setup(vis_file_list,version=version,_Extra=extra)
healpix_path=fhd_path_setup(output_dir=data_directory,subdir='Healpix',output_filename='Combined_obs',version=version,_Extra=extra)
catalog_file_path=filepath('MRC_full_radio_catalog.fits',root=rootdir('FHD'),subdir='catalog_data')
calibration_catalog_file_path=filepath('mwa_calibration_source_list_nofornax.sav',root=rootdir('FHD'),subdir='catalog_data')
image_filter_fn='filter_uv_uniform'

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
no_fits=0
silent=0
smooth_width=11.
nfreq_avg=16.
ps_kbinsize=2.
ps_kspan=600.
split_ps=1
bandpass_calibrate=1
calibration_polyfit=2.
no_restrict_cal_sources=1
no_rephase=1
calibrate_visibilities=1
save_visibilities=0

general_obs,cleanup=cleanup,ps_export=ps_export,recalculate_all=recalculate_all,export_images=export_images,version=version,$
    image_filter_fn=image_filter_fn,data_directory=data_directory,$
    vis_file_list=vis_file_list,fhd_file_list=fhd_file_list,healpix_path=healpix_path,catalog_file_path=catalog_file_path,$
    dimension=dimension,max_sources=max_sources,pad_uv_image=pad_uv_image,precess=precess,$
    FoV=FoV,no_ps=no_ps,min_baseline=min_baseline,nfreq_avg=nfreq_avg,$
    no_fits=no_fits,no_rephase=no_rephase,calibration_catalog_file_path=calibration_catalog_file_path,$
    gain_factor=gain_factor,smooth_width=smooth_width,min_cal_baseline=min_cal_baseline,silent=silent,$
    combine_obs=combine_obs,calibrate_visibilities=calibrate_visibilities,$
    ps_kbinsize=ps_kbinsize,ps_kspan=ps_kspan,split_ps=split_ps,$
    bandpass_calibrate=bandpass_calibrate,calibration_polyfit=calibration_polyfit,$
    no_restrict_cal_sources=no_restrict_cal_sources,save_visibilities=save_visibilities,_Extra=extra
!except=except
END