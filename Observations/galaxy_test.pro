PRO galaxy_test,cleanup=cleanup,ps_export=ps_export,recalculate_all=recalculate_all,export_images=export_images,version=version,$
    beam_recalculate=beam_recalculate,healpix_recalculate=healpix_recalculate,mapfn_recalculate=mapfn_recalculate,$
    grid=grid,deconvolve=deconvolve,channel=channel,data_version=data_version,calibrate_visibilities=calibrate_visibilities,_Extra=extra
except=!except
!except=0 
heap_gc

IF N_Elements(calibrate_visibilities) EQ 0 THEN calibrate_visibilities=0
IF N_Elements(recalculate_all) EQ 0 THEN recalculate_all=1
IF N_Elements(export_images) EQ 0 THEN export_images=1
IF N_Elements(cleanup) EQ 0 THEN cleanup=0
IF N_Elements(ps_export) EQ 0 THEN ps_export=0
IF N_Elements(version) EQ 0 THEN version=''
image_filter_fn='' ;applied ONLY to output images

data_directory=rootdir('mwa')+filepath('',root='DATA3',subdir=['128T','galaxy_test'])
vis_file_list=file_search(data_directory,'*.uvfits',count=n_files)
fhd_file_list=fhd_path_setup(vis_file_list,version=version,_Extra=extra)
healpix_path=fhd_path_setup(output_dir=data_directory,subdir='Healpix',output_filename='Combined_obs',version=version,_Extra=extra)
catalog_file_path=filepath('MRC_full_radio_catalog.fits',root=rootdir('FHD'),subdir='catalog_data')
calibration_catalog_file_path=filepath('mwa_galactic_center_catalog2.sav',root=rootdir('FHD'),subdir='catalog_data')

;calibration_catalog_file_path=filepath('mwa_commissioning_source_list.sav',root=rootdir('FHD'),subdir='catalog_data')

dimension=3072.
max_sources=200000.
pad_uv_image=2.
IF dimension GT 2048 THEN pad_uv_image=1.
FoV=80.
no_ps=1 ;don't save postscript copy of images
min_baseline=1.
min_cal_baseline=10.
ring_radius=10.*pad_uv_image
nfreq_avg=16
no_rephase=0 ;set to use obsra, obsdec for phase center even if phasera, phasedec present in a .metafits file
no_fits=0
combine_healpix=1
gain_factor=2./3.
smooth_width=11.

general_obs,cleanup=cleanup,ps_export=ps_export,recalculate_all=recalculate_all,export_images=export_images,version=version,$
    beam_recalculate=beam_recalculate,healpix_recalculate=healpix_recalculate,mapfn_recalculate=mapfn_recalculate,$
    grid=grid,deconvolve=deconvolve,image_filter_fn=image_filter_fn,data_directory=data_directory,$
    vis_file_list=vis_file_list,fhd_file_list=fhd_file_list,healpix_path=healpix_path,catalog_file_path=catalog_file_path,$
    dimension=dimension,max_sources=max_sources,pad_uv_image=pad_uv_image,precess=precess,$
    complex_beam=complex_beam,double_precison_beam=double_precison_beam,FoV=FoV,no_ps=no_ps,$
    min_baseline=min_baseline,calibrate_visibilities=calibrate_visibilities,nfreq_avg=nfreq_avg,$
    no_fits=no_fits,no_rephase=no_rephase,calibration_catalog_file_path=calibration_catalog_file_path,/mark_zenith,$
    show_obsname=1,silent=silent,smooth_width=smooth_width,gain_factor=gain_factor,combine_healpix=combine_healpix,$
    min_cal_baseline=min_cal_baseline,_Extra=extra
!except=except
END