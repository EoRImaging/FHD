PRO eor_firstpass,cleanup=cleanup,ps_export=ps_export,recalculate_all=recalculate_all,export_images=export_images,version=version,$
    beam_recalculate=beam_recalculate,healpix_recalculate=healpix_recalculate,mapfn_recalculate=mapfn_recalculate,$
    grid=grid,deconvolve=deconvolve,channel=channel,output_directory=output_directory,save_visibilities=save_visibilities,$
    julian_day=julian_day,uvfits_version=uvfits_version,uvfits_subversion=uvfits_subversion,vis_baseline_hist=vis_baseline_hist,$
    silent=silent,combine_healpix=combine_healpix,split_ps_export=split_ps_export,return_cal_visibilities=return_cal_visibilities,$
    snapshot_healpix_export=snapshot_healpix_export,n_avg=n_avg,ps_kbinsize=ps_kbinsize,ps_kspan=ps_kspan,_Extra=extra
except=!except
!except=0 
heap_gc
;wrapper designed to generate decent images as quickly as possible

calibrate_visibilities=1
IF N_Elements(recalculate_all) EQ 0 THEN recalculate_all=0
IF N_Elements(export_images) EQ 0 THEN export_images=1
IF N_Elements(cleanup) EQ 0 THEN cleanup=0
IF N_Elements(ps_export) EQ 0 THEN ps_export=0
IF N_Elements(split_ps_export) EQ 0 THEN split_ps_export=1
IF N_Elements(combine_healpix) EQ 0 THEN combine_healpix=0
IF N_Elements(version) EQ 0 THEN version=1
IF N_Elements(deconvolve) EQ 0 THEN deconvolve=0
IF N_Elements(mapfn_recalculate) THEN mapfn_recalculate=0
IF N_Elements(healpix_recalculate) EQ 0 THEN healpix_recalculate=0
IF N_Elements(flag_visibilities) EQ 0 THEN flag_visibilities=1
IF N_Elements(julian_day) EQ 0 THEN julian_day=2456528
IF N_Elements(uvfits_version) EQ 0 THEN uvfits_version=2
IF N_Elements(uvfits_subversion) EQ 0 THEN uvfits_subversion=0
IF N_Elements(vis_baseline_hist) EQ 0 THEN vis_baseline_hist=1
IF N_Elements(silent) EQ 0 THEN silent=0
IF N_Elements(save_visibilities) EQ 0 THEN save_visibilities=1
IF N_Elements(calibration_visibilities_subtract) EQ 0 THEN calibration_visibilities_subtract=0
IF N_Elements(return_cal_visibilities) EQ 0 THEN return_cal_visibilities=1
IF N_Elements(snapshot_healpix_export) EQ 0 THEN snapshot_healpix_export=1
IF N_Elements(n_avg) EQ 0 THEN n_avg=2
IF N_Elements(ps_kbinsize) EQ 0 THEN ps_kbinsize=3.
IF N_Elements(ps_kspan) EQ 0 THEN ps_kspan=600.

if n_elements(output_directory) eq 0 then output_directory='/nfs/mwa-09/r1/djc/EoR2013/Aug23/'
image_filter_fn='filter_uv_uniform' ;applied ONLY to output images
;image_filter_fn=''

; This file structure works at MIT
data_directory='/nfs/mwa-09/r1/EoRuvfits/jd'+strtrim(julian_day,2)+'v'+strtrim(uvfits_version,2)+'_'+strtrim(uvfits_subversion,2)


vis_file_list=file_search(data_directory,'*.uvfits',count=n_files)
fhd_file_list=fhd_path_setup(vis_file_list,version=version,output_directory=output_directory,_Extra=extra)
healpix_path=fhd_path_setup(output_dir=output_directory,subdir='Healpix',output_filename='Combined_obs',version=version,_Extra=extra)
catalog_file_path=filepath('MRC_full_radio_catalog.fits',root=rootdir('FHD'),subdir='catalog_data')
;calibration_catalog_file_path=filepath('mwa_calibration_source_list.sav',root=rootdir('FHD'),subdir='catalog_data')
;calibration_catalog_file_path=filepath('eor1_calibration_source_list.sav',root=rootdir('FHD'),subdir='catalog_data')
;calibration_catalog_file_path=filepath('mwa_calibration_source_list_nofornax.sav',root=rootdir('FHD'),subdir='catalog_data')
;calibration_catalog_file_path=filepath('eor01_calibration_source_list.sav',root=rootdir('FHD'),subdir='catalog_data')
calibration_catalog_file_path=filepath('mwa_commissioning_source_list.sav',root=rootdir('FHD'),subdir='catalog_data')
;calibration_catalog_file_path=filepath('test_component_catalog.sav',root=rootdir('FHD'),subdir='catalog_data')

dimension=2048.
max_sources=20000.
pad_uv_image=2.
FoV=80.
no_ps=1 ;don't save postscript copy of images
psf_dim=8
min_baseline=1.
min_cal_baseline=50.
ring_radius=10.*pad_uv_image
nfreq_avg=16
;max_calibration_sources=10000.
weights_grid=0
psf_resolution=8.
no_rephase=1 ;set to use obsra, obsdec for phase center even if phasera, phasedec present in a .metafits file
no_fits=1
combine_obs=0
gain_factor=2./3.
smooth_width=11.
bandpass_calibrate=1
calibration_polyfit=2
no_restrict_cal_sources=1


general_obs,cleanup=cleanup,ps_export=ps_export,recalculate_all=recalculate_all,export_images=export_images,version=version,$
    beam_recalculate=beam_recalculate,healpix_recalculate=healpix_recalculate,mapfn_recalculate=mapfn_recalculate,$
    grid=grid,deconvolve=deconvolve,image_filter_fn=image_filter_fn,data_directory=data_directory,output_directory=output_directory,$
    vis_file_list=vis_file_list,fhd_file_list=fhd_file_list,healpix_path=healpix_path,catalog_file_path=catalog_file_path,$
    dimension=dimension,max_sources=max_sources,pad_uv_image=pad_uv_image,psf_dim=psf_dim,$
    FoV=FoV,no_ps=no_ps,min_baseline=min_baseline,flag_visibilities=flag_visibilities,flag_calibration=flag_calibration,$
    calibrate_visibilities=calibrate_visibilities,calibration_catalog_file_path=calibration_catalog_file_path,$
    ring_radius=ring_radius,flag_nsigma=flag_nsigma,nfreq_avg=nfreq_avg,combine_healpix=combine_healpix,$
    calibration_visibilities_subtract=calibration_visibilities_subtract,weights_grid=weights_grid,/mark_zenith,$
    vis_baseline_hist=vis_baseline_hist,psf_resolution=psf_resolution,no_rephase=no_rephase,show_obsname=show_obsname,$
    silent=silent,smooth_width=smooth_width,gain_factor=gain_factor,combine_obs=combine_obs,no_fits=no_fits,snapshot_healpix_export=snapshot_healpix_export,$
    min_cal_baseline=min_cal_baseline,save_visibilities=save_visibilities,split_ps_export=split_ps_export,return_cal_visibilities=return_cal_visibilities,$
    bandpass_calibrate=bandpass_calibrate,calibration_polyfit=calibration_polyfit,no_restrict_cal_sources=no_restrict_cal_sources,$
    n_avg=n_avg,ps_kbinsize=ps_kbinsize,ps_kspan=ps_kspan,_Extra=extra
!except=except
END
