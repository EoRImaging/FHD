PRO make_eor_cubes,cleanup=cleanup,recalculate_all=recalculate_all,version=version,$
    beam_recalculate=beam_recalculate,healpix_recalculate=healpix_recalculate,mapfn_recalculate=mapfn_recalculate,$
    grid=grid,output_directory=output_directory,$
    julian_day=julian_day,uvfits_version=uvfits_version,uvfits_subversion=uvfits_subversion,vis_baseline_hist=vis_baseline_hist,$
    silent=silent,n_avg=n_avg,ps_kbinsize=ps_kbinsize,ps_kspan=ps_kspan,_Extra=extra
; wrapper to just make eor cubes. Nothing else. This wrapper assumes the data has already run through some form of FHD, and you just want the cubes.
  
  except=!except
  !except=0 
  heap_gc
  

  
calibration_visibilities_subtract=0
calibrate_visibilities=0
IF N_Elements(recalculate_all) EQ 0 THEN recalculate_all=0
IF N_Elements(beam_recalculate) EQ 0 THEN beam_recalculate=0
export_images=0
IF N_Elements(cleanup) EQ 0 THEN cleanup=0
ps_export=1
split_ps_export=1
IF N_Elements(version) EQ 0 THEN version=1
deconvolve=0
mapfn_recalculate=0
IF N_Elements(healpix_recalculate) EQ 0 THEN healpix_recalculate=0
IF N_Elements(flag_visibilities) EQ 0 THEN flag_visibilities=0
IF N_Elements(julian_day) EQ 0 THEN julian_day=2456528
IF N_Elements(uvfits_version) EQ 0 THEN uvfits_version=2
IF N_Elements(uvfits_subversion) EQ 0 THEN uvfits_subversion=0
IF N_Elements(vis_baseline_hist) EQ 0 THEN vis_baseline_hist=0
IF N_Elements(silent) EQ 0 THEN silent=0
; ps specific stuff
IF N_Elements(n_avg) EQ 0 THEN n_avg=2
  IF N_Elements(ps_kbinsize) EQ 0 THEN ps_kbinsize=3.
  IF N_Elements(ps_kspan) EQ 0 THEN ps_kspan=600.

; This file structure works at MIT
data_directory='/nfs/mwa-09/r1/EoRuvfits/jd'+strtrim(julian_day,2)+'v'+strtrim(uvfits_version,2)+'_'+strtrim(uvfits_subversion,2)
output_directory='/nfs/mwa-09/r1/djc/EoR2013/Aug23/'

vis_file_list=file_search(data_directory,'*.uvfits',count=n_files)
fhd_file_list=fhd_path_setup(vis_file_list,version=version,output_directory=output_directory,_Extra=extra)
healpix_path=fhd_path_setup(output_dir=output_directory,subdir='Healpix',output_filename='Combined_obs',version=version,_Extra=extra)
catalog_file_path=filepath('MRC_full_radio_catalog.fits',root=rootdir('FHD'),subdir='catalog_data')
calibration_catalog_file_path=filepath('mwa_commissioning_source_list.sav',root=rootdir('FHD'),subdir='catalog_data')

dimension=3072.
max_sources=20000.
pad_uv_image=1.
FoV=80.
no_ps=1 ;don't save postscript copy of images
psf_dim=8
min_baseline=1.
min_cal_baseline=50.
ring_radius=10.*pad_uv_image
nfreq_avg=16
max_calibration_sources=10000.
weights_grid=0
psf_resolution=8.
no_rephase=1 ;set to use obsra, obsdec for phase center even if phasera, phasedec present in a .metafits file
no_fits=1
combine_obs=1
gain_factor=2./3.
smooth_width=11.

general_obs,cleanup=cleanup,ps_export=ps_export,split_ps_export=split_ps_export,recalculate_all=recalculate_all,export_images=export_images,version=version,$
    beam_recalculate=beam_recalculate,healpix_recalculate=healpix_recalculate,mapfn_recalculate=mapfn_recalculate,$
    grid=grid,deconvolve=deconvolve,image_filter_fn=image_filter_fn,data_directory=data_directory,output_directory=output_directory,$
    vis_file_list=vis_file_list,fhd_file_list=fhd_file_list,healpix_path=healpix_path,catalog_file_path=catalog_file_path,$
    dimension=dimension,max_sources=max_sources,pad_uv_image=pad_uv_image,psf_dim=psf_dim,$
    FoV=FoV,no_ps=no_ps,min_baseline=min_baseline,flag_visibilities=flag_visibilities,flag_calibration=flag_calibration,$
    calibrate_visibilities=calibrate_visibilities,calibration_catalog_file_path=calibration_catalog_file_path,$
    ring_radius=ring_radius,flag_nsigma=flag_nsigma,nfreq_avg=nfreq_avg,max_calibration_sources=max_calibration_sources,$
    calibration_visibilities_subtract=calibration_visibilities_subtract,weights_grid=weights_grid,/mark_zenith,$
    vis_baseline_hist=vis_baseline_hist,psf_resolution=psf_resolution,no_rephase=no_rephase,show_obsname=show_obsname,$
    silent=silent,smooth_width=smooth_width,gain_factor=gain_factor,combine_obs=combine_obs,no_fits=no_fits,$
    min_cal_baseline=min_cal_baseline,n_avg=n_avg,ps_kspan=ps_kspan,ps_kbinsize=ps_kbinsize,_Extra=extra
!except=except
END
  