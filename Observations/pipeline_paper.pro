PRO pipeline_paper,subset=subset,all=all,single=single,version=version

;;;;;;
; A simple wrapper to organize the commands being used to generate
; images and cubes for PS for the pipeline paper (Jacobs et. al)
;
; start with full list of observations: start_0 through end_0
; Divide into groups: start_1... end_1, start_2... end_2, etc.
;
; First run: pipeline_paper,subset=i
;   for i=1,...N to run firstpass sets in parallel
; Then: pipeline_paper,/all
;   to create integrated healpix images and integrated cubes for PS
; Then: pipeline_paper,/single
;   to deconvolve and make images on a single snapshot


except=!except
!except=0 
heap_gc

calibrate_visibilities=1
flag_visibilities=1
cleanup=0
silent=0
IF N_Elements(save_visibilities) EQ 0 THEN save_visibilities=1 

if keyword_set(subset) then begin
  ; running a subset of observation in parallel to go faster
  if n_elements(version) eq 0 then version='apb_pipeline_paper_deep_1'
  start_arr=[50,66,81,96,111,126]
  end_arr=[65,80,95,110,125,143]
  start_fi=start_arr(subset-1)
  end_fi=end_arr(subset-1)
  deconvolve=0
  export_images=1
  ps_export=0
  split_ps_export=0
  combine_healpix=0
  calibration_visibilities_subtract=1
  snapshot_healpix_export=1
  return_decon_visibilities=1
endif else if keyword_set(all) then begin
  ; re-running with all observations to generate healpix images and cubes
  start_fi=50
  end_fi=143
  if n_elements(version) eq 0 then version='apb_pipeline_paper_deep_1'
  recalculate_all=0
  deconvolve=0
  force_no_data=1
  export_images=0
  ps_export=1
  split_ps_export=1
  combine_healpix=1
  n_avg=2
  ps_kbinsize=3.
  ps_kspan=600.
endif else if keyword_set(single) then begin
  start_fi=88
  end_fi=88
  if n_elements(version) eq 0 then version='apb_pipeline_paper_snapshot_1'
  recalculate_all=1
  deconvolve=1
  export_images=1
  ps_export=1
  split_ps_export=1
  combine_healpix=0
  n_avg=2
  ps_kbinsize=3.
  ps_kspan=600.
endif



image_filter_fn='filter_uv_uniform' ;applied ONLY to output images

data_directory='/nfs/mwa-09/r1/EoRuvfits/jd2456528v2_0'
output_directory='/nfs/mwa-09/r1/djc/EoR2013/Aug23/'
vis_file_list=file_search(data_directory,'*.uvfits',count=n_files)
fhd_file_list=fhd_path_setup(vis_file_list,version=version,output_directory=output_directory,_Extra=extra)
healpix_path=fhd_path_setup(output_dir=output_directory,subdir='Healpix',output_filename='Combined_obs',version=version,_Extra=extra)
catalog_file_path=filepath('MRC_full_radio_catalog.fits',root=rootdir('FHD'),subdir='catalog_data')
calibration_catalog_file_path=filepath('mwa_commissioning_source_list.sav',root=rootdir('FHD'),subdir='catalog_data')

;noise_calibrate=0
;align=0
dimension=2048.
;max_baseline=900.
max_sources=25000.
pad_uv_image=2.
precess=0 ;set to 1 ONLY for X16 PXX scans (i.e. Drift_X16.pro)
FoV=80.
no_ps=1 ;don't save postscript copy of images
min_baseline=1.
min_cal_baseline=50.
ring_radius=10.*pad_uv_image
;max_calibration_sources=300.
psf_resolution=8.
nfreq_avg=16.
no_rephase=1
gain_factor=0.2
bandpass_calibrate=1
calibration_polyfit=2
no_restrict_cal_sources=1

general_obs,cleanup=cleanup,ps_export=ps_export,split_ps_export=split_ps_export,recalculate_all=recalculate_all,export_images=export_images,version=version,$
    deconvolve=deconvolve,image_filter_fn=image_filter_fn,data_directory=data_directory,combine_healpix=combine_healpix,$
    vis_file_list=vis_file_list,fhd_file_list=fhd_file_list,healpix_path=healpix_path,catalog_file_path=catalog_file_path,$
    dimension=dimension,max_sources=max_sources,pad_uv_image=pad_uv_image,precess=precess,silent=silent,$
    FoV=FoV,no_ps=no_ps,max_baseline=max_baseline,start_fi=start_fi,end_fi=end_fi,$
    min_baseline=min_baseline,calibrate_visibilities=calibrate_visibilities,nfreq_avg=nfreq_avg,gain_factor=gain_factor,$
    no_rephase=no_rephase,calibration_catalog_file_path=calibration_catalog_file_path,psf_resolution=psf_resolution,$
    min_cal_baseline=min_cal_baseline,ring_radius=ring_radius,save_visibilities=save_visibilities,$
    bandpass_calibrate=bandpass_calibrate,calibration_polyfit=calibration_polyfit,no_restrict_cal_sources=no_restrict_cal_sources,$
    flag_visibilities=flag_visibilities,calibration_visibilities_subtract=calibration_visibilities_subtract,ps_kspan=ps_kspan,ps_kbinsize=ps_kbinsize,$
    n_avg=n_avg,force_no_data=force_no_data,snapshot_healpix_export=snapshot_healpix_export,return_decon_visibilities=return_decon_visibilities,_Extra=extra

!except=except
END
