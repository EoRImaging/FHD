PRO generate_eor_sourcelist,cleanup=cleanup,ps_export=ps_export,recalculate_all=recalculate_all,export_images=export_images,version=version,$
    beam_recalculate=beam_recalculate,healpix_recalculate=healpix_recalculate,mapfn_recalculate=mapfn_recalculate,$
    grid=grid,deconvolve=deconvolve,combine_healpix=combine_healpix,silent=silent,save_visibilities=save_visibilities,_Extra=extra
except=!except
!except=0 
heap_gc

calibrate_visibilities=1
IF N_Elements(recalculate_all) EQ 0 THEN recalculate_all=1
IF N_Elements(export_images) EQ 0 THEN export_images=1
IF N_Elements(cleanup) EQ 0 THEN cleanup=0
IF N_Elements(ps_export) EQ 0 THEN ps_export=0
IF N_Elements(version) EQ 0 THEN version='apb_gen_sourcelist_1'
IF N_Elements(combine_healpix) EQ 0 THEN combine_healpix=0
IF N_Elements(silent) EQ 0 THEN silent=0
IF N_Elements(save_visibilities) EQ 0 THEN save_visibilities=1
image_filter_fn='filter_uv_uniform' ;applied ONLY to output images

data_directory='/nfs/mwa-09/r1/EoRuvfits/jd2456528v2_0'
output_directory='/nfs/mwa-09/r1/djc/EoR2013/Aug23/'
vis_file_list=file_search(data_directory,'*.uvfits',count=n_files)
;vis_file_list = ['/nfs/mwa-09/r1/EoRuvfits/jd2456528v2_0/1061316296.uvfits',$
;'/nfs/mwa-09/r1/EoRuvfits/jd2456528v2_0/1061316424.uvfits',$
;'/nfs/mwa-09/r1/EoRuvfits/jd2456528v2_0/1061316544.uvfits',$
;'/nfs/mwa-09/r1/EoRuvfits/jd2456528v2_0/1061316664.uvfits',$
;'/nfs/mwa-09/r1/EoRuvfits/jd2456528v2_0/1061316784.uvfits',$
;'/nfs/mwa-09/r1/EoRuvfits/jd2456528v2_0/1061328136.uvfits',$
;'/nfs/mwa-09/r1/EoRuvfits/jd2456528v2_0/1061328256.uvfits',$
;'/nfs/mwa-09/r1/EoRuvfits/jd2456528v2_0/1061328376.uvfits',$
;'/nfs/mwa-09/r1/EoRuvfits/jd2456528v2_0/1061328496.uvfits',$
;'/nfs/mwa-09/r1/EoRuvfits/jd2456528v2_0/1061328624.uvfits']
;vis_file_list = ['/nfs/mwa-09/r1/EoRuvfits/jd2456528v2_0/1061312520.uvfits',$
;'/nfs/mwa-09/r1/EoRuvfits/jd2456528v2_0/1061314472.uvfits',$
;'/nfs/mwa-09/r1/EoRuvfits/jd2456528v2_0/1061316296.uvfits',$
;'/nfs/mwa-09/r1/EoRuvfits/jd2456528v2_0/1061318128.uvfits',$
;'/nfs/mwa-09/r1/EoRuvfits/jd2456528v2_0/1061319960.uvfits']
fhd_file_list=fhd_path_setup(vis_file_list,version=version,output_directory=output_directory,_Extra=extra)
healpix_path=fhd_path_setup(output_dir=output_directory,subdir='Healpix',output_filename='Combined_obs',version=version,_Extra=extra)
catalog_file_path=filepath('MRC_full_radio_catalog.fits',root=rootdir('FHD'),subdir='catalog_data')
calibration_catalog_file_path=filepath('mwa_commissioning_source_list.sav',root=rootdir('FHD'),subdir='catalog_data')

;noise_calibrate=0
;align=0
dimension=3072.
;max_baseline=900.
max_sources=25000.
pad_uv_image=1.
precess=0 ;set to 1 ONLY for X16 PXX scans (i.e. Drift_X16.pro)
FoV=80.
no_ps=1 ;don't save postscript copy of images
psf_dim=8
min_baseline=1.
min_cal_baseline=50.
ring_radius=10.*pad_uv_image
;max_calibration_sources=10000.
psf_resolution=8.
nfreq_avg=16.
no_rephase=1
gain_factor=2./3.
general_obs,cleanup=cleanup,ps_export=ps_export,recalculate_all=recalculate_all,export_images=export_images,version=version,$
    beam_recalculate=beam_recalculate,healpix_recalculate=healpix_recalculate,mapfn_recalculate=mapfn_recalculate,$
    grid=grid,deconvolve=deconvolve,image_filter_fn=image_filter_fn,data_directory=data_directory,combine_healpix=combine_healpix,$
    vis_file_list=vis_file_list,fhd_file_list=fhd_file_list,healpix_path=healpix_path,catalog_file_path=catalog_file_path,$
    dimension=dimension,max_sources=max_sources,pad_uv_image=pad_uv_image,precess=precess,psf_dim=psf_dim,silent=silent,$
    complex_beam=complex_beam,double_precison_beam=double_precison_beam,FoV=FoV,no_ps=no_ps,max_baseline=max_baseline,$
    min_baseline=min_baseline,calibrate_visibilities=calibrate_visibilities,nfreq_avg=nfreq_avg,gain_factor=gain_factor,$
    no_fits=no_fits,no_rephase=no_rephase,calibration_catalog_file_path=calibration_catalog_file_path,psf_resolution=psf_resolution,$
    min_cal_baseline=min_cal_baseline,ring_radius=ring_radius,save_visibilities=save_visibilities,_Extra=extra
!except=except
END
