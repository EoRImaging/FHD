PRO mwa_paper_joint,cleanup=cleanup,ps_export=ps_export,recalculate_all=recalculate_all,export_images=export_images,version=version,$
    beam_recalculate=beam_recalculate,healpix_recalculate=healpix_recalculate,mapfn_recalculate=mapfn_recalculate,$
    grid=grid,deconvolve=deconvolve,_Extra=extra
except=!except
!except=0 
heap_gc

IF N_Elements(recalculate_all) EQ 0 THEN recalculate_all=1
IF N_Elements(export_images) EQ 0 THEN export_images=1
IF N_Elements(cleanup) EQ 0 THEN cleanup=0
IF N_Elements(ps_export) EQ 0 THEN ps_export=0
IF N_Elements(version) EQ 0 THEN version=0
image_filter_fn='filter_uv_radial' ;applied ONLY to output images

;MWA DATA FIRST
data_directory_mwa=rootdir('mwa')+filepath('',root='DATA2',subdir=['mwa_paper_joint','mwa','EOR1','145'])   
data_directory_paper=rootdir('mwa')+filepath('',root='DATA2',subdir=['mwa_paper_joint','paper','psa32'])
data_directory_all=rootdir('mwa')+filepath('',root='DATA2',subdir=['mwa_paper_joint'])

vis_file_list_mwa=file_search(data_directory_mwa,'*_cal.uvfits',count=n_files)
fhd_file_list_mwa=fhd_path_setup(vis_file_list_mwa,version=version)
healpix_path=fhd_path_setup(output_dir=data_directory_mwa,subdir='Healpix',output_filename='Combined_obs',version=version)
catalog_file_path=filepath('MRC full radio catalog.fits',root=rootdir('FHD'),subdir='catalog_data')

;noise_calibrate=0
;align=0
dimension=1024.
max_sources=10000.
pad_uv_image=2.
precess=0 ;set to 1 ONLY for X16 PXX scans (i.e. Drift_X16.pro)
n_pol=2
complex_beam=1
rephase_to_zenith=1
FoV=90.
flag=1
no_ps=1 ;don't save postscript copy of images
deconvolve=0
export_images=0
combine_healpix=0

general_obs,cleanup=cleanup,ps_export=ps_export,recalculate_all=recalculate_all,export_images=export_images,version=version,$
    beam_recalculate=beam_recalculate,healpix_recalculate=healpix_recalculate,mapfn_recalculate=mapfn_recalculate,$
    grid=grid,deconvolve=deconvolve,image_filter_fn=image_filter_fn,data_directory=data_directory_mwa,$
    vis_file_list=vis_file_list_mwa,fhd_file_list=fhd_file_list_mwa,healpix_path=healpix_path,catalog_file_path=catalog_file_path,$
    dimension=dimension,max_sources=max_sources,pad_uv_image=pad_uv_image,precess=precess,FoV=FoV,$
    complex_beam=complex_beam,double_precison_beam=double_precison_beam,rephase_to_zenith=rephase_to_zenith,$
    combine_healpix=combine_healpix,no_ps=no_ps,flag=flag,_Extra=extra

;PAPER DATA NEXT
vis_file_list_paper=file_search(data_directory_paper,'*.uvfits',count=n_files)
fhd_file_list_paper=fhd_path_setup(vis_file_list_paper,version=version)
healpix_path=fhd_path_setup(output_dir=data_directory_paper,subdir='Healpix',output_filename='Combined_obs',version=version)

;dimension=1024.
FoV=160.
dimension=1024.
psf_dim=6.

max_sources=10000.
pad_uv_image=2.
precess=0 ;set to 1 ONLY for X16 PXX scans (i.e. Drift_X16.pro)
instrument='paper'
lat=Ten(-30,42,17.5)
lon=Ten(21,25,41)
n_pol=2
flag=1
;mirror_X=1
;independent_fit=1 ;not sure of polarization calibration for now!
time_offset=5.*60. ;time offset of phase center from start time. PAPER data are phased to 5 minutes after the start time. 
no_ps=1
deconvolve=0
export_images=0
combine_healpix=0
no_complex_beam=1
nfreq_avg=1.
start_fi=9
end_fi=14

general_obs,cleanup=cleanup,ps_export=ps_export,recalculate_all=recalculate_all,export_images=export_images,version=version,$
    beam_recalculate=beam_recalculate,healpix_recalculate=healpix_recalculate,mapfn_recalculate=mapfn_recalculate,$
    grid=grid,deconvolve=deconvolve,image_filter_fn=image_filter_fn,data_directory=data_directory_paper,n_pol=n_pol,$
    vis_file_list=vis_file_list_paper,fhd_file_list=fhd_file_list_paper,healpix_path=healpix_path,catalog_file_path=catalog_file_path,$
    dimension=dimension,max_sources=max_sources,pad_uv_image=pad_uv_image,precess=precess,no_ps=no_ps,$
    rotate_uv=rotate_uv,scale_uv=scale_uv,mirror_X=mirror_X,lon=lon,lat=lat,FoV=FoV,time_offset=time_offset,$
    complex_beam=complex_beam,double_precison_beam=double_precison_beam,instrument=instrument,psf_dim=psf_dim,$
    no_complex_beam=no_complex_beam,nfreq_avg=nfreq_avg,combine_healpix=combine_healpix,flag=flag,_Extra=extra


;NOW COMBINE BOTH:
vis_file_list_all=[vis_file_list_mwa,vis_file_list_paper]
fhd_file_list_all=[fhd_file_list_mwa,fhd_file_list_paper]
healpix_path=fhd_path_setup(output_dir=data_directory_all,subdir='Healpix',output_filename='Combined_obs',version=version)

max_sources=30000.
pad_uv_image=2.
no_ps=1
deconvolve=0
export_images=1
combine_healpix=1
;no_complex_beam=1
simultaneous=1
IF N_Elements(extra) GT 0 THEN IF Tag_exist(extra,'force_data') THEN extra.force_data=0

general_obs,cleanup=cleanup,ps_export=ps_export,recalculate_all=0,export_images=export_images,version=version,$
    beam_recalculate=0,healpix_recalculate=0,mapfn_recalculate=0,simultaneous=simultaneous,$
    grid=0,deconvolve=0,image_filter_fn=image_filter_fn,data_directory=data_directory_all,n_pol=n_pol,$
    vis_file_list=vis_file_list_all,fhd_file_list=fhd_file_list_all,healpix_path=healpix_path,catalog_file_path=catalog_file_path,$
    dimension=dimension,max_sources=max_sources,pad_uv_image=pad_uv_image,$
    combine_healpix=combine_healpix,flag=0,_Extra=extra
    
END