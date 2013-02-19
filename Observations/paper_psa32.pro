PRO paper_psa32,cleanup=cleanup,ps_export=ps_export,recalculate_all=recalculate_all,export_images=export_images,version=version,$
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
image_filter_fn='filter_uv_hanning' ;applied ONLY to output images

;data_directory=rootdir('mwa')+filepath('',root='PAPER_DATA',subdir=['psa32'])
data_directory='/data2/PAPER/psa32/'
vis_file_list=file_search(data_directory,'*_cal.uvfits',count=n_files)
fhd_file_list=fhd_path_setup(vis_file_list,version=version)

healpix_path=fhd_path_setup(output_dir=data_directory,subdir='Healpix',output_filename='Combined_obs',version=version)

catalog_file_path=filepath('MRC full radio catalog.fits',root=rootdir('mwa'),subdir='DATA')

complex_beam=0
double_precison_beam=0
n_files=N_Elements(vis_file_list)

IF N_Elements(beam_recalculate) EQ 0 THEN beam_recalculate=recalculate_all
IF N_Elements(healpix_recalculate) EQ 0 THEN healpix_recalculate=recalculate_all
IF N_Elements(mapfn_recalculate) EQ 0 THEN mapfn_recalculate=recalculate_all
flag=0
IF N_Elements(grid) EQ 0 THEN grid=recalculate_all
IF N_Elements(deconvolve) EQ 0 THEN deconvolve=recalculate_all
noise_calibrate=0
align=0
;dimension=1024.
kbinsize=0.5
max_sources=10000.
pad_uv_image=2.
precess=0 ;set to 1 ONLY for X16 PXX scans (i.e. Drift_X16.pro)
instrument='paper'
lat=Ten(-30,42,17.5)
lon=Ten(21,25,41)
n_pol=2
complex_beam=1
mirror_X=1
general_obs,cleanup=cleanup,ps_export=ps_export,recalculate_all=recalculate_all,export_images=export_images,version=version,$
    beam_recalculate=beam_recalculate,healpix_recalculate=healpix_recalculate,mapfn_recalculate=mapfn_recalculate,$
    grid=grid,deconvolve=deconvolve,image_filter_fn=image_filter_fn,data_directory=data_directory,n_pol=n_pol,$
    vis_file_list=vis_file_list,fhd_file_list=fhd_file_list,healpix_path=healpix_path,catalog_file_path=catalog_file_path,$
    dimension=dimension,max_sources=max_sources,pad_uv_image=pad_uv_image,precess=precess,$
    rotate_uv=rotate_uv,scale_uv=scale_uv,mirror_X=mirror_X,lon=lon,lat=lat,$
    complex_beam=complex_beam,double_precison_beam=double_precison_beam,instrument=instrument,_Extra=extra

!except=except
END