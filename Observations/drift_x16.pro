PRO Drift_x16,cleanup=cleanup,ps_export=ps_export,recalculate_all=recalculate_all,export_images=export_images,version=version,$
    beam_recalculate=beam_recalculate,healpix_recalculate=healpix_recalculate,mapfn_recalculate=mapfn_recalculate,$
    grid=grid,deconvolve=deconvolve,channel=channel,_Extra=extra
except=!except
!except=0 
heap_gc

IF N_Elements(recalculate_all) EQ 0 THEN recalculate_all=1
IF N_Elements(export_images) EQ 0 THEN export_images=1
IF N_Elements(cleanup) EQ 0 THEN cleanup=0
IF N_Elements(ps_export) EQ 0 THEN ps_export=0
IF N_Elements(version) EQ 0 THEN version=0
IF N_Elements(channel) EQ 0 THEN channel=121
image_filter_fn='filter_uv_radial' ;applied ONLY to output images

IF channel LE 0 THEN data_directory=rootdir('mwa')+filepath('',root='DATA',subdir=['X16','Drift']) $
    ELSE data_directory=rootdir('mwa')+filepath('',root='DATA',subdir=['X16','Drift',Strn(Floor(channel))])
vis_file_list=file_search(data_directory,'*_cal.uvfits',count=n_files)
fhd_file_list=fhd_path_setup(vis_file_list,version=version)
healpix_path=fhd_path_setup(output_dir=data_directory,subdir='Healpix',output_filename='Combined_obs',version=version)
catalog_file_path=filepath('MRC full radio catalog.fits',root=rootdir('mwa'),subdir='DATA')

;noise_calibrate=0
;align=0

;NOTE: The use of EXTRA to catch additional keywords means these defaults will be OVERWRITTEN if supplied on the command line
dimension=1024.
max_sources=10000.
pad_uv_image=2.
precess=1 ;set to 1 ONLY for X16 PXX scans (i.e. Drift_X16.pro)
complex_beam=1
FoV=90.
no_ps=1 ;don't save postscript copy of images
min_baseline=12.

general_obs,cleanup=cleanup,ps_export=ps_export,recalculate_all=recalculate_all,export_images=export_images,version=version,$
    beam_recalculate=beam_recalculate,healpix_recalculate=healpix_recalculate,mapfn_recalculate=mapfn_recalculate,$
    grid=grid,deconvolve=deconvolve,image_filter_fn=image_filter_fn,data_directory=data_directory,$
    vis_file_list=vis_file_list,fhd_file_list=fhd_file_list,healpix_path=healpix_path,catalog_file_path=catalog_file_path,$
    dimension=dimension,max_sources=max_sources,pad_uv_image=pad_uv_image,precess=precess,$
    complex_beam=complex_beam,double_precison_beam=double_precison_beam,FoV=FoV,no_ps=no_ps,min_baseline=min_baseline,_Extra=extra
!except=except
END