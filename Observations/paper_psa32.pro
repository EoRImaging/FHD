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

data_directory=rootdir('mwa')+filepath('',root='PAPER_DATA',subdir=['psa32'])
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
max_sources=1000.
pad_uv_image=2.
precess=0 ;set to 1 ONLY for X16 PXX scans (i.e. Drift_X16.pro)
instrument='paper'
lat=Ten(-30,42,17.5)
lon=Ten(21,25,41)
rotate_uv=0
scale_uv=!Pi
n_pol=1
FOR fi=0L,n_files-1 DO BEGIN
    uvfits2fhd,vis_file_list[fi],file_path_fhd=fhd_file_list[fi],n_pol=n_pol,$
        independent_fit=0,reject_pol_sources=0,beam_recalculate=beam_recalculate,$
        mapfn_recalculate=mapfn_recalculate,flag=flag,grid=grid,healpix_recalculate=healpix_recalculate,$
        /silent,max_sources=max_sources,deconvolve=deconvolve,catalog_file_path=catalog_file_path,$
        export_images=export_images,noise_calibrate=noise_calibrate,align=align,$
        dimension=dimension,image_filter_fn=image_filter_fn,pad_uv_image=pad_uv_image,$
        complex=complex_beam,double=double_precison_beam,precess=precess,$
        instrument=instrument,lon=lon,lat=lat,kbinsize=kbinsize,rotate_uv=rotate_uv,scale_uv=scale_uv,_Extra=extra
ENDFOR

map_projection='orth'
combine_obs_sources,fhd_file_list,calibration,source_list,restore_last=0,output_path=healpix_path
combine_obs_healpix,fhd_file_list,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,obs_arr=obs_arr,$
    nside=nside,restore_last=0,flux_scale=flux_scale,output_path=healpix_path,image_filter_fn=image_filter_fn
combine_obs_hpx_image,fhd_file_list,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,$
    weight_threshold=0.5,fraction_pol=0.5,high_dirty=6.0,low_dirty=-1.5,high_residual=3.0,high_source=3.0,$
    nside=nside,output_path=healpix_path,restore_last=0,obs_arr=obs_arr,map_projection=map_projection

calibration_test,fhd_file_list,output_path=healpix_path

IF Keyword_Set(ps_export) THEN BEGIN
    vis_split_export_multi,n_avg=n_avg,output_path=healpix_path,vis_file_list=vis_file_list,fhd_file_list=fhd_file_list,_Extra=extra
ENDIF
IF Keyword_Set(cleanup) THEN FOR fi=0,n_files-1 DO fhd_cleanup,fhd_file_list[fi]
!except=except
END