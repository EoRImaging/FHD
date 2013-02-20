PRO general_obs,cleanup=cleanup,ps_export=ps_export,recalculate_all=recalculate_all,export_images=export_images,version=version,$
    beam_recalculate=beam_recalculate,healpix_recalculate=healpix_recalculate,mapfn_recalculate=mapfn_recalculate,$
    grid=grid,deconvolve=deconvolve,image_filter_fn=image_filter_fn,data_directory=data_directory,n_pol=n_pol,precess=precess,$
    vis_file_list=vis_file_list,fhd_file_list=fhd_file_list,healpix_path=healpix_path,catalog_file_path=catalog_file_path,$
    complex_beam=complex_beam,double_precison_beam=double_precison_beam,pad_uv_image=pad_uv_image,max_sources=max_sources,$
    update_file_list=update_file_list,combine_healpix=combine_healpix,start_fi=start_fi,end_fi=end_fi,_Extra=extra
except=!except
!except=0 
heap_gc

IF N_Elements(recalculate_all) EQ 0 THEN recalculate_all=0
IF N_Elements(export_images) EQ 0 THEN export_images=1
IF N_Elements(cleanup) EQ 0 THEN cleanup=0
IF N_Elements(ps_export) EQ 0 THEN ps_export=0
IF N_Elements(version) EQ 0 THEN version=0
IF N_Elements(image_filter_fn) EQ 0 THEN image_filter_fn='filter_uv_hanning' ;applied ONLY to output images

IF N_Elements(data_directory) EQ 0 THEN data_directory=rootdir('mwa')+filepath('',root='DATA',subdir=['X16','Drift'])
IF N_Elements(vis_file_list) EQ 0 THEN vis_file_list=file_search(data_directory,'*_cal.uvfits',count=n_files)
IF N_Elements(fhd_file_list) EQ 0 THEN fhd_file_list=fhd_path_setup(vis_file_list,version=version)

IF N_Elements(healpix_path) EQ 0 THEN healpix_path=fhd_path_setup(output_dir=data_directory,subdir='Healpix',output_filename='Combined_obs',version=version)

IF N_Elements(catalog_file_path) EQ 0 THEN catalog_file_path=filepath('MRC full radio catalog.fits',root=rootdir('mwa'),subdir='DATA')

IF N_Elements(complex_beam) EQ 0 THEN complex_beam=1
IF N_Elements(double_precison_beam) EQ 0 THEN double_precison_beam=0
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
max_sources=10000.
IF N_Elements(pad_uv_image) EQ 0 THEN pad_uv_image=2.
IF N_Elements(n_pol) EQ 0 THEN n_pol=2
IF N_Elements(precess) EQ 0 THEN precess=0 ;set to 1 ONLY for X16 PXX scans (i.e. Drift_X16.pro)

IF N_Elements(start_fi) EQ 0 THEN fi=0 ELSE fi=start_fi
IF N_Elements(end_fi) EQ 0 THEN end_fi=n_files
WHILE fi LT end_fi DO BEGIN
    uvfits2fhd,vis_file_list[fi],file_path_fhd=fhd_file_list[fi],n_pol=n_pol,$
        independent_fit=0,reject_pol_sources=0,beam_recalculate=beam_recalculate,$
        mapfn_recalculate=mapfn_recalculate,flag=flag,grid=grid,healpix_recalculate=healpix_recalculate,$
        /silent,max_sources=max_sources,deconvolve=deconvolve,catalog_file_path=catalog_file_path,$
        export_images=export_images,noise_calibrate=noise_calibrate,align=align,$
        dimension=dimension,image_filter_fn=image_filter_fn,pad_uv_image=pad_uv_image,$
        complex=complex_beam,double=double_precison_beam,precess=precess,_Extra=extra
    fi+=1.
    IF Keyword_Set(update_file_list) THEN BEGIN ;use this if simultaneously downloading and deconvolving observations
        vis_file_list=file_search(data_directory,'*_cal.uvfits',count=n_files)
        fhd_file_list=fhd_path_setup(vis_file_list,version=version)
        end_fi=n_files
    ENDIF
ENDWHILE

map_projection='orth'
IF N_Elements(combine_healpix) EQ 0 THEN combine_healpix=recalculate_all*(n_files GT 1)
IF Keyword_Set(combine_healpix) THEN BEGIN
    combine_obs_sources,fhd_file_list,calibration,source_list,restore_last=0,output_path=healpix_path
    combine_obs_healpix,fhd_file_list,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,obs_arr=obs_arr,$
        nside=nside,restore_last=0,flux_scale=flux_scale,output_path=healpix_path,image_filter_fn=image_filter_fn,_Extra=extra
    combine_obs_hpx_image,fhd_file_list,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,$
        weight_threshold=0.5,fraction_pol=0.5,high_dirty=6.0,low_dirty=-1.5,high_residual=3.0,high_source=3.0,$
        nside=nside,output_path=healpix_path,restore_last=0,obs_arr=obs_arr,map_projection=map_projection,_Extra=extra
    calibration_test,fhd_file_list,output_path=healpix_path
ENDIF

IF Keyword_Set(ps_export) THEN BEGIN
    vis_split_export_multi,n_avg=n_avg,output_path=healpix_path,vis_file_list=vis_file_list,fhd_file_list=fhd_file_list,_Extra=extra
ENDIF
IF Keyword_Set(cleanup) THEN FOR fi=0,n_files-1 DO fhd_cleanup,fhd_file_list[fi]


!except=except
END