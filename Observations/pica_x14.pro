PRO PicA_x14,cleanup=cleanup,ps_export=ps_export,recalculate_all=recalculate_all,export_images=export_images,version=version,$
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

data_directory=rootdir('mwa')+filepath('',root='DATA',subdir=['r4','clmw','X14','PicA_121_20100924211938'])
vis_file_list=file_search(data_directory,'*_cal.uvfits',count=n_files)
fhd_file_list=fhd_path_setup(vis_file_list,version=version)
healpix_path=fhd_path_setup(output_dir=data_directory,subdir='Healpix',output_filename='Combined_obs',version=version)
catalog_file_path=filepath('MRC full radio catalog.fits',root=rootdir('mwa'),subdir='DATA')

;noise_calibrate=0
;align=0
dimension=1024.
max_sources=10000.
pad_uv_image=2.
precess=0 ;set to 1 ONLY for X16 PXX scans (i.e. Drift_X16.pro)
FoV=90.
no_ps=1 ;don't save postscript copy of images

general_obs,cleanup=cleanup,ps_export=ps_export,recalculate_all=recalculate_all,export_images=export_images,version=version,$
    beam_recalculate=beam_recalculate,healpix_recalculate=healpix_recalculate,mapfn_recalculate=mapfn_recalculate,$
    grid=grid,deconvolve=deconvolve,image_filter_fn=image_filter_fn,data_directory=data_directory,$
    vis_file_list=vis_file_list,fhd_file_list=fhd_file_list,healpix_path=healpix_path,catalog_file_path=catalog_file_path,$
    dimension=dimension,max_sources=max_sources,pad_uv_image=pad_uv_image,precess=precess,$
    complex_beam=complex_beam,double_precison_beam=double_precison_beam,FoV=FoV,no_ps=no_ps,_Extra=extra
!except=except
END
;
;data_directory=filepath('',root='DATA',subdir=['r4','clmw','X14','PicA_121_20100924211938'])
;filename_list=file_search(rootdir('mwa')+data_directory,'*_cal.uvfits',count=n_files)
;
;filename_list=Strmid(filename_list,Strlen(rootdir('mwa')+data_directory))
;FOR fi=0,n_files-1 DO filename_list[fi]=Strmid(filename_list[fi],0,Strpos(filename_list[fi],'.'))
;
;version=1
;alignment_file_header=['filename','degpix','obsra',' obsdec','zenra',' zendec','obsx','','obsy','zenx','zeny','obs_rotation','dx','dy','theta','scale']
;textfast,alignment_file_header,filename='alignment'+'v'+strn(version),data_dir=data_directory,/write
;FOR fi=0,n_files-1 DO BEGIN
;
;    filename=filename_list[fi]
;    UPNAME=StrUpCase(filename)
;    pcal=strpos(UPNAME,'_CAL')
;    filename_use=StrMid(filename,0,pcal)
;    beam_recalculate=0
;    mapfn=0
;    flag=0
;    grid=0
;    noise_calibrate=1
;    fluxfix=0
;    align=1
;;    uvfits2fhd,data_directory=data_directory,filename=filename,n_pol=2,version=version,$
;;        independant_fit=0,/reject_pol_sources,beam_recalculate=beam_recalculate,$
;;        mapfn_recalculate=mapfn,flag=flag,grid=grid,/silent,noise_calibrate=noise_calibrate,/no_output
;    fhd_output,filename=filename,data_directory=data_directory,version=version,$
;        noise_calibrate=noise_calibrate,fluxfix=fluxfix,align=align;,/restore
;ENDFOR
;
;
;combine_obs_sources,calibration,source_list,filename_list,restore_last=0,version=version,data_directory=data_directory
;combine_obs_healpix,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,mrc_hpx,smooth_hpx,$
;    nside=nside,restore_last=0,version=version,data_directory=data_directory,$
;    lon_arr=lon_arr,lat_arr=lat_arr
;combine_obs_hpx_image,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,mrc_hpx,smooth_hpx,$
;    nside=nside,restore_last=0,weight_threshold=0.5,version=version,data_directory=data_directory,$
;    lon_arr=lon_arr,lat_arr=lat_arr
;    
;!except=except
;END