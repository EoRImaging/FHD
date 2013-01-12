PRO EOR1_X16
except=!except
!except=0 

data_directory=filepath('',root='DATA2',subdir=['X16','EOR1'])
filename_list=file_search(rootdir('mwa')+data_directory,'*_cal.uvfits',count=n_files)

filename_list=Strmid(filename_list,Strlen(rootdir('mwa')+data_directory))
FOR fi=0,n_files-1 DO filename_list[fi]=Strmid(filename_list[fi],0,Strpos(filename_list[fi],'.'))
filename_list=filename_list[0:5]

n_files=N_Elements(filename_list)
version=7
alignment_file_header=['filename','degpix','obsra',' obsdec','zenra',' zendec','obsx','','obsy','zenx','zeny','obs_rotation','dx','dy','theta','scale']
textfast,alignment_file_header,filename='alignment'+'v'+strn(version),data_dir=data_directory,/write
FOR fi=0,n_files-1 DO BEGIN
;IF fi NE 0 THEN CONTINUE
    filename=filename_list[fi]
    UPNAME=StrUpCase(filename)
    pcal=strpos(UPNAME,'_CAL')
    filename_use=StrMid(filename,0,pcal)
    beam_recalculate=0
    mapfn=0
    flag=0
    grid=0
    deconvolve=0
    REPHASE_TO_ZENITH=1
    noise_calibrate=0
    fluxfix=0
    align=0
    silent=1
    GPU_enable=0
    max_sources=10000.
    uvfits2fhd,data_directory=data_directory,filename=filename,n_pol=2,version=version,$
        independent_fit=0,reject_pol_sources=0,beam_recalculate=beam_recalculate,$
        mapfn_recalculate=mapfn,flag=flag,grid=grid,deconvolve=deconvolve,GPU_enable=GPU_enable,$
        silent=silent,noise_calibrate=noise_calibrate,no_output=0,max_sources=max_sources,$
        fluxfix=fluxfix,align=align,rephase_to_zenith=rephase_to_zenith
;    fhd_output,filename=filename,data_directory=data_directory,version=version,$
;        noise_calibrate=noise_calibrate,fluxfix=fluxfix,align=align;,/restore
ENDFOR
;flux_scale=30.6/168.2 ;set to catalog value of VLSSr J035140.2-274401
combine_obs_sources,calibration,source_list,filename_list,restore_last=0,version=version,data_directory=data_directory
combine_obs_healpix,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,mrc_hpx,smooth_hpx,$
    nside=nside,restore_last=0,version=version,data_directory=data_directory,$
    lon_arr=lon_arr,lat_arr=lat_arr,flux_scale=flux_scale
combine_obs_hpx_image,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,mrc_hpx,smooth_hpx,$
    nside=nside,restore_last=0,weight_threshold=0.25,version=version,data_directory=data_directory,$
    lon_arr=lon_arr,lat_arr=lat_arr,high_dirty=30.,high_res=5.,high_source=30.
calibration_test,version=version,data_directory=data_directory  
;vis_split_export_multi2,hpx_inds,filename_list=filename_list,version=version,beam_threshold=0.1,$
;    data_directory=data_directory,n_avg=n_avg,restore_last=1,nside=nside

!except=except
END