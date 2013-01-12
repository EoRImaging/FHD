PRO PicA_x14
except=!except
!except=0 

data_directory=filepath('',root='DATA',subdir=['r4','clmw','X14','PicA_121_20100924211938'])
filename_list=file_search(rootdir('mwa')+data_directory,'*_cal.uvfits',count=n_files)

filename_list=Strmid(filename_list,Strlen(rootdir('mwa')+data_directory))
FOR fi=0,n_files-1 DO filename_list[fi]=Strmid(filename_list[fi],0,Strpos(filename_list[fi],'.'))

version=1
alignment_file_header=['filename','degpix','obsra',' obsdec','zenra',' zendec','obsx','','obsy','zenx','zeny','obs_rotation','dx','dy','theta','scale']
textfast,alignment_file_header,filename='alignment'+'v'+strn(version),data_dir=data_directory,/write
FOR fi=0,n_files-1 DO BEGIN

    filename=filename_list[fi]
    UPNAME=StrUpCase(filename)
    pcal=strpos(UPNAME,'_CAL')
    filename_use=StrMid(filename,0,pcal)
    beam_recalculate=0
    mapfn=0
    flag=0
    grid=0
    noise_calibrate=1
    fluxfix=0
    align=1
;    uvfits2fhd,data_directory=data_directory,filename=filename,n_pol=2,version=version,$
;        independant_fit=0,/reject_pol_sources,beam_recalculate=beam_recalculate,$
;        mapfn_recalculate=mapfn,flag=flag,grid=grid,/silent,noise_calibrate=noise_calibrate,/no_output
    fhd_output,filename=filename,data_directory=data_directory,version=version,$
        noise_calibrate=noise_calibrate,fluxfix=fluxfix,align=align;,/restore
ENDFOR


combine_obs_sources,calibration,source_list,filename_list,restore_last=0,version=version,data_directory=data_directory
combine_obs_healpix,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,mrc_hpx,smooth_hpx,$
    nside=nside,restore_last=0,version=version,data_directory=data_directory,$
    lon_arr=lon_arr,lat_arr=lat_arr
combine_obs_hpx_image,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,mrc_hpx,smooth_hpx,$
    nside=nside,restore_last=0,weight_threshold=0.5,version=version,data_directory=data_directory,$
    lon_arr=lon_arr,lat_arr=lat_arr
    
!except=except
END