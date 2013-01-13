PRO Drift_x16
except=!except
!except=0 
heap_gc

data_directory=filepath('',root='DATA',subdir=['X16','Drift'])
filename_list=file_search(rootdir('mwa')+data_directory,'*_cal.uvfits',count=n_files)

filename_list=Strmid(filename_list,Strlen(rootdir('mwa')+data_directory))
FOR fi=0,n_files-1 DO filename_list[fi]=Strmid(filename_list[fi],0,Strpos(filename_list[fi],'.'))
;filename_list=filename_list[[0,25]]

;filename_list=Reverse(filename_list)

n_files=N_Elements(filename_list)
version=2
alignment_file_header=['filename','degpix','obsra',' obsdec','zenra',' zendec','obsx','','obsy','zenx','zeny','obs_rotation','dx','dy','theta','scale']
textfast,alignment_file_header,filename='alignment'+'v'+strn(version),data_dir=data_directory,/write
FOR fi=3,n_files-1 DO BEGIN
;IF fi LT 27 THEN CONTINUE
    filename=filename_list[fi]
    UPNAME=StrUpCase(filename)
    pcal=strpos(UPNAME,'_CAL')
    filename_use=StrMid(filename,0,pcal)
    beam_recalculate=1
    mapfn=0
    flag=0
    grid=1
    deconvolve=0
    noise_calibrate=0
    fluxfix=0
    align=0
    GPU_enable=0
    max_sources=10000.
    uvfits2fhd,data_directory=data_directory,filename=filename,n_pol=2,version=version,$
        independent_fit=0,reject_pol_sources=0,beam_recalculate=beam_recalculate,$
        mapfn_recalculate=mapfn,flag=flag,grid=grid,GPU_enable=GPU_enable,$
        /silent,max_sources=max_sources,deconvolve=deconvolve,$
        no_output=0,noise_calibrate=noise_calibrate,fluxfix=fluxfix,align=align
;    fhd_output,filename=filename,data_directory=data_directory,version=version,$
;        noise_calibrate=noise_calibrate,fluxfix=fluxfix,align=align;,/restore
ENDFOR

;flux_scale=79.4/2651. ;set 3C444 to catalog value
combine_obs_sources,calibration,source_list,filename_list,restore_last=0,version=version,data_directory=data_directory
combine_obs_healpix,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,mrc_hpx,smooth_hpx,$
    nside=nside,restore_last=0,version=version,data_directory=data_directory,$
    lon_arr=lon_arr,lat_arr=lat_arr,flux_scale=flux_scale
combine_obs_hpx_image,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,mrc_hpx,smooth_hpx,$
    nside=nside,restore_last=0,weight_threshold=0.5,version=version,data_directory=data_directory,$
    lon_arr=lon_arr,lat_arr=lat_arr,fraction_pol=0.5,high_dirty=6.0,low_dirty=-1.5,high_residual=3.0,high_source=3.0

calibration_test,version=version,data_directory=data_directory    
!except=except
END