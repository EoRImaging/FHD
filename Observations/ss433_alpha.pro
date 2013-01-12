PRO SS433_alpha
except=!except
!except=0 
heap_gc

data_directory=filepath('',root='DATA',subdir=['Alpha','SS433'])
filename_list=file_search(rootdir('mwa')+data_directory,'*_cal.uvfits',count=n_files)

filename_list=Strmid(filename_list,Strlen(rootdir('mwa')+data_directory))
FOR fi=0,n_files-1 DO filename_list[fi]=Strmid(filename_list[fi],0,Strpos(filename_list[fi],'.'))

version=0
;alignment_file_header=['filename','degpix','obsra',' obsdec','zenra',' zendec','obsx','','obsy','zenx','zeny','obs_rotation','dx','dy','theta','scale']
;textfast,alignment_file_header,filename='alignment'+'v'+strn(version),data_dir=data_directory,/write
FOR fi=0,n_files-1 DO BEGIN
;IF fi LT 9 THEN CONTINUE
    filename=filename_list[fi]
    UPNAME=StrUpCase(filename)
    pcal=strpos(UPNAME,'_CAL')
    filename_use=StrMid(filename,0,pcal)
    
;    vis_path_default,data_directory,filename,file_path,version=version
;    RESTORE,file_path+'_obs.sav'
;    obs.data_directory=data_directory
;    SAVE,obs,filename=file_path+'_obs.sav'
    
    beam_recalculate=0
    mapfn=0
    flag=0
    grid=0
    noise_calibrate=0
    fluxfix=0
    align=1
    GPU_enable=0
    max_sources=20000.
    uvfits2fhd,data_directory=data_directory,filename=filename,n_pol=2,version=version,$
        independent_fit=0,/reject_pol_sources,beam_recalculate=beam_recalculate,$
        mapfn_recalculate=mapfn,flag=flag,grid=grid,GPU_enable=GPU_enable,$
        /silent,noise_calibrate=noise_calibrate,/no_output,max_sources=max_sources,cut_baselines=0.
    fhd_output,filename=filename,data_directory=data_directory,version=version,$
        noise_calibrate=noise_calibrate,fluxfix=fluxfix,align=align;,/restore
ENDFOR

combine_obs_sources,calibration,source_list,filename_list,restore_last=0,version=version,data_directory=data_directory
combine_obs_healpix,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,mrc_hpx,smooth_hpx,$
    nside=nside,restore_last=0,version=version,data_directory=data_directory
combine_obs_hpx_image,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,mrc_hpx,smooth_hpx,$
    nside=nside,restore_last=0,weight_threshold=0.5,version=version,data_directory=data_directory

!except=except
END