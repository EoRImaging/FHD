PRO allsky_beta
except=!except
!except=0 

version=1

data_directory=filepath('',root='DATA2',subdir=['Beta','Allsky'])
filename_list=file_search(rootdir('mwa')+data_directory,'*_cal.uvfits',count=n_files)

filename_list=Strmid(filename_list,Strlen(rootdir('mwa')+data_directory))
FOR fi=0,n_files-1 DO filename_list[fi]=Strmid(filename_list[fi],0,Strpos(filename_list[fi],'.'))

n_files=N_Elements(filename_list)
alignment_file_header=['filename','degpix','obsra',' obsdec','zenra',' zendec','obsx','','obsy','zenx','zeny','obs_rotation','dx','dy','theta','scale']
textfast,alignment_file_header,filename='alignment'+'v'+strn(version),data_dir=data_directory,/write

fi=0
iter=-1
WHILE fi LT n_files DO BEGIN

;FOR fi=0,n_files-1 DO BEGIN

    filename=filename_list[fi]
    UPNAME=StrUpCase(filename)
    pcal=strpos(UPNAME,'_CAL')
    filename_use=StrMid(filename,0,pcal)
    beam_recalculate=1
    mapfn=1
    flag=1
    grid=1
    deconvolve=1
    noise_calibrate=0
    fluxfix=0
    align=1
    silent=1
    GPU_enable=0
    max_sources=10000.
    n_pol=2
    cut_baselines=12.
    dimension=2048.
    kbinsize=0.5
    psf_dim=10.
    uvfits2fhd,data_directory=data_directory,filename=filename,n_pol=n_pol,version=version,$
        independent_fit=0,/reject_pol_sources,beam_recalculate=beam_recalculate,$
        mapfn_recalculate=mapfn,flag=flag,grid=grid,deconvolve=deconvolve,GPU_enable=GPU_enable,$
        silent=silent,noise_calibrate=noise_calibrate,no_output=0,max_sources=max_sources,$
        cut_baselines=cut_baselines,dimension=dimension,kbinsize=kbinsize,psf_dim=psf_dim,$
        fluxfix=fluxfix,align=align
;    fhd_output,filename=filename,data_directory=data_directory,version=version,$
;        noise_calibrate=noise_calibrate,fluxfix=fluxfix,align=align;,/restore
    
    fi+=1
;    filename_list=file_search(rootdir('mwa')+data_directory,'*_cal.uvfits',count=n_files)
;    filename_list=Strmid(filename_list,Strlen(rootdir('mwa')+data_directory))
;    FOR fi0=0,n_files-1 DO filename_list[fi0]=Strmid(filename_list[fi0],0,Strpos(filename_list[fi0],'.'))
;    n_files=N_Elements(filename_list)
ENDWHILE
;ENDFOR

combine_obs_sources,calibration,source_list,filename_list,restore_last=0,version=version,data_directory=data_directory,/no_align
combine_obs_healpix,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,mrc_hpx,smooth_hpx,$
    nside=nside,restore_last=0,version=version,data_directory=data_directory,$
    lon_arr=lon_arr,lat_arr=lat_arr
combine_obs_hpx_image,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,mrc_hpx,smooth_hpx,$
    nside=nside,restore_last=0,weight_threshold=0.5,version=version,data_directory=data_directory,$
    lon_arr=lon_arr,lat_arr=lat_arr

!except=except
END