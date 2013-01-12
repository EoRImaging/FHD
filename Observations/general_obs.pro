PRO general_obs,field_name=field_name,restore_last=restore_last,version=version

except=!except
!except=0 

IF N_Elements(restore_last) EQ 0 THEN restore_last=0
IF restore_last GE 1 THEN BEGIN
    beam_recalculate=1
    mapfn=1
    flag=1
    grid=1
ENDIF

CASE StrUpCase(field_name) OF
    'EOR1X16':data_directory=filepath('',root='DATA',subdir=['X16','EOR1'])
    'DRIFTX16':data_directory=filepath('',root='DATA',subdir=['X16','Drift'])
    'GALAXYALPHA':data_directory=filepath('',root='DATA',subdir=['Alpha','galaxy'])
    '3C444ALPHA':data_directory=filepath('',root='DATA',subdir=['Alpha','3C444'])
    'PICAX14':data_directory=filepath('',root='DATA',subdir=['r4','clmw','X14','PicA_121_20100924211938'])
ENDCASE

filename_list=file_search(rootdir('mwa')+data_directory,'*_cal.uvfits',count=n_files)

filename_list=Strmid(filename_list,Strlen(rootdir('mwa')+data_directory))
FOR fi=0,n_files-1 DO filename_list[fi]=Strmid(filename_list[fi],0,Strpos(filename_list[fi],'.'))

IF N_Elements(version) EQ 0 THEN version=0

IF restore_last LE 2 THEN BEGIN
    alignment_file_header=['filename','degpix','obsra',' obsdec','zenra',' zendec','obsx','','obsy','zenx','zeny','obs_rotation','dx','dy','theta','scale']
    textfast,alignment_file_header,filename='alignment'+'v'+strn(version),data_dir=data_directory,/write
    FOR fi=0,n_files-1 DO BEGIN
    ;IF fi NE 0 THEN CONTINUE
        filename=filename_list[fi]
        UPNAME=StrUpCase(filename)
        pcal=strpos(UPNAME,'_CAL')
        filename_use=StrMid(filename,0,pcal)
        noise_calibrate=0
        fluxfix=0
        align=1
        silent=1
        GPU_enable=0
        max_sources=20000.
        uvfits2fhd,data_directory=data_directory,filename=filename,n_pol=2,version=version,$
            independent_fit=0,/reject_pol_sources,beam_recalculate=beam_recalculate,$
            mapfn_recalculate=mapfn,flag=flag,grid=grid,GPU_enable=GPU_enable,$
            silent=silent,noise_calibrate=noise_calibrate,/no_output,max_sources=max_sources
        fhd_output,filename=filename,data_directory=data_directory,version=version,$
            noise_calibrate=noise_calibrate,fluxfix=fluxfix,align=align;,/restore
    ENDFOR
ENDIF

IF restore_last GT 2 THEN restore_hpx=1 ELSE restore_hpx=0

combine_obs_sources,calibration,source_list,filename_list,restore_last=0,version=version,data_directory=data_directory
combine_obs_healpix,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,mrc_hpx,smooth_hpx,$
    nside=nside,restore_last=0,version=version,data_directory=data_directory,$
    lon_arr=lon_arr,lat_arr=lat_arr
combine_obs_hpx_image,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,mrc_hpx,smooth_hpx,$
    nside=nside,restore_last=0,weight_threshold=0.5,version=version,data_directory=data_directory,$
    lon_arr=lon_arr,lat_arr=lat_arr

!except=except
END