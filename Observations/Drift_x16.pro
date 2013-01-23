PRO Drift_x16
except=!except
!except=0 
heap_gc

data_directory=rootdir('mwa')+filepath('',root='DATA',subdir=['X16','Drift'])
file_list=file_search(data_directory,'*_cal.uvfits',count=n_files)

;filename_list=filename_list[[0,25]]

;filename_list=Reverse(filename_list)

n_files=N_Elements(file_list)
version=0
FOR fi=0,n_files-1 DO BEGIN
;IF fi LT 27 THEN CONTINUE
    file_path=file_list[fi]
    beam_recalculate=0
    healpix_recalculate=0
    mapfn=0
    flag=1
    grid=0
    deconvolve=1
    no_output=0
    noise_calibrate=0
    align=0
    max_sources=10000.
    uvfits2fhd,file_path,n_pol=2,version=version,$
        independent_fit=0,reject_pol_sources=0,beam_recalculate=beam_recalculate,$
        mapfn_recalculate=mapfn,flag=flag,grid=grid,healpix_recalculate=healpix_recalculate,$
        /silent,max_sources=max_sources,deconvolve=deconvolve,$
        no_output=no_output,noise_calibrate=noise_calibrate,align=align
ENDFOR

;flux_scale=79.4/2651. ;set 3C444 to catalog value
combine_obs_sources,file_list,calibration,source_list,restore_last=0,version=version
combine_obs_healpix,file_list,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,mrc_hpx,smooth_hpx,$
    nside=nside,restore_last=0,version=version,$
    lon_arr=lon_arr,lat_arr=lat_arr,flux_scale=flux_scale
combine_obs_hpx_image,file_list,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,mrc_hpx,smooth_hpx,$
    nside=nside,restore_last=0,weight_threshold=0.5,version=version,$
    lon_arr=lon_arr,lat_arr=lat_arr,fraction_pol=0.5,high_dirty=6.0,low_dirty=-1.5,high_residual=3.0,high_source=3.0

calibration_test,file_list,version=version
!except=except
END