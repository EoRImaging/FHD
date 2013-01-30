PRO beta_eor_high
except=!except
!except=0 
heap_gc

version=3
textfast,obsids,file_path='/data2/MWA/beta/EoR/beta_high_cals.txt',/string,/read
data_directory='/data2/MWA/beta/EoR/'+obsids
;data_directory=rootdir('mwa')+filepath('',root='DATA',subdir=['X16','Drift'])
file_list=file_search(data_directory,'*_cal.uvfits',count=n_files)
fhd_file_list=fhd_path_setup(file_list,version=version)

;healpix_path=fhd_path_setup(file_list,output_dir=data_directory,subdir='Healpix',output_filename='Combined_obs')
healpix_path='/data2/MWA/beta/EoR/skymap_v1/skymap_v1'
catalog_file_path=filepath('MRC full radio catalog.fits',root=rootdir('mwa'),subdir='DATA')
;filename_list=filename_list[[0,25]]

;filename_list=Reverse(filename_list)

n_files=N_Elements(file_list)
;FOR fi=0,n_files-1 DO BEGIN
;    beam_recalculate=0
;    healpix_recalculate=0
;    mapfn=0
;    flag=0
;    grid=0
;    deconvolve=1
;    no_output=0
;    noise_calibrate=0
;    align=0
;    dimension=2048.
;    flag_n_sigma=6
;    kbin_size=0.125
;    psf_dim=10
;    max_sources=10000.
;    uvfits2fhd,file_list[fi],file_path_fhd=fhd_file_list[fi],n_pol=2,dimension=dimension,kbin_size=kbin_size,psf_dim=psf_dim,$
;        independent_fit=0,reject_pol_sources=0,beam_recalculate=beam_recalculate,$
;        mapfn_recalculate=mapfn,flag=flag,grid=grid,healpix_recalculate=healpix_recalculate,$
;        /silent,max_sources=max_sources,deconvolve=deconvolve,$
;        no_output=no_output,noise_calibrate=noise_calibrate,align=align,catalog_file_path=catalog_file_path
;ENDFOR
restore_last = 1
;flux_scale=79.4/2651. ;set 3C444 to catalog value
combine_obs_sources,fhd_file_list,calibration,source_list,restore_last=restore_last,output_path=healpix_path
combine_obs_healpix,fhd_file_list,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,smooth_hpx,$
    nside=nside,restore_last=restore_last,flux_scale=flux_scale,output_path=healpix_path,obs_arr=obs_arr
combine_obs_hpx_image,fhd_file_list,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,smooth_hpx,$
    weight_threshold=0.5,fraction_pol=0.5,high_dirty=6.0,low_dirty=-1.5,high_residual=3.0,high_source=3.0,$
    nside=nside,output_path=healpix_path,restore_last=restore_last,obs_arr=obs_arr

calibration_test,fhd_file_list,output_path=healpix_path
!except=except
END