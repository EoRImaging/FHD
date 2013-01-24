PRO EOR1_X16,cleanup=cleanup
except=!except
!except=0 

data_directory=rootdir('mwa')+filepath('',root='DATA2',subdir=['X16','EOR1'])

file_list=file_search(data_directory,'*_cal.uvfits',count=n_files)
fhd_file_list=fhd_path_setup(file_list,version=version)

healpix_path=fhd_path_setup(output_dir=data_directory,subdir='Healpix',output_filename='Combined_obs')

catalog_file_path=filepath('MRC full radio catalog.fits',root=rootdir('mwa'),subdir='DATA')

n_files=N_Elements(file_list)
FOR fi=0,n_files-1 DO BEGIN
    beam_recalculate=1
    healpix_recalculate=0
    mapfn=0
    flag=0
    grid=0
    deconvolve=0
    no_output=0
    noise_calibrate=0
    align=0
    max_sources=10000.
    uvfits2fhd,file_list[fi],file_path_fhd=fhd_file_list[fi],n_pol=2,$
        independent_fit=0,reject_pol_sources=0,beam_recalculate=beam_recalculate,$
        mapfn_recalculate=mapfn,flag=flag,grid=grid,healpix_recalculate=healpix_recalculate,$
        /silent,max_sources=max_sources,deconvolve=deconvolve,$
        no_output=no_output,noise_calibrate=noise_calibrate,align=align,catalog_file_path=catalog_file_path
ENDFOR

combine_obs_sources,fhd_file_list,calibration,source_list,restore_last=0,output_path=healpix_path
combine_obs_healpix,fhd_file_list,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,smooth_hpx,$
    nside=nside,restore_last=0,flux_scale=flux_scale,output_path=healpix_path,obs_arr=obs_arr
combine_obs_hpx_image,fhd_file_list,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,smooth_hpx,$
    weight_threshold=0.5,fraction_pol=0.5,high_dirty=6.0,low_dirty=-1.5,high_residual=3.0,high_source=3.0,$
    nside=nside,output_path=healpix_path,restore_last=0,obs_arr=obs_arr

calibration_test,fhd_file_list,output_path=healpix_path
IF Keyword_Set(cleanup) THEN FOR fi=0,n_files-1 DO fhd_cleanup,fhd_file_list[fi]
!except=except
END