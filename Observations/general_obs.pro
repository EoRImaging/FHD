PRO general_obs,cleanup=cleanup,ps_export=ps_export,recalculate_all=recalculate_all,export_images=export_images,version=version,$
    beam_recalculate=beam_recalculate,healpix_recalculate=healpix_recalculate,mapfn_recalculate=mapfn_recalculate,$
    grid=grid,deconvolve=deconvolve,image_filter_fn=image_filter_fn,data_directory=data_directory,n_pol=n_pol,precess=precess,$
    vis_file_list=vis_file_list,fhd_file_list=fhd_file_list,healpix_path=healpix_path,catalog_file_path=catalog_file_path,$
    complex_beam=complex_beam,double_precison_beam=double_precison_beam,pad_uv_image=pad_uv_image,max_sources=max_sources,$
    update_file_list=update_file_list,combine_healpix=combine_healpix,start_fi=start_fi,end_fi=end_fi,skip_fi=skip_fi,flag=flag,$
    transfer_mapfn=transfer_mapfn,split_ps_export=split_ps_export,simultaneous=simultaneous,force_no_data=force_no_data,$
    calibration_catalog_file_path=calibration_catalog_file_path,_Extra=extra

except=!except
!except=0 
heap_gc

;Set which procedures are to be run
IF N_Elements(recalculate_all) EQ 0 THEN recalculate_all=0
IF N_Elements(export_images) EQ 0 THEN export_images=0
IF N_Elements(cleanup) EQ 0 THEN cleanup=0
IF N_Elements(ps_export) EQ 0 THEN ps_export=0

;Set up paths
IF N_Elements(version) EQ 0 THEN version=0
IF N_Elements(data_directory) EQ 0 THEN data_directory=rootdir('mwa')+filepath('',root='DATA',subdir=['X16','Drift'])
IF N_Elements(vis_file_list) EQ 0 THEN vis_file_list=file_search(data_directory,'*_cal.uvfits',count=n_files)
IF StrLowCase(Strmid(vis_file_list[0],3,/reverse)) EQ '.txt' THEN Textfast,vis_file_list,file_path=vis_file_list,/read,/string
IF N_Elements(fhd_file_list) EQ 0 THEN fhd_file_list=fhd_path_setup(vis_file_list,version=version)
IF N_Elements(healpix_path) EQ 0 THEN healpix_path=fhd_path_setup(output_dir=data_directory,subdir='Healpix',output_filename='Combined_obs',version=version)
IF N_Elements(catalog_file_path) EQ 0 THEN catalog_file_path=filepath('MRC_full_radio_catalog.fits',root=rootdir('FHD'),subdir='catalog_data')
IF N_Elements(calibration_catalog_file_path) EQ 0 THEN calibration_catalog_file_path=rootdir('mwa')+filepath('calibration_source_list.sav',root='DATA')
n_files=N_Elements(vis_file_list)

;Set which files to restore or recalculate (if the file is not found and needed, it will be recalculated
IF N_Elements(double_precison_beam) EQ 0 THEN double_precison_beam=0
IF N_Elements(beam_recalculate) EQ 0 THEN beam_recalculate=recalculate_all
IF N_Elements(healpix_recalculate) EQ 0 THEN healpix_recalculate=0
IF N_Elements(mapfn_recalculate) EQ 0 THEN mapfn_recalculate=recalculate_all
IF N_Elements(flag) EQ 0 THEN flag=0
IF N_Elements(grid) EQ 0 THEN grid=recalculate_all
IF Keyword_Set(simultaneous) THEN BEGIN
    deconvolve=0
    export_sim=export_images
    export_images=0
ENDIF
IF Keyword_Set(recalculate_all) AND (N_Elements(deconvolve) EQ 0) THEN deconvolve=1
;IF N_Elements(deconvolve) EQ 0 THEN deconvolve=recalculate_all
IF N_Elements(transfer_mapfn) EQ 0 THEN transfer_mapfn=0

;Set up gridding and deconvolution parameters
IF N_Elements(complex_beam) EQ 0 THEN complex_beam=1
IF N_Elements(n_pol) EQ 0 THEN n_pol=2
IF N_Elements(precess) EQ 0 THEN precess=0 ;set to 1 ONLY for X16 PXX scans (i.e. Drift_X16.pro)
IF N_Elements(gain_factor) EQ 0 THEN gain_factor=0.15
IF N_Elements(max_sources) EQ 0 THEN max_sources=10000. ;maximum total number of source components to fit
IF N_Elements(add_threshold) EQ 0 THEN add_threshold=0.8 ;also fit additional components brighter than this threshold
IF N_Elements(independent_fit) EQ 0 THEN independent_fit=0 ;set to 1 to fit I, Q, (U, V) seperately. Otherwise, only I (and U) is fit
;dimension=1024.

;Set up output image parameters
IF N_Elements(pad_uv_image) EQ 0 THEN pad_uv_image=2. ;grid output images at a higher resolution if set (ignored for quickview images)
IF N_Elements(image_filter_fn) EQ 0 THEN image_filter_fn='filter_uv_radial' ;applied ONLY to output images
noise_calibrate=0
align=0

IF N_Elements(start_fi) EQ 0 THEN start_fi=0
fi=start_fi
IF N_Elements(end_fi) GT 0 THEN n_files=end_fi+1 ;changed to allow end_fi and update to both be specified
WHILE fi LT n_files DO BEGIN
    IF ~Keyword_Set(silent) THEN print,String(format='("On observation ",A," of ",A)',Strn(Floor(fi-start_fi+1)),Strn(Floor(n_files-start_fi)))
    IF N_Elements(skip_fi) GT 0 THEN BEGIN
        IF max(skip_fi EQ fi) GT 0 THEN BEGIN
            fi+=1
            CONTINUE
        ENDIF
    ENDIF
    IF (recalculate_all EQ 0) AND Keyword_Set(cleanup) THEN BEGIN IF N_Elements(fi_use) GT 0 THEN fi_use=[fi_use,fi] ELSE fi_use=fi & fi+=1 & CONTINUE & ENDIF
    IF Keyword_Set(force_no_data) THEN BEGIN IF N_Elements(fi_use) GT 0 THEN fi_use=[fi_use,fi] ELSE fi_use=fi & fi+=1 & CONTINUE & ENDIF
    uvfits2fhd,vis_file_list[fi],file_path_fhd=fhd_file_list[fi],n_pol=n_pol,$
        independent_fit=independent_fit,beam_recalculate=beam_recalculate,transfer_mapfn=transfer_mapfn,$
        mapfn_recalculate=mapfn_recalculate,flag=flag,grid=grid,healpix_recalculate=healpix_recalculate,$
        /silent,max_sources=max_sources,deconvolve=deconvolve,catalog_file_path=catalog_file_path,$
        export_images=export_images,noise_calibrate=noise_calibrate,align=align,$
        dimension=dimension,image_filter_fn=image_filter_fn,pad_uv_image=pad_uv_image,$
        complex=complex_beam,double=double_precison_beam,precess=precess,error=error,$
        quickview=quickview,gain_factor=gain_factor,add_threshold=add_threshold,$
        calibration_catalog_file_path=calibration_catalog_file_path,_Extra=extra
    IF Keyword_Set(cleanup) AND cleanup GT 1 THEN fhd_cleanup,fhd_file_list[fi],/minimal
    IF Keyword_Set(error) THEN BEGIN
        print,'Error encountered!'
    ENDIF ELSE $
        IF N_Elements(fi_use) GT 0 THEN fi_use=[fi_use,fi] ELSE fi_use=fi
    fi+=1.
    IF Keyword_Set(update_file_list) THEN BEGIN ;use this if simultaneously downloading and deconvolving observations
        vis_file_list=file_search(data_directory,'*_cal.uvfits',count=n_files)
        fhd_file_list=fhd_path_setup(vis_file_list,version=version)
        IF N_Elements(end_fi) GT 0 THEN n_files=end_fi+1
    ENDIF
ENDWHILE
IF N_Elements(end_fi) EQ 0 THEN end_fi=fi-1

n_files_use=N_Elements(fi_use)
vis_file_list=vis_file_list[fi_use]
fhd_file_list=fhd_file_list[fi_use]
IF Keyword_Set(simultaneous) THEN BEGIN
    IF Total(simultaneous) GT 1 THEN N_simultaneous=simultaneous
    fhd_multi_wrap,fhd_file_list,N_simultaneous=N_simultaneous,n_pol=n_pol,$
        independent_fit=independent_fit,/silent,max_sources=max_sources,catalog_file_path=catalog_file_path,$
        export_images=export_images,image_filter_fn=image_filter_fn,pad_uv_image=pad_uv_image,$
        quickview=quickview,gain_factor=gain_factor,add_threshold=add_threshold,transfer_mapfn=transfer_mapfn,_Extra=extra    
    IF Keyword_Set(export_sim) THEN FOR fi=0L,n_files_use-1 DO BEGIN
        uvfits2fhd,vis_file_list[fi],file_path_fhd=fhd_file_list[fi],n_pol=n_pol,/force_no_data,$
            beam_recalculate=0,transfer_mapfn=0,mapfn_recalculate=0,flag=0,grid=0,healpix_recalculate=0,$
            /silent,max_sources=max_sources,deconvolve=0,catalog_file_path=catalog_file_path,$
            export_images=1,noise_calibrate=noise_calibrate,align=align,$
            dimension=dimension,image_filter_fn=image_filter_fn,pad_uv_image=pad_uv_image,$
            error=error,quickview=0,_Extra=extra
    ENDFOR
ENDIF

map_projection='orth'
IF N_Elements(combine_healpix) EQ 0 THEN combine_healpix=recalculate_all*(n_files_use GT 1)
IF Keyword_Set(combine_healpix) THEN BEGIN
    IF Keyword_Set(ps_export) THEN weight_threshold=0 ELSE weight_threshold=0.2
    combine_obs_sources,fhd_file_list,restore_last=0,output_path=healpix_path,_Extra=extra
    combine_obs_healpix,fhd_file_list,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,mrc_hpx=mrc_hpx,obs_arr=obs_arr,$
        nside=nside,restore_last=0,flux_scale=flux_scale,output_path=healpix_path,image_filter_fn=image_filter_fn,catalog_file_path=catalog_file_path,_Extra=extra
    combine_obs_hpx_image,fhd_file_list,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,mrc_hpx=mrc_hpx,$
        weight_threshold=weight_threshold,fraction_pol=0.5,high_dirty=6.0,low_dirty=-1.5,high_residual=3.0,high_source=3.0,$
        nside=nside,output_path=healpix_path,restore_last=0,obs_arr=obs_arr,map_projection=map_projection,_Extra=extra
;    calibration_test,fhd_file_list,output_path=healpix_path ;currently broken!
ENDIF

IF Keyword_Set(ps_export) THEN BEGIN
    IF Keyword_Set(split_ps_export) THEN BEGIN
        vis_split_export_multi,n_avg=n_avg,output_path=healpix_path,vis_file_list=vis_file_list,fhd_file_list=fhd_file_list,/even,_Extra=extra
        vis_split_export_multi,n_avg=n_avg,output_path=healpix_path,vis_file_list=vis_file_list,fhd_file_list=fhd_file_list,/odd,_Extra=extra
    ENDIF ELSE vis_split_export_multi,n_avg=n_avg,output_path=healpix_path,vis_file_list=vis_file_list,fhd_file_list=fhd_file_list,_Extra=extra
ENDIF
IF Keyword_Set(cleanup) THEN FOR fi=0L,n_files_use-1 DO fhd_cleanup,fhd_file_list[fi]

!except=except
END