PRO general_obs,cleanup=cleanup,recalculate_all=recalculate_all,export_images=export_images,version=version,$
    mapfn_recalculate=mapfn_recalculate,grid_recalculate=grid_recalculate,snapshot_recalculate=snapshot_recalculate,deconvolve=deconvolve,$
    image_filter_fn=image_filter_fn,data_directory=data_directory,output_directory=output_directory,n_pol=n_pol,precess=precess,$
    vis_file_list=vis_file_list,fhd_file_list=fhd_file_list,healpix_path=healpix_path,catalog_file_path=catalog_file_path,$
    complex_beam=complex_beam,pad_uv_image=pad_uv_image,$
    update_file_list=update_file_list,combine_healpix=combine_healpix,start_fi=start_fi,end_fi=end_fi,skip_fi=skip_fi,flag_visibilities=flag_visibilities,$
    transfer_mapfn=transfer_mapfn,transfer_weights=transfer_weights,split_ps_export=split_ps_export,simultaneous=simultaneous,flag_calibration=flag_calibration,$
    calibration_catalog_file_path=calibration_catalog_file_path,transfer_calibration=transfer_calibration,$
    snapshot_healpix_export=snapshot_healpix_export,save_visibilities=save_visibilities,error_method=error_method,$
    firstpass=firstpass,return_cal_visibilities=return_cal_visibilities,cmd_args=cmd_args,silent=silent,_Extra=extra

scope_str=scope_traceback(/structure)
routine_stack=scope_str.routine
n_routine=N_Elements(routine_stack)
general_obs_i=where(routine_stack EQ routine_stack[n_routine-1],n_general_obs_match)
IF n_general_obs_match GT 1 THEN BEGIN
    print,"ERROR! Nested call to general_obs.pro! This is most likely the result of starting a new run after a previous run crashed and did not exit cleanly."
    print,"This is a problem because memory from the previous run is still in use. Clean up your IDL session with a RETALL or .RESET_SESSION and try again."
    print,"Returning..."
    RETURN
ENDIF

except=!except
!except=0 
heap_gc
IF Keyword_Set(!Journal) THEN Journal ;if logs are somehow still being written from a different run, clean that up first

IF N_Elements(error_method) EQ 0 THEN error_method=0
ON_ERROR,error_method

IF ~Keyword_Set(silent) THEN BEGIN
    git,'describe',result=code_version,repo_path=rootdir('fhd'),args='--long --dirty'
    print,"Using FHD version: "+code_version
ENDIF
;Set which procedures are to be run
IF Keyword_Set(firstpass) THEN BEGIN
    IF N_Elements(return_cal_visibilities) EQ 0 THEN return_cal_visibilities=1
    mapfn_recalculate=0
    deconvolve=0
    IF N_Elements(export_images) EQ 0 THEN export_images=1
ENDIF
IF N_Elements(recalculate_all) EQ 0 THEN recalculate_all=0
IF N_Elements(export_images) EQ 0 THEN export_images=0
IF N_Elements(cleanup) EQ 0 THEN cleanup=0
IF N_elements(snapshot_healpix_export) EQ 0 THEN snapshot_healpix_export=0

;Set up paths
;Convoluted way of setting up 'instrument' for use here, while still leaving it to be passed in Extra
IF N_Elements(extra) GT 0 THEN IF Tag_exist(extra,'instrument') THEN instrument=extra.instrument
IF N_Elements(instrument) EQ 0 THEN instrument='mwa'
IF N_Elements(version) EQ 0 THEN version=0
IF N_Elements(data_directory) EQ 0 THEN data_directory=$
    rootdir('mwa')+filepath('',root='DATA',subdir=['X16','Drift'])
IF N_Elements(output_directory) EQ 0 THEN output_directory=data_directory
IF N_Elements(vis_file_list) EQ 0 THEN vis_file_list=file_search(data_directory,'*.uvfits',count=n_files)
IF StrLowCase(Strmid(vis_file_list[0],3,/reverse)) EQ '.txt' THEN $
    vis_file_list=string_list_read(vis_file_list)

IF N_Elements(fhd_file_list) EQ 0 THEN fhd_file_list=fhd_path_setup(vis_file_list,version=version)
IF N_Elements(healpix_path) EQ 0 THEN healpix_path=$
    fhd_path_setup(output_dir=output_directory,subdir='Healpix',output_filename='Combined_obs',version=version)
IF N_Elements(catalog_file_path) EQ 0 THEN catalog_file_path=$
    filepath('MRC_full_radio_catalog.fits',root=rootdir('FHD'),subdir='catalog_data')
IF N_Elements(calibration_catalog_file_path) EQ 0 THEN calibration_catalog_file_path=$
    filepath(instrument+'_calibration_source_list.sav',root=rootdir('FHD'),subdir='catalog_data')
n_files=N_Elements(vis_file_list)

;Set which files to restore or recalculate (if the file is not found and needed, it will be recalculated
IF N_Elements(mapfn_recalculate) EQ 0 THEN mapfn_recalculate=recalculate_all
IF N_Elements(flag_visibilities) EQ 0 THEN flag_visibilities=0
IF N_Elements(flag_calibration) EQ 0 THEN flag_calibration=1
IF N_Elements(grid_recalculate) EQ 0 THEN grid_recalculate=recalculate_all
IF N_Elements(snapshot_recalculate) EQ 0 THEN snapshot_recalculate=recalculate_all
IF Keyword_Set(simultaneous) THEN BEGIN
    snapshot_recalculate1=snapshot_recalculate
    snapshot_recalculate=0
    deconvolve=0
    export_sim=export_images
    export_images=0
ENDIF
IF N_Elements(save_visibilities) EQ 0 THEN save_visibilities=1 
IF N_Elements(transfer_mapfn) EQ 0 THEN transfer_mapfn=0
IF size(transfer_mapfn,/type) EQ 7 THEN IF StrLowCase(Strmid(transfer_mapfn[0],3,/reverse)) EQ '.txt' THEN $
    transfer_mapfn=string_list_read(transfer_mapfn,data_directory=data_directory)
    
;NOTE: IF transfer_mapfn is ever supplied as an array, all later calls to fhd_main will need to be updated
    
IF N_Elements(transfer_calibration) EQ 0 THEN transfer_calibration=0
IF size(transfer_calibration,/type) EQ 7 THEN IF StrLowCase(Strmid(transfer_calibration[0],3,/reverse)) EQ '.txt' THEN $
    transfer_calibration=string_list_read(transfer_calibration,data_directory=data_directory)
CASE 1 OF
    Keyword_Set(transfer_mapfn):transfer_file=transfer_mapfn
    Keyword_Set(transfer_weights):transfer_file=transfer_weights
    Keyword_Set(transfer_calibration):transfer_file=transfer_calibration
    ELSE:transfer_file=''
ENDCASE

IF N_Elements(combine_healpix) EQ 0 THEN combine_healpix=0

;Set up gridding and deconvolution parameters
IF N_Elements(complex_beam) EQ 0 THEN complex_beam=1
IF N_Elements(n_pol) EQ 0 THEN n_pol=2
IF N_Elements(precess) EQ 0 THEN precess=0 ;set to 1 ONLY for X16 PXX scans (i.e. Drift_X16.pro)

;dimension=1024.

;Set up output image parameters
IF N_Elements(pad_uv_image) EQ 0 THEN pad_uv_image=1. ;grid output images at a higher resolution if set (ignored for quickview images)
IF N_Elements(image_filter_fn) EQ 0 THEN image_filter_fn='filter_uv_uniform' ;applied ONLY to output images

IF N_Elements(start_fi) EQ 0 THEN start_fi=0
fi=Long(start_fi)
IF N_Elements(end_fi) GT 0 THEN n_files=end_fi+1 ;changed to allow end_fi and update to both be specified

IF ~Keyword_Set(update_file_list) THEN BEGIN
    n_files-=start_fi
    fi_arr=lindgen(n_files)+start_fi
    fi=0L
    start_fi=0
    
    IF N_Elements(skip_fi) GT 0 THEN BEGIN
        fi_cut=lonarr(n_files)
        fi_cut[skip_fi]=1
        fi_uncut=where(fi_cut NE 1,n_files)
        fi_arr=fi_arr[fi_uncut]
    ENDIF
    vis_file_list=vis_file_list[fi_arr]
    fhd_file_list=fhd_file_list[fi_arr]
    IF size(transfer_file,/type) EQ 7 THEN BEGIN
        fi_init=where(file_basename(fhd_file_list) EQ file_basename(transfer_file[0]),$
            n_fi_init,complement=fi_remainder,ncomplement=n_fi_remainder)
        CASE 1 OF
            (n_fi_init NE 1):fi_order=lindgen(n_files) ;do nothing
            (n_fi_remainder EQ 0):fi_order=lindgen(n_files) ;do nothing
            ELSE: fi_order=[fi_init,fi_remainder]
        ENDCASE
        vis_file_list=vis_file_list[fi_order]
        fhd_file_list=fhd_file_list[fi_order]
    ENDIF
ENDIF
WHILE fi LT n_files DO BEGIN
    IF ~Keyword_Set(silent) THEN print,String(format='("On observation ",A," of ",A)',Strn(Floor(fi-start_fi+1)),Strn(Floor(n_files-start_fi)))
    undefine_fhd,status_str
    print, fhd_file_list[fi]
    
    fhd_main,vis_file_list[fi],status_str,file_path_fhd=fhd_file_list[fi],n_pol=n_pol,recalculate_all=recalculate_all,$
        transfer_mapfn=transfer_mapfn,transfer_weights=transfer_weights,$
        mapfn_recalculate=mapfn_recalculate,flag_visibilities=flag_visibilities,grid_recalculate=grid_recalculate,$
        silent=silent,deconvolve=deconvolve,catalog_file_path=catalog_file_path,$
        export_images=export_images,dimension=dimension,image_filter_fn=image_filter_fn,pad_uv_image=pad_uv_image,$
        complex=complex_beam,precess=precess,error=error,$
        cleanup=cleanup,save_visibilities=save_visibilities,$
        calibration_catalog_file_path=calibration_catalog_file_path,transfer_calibration=transfer_calibration,$
        flag_calibration=flag_calibration,return_cal_visibilities=return_cal_visibilities,$
        snapshot_healpix_export=snapshot_healpix_export,snapshot_recalculate=snapshot_recalculate,$
        split_ps_export=split_ps_export,cmd_args=cmd_args,_Extra=extra
    IF ~Keyword_Set(error) THEN IF Tag_exist(status_str,'complete') THEN status_str.complete=1
    fhd_save_io,status_str,file_path_fhd=fhd_file_list[fi],/text
    IF N_Elements(status_arr) EQ 0 THEN status_arr=status_str ELSE status_arr=[status_arr,status_str]
    IF Keyword_Set(error) THEN BEGIN
        print,'###########################################################################'
        print,'###########################################################################'
        print,'###########################################################################'
        print,'Error encountered!'
        print,'###########################################################################'
        print,'###########################################################################'
        print,'###########################################################################'
    ENDIF ELSE $
        IF N_Elements(fi_use) GT 0 THEN fi_use=[fi_use,fi] ELSE fi_use=fi
    fi+=1.
    IF Keyword_Set(update_file_list) THEN BEGIN ;use this if simultaneously downloading and deconvolving observations
        vis_file_list=file_search(data_directory,'*.uvfits',count=n_files)
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
    fhd_multi_wrap,fhd_file_list,status_arr,N_simultaneous=N_simultaneous,n_pol=n_pol,$
        silent=silent,catalog_file_path=catalog_file_path,$
        export_images=export_images,image_filter_fn=image_filter_fn,pad_uv_image=pad_uv_image,$
        transfer_mapfn=transfer_mapfn,_Extra=extra    
    heap_gc
    IF Keyword_Set(export_sim) THEN FOR fi=0L,n_files_use-1 DO BEGIN
        fhd_main,vis_file_list[fi],status_arr[fi],file_path_fhd=fhd_file_list[fi],n_pol=n_pol,/force_no_data,$
            transfer_mapfn=transfer_mapfn,mapfn_recalculate=0,flag_visibilities=0,grid=0,$
            silent=silent,deconvolve=0,catalog_file_path=catalog_file_path,$
            export_images=1,dimension=dimension,image_filter_fn=image_filter_fn,pad_uv_image=pad_uv_image,$
            error=error,snapshot_recalculate=snapshot_recalculate1,_Extra=extra
        fhd_save_io,status_arr[fi],file_path_fhd=fhd_file_list[fi],/text
    ENDFOR
ENDIF

combine_obs_sources,fhd_file_list,status_arr,restore_last=0,output_path=healpix_path,_Extra=extra
map_projection='orth'
IF Keyword_Set(combine_healpix) THEN BEGIN
    combine_obs_healpix,fhd_file_list,status_arr,hpx_inds,obs_arr,n_obs_hpx=n_obs_hpx,stokes_dirty_hpx=stokes_dirty_hpx,$
        stokes_model_hpx=stokes_model_hpx,weights_hpx=weights_hpx,stokes_sources_hpx=stokes_sources_hpx,nside=nside,$
        output_path=healpix_path,image_filter_fn=image_filter_fn,catalog_file_path=catalog_file_path,_Extra=extra
    combine_obs_hpx_image,fhd_file_list,status_arr,hpx_inds,obs_arr,n_obs_hpx=n_obs_hpx,stokes_dirty_hpx=stokes_dirty_hpx,$
        stokes_model_hpx=stokes_model_hpx,weights_hpx=weights_hpx,stokes_sources_hpx=stokes_sources_hpx,nside=nside,$
        output_path=healpix_path,image_filter_fn=image_filter_fn,_Extra=extra
ENDIF

IF Keyword_Set(cleanup) THEN FOR fi=0L,n_files_use-1 DO fhd_cleanup,fhd_file_list[fi]

heap_gc
!except=except
END
