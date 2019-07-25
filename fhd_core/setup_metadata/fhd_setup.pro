FUNCTION fhd_setup,file_path_vis,status_str,export_images=export_images,cleanup=cleanup,recalculate_all=recalculate_all,$
    mapfn_recalculate=mapfn_recalculate,grid_recalculate=grid_recalculate,$
    n_pol=n_pol,flag_visibilities=flag_visibilities,deconvolve=deconvolve,transfer_mapfn=transfer_mapfn,$
    transfer_weights=transfer_weights,healpix_recalculate=healpix_recalculate,$
    file_path_fhd=file_path_fhd,force_data=force_data,force_no_data=force_no_data,$
    calibrate_visibilities=calibrate_visibilities,transfer_calibration=transfer_calibration,$
    weights_grid=weights_grid,save_visibilities=save_visibilities,$
    snapshot_healpix_export=snapshot_healpix_export,log_store=log_store,compatibility_mode=compatibility_mode

IF Keyword_Set(cleanup) THEN IF cleanup GT 0 THEN no_save=1 ;set to not save the mapping function to disk if it will be just deleted later anyway

;IF N_Elements(GPU_enable) EQ 0 THEN GPU_enable=0
;IF Keyword_Set(GPU_enable) THEN BEGIN
;    Defsysv,'GPU',exist=gpuvar_exist
;    IF gpuvar_exist eq 0 THEN GPUinit
;    IF !GPU.mode NE 1 THEN GPU_enable=0
;ENDIF
fhd_save_io,status_str,file_path_fhd=file_path_fhd,var='status_str',compatibility_mode=compatibility_mode
IF Keyword_Set(compatibility_mode) THEN BEGIN
    names=Tag_names(status_str)
    FOR ni=0,N_Elements(names)-1 DO BEGIN
        FOR pi=0,N_Elements(status_str.(ni))-1 DO $
            fhd_save_io,status_str,file_path_fhd=file_path_fhd,var=names[ni],pol=pi,/compatibility_mode
    ENDFOR
ENDIF

IF N_Elements(recalculate_all) EQ 0 THEN recalculate_all=1
IF N_Elements(calibrate_visibilities) EQ 0 THEN calibrate_visibilities=0
IF N_Elements(grid_recalculate) EQ 0 THEN grid_recalculate=recalculate_all
IF N_Elements(flag_visibilities) EQ 0 THEN flag_visibilities=0
IF N_Elements(transfer_mapfn) EQ 0 THEN transfer_mapfn=0
IF N_Elements(save_visibilities) EQ 0 THEN save_visibilities=1
IF N_Elements(healpix_recalculate) EQ 0 THEN healpix_recalculate=recalculate_all
;IF N_Elements(beam_recalculate) EQ 0 THEN IF status_str.psf EQ 0 THEN beam_recalculate=1 $
;    ELSE beam_recalculate=recalculate_all

IF Keyword_Set(n_pol) THEN n_pol1=n_pol ELSE BEGIN
    IF status_str.obs GT 0 THEN BEGIN
        fhd_save_io,status_str,obs_temp,file_path_fhd=file_path_fhd,var_name='obs',/restore
        n_pol1=obs_temp.n_pol
    ENDIF ELSE n_pol1=4 ;doesn't really matter what this is set to if the obs structure doesn't even exist
ENDELSE

IF Min(status_str.grid_uv[0:n_pol1-1]) LE 0 THEN grid_recalculate=1
IF Tag_exist(status_str,"vis_weights") THEN weight_flag=status_str.vis_weights ELSE weight_flag=status_str.flag_arr
data_flag=status_str.obs*status_str.params*weight_flag*status_str.psf*status_str.jones
;IF Keyword_set(calibrate_visibilities) THEN data_flag=1
IF Keyword_Set(save_visibilities) THEN data_flag*=Min(status_str.vis_ptr[0:n_pol1-1])

IF Keyword_Set(transfer_mapfn) AND size(transfer_mapfn,/type) NE 7 THEN transfer_mapfn=file_basename(file_path_fhd)
IF Keyword_Set(transfer_mapfn) THEN transfer_weights=transfer_mapfn
IF Keyword_Set(transfer_weights) AND size(transfer_weights,/type) NE 7 THEN transfer_weights=file_basename(file_path_fhd)
IF size(transfer_mapfn,/type) EQ 7 THEN BEGIN
    IF file_basename(file_path_fhd) EQ transfer_mapfn THEN BEGIN
        fhd_save_io,status_mapfn,file_path_fhd=file_path_fhd,transfer=transfer_mapfn
        IF Min(status_mapfn.map_fn[0:n_pol1-1]) LE 0 THEN mapfn_recalculate=1 ELSE $
            IF N_Elements(mapfn_recalculate) EQ 0 THEN mapfn_recalculate=recalculate_all
    ENDIF ELSE mapfn_recalculate=0
ENDIF ELSE status_mapfn=status_str

IF N_Elements(deconvolve) EQ 0 THEN IF status_str.fhd LE 0 THEN deconvolve=1
IF Keyword_Set(deconvolve) THEN BEGIN
    IF N_Elements(mapfn_recalculate) EQ 0 THEN mapfn_recalculate=recalculate_all
    IF Min(status_mapfn.map_fn[0:n_pol1-1]) LE 0 THEN BEGIN
        mapfn_recalculate=1
        IF Keyword_Set(transfer_mapfn) THEN BEGIN
            IF file_basename(file_path_fhd) NE transfer_mapfn THEN BEGIN
                print,"ERROR: invalid transfer_mapfn. Mapfn NOT transferred!"
                undefine_fhd,transfer_mapfn
            ENDIF
        ENDIF
    ENDIF
    IF Min(status_str.grid_uv[0:n_pol1-1]) LE 0 THEN grid_recalculate=1
ENDIF ELSE IF N_Elements(mapfn_recalculate) EQ 0 THEN mapfn_recalculate=0
IF mapfn_recalculate GT 0 THEN grid_recalculate=1
IF grid_recalculate GT 0 THEN data_flag=0

;IF Keyword_Set(beam_recalculate) THEN BEGIN
;    status_str.psf=0
;    status_str.antenna=0
;    status_str.jones=0
;ENDIF
status_str.hpx_cnv=0
IF Keyword_Set(mapfn_recalculate) THEN grid_recalculate=1
IF Keyword_Set(grid_recalculate) THEN BEGIN
    status_str.map_fn=0
    status_str.grid_uv=0
    status_str.weights_uv=0
    status_str.grid_uv_model=0
ENDIF
IF Keyword_Set(healpix_recalculate) THEN BEGIN
    status_str.healpix_cube=0
    status_str.hpx_even=0
    status_str.hpx_odd=0
ENDIF

IF Keyword_Set(mapfn_recalculate) THEN $
    IF Keyword_Set(healpix_recalculate) AND Keyword_Set(snapshot_healpix_export) THEN save_visibilities=1

IF Keyword_Set(force_data) THEN data_flag=0
IF Keyword_Set(force_no_data) THEN data_flag=1
IF data_flag EQ 0 THEN BEGIN
    FOR ti=0,n_tags(status_str)-1 DO status_str.(ti)=0
ENDIF

fhd_save_io,status_str,file_path_fhd=file_path_fhd,var='status_str',/force_set,/text
RETURN,data_flag
END
