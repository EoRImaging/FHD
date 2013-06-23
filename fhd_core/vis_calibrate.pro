PRO vis_calibrate,vis_ptr,obs,psf,params,cal=cal,flag_arr=flag_arr,model_ptr=model_ptr,source_arr=source_arr,$
    min_cal_baseline=min_cal_baseline,max_cal_baseline=max_cal_baseline,gain_arr_ptr=gain_arr_ptr,$
    transfer_calibration=transfer_calibration,timing=timing,file_path_fhd=file_path_fhd,$
    error=error,_Extra=extra
t0_0=Systime(1)
error=0
heap_gc
IF Keyword_Set(transfer_calibration) THEN BEGIN
    IF size(transfer_calibration,/type) EQ 7 THEN $
        IF file_test(transfer_calibration) THEN cal=getvar_savefile(transfer_calibration,'cal')
    IF size(cal,/type) EQ 8 THEN BEGIN
        vis_calibration_apply,cal,vis_ptr
        timing=Systime(1)-t0_0
        RETURN
    ENDIF ELSE IF Max(Ptr_valid(gain_arr_ptr)) THEN BEGIN
        cal=vis_struct_init_cal(obs)
        vis_calibration_apply,cal,vis_ptr
        timing=Systime(1)-t0_0
        RETURN
    ENDIF ELSE BEGIN
        print,"Invalid calibration supplied!"
        error=1
        RETURN
    ENDELSE
ENDIF
vis_model_ptr=vis_source_model(source_arr,obs,psf,params,flag_arr,model_uv_arr=model_ptr,$
    file_path=file_path_fhd,timing=model_timing,silent=silent,_Extra=extra)


cal=vis_struct_init_cal(obs)
pol_names=['xx','yy','xy','yx']

;extract information from the structures
n_pol=obs.n_pol
n_freq=obs.n_freq
n_tile=obs.n_tile
dimension=Float(obs.dimension)
elements=Float(obs.elements)
kbinsize=obs.kpix
kx_span=kbinsize*dimension ;Units are # of wavelengths
ky_span=kx_span
min_baseline=obs.min_baseline
max_baseline=obs.max_baseline
IF N_Elements(min_cal_baseline) EQ 0 THEN min_cal_baseline=min_baseline
IF N_Elements(max_cal_baseline) EQ 0 THEN max_cal_baseline=max_baseline

tile_A_i=(*obs.baseline_info).tile_A
tile_B_i=(*obs.baseline_info).tile_B
freq_arr=(*obs.baseline_info).freq
bin_offset=(*obs.baseline_info).bin_offset

END