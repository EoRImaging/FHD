FUNCTION vis_calibrate,vis_ptr,obs,psf,params,cal=cal,flag_arr=flag_arr,model_ptr=model_ptr,source_arr=source_arr,$
    min_cal_baseline=min_cal_baseline,max_cal_baseline=max_cal_baseline,gain_arr_ptr=gain_arr_ptr,$
    transfer_calibration=transfer_calibration,timing=timing,file_path_fhd=file_path_fhd,$
    n_cal_iter=n_cal_iter,error=error,preserve_visibilities=preserve_visibilities,_Extra=extra
t0_0=Systime(1)
error=0
heap_gc

IF N_Elements(n_cal_iter) EQ 0 THEN n_cal_iter=10L

IF Keyword_Set(transfer_calibration) THEN BEGIN
    IF size(transfer_calibration,/type) EQ 7 THEN $
        IF file_test(transfer_calibration) THEN cal=getvar_savefile(transfer_calibration,'cal')
    IF size(cal,/type) EQ 8 THEN BEGIN
        vis_cal=vis_calibration_apply(cal,vis_ptr,preserve_original=preserve_visibilities)
        timing=Systime(1)-t0_0
        RETURN,vis_cal
    ENDIF ELSE IF Max(Ptr_valid(gain_arr_ptr)) THEN BEGIN
        cal=vis_struct_init_cal(obs)
        vis_cal=vis_calibration_apply(cal,vis_ptr,preserve_original=preserve_visibilities)
        timing=Systime(1)-t0_0
        RETURN,vis_cal
    ENDIF ELSE BEGIN
        print,"Invalid calibration supplied!"
        error=1
        RETURN,vis_ptr
    ENDELSE
ENDIF
vis_model_ptr=vis_source_model(source_arr,obs,psf,params,flag_arr,model_uv_arr=model_ptr,$
    file_path=file_path_fhd,timing=model_timing,silent=silent,_Extra=extra)


cal=vis_struct_init_cal(obs,params)
pol_names=['xx','yy','xy','yx']

;extract information from the structures
min_baseline=obs.min_baseline
max_baseline=obs.max_baseline
IF N_Elements(min_cal_baseline) EQ 0 THEN min_cal_baseline=min_baseline
IF N_Elements(max_cal_baseline) EQ 0 THEN max_cal_baseline=max_baseline

n_pol=cal.n_pol
n_freq=cal.n_freq
n_tile=cal.n_tile
n_time=cal.n_time

tile_A_i=cal.tile_A-1
tile_B_i=cal.tile_B-1
freq_arr=cal.freq
bin_offset=cal.bin_offset

;calibration loop
;vis_use=Ptrarr(n_pol,/allocate) 
;FOR pol_i=0,n_pol-1 DO *vis_use[pol_i]=*vis_ptr[pol_i]
FOR i=0L,n_cal_iter-1 DO BEGIN
    FOR pol_i=0,n_pol-1 DO BEGIN
        gain_old=*cal.gain[pol_i]
        vis_use=vis_calibration_apply(cal,vis_ptr,/preserve_original,pol_i=pol_i)
        vis_use/=*vis_model_ptr[pol_i]
        

    
    ENDFOR
    cal=vis_struct_update_cal(cal,gain_new,obs=obs)
ENDFOR

vis_cal=vis_calibration_apply(cal,vis_ptr,preserve_original=preserve_visibilities)
RETURN,vis_cal
END