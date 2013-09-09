FUNCTION vis_calibrate,vis_ptr,cal,obs,psf,params,flag_ptr=flag_ptr,model_uv_arr=model_uv_arr,$
    transfer_calibration=transfer_calibration,timing=timing,file_path_fhd=file_path_fhd,$
    n_cal_iter=n_cal_iter,error=error,preserve_visibilities=preserve_visibilities,$
    calibration_source_list=calibration_source_list,debug=debug,gain_arr_ptr=gain_arr_ptr,$
    return_cal_model=return_cal_model,silent=silent,_Extra=extra
t0_0=Systime(1)
error=0
heap_gc

IF Keyword_Set(transfer_calibration) THEN BEGIN
    IF size(transfer_calibration,/type) EQ 7 THEN BEGIN
        IF file_test(transfer_calibration) EQ 0 THEN BEGIN
            print,'File:'+transfer_calibration+' not found!'
            error=1
            RETURN,vis_ptr
        ENDIF
        CASE StrLowCase(Strmid(transfer_calibration[0],3,/reverse)) OF
            '.sav':cal=getvar_savefile(transfer_calibration,'cal')
            '.txt':BEGIN
                textfast,gain_arr,/read,file_path=transfer_calibration
                gain_arr_ptr=Ptr_new(gain_arr)
                vis_cal=vis_calibrate(vis_ptr,cal,obs,psf,params,flag_ptr=flag_ptr,file_path_fhd=file_path_fhd,$
                    transfer_calibration=1,timing=timing,error=error,gain_arr_ptr=gain_arr_ptr,$
                    calibration_source_list=calibration_source_list,_Extra=extra)
                RETURN,vis_cal
            END
            '.npz':BEGIN
                gain_arr=read_numpy(transfer_calibration)
                gain_arr_ptr=Ptr_new(gain_arr)
                vis_cal=vis_calibrate(vis_ptr,cal,obs,psf,params,flag_ptr=flag_ptr,file_path_fhd=file_path_fhd,$
                    transfer_calibration=1,timing=timing,error=error,gain_arr_ptr=gain_arr_ptr,$
                    calibration_source_list=calibration_source_list,_Extra=extra)
                RETURN,vis_cal
            END
            '.npy':BEGIN
                gain_arr=read_numpy(transfer_calibration)
                gain_arr_ptr=Ptr_new(gain_arr)
                vis_cal=vis_calibrate(vis_ptr,cal,obs,psf,params,flag_ptr=flag_ptr,file_path_fhd=file_path_fhd,$
                    transfer_calibration=1,timing=timing,error=error,gain_arr_ptr=gain_arr_ptr,$
                    calibration_source_list=calibration_source_list,_Extra=extra)
                RETURN,vis_cal
            END
            ELSE: BEGIN
                print,'Unknown file format: ',transfer_calibration
                error=1
                RETURN,vis_ptr
            ENDELSE
        ENDCASE
    ENDIF
    IF size(cal,/type) EQ 8 THEN BEGIN
        vis_cal=vis_calibration_apply(vis_ptr,cal,preserve_original=preserve_visibilities)
        timing=Systime(1)-t0_0
        RETURN,vis_cal
    ENDIF ELSE IF Max(Ptr_valid(gain_arr_ptr)) THEN BEGIN
        cal=vis_struct_init_cal(obs,params)
        vis_cal=vis_calibration_apply(vis_ptr,cal,preserve_original=preserve_visibilities)
        timing=Systime(1)-t0_0
        RETURN,vis_cal
    ENDIF ELSE BEGIN
        print,"Invalid calibration supplied!"
        error=1
        timing=Systime(1)-t0_0
        RETURN,vis_ptr
    ENDELSE
ENDIF

cal=vis_struct_init_cal(obs,params,source_list=calibration_source_list,_Extra=extra)
CASE size(initial_calibration,/type) OF
    0:;do nothing if undefined
    7:BEGIN
        file_path_use=initial_calibration
        IF StrLowCase(Strmid(file_path_use,0,3,/reverse_offset)) NE 'sav' THEN file_path_use+='.sav'
        IF file_test(file_path_use) EQ 0 THEN file_path_use=filepath(file_path_use,root=file_dirname(file_path_fhd))
        cal_init=getvar_savefile(file_path_use,'cal')
        cal.gain=cal_init.gain
    END
    8:cal.gain=initial_calibration.gain
    10:cal.gain=initial_calibration
    ELSE:
ENDCASE

vis_model_ptr=vis_source_model(calibration_source_list,obs,psf,params,cal,flag_ptr,model_uv_arr=model_uv_arr,$
    timing=model_timing,silent=silent,error=error,_Extra=extra)    
t1=Systime(1)-t0_0

IF Keyword_Set(error) THEN BEGIN
    timing=Systime(1)-t0_0
    RETURN,vis_ptr
ENDIF
pol_names=['xx','yy','xy','yx']

;extract information from the structures
n_pol=cal.n_pol
n_freq=cal.n_freq
n_tile=cal.n_tile
n_time=cal.n_time

tile_A_i=cal.tile_A-1
tile_B_i=cal.tile_B-1
freq_arr=cal.freq
bin_offset=cal.bin_offset
n_baselines=bin_offset[1]
tile_A_i=tile_A_i[0:bin_offset[1]-1]
tile_B_i=tile_B_i[0:bin_offset[1]-1]

IF N_Elements(flag_ptr) EQ 0 THEN BEGIN
    flag_init=Replicate(1.,n_freq,n_baselines*Float(n_time))
    flag_ptr=Ptrarr(n_pol,/allocate)
    FOR pol_i=0,n_pol-1 DO *flag_ptr[pol_i]=flag_init
ENDIF

;calibration loop
t2_a=Systime(1)
cal=vis_calibrate_subroutine(vis_ptr,vis_model_ptr,flag_ptr,obs,params,cal,_Extra=extra)
t3_a=Systime(1)
t2=t2_a-t3_a

vis_cal=vis_calibration_apply(vis_ptr,cal)
t3=Systime(1)-t3_a
timing=Systime(1)-t0_0
IF not Keyword_Set(silent) THEN print,timing,t1,t2,t3
RETURN,vis_cal
END
