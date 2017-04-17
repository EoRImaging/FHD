FUNCTION vis_calibrate,vis_ptr,cal,obs,status_str,psf,params,jones,vis_weight_ptr=vis_weight_ptr,model_uv_arr=model_uv_arr,$
    transfer_calibration=transfer_calibration,timing=timing,file_path_fhd=file_path_fhd,$
    n_cal_iter=n_cal_iter,error=error,preserve_visibilities=preserve_visibilities,$
    debug=debug,gain_arr_ptr=gain_arr_ptr,calibration_flag_iterate=calibration_flag_iterate,$
    return_cal_visibilities=return_cal_visibilities,silent=silent,initial_calibration=initial_calibration,$
    calibration_visibilities_subtract=calibration_visibilities_subtract,vis_baseline_hist=vis_baseline_hist,$
    flag_calibration=flag_calibration,vis_model_arr=vis_model_arr,calibration_bandpass_iterate=calibration_bandpass_iterate,$
    calibration_auto_fit=calibration_auto_fit,_Extra=extra
t0_0=Systime(1)
error=0
timing=-1
heap_gc
IF N_Elements(flag_calibration) EQ 0 THEN flag_calibration=1

IF Keyword_Set(transfer_calibration) THEN BEGIN
    IF size(transfer_calibration,/type) EQ 7 THEN BEGIN
        cal_file_use=transfer_calibration
        IF file_test(cal_file_use,/directory) THEN BEGIN
            fhd_save_io,file_path_fhd=file_path_fhd,transfer=transfer_calibration,var='cal',path_use=cal_file_use2,_Extra=extra 
            cal_file_use2+='.sav'
            IF file_test(cal_file_use2) THEN cal_file_use=cal_file_use2 ELSE BEGIN
                print,'File:'+cal_file_use2+' not found!'
                error=1
                RETURN,vis_ptr
            ENDELSE
        ENDIF ELSE BEGIN
            IF file_test(cal_file_use) EQ 0 THEN BEGIN
                fhd_save_io,file_path_fhd=cal_file_use,var='cal',path_use=cal_file_use2,_Extra=extra
                cal_file_use2+='.sav'
                IF file_test(cal_file_use2) THEN cal_file_use=cal_file_use2 ELSE BEGIN
                    print,'File:'+cal_file_use2+' not found!'
                    error=1
                    RETURN,vis_ptr
                ENDELSE
            ENDIF
        ENDELSE
        CASE StrLowCase(Strmid(cal_file_use[0],3,/reverse)) OF
            '.sav':BEGIN
                cal=getvar_savefile(cal_file_use,'cal')
                gain_arr_ptr=cal.gain
                IF ~Keyword_Set(cal.cal_origin) THEN cal.cal_origin=cal_file_use
                cal=fhd_struct_init_cal(obs,params,calibration_origin=cal.cal_origin,gain_arr_ptr=cal.gain,_Extra=extra)
            END
            '.txt':BEGIN
                textfast,gain_arr,/read,file_path=cal_file_use
                gain_arr_ptr=Ptr_new(gain_arr)
                cal=fhd_struct_init_cal(obs,params,calibration_origin=cal_file_use,gain_arr_ptr=gain_arr_ptr,_Extra=extra)
            END
            '.npz':BEGIN
                gain_arr=read_numpy(cal_file_use)
                gain_arr_ptr=Ptr_new(gain_arr)
                cal=fhd_struct_init_cal(obs,params,calibration_origin=cal_file_use,gain_arr_ptr=gain_arr_ptr,_Extra=extra)
            END
            '.npy':BEGIN
                gain_arr=read_numpy(cal_file_use)
                gain_arr_ptr=Ptr_new(gain_arr)
                cal=fhd_struct_init_cal(obs,params,calibration_origin=cal_file_use,gain_arr_ptr=gain_arr_ptr,_Extra=extra)
            END
            ELSE: BEGIN
                print,'Unknown file format: ',cal_file_use
                error=1
                RETURN,vis_ptr
            ENDELSE
        ENDCASE
    ENDIF
;    IF Keyword_Set(flag_calibration) THEN vis_calibration_flag,obs,cal,_Extra=extra
;    nc_pol=cal.n_pol
;    cal_base=cal & FOR pol_i=0,nc_pol-1 DO cal_base.gain[pol_i]=Ptr_new(*cal.gain[pol_i])
;    IF tag_exist(cal,'bandpass') THEN bandpass_calibrate=cal.bandpass
;    IF tag_exist(cal,'polyfit') THEN calibration_polyfit=cal.polyfit
;    
;    IF Keyword_Set(bandpass_calibrate) THEN BEGIN
;        cal_bandpass=vis_cal_bandpass(cal,obs,cal_remainder=cal_remainder,file_path_fhd=file_path_fhd)
;        IF Keyword_Set(calibration_polyfit) THEN BEGIN
;            cal_polyfit=vis_cal_polyfit(cal_remainder,obs,degree=calibration_polyfit)
;            cal=vis_cal_combine(cal_polyfit,cal_bandpass)
;        ENDIF ELSE cal=cal_bandpass
;    ENDIF ELSE IF Keyword_Set(calibration_polyfit) THEN cal=vis_cal_polyfit(cal,obs,degree=calibration_polyfit)
;    vis_cal=vis_calibration_apply(vis_ptr,cal)
;    cal_res=vis_cal_subtract(cal_base,cal)
;    
;    IF Keyword_Set(return_cal_visibilities) OR Keyword_Set(calibration_visibilities_subtract) THEN BEGIN
;    
;    ENDIF
    vis_cal=vis_calibration_apply(vis_ptr,cal)
    timing=Systime(1)-t0_0
    RETURN,vis_cal
ENDIF

fill_model_vis=1
vis_model_arr=vis_source_model(cal.skymodel,obs,status_str,psf,params,vis_weight_ptr,cal,jones,model_uv_arr=model_uv_arr,fill_model_vis=fill_model_vis,$
    timing=model_timing,silent=silent,error=error,/calibration_flag,spectral_model_uv_arr=spectral_model_uv_arr,_Extra=extra)    
t1=Systime(1)-t0_0

vis_auto=vis_extract_autocorr(obs,vis_arr = vis_ptr,/time_average,auto_tile_i=auto_tile_i)
IF Keyword_Set(cal.auto_initialize) THEN BEGIN
    IF Keyword_Set(vis_auto) THEN $
        initial_calibration=vis_cal_auto_init(obs,psf,cal,vis_arr=vis_ptr,vis_model_arr=vis_model_arr,_Extra=extra) $
    ELSE print,"calibration_auto_initialize is set, but autocorrelation visibilities are missing. Skipping." 
ENDIF
   
IF Keyword_Set(fill_model_vis) THEN vis_auto_model=vis_extract_autocorr(obs,vis_arr = vis_model_arr,/time_average,auto_tile_i=auto_tile_i)

;IF N_Elements(cal) EQ 0 THEN cal=fhd_struct_init_cal(obs,params,_Extra=extra)
CASE size(initial_calibration,/type) OF
    0:;do nothing if undefined
    
    7:BEGIN ;type code 7 is string
        file_path_use=initial_calibration
        IF StrLowCase(Strmid(file_path_use,2,3,/reverse_offset)) NE 'sav' THEN file_path_use+='.sav'
        IF file_test(file_path_use) EQ 0 THEN file_path_use=filepath(file_path_use,root=file_dirname(file_path_fhd))
        IF file_test(file_path_use) THEN BEGIN
            cal_init=getvar_savefile(file_path_use,'cal')
            cal.gain=cal_init.gain
            print,'Using initial calibration solution from '+initial_calibration
        ENDIF else print, 'Initial calibration file not found'
    END
    8:cal.gain=initial_calibration.gain ;type code 8 is structure
    10:cal.gain=initial_calibration ;type code 10 is pointer
    ELSE:IF Keyword_Set(initial_calibration) THEN initial_calibration=file_path_fhd+'_cal' ;if set to a numeric type, assume this calibration solution will be wanted for future iterations
ENDCASE

IF Keyword_Set(error) THEN BEGIN
    timing=Systime(1)-t0_0
    RETURN,vis_ptr
ENDIF
pol_names=obs.pol_names

;extract information from the structures
n_pol=obs.n_pol
nc_pol=cal.n_pol
n_freq=cal.n_freq
n_tile=cal.n_tile
n_time=cal.n_time

bandpass_calibrate=cal.bandpass
calibration_polyfit=cal.polyfit

tile_A_i=cal.tile_A-1
tile_B_i=cal.tile_B-1
freq_arr=cal.freq
bin_offset=cal.bin_offset
n_baselines=obs.nbaselines
tile_A_i=tile_A_i[0:n_baselines-1]
tile_B_i=tile_B_i[0:n_baselines-1]

IF N_Elements(vis_weight_ptr) EQ 0 THEN BEGIN
    flag_init=Replicate(1.,n_freq,n_baselines*Float(n_time))
    vis_weight_ptr=Ptrarr(n_pol,/allocate)
    FOR pol_i=0,n_pol-1 DO *vis_weight_ptr[pol_i]=flag_init
ENDIF

;calibration loop
IF N_Elements(preserve_visibilities) EQ 0 THEN preserve_visibilities=0
IF Keyword_Set(calibration_visibilities_subtract) OR Keyword_Set(vis_baseline_hist) $
    OR Keyword_Set(return_cal_visibilities) THEN preserve_visibilities=1
IF N_Elements(calibration_flag_iterate) EQ 0 THEN calibration_flag_iterate=0
;    IF Keyword_Set(flag_calibration) THEN calibration_flag_iterate=1 ELSE calibration_flag_iterate=0

t2=0
FOR iter=0,calibration_flag_iterate DO BEGIN
    t2_a=Systime(1)
    IF iter LT calibration_flag_iterate THEN preserve_flag=1 ELSE preserve_flag=preserve_visibilities
    cal=vis_calibrate_subroutine(vis_ptr,vis_model_arr,vis_weight_ptr,obs,params,cal,$
        preserve_visibilities=preserve_flag,_Extra=extra)
    t3_a=Systime(1)
    t2+=t3_a-t2_a
    
    IF Keyword_Set(flag_calibration) THEN vis_calibration_flag,obs,cal,n_tile_cut=n_tile_cut,_Extra=extra
    IF Keyword_Set(n_tile_cut) THEN BREAK

ENDFOR
cal_base=Pointer_copy(cal)

IF Keyword_Set(bandpass_calibrate) THEN BEGIN
    cal_bandpass=vis_cal_bandpass(cal,obs,cal_remainder=cal_remainder,file_path_fhd=file_path_fhd,_Extra=extra)
    IF Keyword_Set(calibration_polyfit) THEN BEGIN        
        IF Keyword_Set(calibration_bandpass_iterate) THEN BEGIN
            cal_remainder.amp_degree = 1 & cal_remainder.phase_degree=0
            cal_polyfit=vis_cal_polyfit(cal_remainder,obs,_Extra=extra)
            cal_poly_sub=vis_cal_divide(cal_base,cal_polyfit)
            cal_bandpass2=vis_cal_bandpass(cal_poly_sub,obs,file_path_fhd=file_path_fhd,_Extra=extra)
            cal_remainder2=vis_cal_divide(cal_base,cal_bandpass2)
            cal_polyfit2=vis_cal_polyfit(cal_remainder2,obs,_Extra=extra)
            cal=vis_cal_combine(cal_polyfit2,cal_bandpass2)
        ENDIF ELSE BEGIN
            cal_polyfit=vis_cal_polyfit(cal_remainder,obs,_Extra=extra)
            cal=vis_cal_combine(cal_polyfit,cal_bandpass)
        ENDELSE
    ENDIF ELSE cal=cal_bandpass
ENDIF ELSE IF Keyword_Set(calibration_polyfit) THEN cal=vis_cal_polyfit(cal,obs,_Extra=extra)
IF Keyword_Set(vis_auto_model) THEN cal_auto=vis_cal_auto_fit(obs,cal,vis_auto=vis_auto,vis_model_auto=vis_auto_model,auto_tile_i=auto_tile_i)
IF Keyword_Set(calibration_auto_fit) THEN cal_res=vis_cal_subtract(cal_base,cal_auto) ELSE cal_res=vis_cal_subtract(cal_base,cal)
basename=file_basename(file_path_fhd)
dirpath=file_dirname(file_path_fhd)
image_path=filepath(basename,root=dirpath,sub='output_images')
;make sure to plot both, if autocorrelations are used for the calibration solution
plot_cals,cal,obs,cal_res=cal_res,cal_auto=cal_auto,file_path_base=image_path,_Extra=extra

IF Keyword_Set(calibration_auto_fit) THEN cal=cal_auto
vis_cal=vis_calibration_apply(vis_ptr,cal)
cal.gain_residual=cal_res.gain
undefine_fhd,cal_base

IF Keyword_Set(vis_baseline_hist) THEN $
    vis_baseline_hist,obs,params,vis_arr=vis_cal,vis_model_arr=vis_model_arr,file_path_fhd=file_path_fhd

IF ~Keyword_Set(return_cal_visibilities) THEN preserve_visibilities=0
IF Keyword_Set(calibration_visibilities_subtract) THEN BEGIN
    FOR pol_i=0,n_pol-1 DO *vis_cal[pol_i]-=*vis_model_arr[pol_i] 
    IF tag_exist(obs,'residual') THEN obs.residual=1
ENDIF
IF ~Keyword_Set(return_cal_visibilities) THEN undefine_fhd,vis_model_arr

cal_gain_avg=Fltarr(nc_pol)
cal_res_avg=Fltarr(nc_pol)
cal_res_restrict=Fltarr(nc_pol)
cal_res_stddev=Fltarr(nc_pol)
FOR pol_i=0,nc_pol-1 DO BEGIN
    tile_use_i=where((*obs.baseline_info).tile_use,n_tile_use)
    freq_use_i=where((*obs.baseline_info).freq_use,n_freq_use)
    IF n_tile_use EQ 0 OR n_freq_use EQ 0 THEN CONTINUE
    gain_ref=extract_subarray(*cal.gain[pol_i],freq_use_i,tile_use_i)
    gain_res=extract_subarray(*cal_res.gain[pol_i],freq_use_i,tile_use_i)
    cal_gain_avg[pol_i]=Mean(Abs(gain_ref))
    cal_res_avg[pol_i]=Mean(Abs(gain_res))
    resistant_mean,Abs(gain_res),2,res_mean
    cal_res_restrict[pol_i]=res_mean
    cal_res_stddev[pol_i]=Stddev(Abs(gain_res))
ENDFOR
IF Tag_exist(cal,'Mean_gain') THEN cal.mean_gain=cal_gain_avg
IF Tag_exist(cal,'Mean_gain_residual') THEN cal.mean_gain_residual=cal_res_avg
IF Tag_exist(cal,'Mean_gain_restrict') THEN cal.mean_gain_restrict=cal_res_restrict
IF Tag_exist(cal,'Stddev_gain_residual') THEN cal.stddev_gain_residual=cal_res_stddev

t3=Systime(1)-t3_a
timing=Systime(1)-t0_0
IF ~Keyword_Set(silent) THEN print,timing,t1,t2,t3
RETURN,vis_cal
END
