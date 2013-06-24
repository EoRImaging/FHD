FUNCTION vis_calibrate,vis_ptr,cal,obs,psf,params,flag_ptr=flag_ptr,model_ptr=model_ptr,$
    min_cal_baseline=min_cal_baseline,max_cal_baseline=max_cal_baseline,gain_arr_ptr=gain_arr_ptr,$
    transfer_calibration=transfer_calibration,timing=timing,file_path_fhd=file_path_fhd,$
    n_cal_iter=n_cal_iter,error=error,preserve_visibilities=preserve_visibilities,$
    calibration_source_list=calibration_source_list,_Extra=extra
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
        timing=Systime(1)-t0_0
        RETURN,vis_ptr
    ENDELSE
ENDIF

;IF Keyword_Set(calibration_source_list) THEN BEGIN
;    
;ENDIF

vis_model_ptr=vis_source_model(calibration_source_list,obs,psf,params,flag_ptr,model_uv_arr=model_ptr,$
    timing=model_timing,silent=silent,_Extra=extra)

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
n_baselines=bin_offset[1]
tile_A_i=tile_A_i[0:bin_offset[1]-1]
tile_B_i=tile_B_i[0:bin_offset[1]-1]

IF N_Elements(flag_ptr) EQ 0 THEN BEGIN
    flag_init=Replicate(1.,n_freq,n_baselines*Float(n_time))
    flag_ptr=Ptrarr(n_pol,/allocate)
    FOR pol_i=0,n_pol-1 DO *flag_ptr[pol_i]=flag_init
ENDIF

;flag_freq_test=fltarr(n_freq)
;flag_tile_test=fltarr(n_tile)
;FOR pol_i=0,n_pol-1 DO flag_freq_test+=Max(*flag_ptr[pol_i],dimension=2)>0

;FOR pol_i=0,n_pol-1 DO BEGIN
;    FOR tile_i=0L,n_tile-1 DO BEGIN
;        flag_tile_test[tile_i]+=Max((*flag_ptr[pol_i])[*,tile_A_i
;    ENDFOR
;ENDFOR
;
;freq_use_i=where(flag_freq_test,n_freq_use)
;tile_i_use=where(flag_tile_test,n_tile_use)


;freq_flag=fltarr(n_freq) & freq_flag[freq_use_i]=1
;tile_flag=fltarr(n_tile) & tile_flag[tile_i_use]=1

;calibration loop
;vis_use=Ptrarr(n_pol,/allocate) 
;FOR pol_i=0,n_pol-1 DO *vis_use[pol_i]=*vis_ptr[pol_i]
FOR pol_i=0,n_pol-1 DO BEGIN
    gain_arr=*cal.gain[pol_i]
    vis_use=*vis_ptr[pol_i]
    vis_use/=*vis_model_ptr[pol_i]
    flag_use=0>*flag_ptr[pol_i]<1
;    i_nan=where(Finite(result,/nan),n_nan)
;    IF n_nan GT 0 THEN vis_use[i_nan]=0.
    
    ;average over time
    ;the visibilities have dimension nfreq x (n_baselines x n_time), 
    ; which can be reformed to nfreq x n_baselines x n_time 
    vis_use=Total(Reform(vis_use,n_freq,n_baselines,n_time),3,/NAN)
    weight=Total(Reform(flag_use,n_freq,n_baselines,n_time),3,/NAN)
    i_use=where(weight GT 0,n_use)
    freq_weight=Total(weight,2)
    baseline_weight=Total(weight,1)
    freq_use=where(freq_weight,n_freq_use)
    baseline_use=where(baseline_weight,n_baseline_use)
    tile_use=where(histogram(tile_A_i[baseline_use],min=0,/bin,max=n_tile-1) $
        +histogram(tile_B_i[baseline_use],min=0,/bin,max=n_tile-1))
;    tile_use=Uniq(tile_A_i[baseline_use])
;    n_tile_use=N_Elements(tile_use)
    
    vis_use*=weight_invert(weight)
    FOR fii=0L,n_freq_use-1 DO BEGIN
        fi=freq_use[fii]
        gain_curr=Reform(gain_arr[fi,*])
        vis_matrix=Complexarr(n_tile,n_baseline_use)
        gain_curr=Reform(gain_arr[fi,tile_A_i[baseline_use]])*Reform(gain_arr[fi,tile_B_i[baseline_use]])
        vis_matrix[tile_A_i[baseline_use],baseline_use]+=vis_use[fi,baseline_use]
        vis_matrix[tile_B_i[baseline_use],baseline_use]+=vis_use[fi,baseline_use]
;        vis_matrix+=Conj(transpose(vis_matrix))
        FOR i=0L,(n_cal_iter-1)>1 DO BEGIN
            gain_new=LA_Least_Squares(vis_matrix,gain_curr,method=2)
            gain_new=1./gain_new
            gain_new2=gain_new[tile_A_i[baseline_use]]*gain_new[tile_B_i[baseline_use]]
            gain_curr=(gain_new2+gain_curr)/2.
        ENDFOR 
        gain_arr[fi,*]=gain_new
    ENDFOR
    
    *cal.gain[pol_i]=gain_arr
ENDFOR
;cal=vis_struct_update_cal(cal,gain_new,obs=obs)

vis_cal=vis_calibration_apply(cal,vis_ptr,preserve_original=1)
timing=Systime(1)-t0_0
RETURN,vis_cal
END