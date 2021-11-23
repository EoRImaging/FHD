FUNCTION fhd_struct_init_cal, obs, params, skymodel, gain_arr_ptr=gain_arr_ptr, $
    n_pol=n_pol, n_freq=n_freq, n_tile=n_tile, n_time=n_time, $
    tile_A=tile_A, tile_B=tile_B, freq=freq, bin_offset=bin_offset, tile_names=tile_names, $
    u_loc=u_loc, v_loc=v_loc, min_cal_baseline=min_cal_baseline, max_cal_baseline=max_cal_baseline, $
    n_vis_cal=n_vis_cal, cal_time_average=cal_time_average, ref_antenna=ref_antenna, $
    cal_convergence_threshold=cal_convergence_threshold, max_cal_iter=max_cal_iter, $
    calibration_origin=calibration_origin, catalog_path=catalog_path, $
    calibration_polyfit=calibration_polyfit, bandpass_calibrate=bandpass_calibrate, $
    cal_mode_fit=cal_mode_fit, file_path_fhd=file_path_fhd, transfer_calibration=transfer_calibration, $
    calibration_auto_initialize=calibration_auto_initialize, cal_gain_init=cal_gain_init, $
    phase_fit_iter=phase_fit_iter, cal_amp_degree_fit=cal_amp_degree_fit,cal_phase_degree_fit=cal_phase_degree_fit, $
    use_adaptive_calibration_gain=use_adaptive_calibration_gain, calibration_base_gain=calibration_base_gain,$
    min_cal_solutions=min_cal_solutions,_Extra=extra

IF N_Elements(obs) EQ 0 THEN fhd_save_io,0,obs,var='obs',/restore,file_path_fhd=file_path_fhd
IF N_Elements(params) EQ 0 THEN fhd_save_io,0,params,var='params',/restore,file_path_fhd=file_path_fhd
IF N_Elements(tile_A) EQ 0 THEN tile_A=(*obs.baseline_info).tile_A
IF N_Elements(tile_B) EQ 0 THEN tile_B=(*obs.baseline_info).tile_B
IF N_Elements(freq) EQ 0 THEN freq=(*obs.baseline_info).freq
IF N_Elements(bin_offset) EQ 0 THEN bin_offset=(*obs.baseline_info).bin_offset
IF N_Elements(tile_names) EQ 0 THEN tile_names=(*obs.baseline_info).tile_names
IF N_Elements(u_loc) EQ 0 THEN u_loc=params.uu
IF N_Elements(v_loc) EQ 0 THEN v_loc=params.vv

IF N_Elements(n_vis_cal) EQ 0 THEN n_vis_cal=obs.n_vis
IF N_Elements(n_pol) EQ 0 THEN n_pol=obs.n_pol<2 ; since only x and y pols, not xx, yy, xy, yx
IF N_Elements(n_freq) EQ 0 THEN n_freq=obs.n_freq
IF N_Elements(n_tile) EQ 0 THEN n_tile=obs.n_tile
IF N_Elements(n_time) EQ 0 THEN n_time=obs.n_time
IF N_Elements(skymodel) EQ 0 THEN skymodel=fhd_struct_init_skymodel(obs)
IF N_Elements(calibration_auto_initialize) EQ 0 THEN auto_initialize=1 ELSE auto_initialize=calibration_auto_initialize
IF N_Elements(min_cal_baseline) EQ 0 THEN min_cal_baseline=obs.min_baseline ELSE min_cal_baseline=min_cal_baseline>obs.min_baseline
IF N_Elements(max_cal_baseline) EQ 0 THEN max_cal_baseline=obs.max_baseline ELSE max_cal_baseline=max_cal_baseline<obs.max_baseline
IF N_Elements(cal_time_average) EQ 0 THEN cal_time_average=1 ;time average visibilities before calculating calibration solutions by default
IF N_Elements(min_cal_solutions) EQ 0 THEN min_cal_solutions=5
IF N_Elements(max_cal_iter) EQ 0 THEN max_cal_iter=100L
IF N_Elements(ref_antenna) EQ 0 THEN ref_antenna=1L
ref_antenna_name=(*obs.baseline_info).tile_names[ref_antenna]
IF N_Elements(cal_convergence_threshold) EQ 0 THEN cal_convergence_threshold=1E-7
IF N_Elements(calibration_origin) EQ 0 THEN $
    IF Tag_exist(obs,'obsname') THEN calibration_origin=obs.obsname ELSE calibration_origin=''
IF N_Elements(cal_gain_init) EQ 0 THEN cal_gain_init=1.
IF N_Elements(gain_arr_ptr) EQ 0 THEN BEGIN
    gain_arr=Complexarr(n_freq,n_tile)+cal_gain_init
    gain_arr_ptr=Ptrarr(n_pol,/allocate)
    FOR pol_i=0,n_pol-1 DO *gain_arr_ptr[pol_i]=gain_arr
ENDIF
gain_residual=Ptrarr(n_pol,/allocate)
FOR pol_i=0,n_pol-1 DO *gain_residual[pol_i]=Complexarr(n_freq,n_tile)
IF N_Elements(calibration_polyfit) EQ 0 THEN calibration_polyfit=0
IF N_Elements(cal_amp_degree_fit) EQ 0 THEN cal_amp_degree_fit=2
IF N_Elements(cal_phase_degree_fit) EQ 0 THEN cal_phase_degree_fit=1
amp_params=Ptrarr(n_pol,n_tile)
phase_params=Ptrarr(n_pol,n_tile)

IF N_Elements(bandpass_calibrate) EQ 0 THEN bandpass_calibrate=1
IF N_Elements(cal_mode_fit) EQ 0 THEN cal_mode_fit=0.
;whether to use a Kalman Filter to adjust the gain to use for each iteration of calculating calibration
IF N_Elements(use_adaptive_calibration_gain) EQ 0 THEN use_adaptive_calibration_gain=0 
;The relative weight to give the old calibration solution when averaging with the new. 
IF N_Elements(calibration_base_gain) EQ 0 THEN BEGIN
    IF N_Elements(use_adaptive_calibration_gain) EQ 0 THEN calibration_base_gain=1. ELSE calibration_base_gain=0.75
ENDIF
IF N_Elements(phase_fit_iter) EQ 0 THEN phase_fit_iter=Long((calibration_base_gain*4)>4) ELSE phase_fit_iter=Long(phase_fit_iter)
convergence=Ptrarr(2)
conv_iter=Ptrarr(2)
mode_params=Ptrarr(n_pol,n_tile)
auto_params=Ptrarr(2)
auto_scale=Fltarr(2)

cal_struct={n_pol:n_pol, n_freq:n_freq, n_tile:n_tile, n_time:n_time, uu:u_loc, vv:v_loc, $
    auto_initialize:auto_initialize, max_iter:max_cal_iter, phase_iter:phase_fit_iter, $
    tile_A:tile_A, tile_B:tile_B, tile_names:tile_names, bin_offset:bin_offset, freq:freq, gain:gain_arr_ptr, $
    adaptive_gain:use_adaptive_calibration_gain, base_gain:calibration_base_gain, $
    gain_residual:gain_residual, auto_scale:auto_scale, auto_params:auto_params, cross_phase:0.0, stokes_mix_phase:0.0, $
    min_cal_baseline:min_cal_baseline, max_cal_baseline:max_cal_baseline, n_vis_cal:n_vis_cal, $
    time_avg:cal_time_average, min_solns:min_cal_solutions, ref_antenna:ref_antenna, $
    ref_antenna_name:ref_antenna_name, conv_thresh:cal_convergence_threshold, $
    convergence:convergence, n_converged:Lonarr(n_pol)-1, conv_iter:conv_iter, $
    polyfit:calibration_polyfit, amp_degree:cal_amp_degree_fit, phase_degree:cal_phase_degree_fit, $
    amp_params:amp_params, phase_params:phase_params, $
    mean_gain:Fltarr(n_pol), mean_gain_residual:Fltarr(n_pol), mean_gain_restrict:Fltarr(n_pol), $
    stddev_gain_residual:Fltarr(n_pol), bandpass:bandpass_calibrate, mode_fit:cal_mode_fit, $
    mode_params:mode_params, cal_origin:calibration_origin, skymodel:skymodel}
RETURN,cal_struct
END
