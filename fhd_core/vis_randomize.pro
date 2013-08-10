FUNCTION vis_randomize,vis_ptr,obs,params,cal_rand

cal_rand=vis_struct_init_cal(obs,params)
vis_rand=vis_calibration_apply(vis_ptr,cal_rand,preserve_original=1)

RETURN,vis_rand

END