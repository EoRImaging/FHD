FUNCTION vis_randomize,vis_ptr,obs,params,cal_rand=cal_rand,random_gain_amplitude=random_gain_amplitude,random_gain_phase=random_gain_phase

IF N_Elements(random_gain_amplitude) EQ 0 THEN random_gain_amplitude=0.1
IF N_Elements(random_gain_phase) EQ 0 THEN random_gain_phase=10. ;degrees
icomp=Complex(0,1)
cal_rand=vis_struct_init_cal(obs,params)
n_pol=obs.n_pol
FOR pol_i=0,n_pol-1 DO BEGIN
    gain_arr=*(cal_rand.gain[pol_i])
    dims=size(gain_arr,/dimension)
    amp=1.+random_gain_amplitude*RandomN(seed,dims)
    phase=random_gain_phase*RandomN(seed,dims)*!DtoR ;convert to radians
    gain_arr*=amp*Exp(icomp*phase)
    *(cal_rand.gain[pol_i])=gain_arr
ENDFOR
vis_rand=vis_calibration_apply(vis_ptr,cal_rand,/preserve_original)

RETURN,vis_rand

END