PRO vis_calibrate_qu_mixing,vis_ptr,vis_model_ptr,vis_weight_ptr,obs,cal, ionosphere_tec=ionosphere_tec

n_pol = obs.n_pol
IF n_pol LT 4 THEN RETURN

n_freq=obs.n_freq
n_tile=obs.n_tile
n_time=obs.n_time
n_baselines=obs.nbaselines

;Use the xx weightss (yy should be identical at this point)
weights_use = 0>Reform(*vis_weight_ptr[0],n_freq,n_baselines,n_time)<1

;average the visibilities in time
pseudo_Q = Reform(*vis_ptr[1] - *vis_ptr[0],n_freq,n_baselines,n_time)
pseudo_Q = Total(Temporary(pseudo_Q)*weights_use,3)
pseudo_U = Reform(*vis_ptr[3] + *vis_ptr[2],n_freq,n_baselines,n_time)
pseudo_U = Total(Temporary(pseudo_U)*weights_use,3)
pseudo_Q_model = Reform(*vis_model_ptr[1] - *vis_model_ptr[0],n_freq,n_baselines,n_time)
pseudo_Q_model = Total(Temporary(pseudo_Q_model)*weights_use,3)
pseudo_U_model = Reform(*vis_model_ptr[3] + *vis_model_ptr[2],n_freq,n_baselines,n_time)
pseudo_U_model = Total(Temporary(pseudo_U_model)*weights_use,3)
weight = Total(weights_use,3)

i_use = where(weight,n_use)
pseudo_U = Reform(pseudo_U[i_use],1,n_use)
pseudo_Q = Reform(pseudo_Q[i_use],1,n_use)

U_Q_mix = LA_Least_Squares(pseudo_U,pseudo_Q)
U_Q_phase = Atan(U_Q_mix[0],/phase)

IF Keyword_Set(ionosphere_tec) THEN BEGIN
    print, "Calculating the Q-U mixing angle from the measured Ionospheric TEC is not supported yet!"
    print, "Continuing, using the model as a basis for calculating the relative rotation."
ENDIF
;ENDIF ELSE BEGIN
    pseudo_Q_model = Reform(*vis_model_ptr[1] - *vis_model_ptr[0],n_freq,n_baselines,n_time)
    pseudo_Q_model = Total(Temporary(pseudo_Q_model)*weights_use,3)
    pseudo_U_model = Reform(*vis_model_ptr[3] + *vis_model_ptr[2],n_freq,n_baselines,n_time)
    pseudo_U_model = Total(Temporary(pseudo_U_model)*weights_use,3)
    pseudo_U_model = Reform(pseudo_U_model[i_use],1,n_use)
    pseudo_Q_model = Reform(pseudo_Q_model[i_use],1,n_use)
    U_Q_mix_model = LA_Least_Squares(pseudo_U_model,pseudo_Q_model)
    U_Q_phase_model = Atan(U_Q_mix_model[0],/phase)
;ENDELSE

calc_phase = U_Q_phase_model - U_Q_phase
print,"Calculated Q-U mixing angle (radians):", calc_phase
IF N_Elements(cal) GT 0 THEN cal.stokes_mix_phase = calc_phase

END