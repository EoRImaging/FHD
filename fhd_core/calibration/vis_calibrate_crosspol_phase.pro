FUNCTION vis_calibrate_crosspol_phase,vis_ptr,vis_weight_ptr,obs,cal

n_pol = obs.n_pol
IF n_pol LT 4 THEN RETURN, cal

icomp = Complex(0,1)
n_freq=cal.n_freq
n_tile=cal.n_tile
n_time=cal.n_time
n_baselines=obs.nbaselines

;Use the xx flags (yy should be identical at this point)
weights_use = 0>Reform(*vis_weight_ptr[0],n_freq,n_baselines,n_time)<1

;average the visibilities in time
vis_xy = Reform(*vis_ptr[3],n_freq,n_baselines,n_time)
vis_xy = Total(Temporary(vis_xy)*weights_use,3)
vis_yx = Reform(*vis_ptr[2],n_freq,n_baselines,n_time)
vis_yx = Total(Temporary(vis_yx)*weights_use,3)

;remove zeros
weight = Total(Temporary(weights_use),3)
i_use = Where(weight,n_use)
vis_xy = Reform(vis_xy[i_use],1,n_use)
vis_yx = Reform(vis_yx[i_use],1,n_use)

vis_sum = Total(Temporary(vis_xy)*Conj(Temporary(vis_yx)))
cross_phase = 0.5*atan(vis_sum, /phase)

cal.cross_phase = cross_phase
*(cal.gain[0]) *= Exp(-icomp * cross_phase / 2.0)
*(cal.gain[1]) *= Exp(icomp * cross_phase / 2.0)

print,"Phase fit between X and Y antenna polarizations:", cross_phase
RETURN,cal
END
