PRO calculate_redundant_cal_correction, vis_data, vis_model, covariance_map_fn, A_ind, B_ind, gain,$
    redundant_cal_correction=redundant_cal_correction, baseline_inds=baseline_inds, covariance_threshold=covariance_threshold,$
    redundant_calibration_weight=redundant_calibration_weight

    n_baselines = N_Elements(redundant_cal_correction)
    redundant_cal_delta = Complexarr(n_baselines)
    vis_residual = vis_data*weight_invert(gain[A_ind]*Conj(gain[B_ind])) - vis_model
    vis_delta2 = [redundant_cal_correction[baseline_inds], Conj(redundant_cal_correction[baseline_inds])]
    vis_residual -= vis_delta2
    IF Keyword_Set(redundant_calibration_weight) THEN vis_residual /= redundant_calibration_weight
    SPRSAX2,covariance_map_fn,vis_residual,redundant_cal_delta,/complex

    redundant_cal_correction = redundant_cal_correction + redundant_cal_delta/2.
 END
