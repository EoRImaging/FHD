FUNCTION apply_redundant_cal_correction, vis_model, redundant_cal_correction=redundant_cal_correction, baseline_inds=baseline_inds,$
    use_conjugate=use_conjugate, redundant_calibration_weight=redundant_calibration_weight

    IF N_Elements(redundant_calibration_weight) EQ 0 THEN redundant_calibration_weight=1.
    
    IF N_Elements(baseline_inds) GT 0 THEN $
        redundant_cal_correction_use = redundant_cal_correction[baseline_inds] ELSE $
        redundant_cal_correction_use = redundant_cal_correction
    IF Keyword_Set(use_conjugate) THEN redundant_cal_correction_use = [redundant_cal_correction_use, Conj(redundant_cal_correction_use)]
    vis_model_corrected = vis_model + redundant_cal_correction_use*redundant_calibration_weight

    RETURN, vis_model_corrected
END
