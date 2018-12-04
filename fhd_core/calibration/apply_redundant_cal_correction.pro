FUNCTION apply_redundant_cal_correction, vis_model, freq_i=freq_i, redundant_cal_correction=redundant_cal_correction,$
    baseline_inds=baseline_inds, use_conjugate=use_conjugate, redundant_calibration_weight=redundant_calibration_weight

    IF N_Elements(redundant_calibration_weight) EQ 0 THEN redundant_calibration_weight=1.

    IF N_Elements(freq_i) GT 0 THEN BEGIN
        redundant_cal_correction_use = Reform(redundant_cal_correction[freq_i, *])
        IF N_Elements(baseline_inds) GT 0 THEN $
            redundant_cal_correction_use = redundant_cal_correction_use[baseline_inds]
        IF Keyword_Set(use_conjugate) THEN redundant_cal_correction_use = [redundant_cal_correction_use, Conj(redundant_cal_correction_use)]
    ENDIF ELSE BEGIN
        IF N_Elements(baseline_inds) GT 0 THEN $
            redundant_cal_correction_use = redundant_cal_correction[*, baseline_inds] ELSE $
            redundant_cal_correction_use = redundant_cal_correction
    ENDELSE
    
    vis_model_corrected = vis_model + redundant_cal_correction_use*redundant_calibration_weight

    RETURN, vis_model_corrected
END
