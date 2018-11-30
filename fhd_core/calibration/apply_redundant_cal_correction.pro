FUNCTION apply_redundant_cal_correction, vis_model, redundant_delta=redundant_delta, baseline_inds=baseline_inds
    IF N_Elements(baseline_inds) GT 0 THEN 
        vis_model_corrected = vis_model + redundant_delta[baseline_inds]
    ENDIF ELSE BEGIN
        vis_model_corrected = vis_model + redundant_delta
    ENDELSE

    RETURN, vis_model_corrected
END
