FUNCTION apply_redundant_cal_correction, vis_model, redundant_delta=redundant_delta, baseline_inds=baseline_inds, use_conjugate=use_conjugate
    
    IF N_Elements(baseline_inds) GT 0 THEN $
        redundant_delta_use = redundant_delta[baseline_inds] ELSE $
        redundant_delta_use = redundant_delta
    IF Keyword_Set(use_conjugate) THEN redundant_delta_use = [redundant_delta_use, Conj(redundant_delta_use)]
    vis_model_corrected = vis_model + redundant_delta_use

    RETURN, vis_model_corrected
END
