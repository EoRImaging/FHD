PRO calculate_redundant_cal_correction, vis_data, vis_model, covariance_map_fn, A_ind, B_ind,$
    redundant_delta=redundant_delta, baseline_inds=baseline_inds

    covariance_threshold = 0.9

    n_baselines_full = N_Elements(redundant_delta)
    new_vis_delta = Complexarr(n_baselines_full)

    n_baselines = N_Elements(vis_data)
    FOR b_ii=0,n_baselines-1 DO BEGIN
        ; b_i = baseline_inds[b_i]
        bi_redundant = *covariance_map_fn.ija[b_ii]
        bi_red_use = where(Abs(*covariance_map_fn.sa[b_ii]), n_use)
        IF n_use EQ 0 THEN CONTINUE
        bi_redundant = bi_redundant[bi_red_use]
        n_bi_redundant = N_Elements(bi_redundant)
        tile_A_redundant = A_ind[bi_redundant]
        tile_B_redundant = B_ind[bi_redundant]
        gain_A_red = gain_old[tile_A_redundant]
        gain_B_red = gain_old[tile_B_redundant]
        vis_redundant = vis_data[bi_redundant]
        model_redundant = Total(vis_model[bi_redundant])/n_bi_redundant
        new_vis_delta[baseline_inds[b_ii]] = Total(vis_redundant*Conj(gain_A_red)*gain_B_red)/Total(Abs(gain_A_red)^2.*Abs(gain_B_red)^2.) - model_redundant
    ENDFOR
    redundant_delta = (redundant_delta + new_vis_delta)/2.
 END
