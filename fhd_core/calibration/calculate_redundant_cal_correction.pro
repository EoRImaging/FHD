FUNCTION calculate_redundant_cal_correction, vis_data, vis_model, covariance_map_fn, A_ind, B_ind
    FOR g_i=0,n_redundant-1 DO BEGIN
        bi_redundant = ???
        n_bi_redundant = N_Elements(bi_redundant)
        tile_A_redundant = A_ind[bi_redundant]
        tile_B_redundant = B_ind[bi_redundant]
        gain_A_red = gain_old[tile_A_redundant]
        gain_B_red = gain_old[tile_B_redundant]
        vis_redundant = vis_data[bi_redundant]
        model_redundant = Total(vis_model[bi_redundant])/n_bi_redundant
        redundant_vis_delta = Total(vis_redundant*Conj(gain_A_red)*gain_B_red)/Total(Abs(gain_A_red)^2.*Abs(gain_B_red)^2.) - model_redundant
     RETURN, redundant_delta_arr
 END
