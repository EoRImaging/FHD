; Fill gaps in the U-V plane by replacing flagged visibilities with the values in the model.
; This will affect all downstream calculations using the data visibilities,
; including calculating the power spectrum

PRO interpolate_visibilities, vis_data, vis_weights, vis_model, obs, psf, params

  flagged_vis_i = where(*vis_weights LE 0, n_flagged)
  IF n_flagged GT 0 THEN BEGIN
    *vis_data[flagged_vis_i] = *vis_model[flagged_vis_i]
    *vis_weights[flagged_vis_i] = 1
    vis_weights_update, vis_weights, obs, psf, params, /no_frequency_flagging
  ENDIF

END
