FUNCTION calculate_adaptive_gain, gain_list, convergence_list, iter, base_gain=base_gain,$
    final_convergence_estimate=final_convergence_estimate
;Calculate the best gain factor to use for an iterative forward modeling algorithm, using a Kalman filter.

IF iter GT 2 THEN BEGIN
    ; To calculate the best gain to use, compare the past gains that have been used
    ; with the resulting convergences to estimate the best gain to use.
    ; Algorithmically, this is a Kalman filter.

    ; If forward modeling proceeds perfectly, the convergence metric should
    ; asymptotically approach a final value.
    ; We can estimate that value from the measured changes in convergence
    ; weighted by the gains used in each previous iteration.
    ; For some applications such as calibration this may be known in advance.
    ; In calibration, it is expressed as the change in a
    ; value, in which case the final value should be zero.
    IF N_Elements(final_convergence_estimate) EQ 0 THEN BEGIN
        est_final_conv = Fltarr(iter - 1)
        FOR i=0, iter-2 DO BEGIN
            final_convergence_test = ((1 + gain_list[i])*convergence_list[i + 1] - convergence_list[i])/gain_list[i]
            ; The convergence metric is strictly positive, so if the estimated final convergence is
            ; less than zero, force it to zero.
            est_final_conv[i] = 0 > final_convergence_test
        ENDFOR
        ; Because the estimate may slowly change over time, only use the most recent measurements.
        final_convergence_estimate = median(est_final_conv[((iter - 5) > 0):*])
    ENDIF
    last_gain = gain_list[iter - 1]
    last_conv = convergence_list[iter - 2]
    new_conv = convergence_list[iter - 1]
    ; The predicted convergence is the value we would get if the new model calculated
    ; in the previous iteration was perfect. Recall that the updated model that is
    ; actually used is the gain-weighted average of the new and old model,
    ; so the convergence would be similarly weighted.
    predicted_conv = (final_convergence_estimate*last_gain + last_conv)/(base_gain + last_gain)
    ; If the measured and predicted convergence are very close, that indicates
    ; that our forward model is accurate and we can use a more aggressive gain
    ; If the measured convergence is significantly worse (or better!) than predicted,
    ; that indicates that the model is not converging as expected and
    ; we should use a more conservative gain.
    delta = (predicted_conv - new_conv)/((last_conv - final_convergence_estimate)/(base_gain + last_gain))
    new_gain = 1 - abs(delta)
    ; Average the gains to prevent oscillating solutions.
    new_gain = (new_gain + last_gain)/2.
    gain = base_gain/2. > new_gain
ENDIF ELSE BEGIN
    gain = base_gain
ENDELSE
gain_list[iter] = gain

RETURN, gain
END
