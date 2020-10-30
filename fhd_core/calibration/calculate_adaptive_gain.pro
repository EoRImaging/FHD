FUNCTION calculate_adaptive_gain, gain_list, convergence_list, iter, base_gain=base_gain
;Calculate the best gain factor to use for an iterative forward modeling algorithm, using a Kalman filter.

IF N_Elements(base_gain) EQ 0 THEN base_gain=0.5

IF iter GT 2 THEN BEGIN
    ; To calculate the best gain to use, compare the past gains that have been used
    ; with the resulting convergences to estimate the best gain to use.
    ; Algorithmically, this is a Kalman filter.
    ; If forward modeling proceeds perfectly, the convergence metric should
    ; asymptotically approach a final value.
    ; We can estimate that value from the measured changes in convergence
    ; weighted by the gains used in each previous iteration.
    est_final_conv = Fltarr(iter - 1)
    FOR i=0, iter-2 DO BEGIN
        final_convergence_test = ((1 + gain_list[i])*convergence_list[i + 1] - convergence_list[i])/gain_list[i]
        ; The convergence metric is strictly positive, so if the estimated final convergence is
        ; less than zero, force it to zero.
        est_final_conv[i] = 0 > final_convergence_test
    ENDFOR
    ; Because the estimate may slowly change over time, only use the most recent measurements.
    est_final_conv = median(est_final_conv[((iter - 5) > 0):])
    last_gain = gain_list[iter]
    last_conv = convergence_list[iter - 1]
    new_conv = convergence_list[iter]
    ; The predicted convergence is the value we would get if the new model calculated
    ; in the previous iteration was perfect. Recall that the updated model that is
    ; actually used is the gain-weighted average of the new and old model,
    ; so the convergence would be similarly weighted.
    predicted_conv = (est_final_conv*last_gain + last_conv)/(base_gain + last_gain)
    ; If the measured and predicted convergence are very close, that indicates
    ; that our forward model is accurate and we can use a more aggressive gain
    ; If the measured convergence is significantly worse (or better!) than predicted,
    ; that indicates that the model is not converging as expected and
    ; we should use a more conservative gain.
    delta = (predicted_conv - new_conv)/((last_conv - est_final_conv)/(base_gain + last_gain))
    new_gain = 1 - abs(delta)
    ; Average the gains to prevent oscillating solutions.
    new_gain = (new_gain + last_gain)/2.
    gain = base_gain > new_gain
ENDIF ELSE BEGIN
    gain = base_gain
ENDELSE
gain_list[iter] = gain

RETURN, gain
END
