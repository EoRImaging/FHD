FUNCTION cal_auto_ratio,obs,cal,auto_ratio=auto_ratio,vis_auto=vis_auto,auto_tile_i=auto_tile_i,$
    divide=divide,remultiply=remultiply

n_pol=cal.n_pol
n_freq=obs.n_freq
n_tile=obs.n_tile
i_comp=Complex(0,1)

n_tile_use=N_Elements(auto_tile_i)

;; Create autos which are normalized via a reference to reveal antenna-dependent parameters
;;  (i.e. cable reflections). Then weight the crosses by their auto ratios in order to remove
;;  antenna-dependent parameters before creation of a global bandpass.
IF keyword_set(divide) THEN BEGIN
    auto_ratio = Ptrarr(n_pol)
    FOR pol_i=0,n_pol-1 DO BEGIN
        gain_arr = FLTARR(n_freq,n_tile)
        v0 = (*vis_auto[pol_i])[*,auto_tile_i[cal.ref_antenna]]
            FOR tile_i=0,n_tile_use-1 DO BEGIN
                gain_single = Sqrt((*vis_auto[pol_i])[*,tile_i]*weight_invert(v0))
                gain_arr[*,auto_tile_i[tile_i]] = gain_single
            ENDFOR
        auto_ratio[pol_i] = Ptr_new(gain_arr)
    ENDFOR

    gain_cross=cal.gain
    FOR pol_i=0,n_pol-1 DO BEGIN
        (*cal.gain[pol_i]) = (*gain_cross[pol_i])*weight_invert(*auto_ratio[pol_i])
    ENDFOR
ENDIF

;; Reform the original calibration gains using the auto ratios
IF keyword_set(remultiply) THEN BEGIN
    gain_cross = cal.gain
    FOR pol_i=0,n_pol-1 DO BEGIN
        gain_tile = (*gain_cross[pol_i])[*,auto_tile_i]
        gain_auto = Abs((*auto_ratio[pol_i])[*,auto_tile_i])
        (*cal.gain[pol_i])[*,auto_tile_i] = gain_tile * gain_auto
    ENDFOR
ENDIF


RETURN,cal
END
