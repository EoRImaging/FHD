FUNCTION vis_cal_divide,cal_base,cal
IF N_Params() LT 2 THEN RETURN,cal_base

n_pol=cal_base.n_pol
gain0a=*cal_base.gain[0]
gain1a=*cal.gain[0]
IF n_pol GT 1 THEN BEGIN
    gain0b=*cal_base.gain[1]
    gain1b=*cal.gain[1]
ENDIF

cal_residual=cal_base

gain2a=gain0a/gain1a
IF n_pol GT 1 THEN gain2b=gain0b/gain1b

cal_residual.gain[0]=Ptr_new(gain2a)
IF n_pol GT 1 THEN cal_residual.gain[1]=Ptr_new(gain2b) 

RETURN,cal_residual
END