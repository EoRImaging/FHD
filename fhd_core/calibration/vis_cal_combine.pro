FUNCTION vis_cal_combine,cal1,cal2

n_pol=cal1.n_pol<cal2.n_pol
gain_arr_ptr=Ptrarr(n_pol,/allocate)
FOR pol_i=0,n_pol-1 DO BEGIN
    gain1=*(cal1.gain)[pol_i]
    gain2=*(cal2.gain)[pol_i]
    gain=gain1*gain2
    *gain_arr_ptr[pol_i]=gain
ENDFOR
cal_out=cal1
cal_out.gain=gain_arr_ptr

RETURN,cal_out
END