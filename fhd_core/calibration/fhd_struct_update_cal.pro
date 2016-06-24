FUNCTION fhd_struct_update_cal,cal1,cal2,obs=obs

n_pol=cal1.n_pol


u_loc=cal1.uu
v_loc=cal1.vv
cal=fhd_struct_init_cal(obs,n_pol=n_pol,u_loc=u_loc,v_loc=v_loc)

FOR pol_i=0,n_pol-1 DO BEGIN
    gain1=*cal1.gain[pol_i]
    IF size(cal2,/type) EQ 8 THEN gain2=*cal2.gain[pol_i] ELSE gain2=*gain_ptr[pol_i] 
    *cal.gain[pol_i]=gain1*gain2
ENDFOR

RETURN,cal
END