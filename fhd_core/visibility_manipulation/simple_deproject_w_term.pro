FUNCTION simple_deproject_w_term,obs,params,vis_arr,direction=direction

kbinsize=obs.kpix
n_pol=obs.n_pol
icomp=Complex(0,1)
frequency_array=(*obs.baseline_info).freq
ww_arr=params.ww
zcen=frequency_array#Temporary(ww_arr)
sign=(direction GT 0) ? 1.:-1.
phase=Exp(direction*icomp*zcen)

FOR pol_i=0,n_pol-1 DO *vis_arr[pol_i]*=phase
undefine_fhd,phase

IF sign GT 0 THEN sign_str=' +1' ELSE sign_str=' -1'
print,"Applying simple w-term deprojection:"+sign_str
RETURN,vis_arr
END