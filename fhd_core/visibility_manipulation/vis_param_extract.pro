FUNCTION vis_param_extract,params,header


uu_arr=Double(reform(params[header.uu_i,*]))
vv_arr=Double(reform(params[header.vv_i,*]))
ww_arr=Double(reform(params[header.ww_i,*]))
time=reform(params[header.date_i,*])

; Sort out the baseline numbering and antenna numbers
IF header.baseline_i GE 0 THEN $
    baseline_arr=reform(params[header.baseline_i,*]) $
    ELSE baseline_arr = Lonarr(N_Elements(time))
; Define default antenna numbers
name_mod=2.^((Ceil(Alog(Sqrt(header.nbaselines*2.-header.n_tile))/Alog(2.)))>Floor(Alog(Min(baseline_arr))/Alog(2.)))        
ant1_arr=Long(Floor(baseline_arr/name_mod)) ;tile numbers start from 1      
ant2_arr=Long(Fix(baseline_arr mod name_mod))
; Use the recorded antenna numbers if those are present
IF Tag_exist(header, "ant1_i") THEN $
    IF header.ant1_i GE 0 THEN ant1_arr = reform(params[header.ant1_i,*])
IF Tag_exist(header, "ant2_i") THEN $
    IF header.ant2_i GE 0 THEN ant2_arr = reform(params[header.ant2_i,*])

struct={uu:uu_arr,vv:vv_arr,ww:ww_arr,baseline_arr:baseline_arr,time:time, antenna1:ant1_arr, antenna2:ant2_arr}
RETURN,struct
END