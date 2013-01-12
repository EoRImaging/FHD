FUNCTION vis_param_extract,params,header


uu_arr=reform(params[header.uu_i,*])
vv_arr=reform(params[header.vv_i,*])
ww_arr=reform(params[header.ww_i,*])
baseline_arr=reform(params[header.baseline_i,*]) 
time=reform(params[header.date_i,*])

struct={uu:uu_arr,vv:vv_arr,ww:ww_arr,baseline_arr:baseline_arr,time:time}
RETURN,struct
END