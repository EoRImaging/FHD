FUNCTION vis_param_extract,params,header, antenna_mod_index=antenna_mod_index


uu_arr=Double(reform(params[header.uu_i,*]))
vv_arr=Double(reform(params[header.vv_i,*]))
ww_arr=Double(reform(params[header.ww_i,*]))

; Use both date fields if available in the uvfits file
; This doesn't give julian dates; those are calcuated in fhd_struct_init_meta
IF n_elements(header.date_i) EQ 2 THEN BEGIN
    time=Double(reform(params[header.date_i[0],*])) + Double(reform(params[header.date_i[1],*]))
ENDIF ELSE time=Double(reform(params[header.date_i,*]))

; Sort out the baseline numbering and antenna numbers
IF header.baseline_i GE 0 THEN $
    baseline_arr=reform(params[header.baseline_i,*]) $
    ELSE baseline_arr = Lonarr(N_Elements(time))
; Define default antenna numbers
IF not Keyword_Set(antenna_mod_index) THEN BEGIN
    antenna_mod_index_use=Long(2^Floor(Alog(min(baseline_arr))/Alog(2.))) 
    tile_B_test=min(baseline_arr) mod antenna_mod_index_use
    IF tile_B_test GT 1 THEN $ ; Check if a bad fit
        IF min(baseline_arr) mod 2 EQ 1 THEN $ ; but not if autocorrelations or the first tile are missing
            antenna_mod_index_use/=Long(2^Floor(Alog(tile_B_test)/Alog(2.))) 
ENDIF ELSE antenna_mod_index_use=antenna_mod_index        
ant1_arr=Long(Floor(baseline_arr/antenna_mod_index_use)) ;tile numbers start from 1      
ant2_arr=Long(Fix(baseline_arr mod antenna_mod_index_use))
; Use the recorded antenna numbers if those are present
IF Tag_exist(header, "ant1_i") THEN $
    IF header.ant1_i GE 0 THEN ant1_arr = reform(params[header.ant1_i,*])
IF Tag_exist(header, "ant2_i") THEN $
    IF header.ant2_i GE 0 THEN ant2_arr = reform(params[header.ant2_i,*])

struct={uu:uu_arr,vv:vv_arr,ww:ww_arr,baseline_arr:baseline_arr,time:time, antenna1:ant1_arr, antenna2:ant2_arr}
RETURN,struct
END