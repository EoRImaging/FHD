FUNCTION vis_extract_autocorr,obs,status_str,vis_arr=vis_arr,file_path_fhd=file_path_fhd,time_average=time_average,_Extra=extra

autocorr_i=where((*obs.baseline_info).tile_A EQ (*obs.baseline_info).tile_B,n_autocorr)
tile_i=((*obs.baseline_info).tile_A)[autocorr_i]
n_pol=obs.n_pol
n_tile=obs.n_tile
n_time=obs.n_time
auto_corr=Ptrarr(n_pol)
IF n_autocorr GT 0 THEN FOR pol_i=0,n_pol-1 DO BEGIN
    auto_vals=(*vis_arr[pol_i])[*,autocorr_i]
    IF Keyword_Set(time_average) THEN BEGIN
        
    ENDIF
    auto_corr[pol_i]=Ptr_new(auto_vals)
ENDFOR

IF Keyword_Set(file_path_fhd) THEN $
    fhd_save_io,status_str,auto_corr,var='auto_corr',/compress,file_path_fhd=file_path_fhd,obs=obs,_Extra=extra

RETURN,auto_corr
END