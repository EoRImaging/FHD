FUNCTION vis_extract_autocorr,obs,status_str,vis_arr=vis_arr,file_path_fhd=file_path_fhd,time_average=time_average,auto_tile_i=auto_tile_i,_Extra=extra

autocorr_i=where((*obs.baseline_info).tile_A EQ (*obs.baseline_info).tile_B,n_autocorr)
IF n_autocorr GT 0 THEN BEGIN
    auto_tile_i=((*obs.baseline_info).tile_A)[autocorr_i]-1
    auto_tile_i_single=auto_tile_i[Uniq(auto_tile_i,Sort(auto_tile_i))]
    n_pol=obs.n_pol
    n_tile=obs.n_tile
    n_tile_use=N_Elements(auto_tile_i_single)
    n_freq=obs.n_freq
    n_time=obs.n_time
    time_use=(*obs.baseline_info).time_use
    auto_corr=Ptrarr(n_pol)
    FOR pol_i=0,n_pol-1 DO BEGIN
        auto_vals=Real_part((*vis_arr[pol_i])[*,autocorr_i])
        IF Keyword_Set(time_average) THEN BEGIN
            auto_single=Fltarr(n_freq,n_tile_use)
            time_inds=where(time_use,n_time_use)
            FOR tile_i=0,n_tile_use-1 DO BEGIN
                baseline_i=where(auto_tile_i EQ auto_tile_i_single[tile_i])
                baseline_i=baseline_i[time_inds]
                IF n_time_use GT 1 THEN BEGIN
                  auto_single[*,tile_i]=Total(extract_subarray(auto_vals,indgen(n_freq),baseline_i),2)/n_time_use
                ENDIF ELSE BEGIN
                  auto_single[*,tile_i]=extract_subarray(auto_vals,indgen(n_freq),baseline_i)
                ENDELSE
            ENDFOR
            auto_vals=auto_single
        ENDIF
        auto_corr[pol_i]=Ptr_new(auto_vals)
    ENDFOR
    IF Keyword_Set(time_average) THEN auto_tile_i=auto_tile_i_single
    IF Keyword_Set(file_path_fhd) THEN $
        fhd_save_io,status_str,auto_corr,var='auto_corr',/compress,file_path_fhd=file_path_fhd,obs=obs,_Extra=extra
ENDIF ELSE auto_corr = 0

RETURN,auto_corr
END