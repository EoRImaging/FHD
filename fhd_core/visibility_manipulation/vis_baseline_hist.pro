PRO vis_baseline_hist,obs,params,vis_arr=vis_arr,vis_model_arr=vis_model_arr,$
    file_path_fhd=file_path_fhd,baseline_hist_plot=baseline_hist_plot
n_pol=obs.n_pol
freq_arr=(*obs.baseline_info).freq
kx_arr=params.uu/obs.kpix ;ignore slight variation with time
ky_arr=params.vv/obs.kpix
kr_arr=Sqrt(kx_arr^2.+ky_arr^2.)
dist_arr=(freq_arr#kr_arr)*obs.kpix
dist_hist = histogram(dist_arr, min=obs.min_baseline, binsize=5, max=obs.max_baseline, locations = dist_locs, reverse_indices = dist_ri)

;IF N_Elements(vis_model_arr) GT 0 THEN BEGIN
    vis_res_ratio_mean = fltarr(n_pol, n_elements(dist_locs))
    vis_res_sigma = fltarr(n_pol, n_elements(dist_locs))
    FOR pol_i=0,n_pol-1 DO BEGIN
        FOR i=0, n_elements(dist_locs)-1 DO IF dist_hist[i] GT 0 THEN BEGIN
            inds = dist_ri[dist_ri[i]:dist_ri[i+1]-1]
            model_vals=(*vis_model_arr[pol_i])[inds]
            wh_noflag = where(Abs(model_vals) GT 0, count_noflag)
            IF count_noflag EQ 0 THEN CONTINUE ELSE inds = inds[wh_noflag]
            if Keyword_Set(calibration_visibilities_subtract) THEN BEGIN
                vis_res_ratio_mean[pol_i, i] = mean(abs((*vis_arr[pol_i])[inds]))/mean(abs(model_vals))
                vis_res_sigma[pol_i, i] = sqrt(variance(abs((*vis_arr[pol_i])[inds])))/mean(abs(model_vals))
            ENDIF ELSE BEGIN
                vis_res_ratio_mean[pol_i, i] = mean(abs((*vis_arr[pol_i])[inds]-model_vals))/mean(abs(model_vals))
                vis_res_sigma[pol_i, i] = sqrt(variance(abs((*vis_arr[pol_i])[inds]-model_vals)))/mean(abs(model_vals))
            ENDELSE
        ENDIF
    ENDFOR
    vis_baseline_hist = {baseline_length:dist_locs, vis_res_ratio_mean:vis_res_ratio_mean, vis_res_sigma:vis_res_sigma}
    IF Keyword_Set(file_path_fhd) THEN BEGIN
        path_use=filepath(file_basename(file_path_fhd+'_cal_hist.sav'),root=file_dirname(file_path_fhd),subdir='output_data')
        IF file_test(file_dirname(path_use)) EQ 0 THEN file_mkdir,file_dirname(path_use)
        SAVE,vis_baseline_hist,filename=path_use
    ENDIF
;ENDIF ELSE BEGIN
;    FOR pol_i=0,n_pol-1 DO BEGIN
;        FOR i=0, n_elements(dist_locs)-1 DO IF dist_hist[i] GT 0 THEN BEGIN
;            inds = dist_ri[dist_ri[i]:dist_ri[i+1]-1]
;            vis_vals=(*vis_arr[pol_i])[inds]
;            wh_noflag = where(vis_vals GT 0, count_noflag)
;            IF count_noflag EQ 0 THEN CONTINUE ELSE inds = inds[wh_noflag]
;                vis_res_ratio_mean[i] = mean(abs((*vis_arr[pol_i])[inds]))/mean(abs(model_vals))
;                vis_res_sigma[i] = sqrt(variance(abs((*vis_arr[pol_i])[inds])))/mean(abs(model_vals))
;        ENDIF
;    ENDFOR
;ENDELSE
END