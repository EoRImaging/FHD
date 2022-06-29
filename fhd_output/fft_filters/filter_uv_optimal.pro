FUNCTION filter_uv_optimal,image_uv,obs=obs,psf=psf,params=params,weights=weights,name=name,filter=filter,$
    file_path_fhd=file_path_fhd,return_name_only=return_name_only,uvfilter_weight_threshold=uvfilter_weight_threshold,$
    uvfilter_count_threshold=uvfilter_count_threshold,_Extra=extra
;NOTE: 'params' can actually be EITHER params OR 'cal' structure!

name='optimal'
IF Keyword_Set(return_name_only) THEN RETURN,image_uv

IF ~(Keyword_Set(obs) AND Keyword_Set(psf) AND Keyword_Set(params)) THEN BEGIN
    IF Keyword_Set(file_path_fhd) THEN BEGIN
        fhd_save_io,status_str,vis_count,var='vis_count',/restore,file_path_fhd=file_path_fhd,_Extra=extra
        IF ~Keyword_Set(vis_count) THEN vis_count=visibility_count(obs,psf,params,vis_weight_ptr=weights,$
            file_path_fhd=file_path_fhd,_Extra=extra)
    ENDIF ELSE BEGIN
        IF N_Elements(weights) NE N_Elements(image_uv) THEN RETURN,image_uv
        vis_count=weights/Min(weights[where(weights GT 0)])
    ENDELSE
ENDIF ELSE BEGIN
    IF Keyword_Set(file_path_fhd) THEN fhd_save_io,status_str,vis_count,var='vis_count',/restore,file_path_fhd=file_path_fhd,_Extra=extra
    IF ~Keyword_Set(vis_count) THEN vis_count=visibility_count(obs,psf,params,vis_weight_ptr=weights,$
        file_path_fhd=file_path_fhd,_Extra=extra)
ENDELSE


IF Keyword_Set(uvfilter_weight_threshold) THEN BEGIN
    weights_use = abs(weights)
    filter_thresh = weights_use * weight_invert(vis_count,1.)
    weights_use /= Max(filter_thresh)
    i_cut_weight = where(weights_use LT uvfilter_weight_threshold, n_cut_weight)
    IF n_cut_weight GT 0 THEN weights_use[i_cut_weight] = 0
    filter_use = weight_invert(weights_use)
ENDIF ELSE filter_use = weight_invert(abs(weights))

IF Keyword_Set(uvfilter_count_threshold) THEN BEGIN
    i_cut_count = where(vis_count LT uvfilter_count_threshold, n_cut_count)
    IF n_cut_count GT 0 THEN filter_use[i_cut_count] = 0
ENDIF

image_uv_filtered=image_uv*filter_use
RETURN,image_uv_filtered
END
