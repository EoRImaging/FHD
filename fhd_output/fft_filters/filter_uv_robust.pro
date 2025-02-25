FUNCTION filter_uv_robust,image_uv,obs=obs,psf=psf,params=params,weights=weights,name=name,filter=filter,$
    file_path_fhd=file_path_fhd,return_name_only=return_name_only,robust_param=robust_param,_Extra=extra
;NOTE: 'params' can actually be EITHER params OR 'cal' structure!

name='robust' + number_formatter(robust_param)
IF Keyword_Set(return_name_only) THEN RETURN,image_uv
;NOTE: This does not make use of fine-grained flagging, but relies on coarse flags from the obs structure 
; (i.e. a list of tiles completely flagged, and of frequencies completely flagged)

IF ~(Keyword_Set(obs) AND Keyword_Set(psf) AND Keyword_Set(params)) THEN BEGIN
    IF Keyword_Set(file_path_fhd) THEN BEGIN
        fhd_save_io,status_str,vis_count,var='vis_count',/restore,file_path_fhd=file_path_fhd,_Extra=extra
        IF ~Keyword_Set(vis_count) THEN vis_count=visibility_count(obs,psf,params,file_path_fhd=file_path_fhd,_Extra=extra)
    ENDIF ELSE BEGIN
        IF N_Elements(weights) NE N_Elements(image_uv) THEN RETURN,image_uv
        vis_count=weights/Min(weights[where(weights GT 0)])
    ENDELSE
ENDIF ELSE BEGIN
    IF Keyword_Set(file_path_fhd) THEN fhd_save_io,status_str,vis_count,var='vis_count',/restore,file_path_fhd=file_path_fhd,_Extra=extra
    IF ~Keyword_Set(vis_count) THEN vis_count=visibility_count(obs,psf,params,file_path_fhd=file_path_fhd,_Extra=extra)
ENDELSE
filter_use=1/(1+vis_count*(5.*10^(-robust_param))^2./(total(vis_count^2.)/total(vis_count))) ;should have psf.dim^2. factor, but that would divide out in the normalization later anyway

IF N_Elements(weights) EQ N_Elements(image_uv) THEN wts_i=where(weights,n_wts) ELSE wts_i=where(filter_use,n_wts)
IF n_wts GT 0 THEN filter_use/=Mean(filter_use[wts_i]) ELSE filter_use/=Mean(filter_use)

IF Ptr_valid(filter) THEN *filter=filter_use

image_uv_filtered=image_uv*filter_use
RETURN,image_uv_filtered
END
