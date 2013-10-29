FUNCTION filter_uv_uniform2,image_uv,obs=obs,psf=psf,params=params,weights=weights,name=name,filter=filter,_Extra=extra
;NOTE: 'params' can actually be EITHER params OR 'cal' structure!

name='uniform2'

;NOTE: This does not make use of fine-grained flagging, but relies on coarse flags from the obs structure 
; (i.e. a list of tiles completely flagged, and of frequencies completely flagged)
IF ~(Keyword_Set(obs) AND Keyword_Set(psf) AND Keyword_Set(params)) THEN BEGIN
    IF N_Elements(weights) NE N_Elements(image_uv) THEN RETURN,image_uv
    vis_count=weights 
ENDIF ELSE vis_count=visibility_count(obs,psf,params)
vis_count[where(vis_count)]=vis_count[where(vis_count)]>(Max(vis_count)/1000.)
filter_use=weight_invert(vis_count) ;should have psf.dim^2. factor, but that would divided out in the normalization later anyway

IF N_Elements(weights) EQ N_Elements(image_uv) THEN wts_i=where(weights,n_wts) ELSE wts_i=where(filter_use,n_wts)
IF n_wts GT 0 THEN filter_use/=Mean(filter_use[wts_i]) ELSE filter_use/=Mean(filter_use)

IF Ptr_valid(filter) THEN *filter=filter_use

image_uv_filtered=image_uv*filter_use
RETURN,image_uv_filtered
END