FUNCTION beam_estimate,residual_image,radius=radius,mask=mask,beam_model=beam_model,nsigma=nsigma
;assumes apparent brightness frame if beam_model is supplied, otherwise assumes holographic frame

dimension=(size(residual_image,/dimension))[0]
elements=(size(residual_image,/dimension))[1]
IF N_Elements(radius) EQ 0 THEN radius=Round(sqrt(dimension))
IF N_Elements(mask) EQ 0 THEN mask_use=fltarr(dimension,elements)+1 ELSE mask_use=mask
IF N_Elements(nsigma) EQ 0 THEN nsigma=3.

IF Keyword_Set(beam_model) THEN BEGIN
    background=Max_filter(residual_image,radius=((radius/2.)<10.),mask=mask_use,/circle,/median)
    sigma=Stddev(residual_image[where(mask_use)])
    cut_i=where(Abs(residual_image-background) GT nsigma*sigma,n_cut)
    IF n_cut GT 0 THEN mask_use[cut_i]=0
ENDIF

beam_est=Max_filter(residual_image,/stddev,radius=radius,mask=mask_use,/edge_truncate,/circle)

IF Keyword_Set(beam_model) THEN beam_est*=beam_model

IF Keyword_Set(beam_model) THEN BEGIN
    i_use=where(mask_use)
    scale=(linfit(beam_est[i_use],beam_model[i_use]))[1]
    beam_est*=scale
ENDIF ELSE BEGIN
    beam_est_smooth=Smooth(beam_est,width/2.)
    scale=1./Max(beam_est_smooth)
    beam_est*=scale
ENDELSE
RETURN,beam_est
END