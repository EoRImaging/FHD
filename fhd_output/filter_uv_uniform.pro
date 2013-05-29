FUNCTION filter_uv_uniform,image_uv,name=name,weights=weights,filter=filter,_Extra=extra
name='uniform'
;IF N_Elements(filter) EQ N_Elements(image_uv) THEN RETURN,image_uv*filter
IF N_Elements(weights) NE N_Elements(image_uv) THEN RETURN,image_uv
dimension=(size(image_uv,/dimension))[0]
elements=(size(image_uv,/dimension))[1]

filter_use=Median(weights,3)
filter_use=weight_invert(filter_use)

IF Max(filter_use) EQ 0 THEN RETURN,image_uv 

wts_i=where(weights,n_wts)
IF n_wts GT 0 THEN filter_use/=Mean(filter_use[wts_i]) ELSE filter_use/=Mean(filter_use)

IF Ptr_valid(filter) THEN *filter=filter_use

;filter/=Mean(filter) ;preserve mean value
image_uv_filtered=image_uv*filter
RETURN,image_uv_filtered
END