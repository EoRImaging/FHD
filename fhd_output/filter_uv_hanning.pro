FUNCTION filter_uv_hanning,image_uv,name=name,weights=weights,filter=filter,_Extra=extra
name='hanning'
dimension=(size(image_uv,/dimension))[0]
elements=(size(image_uv,/dimension))[1]
filter_use=(1.-Hanning(dimension,elements))

IF N_Elements(weights) EQ N_Elements(image_uv) THEN BEGIN
    wts_i=where(weights,n_wts)
    IF n_wts GT 0 THEN filter_use/=Mean(filter_use[wts_i]) ELSE filter_use/=Mean(filter_use)
ENDIF

IF Ptr_valid(filter) THEN *filter=filter_use
;filter_use/=Mean(filter_use) ;preserve mean value
image_uv_filtered=image_uv*filter_use
RETURN,image_uv_filtered
END