FUNCTION filter_uv_radial,image_uv,name=name,weights=weights,filter=filter,_Extra=extra
name='radial'
;IF N_Elements(filter) EQ N_Elements(image_uv) THEN RETURN,image_uv*filter
IF N_Elements(weights) NE N_Elements(image_uv) THEN RETURN,image_uv
dimension=(size(image_uv,/dimension))[0]
elements=(size(image_uv,/dimension))[1]

radial_map=fft_shift(dist(dimension,elements))
rad_hist=histogram(radial_map,min=1,/binsize,reverse_ind=ri)
rad_bin=where(rad_hist,n_bin)

filter_use=Sqrt(radial_map)
wts_i=where(weights,n_wts)
IF n_wts GT 0 THEN filter_use/=Mean(filter_use[wts_i]) ELSE filter_use/=Mean(filter_use)

IF Ptr_valid(filter) THEN *filter=filter_use

image_uv_filtered=image_uv*filter_use
RETURN,image_uv_filtered
END