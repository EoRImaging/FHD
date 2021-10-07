FUNCTION filter_uv_natural,image_uv,obs=obs,psf=psf,params=params,name=name,weights=weights,$
    filter=filter,return_name_only=return_name_only,_Extra=extra
name='natural'
IF Keyword_Set(return_name_only) THEN RETURN,image_uv
IF N_Elements(weights) NE N_Elements(image_uv) THEN RETURN,image_uv
dimension=(size(image_uv,/dimension))[0]
elements=(size(image_uv,/dimension))[1]

filter_use=make_array(dimension,elements,/float,value=1.0)

IF Ptr_valid(filter) THEN *filter=filter_use

image_uv_filtered=image_uv*filter_use
RETURN,image_uv_filtered
END
