FUNCTION filter_uv_hanning,image_uv,_Extra=extra

dimension=(size(image_uv,/dimension))[0]
elements=(size(image_uv,/dimension))[1]
image_uv_filtered=image_uv*(1.-Hanning(dimension,elements))
RETURN,image_uv_filtered
END