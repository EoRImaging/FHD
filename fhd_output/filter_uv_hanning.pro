FUNCTION filter_uv_hanning,image_uv,name=name,_Extra=extra
name='hanning'
dimension=(size(image_uv,/dimension))[0]
elements=(size(image_uv,/dimension))[1]
filter=(1.-Hanning(dimension,elements))

;filter/=Mean(filter) ;preserve mean value
image_uv_filtered=image_uv*filter
RETURN,image_uv_filtered
END