FUNCTION filter_uv_hanning,image_uv

image_uv_filtered=image_uv*(1.-Hanning(size(image_uv_filtered,/dimension)))
RETURN,image_uv_filtered
END