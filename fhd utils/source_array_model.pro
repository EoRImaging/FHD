FUNCTION source_array_model,source_array,dimension=dimension,elements=elements,pol_i=pol_i,beam_correction=beam_correction

IF N_Elements(elements) EQ 0 THEN elements=dimension
ns=(size(source_array,/dimension))[0]
icomp=Complex(0,1)
xvals=meshgrid(dimension,elements,1)-dimension/2
yvals=meshgrid(dimension,elements,2)-elements/2

model_uv=Dcomplexarr(dimension,elements)
FOR si=0L,ns-1 DO BEGIN
    IF not Keyword_Set(beam_correction) THEN beam_corr_src=1. $
        ELSE beam_corr_src=(*beam_correction[pol_i])[Floor(source_array[si].x),Floor(source_array[si].y)]
    source_uv=Exp(icomp*(2d*!DPi/dimension)*((source_array[si].x-dimension/2.)*xvals+(source_array[si].y-elements/2.)*yvals))
    model_uv+=source_array[si].flux.(pol_i)*beam_corr_src*source_uv 
ENDFOR

RETURN,model_uv
END