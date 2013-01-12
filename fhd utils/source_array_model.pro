FUNCTION source_array_model,source_array,dimension=dimension,elements=elements,pol_i=pol_i,beam_correction=beam_correction,mask=mask

IF N_Elements(elements) EQ 0 THEN elements=dimension
ns=(size(source_array,/dimension))[0]
icomp=Complex(0,1)
xvals=meshgrid(dimension,elements,1)-dimension/2
yvals=meshgrid(dimension,elements,2)-elements/2

IF Keyword_Set(mask) THEN BEGIN
    i_use=where(mask,n_use)
    xvals=xvals[i_use]
    yvals=yvals[i_use]
    model_uv=Dcomplexarr(n_use)
ENDIF ELSE model_uv=Dcomplexarr(dimension,elements)

FOR si=0L,ns-1 DO BEGIN
    IF not Keyword_Set(beam_correction) THEN beam_corr_src=1. $
        ELSE beam_corr_src=(*beam_correction[pol_i])[Floor(source_array[si].x),Floor(source_array[si].y)]
    source_uv=Exp(icomp*(2d*!DPi/dimension)*((source_array[si].x-dimension/2.)*xvals+(source_array[si].y-elements/2.)*yvals))
    model_uv+=source_array[si].flux.(pol_i)*beam_corr_src*source_uv 
ENDFOR

IF Keyword_Set(mask) THEN BEGIN
    model_uv_full=Dcomplexarr(dimension,elements)
    model_uv_full[i_use]=model_uv
    RETURN,model_uv_full
ENDIF ELSE RETURN,model_uv

END