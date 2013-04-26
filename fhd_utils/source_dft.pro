FUNCTION source_dft,x_loc,y_loc,xvals,yvals,dimension=dimension,elements=elements,mirror_inds=mirror_inds,degpix=degpix
icomp=Complex(0,1)
IF Keyword_Set(degpix) THEN fft_norm=(degpix*!DtoR)^2. ELSE fft_norm=1.

IF N_Elements(xvals) EQ 0 THEN BEGIN
    IF N_Elements(elements) EQ 0 THEN elements=dimension
    xvals=meshgrid(dimension,elements,1)
    yvals=meshgrid(dimension,elements,2)
ENDIF

IF N_Elements(dimension) EQ 0 THEN BEGIN
    dims=size(xvals,/dimension)
    IF N_Elements(dims) EQ 1 THEN dimension=(elements=Sqrt(dims)) ELSE BEGIN
        dimension=dims[0]
        elements=dims[1]
    ENDELSE
ENDIF
IF N_Elements(elements) EQ 0 THEN elements=dimension

IF Keyword_Set(mirror_inds) THEN BEGIN
    dims=size(xvals,/dimension)
    IF N_Elements(dims) EQ 1 THEN BEGIN
        source_uv_vals=Complexarr(Max(mirror_inds)+1)
        dft_vals=fft_norm*Exp(icomp*(2.*!Pi/dimension)*((x_loc-dimension/2.)*xvals+(y_loc-elements/2.)*yvals))
        source_uv_vals[0:dims-1]=dft_vals
        source_uv_vals[mirror_inds]=Conj(dft_vals)
    ENDIF ELSE BEGIN
        dft_vals=fft_norm*Exp(icomp*(2.*!Pi/dimension)*((x_loc-dimension/2.)*xvals[*,0:elements/2-1]+(y_loc-elements/2.)*yvals[*,0:elements/2-1]))
        source_uv_vals=Complexarr(dims[0],2.*dims[1])
        source_uv_vals[*,0:elements/2-1]=dft_vals
        source_uv_vals+=Shift(Reverse(reverse(Conj(source_uv_vals),1),2),1,1)
    ENDELSE
ENDIF ELSE source_uv_vals=fft_norm*Exp(icomp*(2.*!Pi/dimension)*((x_loc-dimension/2.)*xvals+(y_loc-elements/2.)*yvals))

RETURN,source_uv_vals
END