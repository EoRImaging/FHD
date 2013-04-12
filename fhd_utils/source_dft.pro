FUNCTION source_dft,x_loc,y_loc,xvals,yvals,dimension=dimension,elements=elements
icomp=Complex(0,1)
fft_norm=1.

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

source_uv_vals=fft_norm*Exp(icomp*(2.*!Pi/dimension)*((x_loc-dimension/2.)*xvals+(y_loc-elements/2.)*yvals))

RETURN,source_uv_vals
END