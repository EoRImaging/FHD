FUNCTION source_dft,x_loc,y_loc,xvals,yvals,dimension=dimension,elements=elements,xvals_mirror=xvals_mirror,yvals_mirror=yvals_mirror
icomp=Complex(0,1)
fft_norm=1.

IF N_Elements(xvals) EQ 0 THEN BEGIN
    IF N_Elements(elements) EQ 0 THEN elements=dimension
    xvals=meshgrid(dimension,elements,1)
    yvals=meshgrid(dimension,elements,2)
    IF Keyword_Set(xvals_mirror) THEN BEGIN
        xvals_mirror=(Shift(Reverse(reverse(xvals,1),2),1,1))[*,0:elements/2-1]
        yvals_mirror=(Shift(Reverse(reverse(yvals,1),2),1,1))[*,0:elements/2-1]
        xvals=xvals[*,0:elements/2-1]
        yvals=yvals[*,0:elements/2-1]
    ENDIF
ENDIF

IF N_Elements(dimension) EQ 0 THEN BEGIN
    dims=size(xvals,/dimension)
    IF N_Elements(dims) EQ 1 THEN dimension=(elements=Sqrt(dims)) ELSE BEGIN
        dimension=dims[0]
        elements=dims[1]
    ENDELSE
ENDIF
IF N_Elements(elements) EQ 0 THEN elements=dimension

IF Keyword_Set(mirror) THEN BEGIN
    source_uv_vals=Complexarr
ENDIF ELSE source_uv_vals=fft_norm*Exp(icomp*(2.*!Pi/dimension)*((x_loc-dimension/2.)*xvals+(y_loc-elements/2.)*yvals))

RETURN,source_uv_vals
END