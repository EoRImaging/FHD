FUNCTION source_dft,x_loc,y_loc,xvals,yvals,dimension=dimension,elements=elements,degpix=degpix,flux=flux,$
    conserve_memory=conserve_memory
icomp=Complex(0,1)
fft_norm=1.
IF N_Elements(conserve_memory) EQ 0 THEN conserve_memory=0
;IF Keyword_Set(degpix) THEN fft_norm=(degpix*!DtoR)^2. ELSE fft_norm=1.

IF N_Elements(xvals) EQ 0 THEN BEGIN
    IF N_Elements(elements) EQ 0 THEN elements=dimension
    xvals=meshgrid(dimension,elements,1)-dimension/2
    yvals=meshgrid(dimension,elements,2)-elements/2
ENDIF
IF N_Elements(flux) EQ 0 THEN flux=fltarr(size(x_loc,/dimension)>1)+1.

IF N_Elements(dimension) EQ 0 THEN BEGIN
    dims=size(xvals,/dimension)
    IF N_Elements(dims) EQ 1 THEN dimension=(elements=Sqrt(dims)) ELSE BEGIN
        dimension=dims[0]
        elements=dims[1]
    ENDELSE
ENDIF
IF N_Elements(elements) EQ 0 THEN elements=dimension
;source_uv_vals=fft_norm*Exp(icomp*(2.*!Pi/dimension)*((x_loc-dimension/2.)*xvals+(y_loc-elements/2.)*yvals))

x_use=x_loc-dimension/2.
y_use=y_loc-elements/2.
x_use*=(2.*!Pi/dimension)
y_use*=(2.*!Pi/dimension)

element_check=Float(N_Elements(xvals))*Float(N_Elements(x_use))

;phase=Exp(icomp*(2.*!Pi/dimension)*phase)
;source_uv_vals=matrix_multiply(phase,flux)
IF Keyword_Set(conserve_memory) AND (element_check GT 1E8) THEN BEGIN
    memory_bins=Round(element_check/1E8)
    source_uv_vals=Complexarr(size(xvals,/dimension))
    n0=N_Elements(x_use)
    binsize=Lonarr(memory_bins)+Round(n0/memory_bins)
    binsize[memory_bins-1]-=Total(binsize)-n0
    bin_start=[0,Total(binsize,/cumulative)]
    FOR bin_i=0L,memory_bins-1 DO BEGIN
        inds=lindgen(binsize[bin_i])+bin_start[bin_i]
        phase=matrix_multiply(xvals,x_use[inds])+matrix_multiply(yvals,y_use[inds])
        cos_term=Cos(phase)
        source_uv_real_vals=matrix_multiply(Temporary(cos_term),flux[inds])
        sin_term=Sin(Temporary(phase))
        source_uv_im_vals=matrix_multiply(Temporary(sin_term),flux[inds])
        source_uv_vals+=Complex(source_uv_real_vals,source_uv_im_vals)
    ENDFOR
ENDIF ELSE BEGIN
    phase=matrix_multiply(xvals,x_use)+matrix_multiply(yvals,y_use)
    cos_term=Cos(phase)
    source_uv_real_vals=matrix_multiply(Temporary(cos_term),flux)
    sin_term=Sin(Temporary(phase))
    source_uv_im_vals=matrix_multiply(Temporary(sin_term),flux)
    source_uv_vals=Complex(source_uv_real_vals,source_uv_im_vals)
ENDELSE
source_uv_vals*=fft_norm

RETURN,source_uv_vals
END