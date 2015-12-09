FUNCTION source_dft,x_loc,y_loc,xvals,yvals,dimension=dimension,elements=elements,flux=flux,$
    silent=silent,conserve_memory=conserve_memory,inds_use=inds_use,double_precision=double_precision
fft_norm=1.
IF N_Elements(conserve_memory) EQ 0 THEN conserve_memory=1
IF conserve_memory GT 1E6 THEN mem_thresh=conserve_memory ELSE mem_thresh=1E8
;IF Keyword_Set(degpix) THEN fft_norm=(degpix*!DtoR)^2. ELSE fft_norm=1.

IF N_Elements(flux) EQ 0 THEN flux=fltarr(size(x_loc,/dimension)>1)+1.
IF Keyword_Set(inds_use) THEN BEGIN
    x_loc_use=x_loc[inds_use]
    y_loc_use=y_loc[inds_use]
    IF size(flux,/type) EQ 10 THEN BEGIN ;check if pointer type. This allows the same locations to be used for multiple sets of fluxes
        fbin_use=where(Ptr_valid(flux),n_fbin)
        flux_use=Ptrarr(size(flux,/dimension))
        flux_ptr_cleanup=1
        FOR f_i=0L,n_fbin-1 DO flux_use[fbin_use[f_i]]=Ptr_new((*flux[fbin_use[f_i]])[inds_use])
    ENDIF ELSE flux_use=flux[inds_use]
ENDIF ELSE BEGIN
    x_loc_use=x_loc
    y_loc_use=y_loc
    flux_use=flux
ENDELSE

IF N_Elements(xvals) EQ 0 THEN BEGIN
    IF N_Elements(elements) EQ 0 THEN elements=Float(dimension)
    xvals=Reform(meshgrid(dimension,elements,1)-dimension/2,dimension*elements)
    yvals=Reform(meshgrid(dimension,elements,2)-elements/2,dimension*elements)
ENDIF

IF N_Elements(dimension) EQ 0 THEN BEGIN
    dims=size(xvals,/dimension)
    IF N_Elements(dims) EQ 1 THEN dimension=(elements=Sqrt(dims)) ELSE BEGIN
        dimension=dims[0]
        elements=dims[1]
    ENDELSE
ENDIF
IF N_Elements(elements) EQ 0 THEN elements=dimension
IF Keyword_Set(double_precision) THEN Pi=!DPi ELSE Pi=!Pi
icomp=Complex(0,1,double=double_precision)

x_use=x_loc_use-dimension/2.
y_use=y_loc_use-elements/2.
x_use*=(2.*Pi/dimension)
y_use*=(2.*Pi/dimension)

element_check=Float(N_Elements(xvals))*Float(N_Elements(x_use))

IF size(flux_use,/type) EQ 10 THEN BEGIN ;check if pointer type. This allows the same locations to be used for multiple sets of fluxes
    fbin_use=where(Ptr_valid(flux_use),n_fbin)
    source_uv_vals=Ptrarr(size(flux_use,/dimension))
    FOR fbin_i=0L,n_fbin-1 DO source_uv_vals[fbin_use[fbin_i]]=Ptr_new(Complexarr(size(xvals,/dimension)))
    IF Keyword_Set(conserve_memory) AND (element_check GT mem_thresh) THEN BEGIN
        memory_bins=Round(element_check/mem_thresh)
        
        n0=N_Elements(x_use)
        binsize=Lonarr(memory_bins)+Round(n0/memory_bins)
        binsize[memory_bins-1]-=Total(binsize)-n0
        bin_start=[0,Total(binsize,/cumulative)]
        FOR bin_i=0L,memory_bins-1 DO BEGIN
            inds=lindgen(binsize[bin_i])+bin_start[bin_i]
            phase=matrix_multiply(xvals,x_use[inds])+matrix_multiply(yvals,y_use[inds])
            cos_term=Cos(phase)
            sin_term=Sin(Temporary(phase))
            FOR fbin_i=0L,n_fbin-1 DO BEGIN
                flux_vals=(*flux_use[fbin_use[fbin_i]])[inds]
                source_uv_real_vals=matrix_multiply(cos_term,flux_vals)
                source_uv_im_vals=matrix_multiply(sin_term,flux_vals)
                *source_uv_vals[fbin_use[fbin_i]]+=Complex(source_uv_real_vals,source_uv_im_vals,double=double_precision)
            ENDFOR
            cos_term=(sin_term=0) ;free memory
        ENDFOR
    ENDIF ELSE BEGIN
        phase=matrix_multiply(xvals,x_use)+matrix_multiply(yvals,y_use)
        cos_term=Cos(phase)
        sin_term=Sin(Temporary(phase))
        FOR fbin_i=0L,n_fbin-1 DO BEGIN
            source_uv_real_vals=matrix_multiply(cos_term,*flux_use[fbin_use[fbin_i]])
            source_uv_im_vals=matrix_multiply(sin_term,*flux_use[fbin_use[fbin_i]])
            *source_uv_vals[fbin_use[fbin_i]]+=Complex(source_uv_real_vals,source_uv_im_vals,double=double_precision)
        ENDFOR
        cos_term=(sin_term=0) ;free memory
    ENDELSE
    FOR fbin_i=0L,n_fbin-1 DO *source_uv_vals[fbin_use[fbin_i]]*=fft_norm
ENDIF ELSE BEGIN
    IF Keyword_Set(conserve_memory) AND (element_check GT mem_thresh) THEN BEGIN
        memory_bins=Round(element_check/mem_thresh)
        source_uv_vals=Complexarr(size(xvals,/dimension))
        n0=N_Elements(x_use)
        binsize=Lonarr(memory_bins)+Round(n0/memory_bins)
        binsize[memory_bins-1]-=Total(binsize)-n0
        bin_start=[0,Total(binsize,/cumulative)]
        FOR bin_i=0L,memory_bins-1 DO BEGIN
            inds=lindgen(binsize[bin_i])+bin_start[bin_i]
            phase=matrix_multiply(xvals,x_use[inds])+matrix_multiply(yvals,y_use[inds])
            cos_term=Cos(phase)
            source_uv_real_vals=matrix_multiply(Temporary(cos_term),flux_use[inds])
            sin_term=Sin(Temporary(phase))
            source_uv_im_vals=matrix_multiply(Temporary(sin_term),flux_use[inds])
            source_uv_vals+=Complex(source_uv_real_vals,source_uv_im_vals,double=double_precision)
        ENDFOR
    ENDIF ELSE BEGIN
        phase=matrix_multiply(xvals,x_use)+matrix_multiply(yvals,y_use)
        cos_term=Cos(phase)
        source_uv_real_vals=matrix_multiply(Temporary(cos_term),flux_use)
        sin_term=Sin(Temporary(phase))
        source_uv_im_vals=matrix_multiply(Temporary(sin_term),flux_use)
        source_uv_vals=Complex(source_uv_real_vals,source_uv_im_vals,double=double_precision)
    ENDELSE
    source_uv_vals*=fft_norm
ENDELSE

IF Keyword_Set(flux_ptr_cleanup) THEN Ptr_free,flux_use
RETURN,source_uv_vals
END