FUNCTION source_dft,x_loc,y_loc,xvals,yvals,dimension=dimension,elements=elements,flux=flux,$
    silent=silent,conserve_memory=conserve_memory,inds_use=inds_use,double_precision=double_precision, $
    gaussian_source_models = gaussian_source_models

fft_norm=1.
icomp = Complex(0,1)
IF N_Elements(conserve_memory) EQ 0 THEN conserve_memory=1
IF conserve_memory GT 1E6 THEN mem_thresh=conserve_memory ELSE mem_thresh=1E8
; NOTE: Now perform ALL calculations as double precision, and convert to floats if double_precision is not set
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
Pi=!DPi
icomp=DComplex(0,1)

x_use=x_loc_use-dimension/2.
y_use=y_loc_use-elements/2.
x_use*=(2.*Pi/dimension)
y_use*=(2.*Pi/dimension)
if keyword_set(gaussian_source_models) then begin
    ; gauss_inds_use is the subset of indices in inds_use that are gaussian sources
    ; gauss_inds_dft is the subset of indices in the input gaussian_source_models structure that are currently being dft'd
    match, inds_use, gaussian_source_models.gauss_inds, gauss_inds_use, gauss_inds_dft
    n_gauss = N_elements(gauss_inds_use)
    ; Define gaussian parameters for only the sources in this inds_use set that are gaussian
    gauss_x_use2=(gaussian_source_models.gaussian_x[gauss_inds_dft]*(2.*Pi/dimension))^2
    gauss_y_use2=(gaussian_source_models.gaussian_y[gauss_inds_dft]*(2.*Pi/dimension))^2
    gaussian_rot=gaussian_source_models.gaussian_rot[gauss_inds_dft]
    xvals2 = xvals*xvals
    yvals2 = yvals*yvals
endif

element_check=Long64(N_Elements(xvals))*Long64(N_Elements(x_use))

; Check if pointer type. This allows the same locations to be used for multiple sets of fluxes
IF size(flux_use,/type) EQ 10 THEN BEGIN 
    fbin_use=where(Ptr_valid(flux_use),n_fbin)
    source_uv_vals=Ptrarr(size(flux_use,/dimension))
    IF size(*flux_use[0],/type) GE 6 THEN complex_flag=1 ELSE complex_flag=0
    FOR fbin_i=0L,n_fbin-1 DO source_uv_vals[fbin_use[fbin_i]]=Ptr_new(Complexarr(size(xvals,/dimension)))
ENDIF

IF Keyword_Set(conserve_memory) AND (element_check GT mem_thresh) THEN BEGIN
    ; DFT with memory management
    ; If the max memory is less than the estimated memory needed to DFT all sources at once, then break the DFT into chunks

    ; Using gaussian sources requires more more memory
    if keyword_set(gaussian_source_models) then extra_factor=12 else extra_factor = 8
    ; Estimate number of memory bins needed to maintain memory threshold
    memory_bins=Ceil(element_check*extra_factor/mem_thresh) 

    n0=N_Elements(x_use) ;Number of sources in primary beam to be DFTd
    sources_per_bin=Round(n0/memory_bins)
    binsize=Lonarr(memory_bins)+sources_per_bin ;Array of number of sources to DFT per bin

    ;Special case of memory bins not being able to hold needed binsize for all DFTs
    if memory_bins*sources_per_bin LT n0 then begin
        unbinned_sources=n0-Total(binsize) ;Number of leftover sources that can't held currently
        num_more_bins=Ceil(unbinned_sources/sources_per_bin) ;Number of bins that need to be added to hold all sources at required binsize
        memory_bins+=num_more_bins ;Recalculate number of bins necessary
        binsize=Lonarr(memory_bins)+sources_per_bin ;Recalculate binsize given new memory bins
    endif

    ;Last bin may contain less sources than the other bins (number of bins may not evenly divide the number of sources)
    binsize[memory_bins-1]-=Total(binsize)-n0
    ;Index of the first source in each bin
    bin_start=[0,Total(binsize,/cumulative)]

    FOR bin_i=0L,memory_bins-1 DO BEGIN
        inds=lindgen(binsize[bin_i])+bin_start[bin_i]
        ; Calculate sin and cosine of exponential in DFT (faster than a direct exp)
        phase=matrix_multiply(x_use[inds],xvals) + matrix_multiply(y_use[inds],yvals)
        cos_term=Cos(phase)
        sin_term=Sin(Temporary(phase))

        IF keyword_set(gaussian_source_models) THEN BEGIN
            ; suba is the subset of indices in gauss_inds_use that are in this memory chunk loop
            ; subb is the subset of indices in inds in this memory chunk loop that are gaussian
            match, gauss_inds_use, inds, suba, subb
            source_envelope = fltarr(N_elements(suba),N_elements(xvals),/NOZERO)
            rot_inds = where(gaussian_rot[suba] NE 0,n_rot,complement=unrot_inds,ncomplement=n_unrot)
            if n_rot gt 0 then begin
                ; For gaussians that are rotated with respect to the x,y plane, calculate their source envelope
                bin_rot_subset = suba[rot_inds]
                cos_rot = Cos(gaussian_rot[bin_rot_subset])
                sin_rot = Sin(gaussian_rot[bin_rot_subset])

                source_envelope[rot_inds,*] = exp(-1./2 * $
                  (matrix_multiply(gauss_x_use2[bin_rot_subset]*cos_rot^2.+gauss_y_use2[bin_rot_subset]*sin_rot^2., xvals2) $
                  + matrix_multiply(gauss_x_use2[bin_rot_subset]*sin_rot^2.+gauss_y_use2[bin_rot_subset]*cos_rot^2., yvals2)) $
                  + matrix_multiply((gauss_y_use2[bin_rot_subset]-gauss_x_use2[bin_rot_subset])*cos_rot*sin_rot, xvals*yvals))
            endif
            if n_unrot gt 0 then begin
                ; For aligned gaussians, avoid extra operations by directly calculating their source envelope with rot=0
                bin_unrot_subset = suba[unrot_inds]
                source_envelope[unrot_inds,*] = exp(-1./2*(matrix_multiply(gauss_x_use2[bin_unrot_subset], xvals2) $
                  + matrix_multiply(gauss_y_use2[bin_unrot_subset], yvals2)))
            endif

            cos_term[subb,*] *= source_envelope
            sin_term[subb,*] *= source_envelope
        ENDIF

        IF size(flux_use,/type) EQ 10 THEN BEGIN
            FOR fbin_i=0L,n_fbin-1 DO BEGIN
                flux_vals=(*flux_use[fbin_use[fbin_i]])[inds]
                source_uv_real_vals=matrix_multiply(flux_vals,cos_term)
                source_uv_im_vals=matrix_multiply(flux_vals,sin_term)
                *source_uv_vals[fbin_use[fbin_i]]+=Temporary(source_uv_real_vals) + icomp * Temporary(source_uv_im_vals)
            ENDFOR
        ENDIF ELSE BEGIN
            source_uv_real_vals=matrix_multiply(flux_use[inds], Temporary(cos_term))
            source_uv_im_vals=matrix_multiply(flux_use[inds], Temporary(sin_term))
            source_uv_vals+=Temporary(source_uv_real_vals) + icomp * Temporary(source_uv_im_vals)
        ENDELSE

    ENDFOR

ENDIF ELSE BEGIN
    ; DFT without memory management

    phase=matrix_multiply(x_use,xvals)+matrix_multiply(y_use,yvals)
    cos_term=Cos(phase)
    sin_term=Sin(Temporary(phase))

    IF keyword_set(gaussian_source_models) THEN BEGIN
        source_envelope = fltarr(n_gauss, N_elements(xvals),/NOZERO)
        rot_inds = where(gaussian_rot NE 0,n_rot,complement=unrot_inds,ncomplement=n_unrot)
           
        if n_rot gt 0 then begin
            ; For gaussians that are rotated with respect to the x,y plane, calculate their source envelope
            cos_rot = Cos(gaussian_rot[rot_inds])
            sin_rot = Sin(gaussian_rot[rot_inds])
            source_envelope[rot_inds,*] = exp(-1./2 * $
              (matrix_multiply(gauss_x_use2[rot_inds]*cos_rot^2.+gauss_y_use2[rot_inds]*sin_rot^2., xvals2) $
              + matrix_multiply(gauss_x_use2[rot_inds]*sin_rot^2.+gauss_y_use2[rot_inds]*cos_rot^2., yvals2)) $
              + matrix_multiply((gauss_y_use2[rot_inds]-gauss_x_use2[rot_inds])*cos_rot*sin_rot, xvals*yvals))
        endif
        if n_unrot gt 0 then begin
            ; For aligned gaussians, avoid extra operations by directly calculating their source envelope with rot=0
            source_envelope[unrot_inds,*] = exp(-1./2 *(matrix_multiply(gauss_x_use2[unrot_inds], xvals2) $
              + matrix_multiply(gauss_y_use2[unrot_inds], yvals2)))
        endif
        cos_term[gauss_inds_use,*] *= source_envelope
        sin_term[gauss_inds_use,*] *= source_envelope
    ENDIF

    IF size(flux_use,/type) EQ 10 THEN BEGIN
        FOR fbin_i=0L,n_fbin-1 DO BEGIN
            source_uv_real_vals=matrix_multiply(*flux_use[fbin_use[fbin_i]], cos_term)
            source_uv_im_vals=matrix_multiply(*flux_use[fbin_use[fbin_i]], sin_term)
            *source_uv_vals[fbin_use[fbin_i]]+=DComplex(source_uv_real_vals,source_uv_im_vals)
        ENDFOR
    ENDIF ELSE BEGIN
        source_uv_real_vals=matrix_multiply(flux_use[inds], Temporary(cos_term))
        source_uv_im_vals=matrix_multiply(flux_use, Temporary(sin_term))
        source_uv_vals=Temporary(source_uv_real_vals) + icomp * Temporary(source_uv_im_vals)
    ENDELSE 

ENDELSE
cos_term=(sin_term=0) ;free memory

IF size(flux_use,/type) EQ 10 THEN BEGIN    
    FOR fbin_i=0L,n_fbin-1 DO *source_uv_vals[fbin_use[fbin_i]]*=fft_norm
    IF not Keyword_Set(double_precision) THEN $
      FOR fbin_i=0L,n_fbin-1 DO $
      *source_uv_vals[fbin_use[fbin_i]]=Complex(Temporary(*source_uv_vals[fbin_use[fbin_i]]))
ENDIF ELSE BEGIN
    source_uv_vals*=fft_norm
    IF not Keyword_Set(double_precision) THEN source_uv_vals=Complex(source_uv_vals)
ENDELSE

IF Keyword_Set(flux_ptr_cleanup) THEN Ptr_free,flux_use
RETURN,source_uv_vals
END
