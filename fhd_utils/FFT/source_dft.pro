FUNCTION source_dft,x_loc,y_loc,xvals,yvals,dimension=dimension,elements=elements,flux=flux,$
    silent=silent,conserve_memory=conserve_memory,inds_use=inds_use,double_precision=double_precision, $
    gaussian_source_models = gaussian_source_models, gaussian_x = gaussian_x, gaussian_y = gaussian_y, $
    gaussian_rot = gaussian_rot

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

element_check=Long64(N_Elements(xvals))*Long64(N_Elements(x_use))

IF size(flux_use,/type) EQ 10 THEN BEGIN ;check if pointer type. This allows the same locations to be used for multiple sets of fluxes
    fbin_use=where(Ptr_valid(flux_use),n_fbin)
    source_uv_vals=Ptrarr(size(flux_use,/dimension))
    IF size(*flux_use[0],/type) GE 6 THEN complex_flag=1 ELSE complex_flag=0
    FOR fbin_i=0L,n_fbin-1 DO source_uv_vals[fbin_use[fbin_i]]=Ptr_new(Dcomplexarr(size(xvals,/dimension)))

    ;*****Start memory-managed DFT
    ;If the max memory is less than the estimated memory needed to DFT all sources at once, then break the DFT into chunks
    IF Keyword_Set(conserve_memory) AND (element_check GT mem_thresh) THEN BEGIN
      
        memory_bins=Ceil(element_check/mem_thresh) ;Estimate number of memory bins needed to maintain memory threshold

        n0=N_Elements(x_use) ;Number of sources in primary beam to be DFTd
        sources_per_bin=Round(n0/memory_bins)
        binsize=Lonarr(memory_bins)+sources_per_bin ;Array of number of sources to DFT per bin

        ;***Start special case of memory bins not being able to hold needed binsize for all DFTs
        if memory_bins*sources_per_bin LT n0 then begin
            unbinned_sources=n0-Total(binsize) ;Number of leftover sources that can't held currently
            num_more_bins=Ceil(unbinned_sources/sources_per_bin) ;Number of bins that need to be added to hold all sources at required binsize
            memory_bins+=num_more_bins ;Recalculate number of bins necessary
            binsize=Lonarr(memory_bins)+sources_per_bin ;Recalculate binsize given new memory bins
        endif
        ;***End special case

        ;Make the last bin forces number of sources distributed throughout bins equals the total num of sources.
        binsize[memory_bins-1]-=Total(binsize)-n0
        bin_start=[0,Total(binsize,/cumulative)]
        FOR bin_i=0L,memory_bins-1 DO BEGIN
            inds=lindgen(binsize[bin_i])+bin_start[bin_i]
            phase=matrix_multiply(xvals,x_use[inds])+matrix_multiply(yvals,y_use[inds])
            cos_term=Cos(phase)
            sin_term=Sin(Temporary(phase))
            IF keyword_set(gaussian_source_models) THEN BEGIN
              if n_elements(gaussian_rot) gt 0 then begin
                source_envelope = exp(-2*Pi^2.*(matrix_multiply(xvals^2., gaussian_x[inds]^2.*Cos(gaussian_rot[inds])^2.+gaussian_y[inds]^2.*Sin(gaussian_rot[inds])^2.) $
                  + matrix_multiply(yvals^2., gaussian_x[inds]^2.*Sin(gaussian_rot[inds])^2.+gaussian_y[inds]^2.*Cos(gaussian_rot[inds])^2.)) $
                  + matrix_multiply(xvals*yvals, (gaussian_y[inds]^2.-gaussian_x[inds]^2.)*Cos(gaussian_rot[inds])*Sin(gaussian_rot[inds])))
              endif else begin
                source_envelope = exp(-2*Pi^2.*(matrix_multiply(xvals^2., gaussian_x[inds]^2.)+matrix_multiply(yvals^2., gaussian_y[inds]^2.)))
              endelse
              cos_term *= source_envelope
              sin_term *= source_envelope
            ENDIF
            FOR fbin_i=0L,n_fbin-1 DO BEGIN
                flux_vals=(*flux_use[fbin_use[fbin_i]])[inds]
                source_uv_real_vals=matrix_multiply(cos_term,flux_vals)
                source_uv_im_vals=matrix_multiply(sin_term,flux_vals)
                *source_uv_vals[fbin_use[fbin_i]]+=Temporary(source_uv_real_vals) + icomp * Temporary(source_uv_im_vals)
            ENDFOR
            cos_term=(sin_term=0) ;free memory
        ENDFOR
    ;*****End of memory-managed DFT

    ENDIF ELSE BEGIN
      phase=matrix_multiply(xvals,x_use)+matrix_multiply(yvals,y_use)
      cos_term=Cos(phase)
      sin_term=Sin(Temporary(phase))
      IF keyword_set(gaussian_source_models) THEN BEGIN
        if n_elements(gaussian_rot) gt 0 then begin
          source_envelope = exp(-2*Pi^2.*(matrix_multiply(xvals^2., gaussian_x^2.*Cos(gaussian_rot)^2.+gaussian_y^2.*Sin(gaussian_rot)^2.) $
            + matrix_multiply(yvals^2., gaussian_x^2.*Sin(gaussian_rot)^2.+gaussian_y^2.*Cos(gaussian_rot)^2.)) $
            + matrix_multiply(xvals*yvals, (gaussian_y^2.-gaussian_x^2.)*Cos(gaussian_rot)*Sin(gaussian_rot)))
        endif else begin
          source_envelope = exp(-2*Pi^2.*(matrix_multiply(xvals^2., gaussian_x^2.)+matrix_multiply(yvals^2., gaussian_y^2.)))
        endelse
        cos_term *= source_envelope
        sin_term *= source_envelope
      ENDIF
      FOR fbin_i=0L,n_fbin-1 DO BEGIN
        source_uv_real_vals=matrix_multiply(cos_term,*flux_use[fbin_use[fbin_i]])
        source_uv_im_vals=matrix_multiply(sin_term,*flux_use[fbin_use[fbin_i]])
        *source_uv_vals[fbin_use[fbin_i]]+=Temporary(source_uv_real_vals) + icomp*Temporary(source_uv_im_vals)
      ENDFOR
      cos_term=(sin_term=0) ;free memory
    ENDELSE
    FOR fbin_i=0L,n_fbin-1 DO *source_uv_vals[fbin_use[fbin_i]]*=fft_norm
    IF not Keyword_Set(double_precision) THEN $
      FOR fbin_i=0L,n_fbin-1 DO $
      *source_uv_vals[fbin_use[fbin_i]]=Complex(Temporary(*source_uv_vals[fbin_use[fbin_i]]))
  ENDIF ELSE BEGIN
    IF Keyword_Set(conserve_memory) AND (element_check GT mem_thresh) THEN BEGIN
        if keyword_set(gaussian_source_models) then begin
          print, 'Gaussian source modeling is not compatible with keyword conserve_memory at this time. Unsetting keyword gaussian_source_models.'
          undefine, gaussian_source_models
        endif
        memory_bins=Round(element_check/mem_thresh)
        source_uv_vals=DComplexarr(size(xvals,/dimension))
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
            IF keyword_set(gaussian_source_models) THEN BEGIN
              if n_elements(gaussian_rot) gt 0 then begin
                source_envelope = exp(-2*Pi^2.*(matrix_multiply(xvals^2., gaussian_x[inds]^2.*Cos(gaussian_rot[inds])^2.+gaussian_y[inds]^2.*Sin(gaussian_rot[inds])^2.) $
                  + matrix_multiply(yvals^2., gaussian_x[inds]^2.*Sin(gaussian_rot[inds])^2.+gaussian_y[inds]^2.*Cos(gaussian_rot[inds])^2.)) $
                  + matrix_multiply(xvals*yvals, (gaussian_y[inds]^2.-gaussian_x[inds]^2.)*Cos(gaussian_rot[inds])*Sin(gaussian_rot[inds])))
              endif else begin
                source_envelope = exp(-2*Pi^2.*(matrix_multiply(xvals^2., gaussian_x[inds]^2.)+matrix_multiply(yvals^2., gaussian_y[inds]^2.)))
              endelse
              cos_term *= source_envelope
              sin_term *= source_envelope
            ENDIF
            source_uv_im_vals=matrix_multiply(Temporary(sin_term),flux_use[inds])
            source_uv_vals+=Temporary(source_uv_real_vals) + icomp * Temporary(source_uv_im_vals)
        ENDFOR
    ENDIF ELSE BEGIN
        phase=matrix_multiply(xvals,x_use)+matrix_multiply(yvals,y_use)
        cos_term=Cos(phase)
        source_uv_real_vals=matrix_multiply(Temporary(cos_term),flux_use[inds])
        sin_term=Sin(Temporary(phase))
        IF keyword_set(gaussian_source_models) THEN BEGIN
          if n_elements(gaussian_rot) gt 0 then begin
            source_envelope = exp(-2*Pi^2.*(matrix_multiply(xvals^2., gaussian_x^2.*Cos(gaussian_rot)^2.+gaussian_y^2.*Sin(gaussian_rot)^2.) $
              + matrix_multiply(yvals^2., gaussian_x^2.*Sin(gaussian_rot)^2.+gaussian_y^2.*Cos(gaussian_rot)^2.)) $
              + matrix_multiply(xvals*yvals, (gaussian_y^2.-gaussian_x^2.)*Cos(gaussian_rot)*Sin(gaussian_rot)))
          endif else begin
            source_envelope = exp(-2*Pi^2.*(matrix_multiply(xvals^2., gaussian_x^2.)+matrix_multiply(yvals^2., gaussian_y^2.)))
          endelse
          cos_term *= source_envelope
          sin_term *= source_envelope
        ENDIF
        source_uv_im_vals=matrix_multiply(Temporary(sin_term),flux_use)
        source_uv_vals=Temporary(source_uv_real_vals) + icomp * Temporary(source_uv_im_vals)
    ENDELSE
    source_uv_vals*=fft_norm
    IF not Keyword_Set(double_precision) THEN source_uv_vals=Complex(source_uv_vals)
ENDELSE

IF Keyword_Set(flux_ptr_cleanup) THEN Ptr_free,flux_use
RETURN,source_uv_vals
END
