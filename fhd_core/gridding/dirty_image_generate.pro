FUNCTION dirty_image_generate,dirty_image_uv,baseline_threshold=baseline_threshold,mask=mask,$
    normalization=normalization,resize=resize,width_smooth=width_smooth,degpix=degpix,$
    no_real=no_real,image_filter_fn=image_filter_fn,pad_uv_image=pad_uv_image,$
    filter=filter,weights=weights,antialias=antialias,beam_ptr=beam_ptr,_Extra=extra

compile_opt idl2,strictarrsubs  
IF N_Elements(baseline_threshold) EQ 0 THEN baseline_threshold=0.

dimension=(size(dirty_image_uv,/dimension))[0]
elements=(size(dirty_image_uv,/dimension))[1]
di_uv_use=dirty_image_uv
double_flag = (size(di_uv_use,/type) EQ 9)

IF Keyword_Set(baseline_threshold) THEN BEGIN
    IF N_Elements(width_smooth) EQ 0 THEN width_smooth=Floor(Sqrt(dimension*elements)/100.)
    rarray=Sqrt((meshgrid(dimension,1)-dimension/2)^2.+(meshgrid(elements,2)-elements/2.)^2.)
    IF baseline_threshold GE 0 THEN cut_i=where(rarray LT baseline_threshold,n_cut) $
        ELSE cut_i=where(rarray GT Abs(baseline_threshold),n_cut)
    mask_bt=fltarr(dimension,elements)+1.
    IF n_cut GT 0 THEN mask_bt[cut_i]=0
    IF Keyword_Set(width_smooth) THEN mask_bt=Smooth(mask_bt,width_smooth>1.)
    cut_i=where(dirty_image_uv*mask_bt EQ 0,n_cut,comp=keep_i,ncomp=n_keep)
    di_uv_use*=mask_bt
ENDIF

IF Keyword_Set(mask) THEN di_uv_use*=mask 

IF Keyword_Set(filter) THEN BEGIN
    IF Ptr_valid(filter) THEN BEGIN
        IF N_Elements(*filter) EQ N_Elements(di_uv_use) THEN di_uv_use*=*filter $
            ELSE BEGIN
                IF ~Keyword_Set(image_filter_fn) THEN image_filter_fn='filter_uv_uniform'
                di_uv_use=Call_Function(image_filter_fn,di_uv_use,weights=weights,filter=filter,_Extra=extra)
            ENDELSE
    ENDIF ELSE BEGIN
        filter=Ptr_new(/allocate)
        IF N_Elements(image_filter_fn) EQ 0 THEN image_filter_fn='filter_uv_uniform'
        di_uv_use=Call_Function(image_filter_fn,di_uv_use,weights=weights,filter=filter,_Extra=extra)
    ENDELSE
ENDIF ELSE BEGIN
    IF Keyword_Set(image_filter_fn) THEN $
        di_uv_use=Call_Function(image_filter_fn,di_uv_use,weights=weights,filter=filter,_Extra=extra)
ENDELSE

;IF Keyword_Set(antialias) THEN BEGIN
;    alias_filter=Sqrt(Hanning(dimension,elements))
;    di_uv_use*=alias_filter
;ENDIF

IF Keyword_Set(resize) THEN BEGIN
    dimension=dimension*resize
    elements=elements*resize
    di_uv_real=Real_part(di_uv_use)
    di_uv_img=Imaginary(di_uv_use)
    di_uv_real=REBIN(di_uv_real,dimension,elements)
    di_uv_img=REBIN(di_uv_img,dimension,elements)
    di_uv_use=complex(di_uv_real,di_uv_img)    
ENDIF

IF Keyword_Set(pad_uv_image) THEN BEGIN
    dimension_new=((dimension>elements)*pad_uv_image)>(dimension>elements)
    di_uv1=Complexarr(dimension_new,dimension_new)
    di_uv1[dimension_new/2.-dimension/2.:dimension_new/2.+dimension/2.-1,$
        dimension_new/2.-elements/2.:dimension_new/2.+elements/2.-1]=di_uv_use
    di_uv_use=di_uv1*pad_uv_image^2.
ENDIF

IF Keyword_Set(degpix) THEN di_uv_use/=(degpix*!DtoR)^2. ;FFT normalization
IF Keyword_Set(no_real) THEN dirty_image=fft_shift(FFT(fft_shift(di_uv_use),double=1)) $
    ELSE dirty_image=Real_part(fft_shift(FFT(fft_shift(di_uv_use),double=1)))

IF Keyword_Set(image_filter_fn) THEN BEGIN
    CASE image_filter_fn OF
        'filter_uv_weighted': IF Ptr_valid(beam_ptr) THEN dirty_image *= *beam_ptr
        ELSE:
    ENDCASE
ENDIF

IF not Keyword_Set(double_flag) THEN BEGIN
    IF Keyword_Set(no_real) THEN dirty_image=Complex(dirty_image) ELSE dirty_image=Float(dirty_image)
ENDIF
IF Keyword_Set(normalization) THEN dirty_image*=normalization
RETURN,dirty_image
END