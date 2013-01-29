FUNCTION dirty_image_generate,dirty_image_uv,baseline_threshold=baseline_threshold,mask=mask,$
    normalization=normalization,resize=resize,width_smooth=width_smooth,$
    hanning_filter=hanning_filter,no_real=no_real,image_filter_fn=image_filter_fn,pad_uv_image=pad_uv_image,$
    _Extra=extra

compile_opt idl2,strictarrsubs  
IF N_Elements(baseline_threshold) EQ 0 THEN baseline_threshold=0.

dimension=(size(dirty_image_uv,/dimension))[0]
elements=(size(dirty_image_uv,/dimension))[1]

IF Keyword_Set(baseline_threshold) THEN BEGIN
    ;IF N_Elements(normalization) EQ 0 THEN normalization=1./dimension*elements
    IF N_Elements(width_smooth) EQ 0 THEN width_smooth=Floor(Sqrt(dimension*elements)/100.)
    rarray=Sqrt((meshgrid(dimension,1)-dimension/2)^2.+(meshgrid(elements,2)-elements/2.)^2.)
    IF baseline_threshold GE 0 THEN cut_i=where(rarray LT baseline_threshold,n_cut) $
        ELSE cut_i=where(rarray GT Abs(baseline_threshold),n_cut)
    mask=fltarr(dimension,elements)+1.
    IF n_cut GT 0 THEN mask[cut_i]=0
    IF Keyword_Set(width_smooth) THEN mask=Smooth(mask,width_smooth>1.)
    cut_i=where(dirty_image_uv*mask EQ 0,n_cut,comp=keep_i,ncomp=n_keep)
    di_uv_use=dirty_image_uv*mask
ENDIF ELSE di_uv_use=dirty_image_uv

IF Keyword_Set(hanning_filter) THEN IF hanning_filter EQ -1 THEN $
    di_uv_use*=fft_shift(hanning(dimension,elements)) ELSE di_uv_use*=hanning(dimension,elements)

IF Keyword_Set(image_filter_fn) THEN di_uv_use=Call_Function(image_filter_fn,di_uv_use,_Extra=extra)

IF Keyword_Set(resize) THEN BEGIN
    dimension=dimension*resize
    elements=elements*resize
    di_uv_real=Real_part(di_uv_use)
    di_uv_img=Imaginary(di_uv_use)
    di_uv_real=REBIN(di_uv_real,dimension,elements)
    di_uv_img=REBIN(di_uv_img,dimension,elements)
    di_uv_use=complex(di_uv_real,di_uv_img)    
ENDIF

;IF Keyword_Set(no_real) THEN dirty_image=fft_shift(FFT(fft_shift(di_uv_use)))/(dimension*elements) $
;    ELSE dirty_image=Real_part(fft_shift(FFT(fft_shift(di_uv_use))))/(dimension*elements)

IF Keyword_Set(pad_uv_image) THEN BEGIN
    dimension_new=((dimension>elements)*pad_uv_image)>(dimension>elements)
    di_uv1=Complexarr(dimension_new,dimension_new)
    di_uv1[dimension_new/2.-dimension/2.:dimension_new/2.+dimension/2.-1,$
        dimension_new/2.-elements/2.:dimension_new/2.+elements/2.-1]=di_uv_use
    di_uv_use=di_uv1
ENDIF

IF Keyword_Set(no_real) THEN dirty_image=fft_shift(FFT(fft_shift(di_uv_use))) $
    ELSE dirty_image=Real_part(fft_shift(FFT(fft_shift(di_uv_use))))
RETURN,dirty_image
END