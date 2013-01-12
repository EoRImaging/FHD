;+
; :Description:
;    Computes the FFT of an image, accounting for IDL's convention of putting the k=0 mode in the center of an image
;    
;    NOT NORMALIZED CORRECTLY (needs a ~1/sqrt(2*!Pi) /dimension correction)
;
; :Params:
;    dirty_image_uv
;
; :Keywords:
;    baseline_threshold - radius of mask to apply, in wavelengths. If negative, masks everything larger than the specified threshold
;    
;    mask - mask to apply before taking the FFT
;    
;    normalization - if the correct FFT normalization factor is known, supply it here
;    
;    resize - Obsolete, not recommended
;    
;    width_smooth - width of taper to reduce ringing in the image if baseline_threshold or mask are used
;
; :Author: isullivan May 4, 2012
;-
FUNCTION dirty_image_generate,dirty_image_uv,baseline_threshold=baseline_threshold,mask=mask,$
    normalization=normalization,resize=resize,width_smooth=width_smooth,hanning_filter=hanning_filter,no_real=no_real

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

IF Keyword_Set(resize) THEN BEGIN
    dimension2=dimension*resize
    elements2=elements*resize
    di_uv_real=Real_part(di_uv_use)
    di_uv_img=Imaginary(di_uv_use)
    di_uv_real=REBIN(di_uv_real,dimension2,elements2)
    di_uv_img=REBIN(di_uv_img,dimension2,elements2)
    di_uv_use=complex(di_uv_real,di_uv_img)    
ENDIF

;IF Keyword_Set(no_real) THEN dirty_image=fft_shift(FFT(fft_shift(di_uv_use)))/(dimension*elements) $
;    ELSE dirty_image=Real_part(fft_shift(FFT(fft_shift(di_uv_use))))/(dimension*elements)

IF Keyword_Set(no_real) THEN dirty_image=fft_shift(FFT(fft_shift(di_uv_use))) $
    ELSE dirty_image=Real_part(fft_shift(FFT(fft_shift(di_uv_use))))
RETURN,dirty_image
END