;+
; :Description:
;    Applies the Holographic Mapping Function to a model uv gridded image. 
;
; :Params:
;    image - the uv-plane model image to transform with the HMF
;    
;    map_fn - the HMF to apply
;    
;    mask - Must be the same dimensions as image. If supplied, masked pixels are skipped in the sparse matrix multiplication, which provides a significant speed increase over masking the input image. (not currently supported) 
;
; :Keywords:
;    timing
;    
;    transpose - use the transpose HMF. (Not currently supported)
;
; :Author: isullivan May 6, 2012
;-
FUNCTION holo_mapfn_apply,image,map_fn,mask,timing=timing,transpose=transpose,complex=complex,double=double,_Extra=extra
t0=Systime(1)

IF N_Elements(complex) EQ 0 THEN complex=1

dimension=Float((size(image,/dimension))[0])
elements=Float((size(image,/dimension))[1])

IF size(map_fn,/type) EQ 7 THEN BEGIN ;IF map_fn is a string, assume it is a full filepath
    map_fn_use=getvar_savefile(map_fn,'map_fn')
    IF Keyword_Set(complex) THEN BEGIN
        image_complex_vector=reform(image,dimension*elements)
        SPRSAX2,map_fn_use,image_complex_vector,result_image_complex,complex=complex,double=double,transpose=0,mask=0
        result_image=reform(result_image_complex,dimension,elements)
    ENDIF ELSE BEGIN
        image_real_vector=reform(Real_part(image),dimension*elements)
        image_comp_vector=reform(Imaginary(image),dimension*elements)
        SPRSAX2,map_fn_use,image_real_vector,result_image_real,image_comp_vector,result_image_comp,complex=complex,double=double,transpose=0,mask=0
        result_image=Complex(result_image_real,result_image_comp)
    ENDELSE
ENDIF ELSE BEGIN
    IF Keyword_Set(complex) THEN BEGIN
        image_complex_vector=reform(image,dimension*elements)
        SPRSAX2,map_fn,image_complex_vector,result_image_complex,complex=complex,double=double,transpose=0,mask=0
        result_image=reform(result_image_complex,dimension,elements)
    ENDIF ELSE BEGIN
        image_real_vector=reform(Real_part(image),dimension*elements)
        image_comp_vector=reform(Imaginary(image),dimension*elements)
        SPRSAX2,map_fn,image_real_vector,result_image_real,image_comp_vector,result_image_comp,complex=complex,double=double,transpose=0,mask=0
        result_image=Complex(result_image_real,result_image_comp)
    ENDELSE
ENDELSE
result_image=reform(result_image,dimension,elements)
result_image_conj=Shift(Reverse(reverse(Conj(result_image),1),2),1,1)
result_image+=result_image_conj
result_image/=2.

;;normalization=2.*!Pi*!Radeg^2.
;normalization=((dimension*elements)/(2.*!Pi));^2.
;result_image/=normalization

timing=Systime(1)-t0
RETURN,result_image
END