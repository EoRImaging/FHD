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
FUNCTION holo_mapfn_apply,image,map_fn,mask,timing=timing,transpose=transpose
t0=Systime(1)

dimension=(size(image,/dimension))[0]
elements=(size(image,/dimension))[1]

image_real_vector=reform(Real_part(image),Float(dimension)*elements)
image_comp_vector=reform(Imaginary(image),Float(dimension)*elements)

SPRSAX2,map_fn,image_real_vector,result_image_real,image_comp_vector,result_image_comp,transpose=0,mask=0
result_image=Complex(result_image_real,result_image_comp)
result_image=reform(result_image,dimension,elements)
result_image_conj=Shift(Reverse(reverse(Conj(result_image),1),2),1,1)
result_image+=result_image_conj


;;normalization=2.*!Pi*!Radeg^2.
;normalization=((dimension*elements)/(2.*!Pi));^2.
;result_image/=normalization

timing=Systime(1)-t0
RETURN,result_image
END