;+
; :Description:
;    Applies the Holographic Mapping Function to a model uv gridded image. 
;
; :Params:
;    image - the uv-plane model image to transform with the HMF
;    
;    map_fn - the HMF to apply
;    
; :Keywords:
;    timing
;    
;    transpose - use the transpose HMF. (Not currently supported)
;
; :Author: isullivan May 6, 2012
;-
FUNCTION holo_mapfn_apply,image,map_fn,mask,timing=timing,transpose=transpose,complex=complex,double=double,$
    no_conjugate=no_conjugate,indexed=indexed,holo2_return=holo2_return,map_fn2=map_fn2,_Extra=extra
t0=Systime(1)

IF N_Elements(complex) EQ 0 THEN complex=1
IF N_Elements(transpose) EQ 0 THEN transpose=0

dimension=Float((size(image,/dimension))[0])
elements=Float((size(image,/dimension))[1])

IF size(map_fn,/type) EQ 7 THEN BEGIN ;IF map_fn is a string, assume it is a full filepath. 
    ;NOTE: String map_fn NOT compatible with several options!!
    file_path_mapfn=map_fn
    restore,file_path_mapfn ;map_fn
    map_fn_use=Ptr_new(map_fn)
    image_vector=reform(image,dimension*elements)
    SPRSAX2,map_fn_use,image_vector,result_image_vector,$
        complex=complex,double=double,transpose=transpose,mask=0,indexed=indexed
        result_image=reform(result_image_vector,dimension,elements)
    Ptr_free,map_fn_use
    map_fn=file_path_mapfn
ENDIF ELSE BEGIN
    image_vector=reform(image,dimension*elements)
    IF Keyword_Set(map_fn2) AND Arg_present(holo2_return) THEN BEGIN
        SPRSAX2,map_fn,image_vector,result_image_vector,A2=map_fn2,B2=holo2_return,$
            complex=complex,double=double,transpose=transpose,mask=0,indexed=indexed
        holo2_return=reform(holo2_return,dimension,elements)
    ENDIF ELSE SPRSAX2,map_fn,image_vector,result_image_vector,complex=complex,double=double,transpose=transpose,mask=0,indexed=indexed
    result_image=reform(result_image_vector,dimension,elements)
ENDELSE
result_image=reform(result_image,dimension,elements)

IF Keyword_Set(no_conjugate) THEN BEGIN
    timing=Systime(1)-t0
    RETURN,result_image
ENDIF
result_image_conj=Shift(Reverse(reverse(Conj(result_image),1),2),1,1)
result_image+=result_image_conj
result_image/=2.

IF Keyword_Set(holo2_return) THEN BEGIN
    result_image_conj2=Shift(Reverse(reverse(Conj(holo2_return),1),2),1,1)
    holo2_return+=result_image_conj2
    holo2_return/=2.
ENDIF

;;normalization=2.*!Pi*!Radeg^2.
;normalization=((dimension*elements)/(2.*!Pi));^2.
;result_image/=normalization

timing=Systime(1)-t0
RETURN,result_image
END