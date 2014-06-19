PRO indexed_image,vals,inds,dimension,elements,_Ref_Extra=extra

IF N_Elements(dimension) EQ 0 THEN BEGIN
    max_i=Max(inds)
    dim_ref=Sqrt(max_i)
    dimension=2.^Ceil(Alog(dim_ref)/Alog(2.))
ENDIF
IF N_Elements(elements) EQ 0 THEN elements=dimension
img=Make_array(dimension,elements)
img[inds]=vals

os_type=!version.os_family
CASE os_type OF
    'Windows':look,img
    'unix': cgimage,img,_Ref_Extra=extra
ENDCASE
END