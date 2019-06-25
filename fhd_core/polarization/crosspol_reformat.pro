FUNCTION crosspol_reformat, image_uv_arr, inverse=inverse
n_pol=Total(ptr_valid(image_uv_arr))
icomp = Complex(0,1)
IF n_pol EQ 4 THEN BEGIN
    IF not Keyword_Set(inverse) THEN BEGIN ;instrumental -> Stokes
        crosspol_image = (*image_uv_arr[2] + conjugate_mirror(*image_uv_arr[3]))/2. 
    ENDIF ELSE BEGIN ;Stokes -> instrumental
        crosspol_image = (2.*(*image_uv_arr[2]) - conjugate_mirror(*image_uv_arr[3]))
    ENDELSE
    *image_uv_arr[2] = crosspol_image
    *image_uv_arr[3] = conjugate_mirror(crosspol_image)
ENDIF
RETURN,image_uv_arr
END
