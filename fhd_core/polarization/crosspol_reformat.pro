FUNCTION crosspol_reformat, image_uv_arr, inverse=inverse
n_pol=Total(ptr_valid(image_uv_arr))
icomp = Complex(0,1)
IF n_pol EQ 4 THEN BEGIN
    IF not Keyword_Set(inverse) THEN BEGIN
        crosspol_image = (*image_uv_arr[2] + conjugate_mirror(*image_uv_arr[3]))/2.
        pseudo_stokes_U = (crosspol_image + conjugate_mirror(crosspol_image))/2.
        pseudo_stokes_V = -icomp * (crosspol_image - conjugate_mirror(crosspol_image))/2.
        *image_uv_arr[2] = pseudo_stokes_U + pseudo_stokes_V
        *image_uv_arr[3] = pseudo_stokes_U - pseudo_stokes_V
    ENDIF ELSE BEGIN
        pseudo_stokes_U = (*image_uv_arr[2] + *image_uv_arr[3])/2.
        pseudo_stokes_V = (*image_uv_arr[2] - *image_uv_arr[3])/2.
        *image_uv_arr[2] = pseudo_stokes_U + icomp * pseudo_stokes_V
        *image_uv_arr[3] = pseudo_stokes_U - icomp * pseudo_stokes_V
    ENDELSE
ENDIF
RETURN,image_uv_arr
END