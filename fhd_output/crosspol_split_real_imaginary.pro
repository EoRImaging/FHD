PRO crosspol_split_real_imaginary, image_ptr_array, pol_names=pol_names

n_pol=N_Elements(image_ptr_array)
IF n_pol EQ 4 THEN BEGIN
    crosspol_image = *image_ptr_array[2]
    image_ptr_array[2] = Ptr_new(Real_part(crosspol_image))
    image_ptr_array[3] = Ptr_new(Imaginary(crosspol_image))
ENDIF
IF N_Elements(pol_names) GE 4 THEN BEGIN
    crosspol_name = pol_names[2]
    pol_names[2] = crosspol_name + '_real'
    pol_names[3] = crosspol_name + '_imaginary'
ENDIF
END
