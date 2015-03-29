FUNCTION source_dft_cube,image_reference,spectral_index_powers_arr=spectral_index_powers_arr,freq_arr=freq_arr,freq_ref=freq_ref

n_freq=N_Elements(freq_arr)
image_cube=Ptrarr(n_freq)
init_arr=Make_array(size(image_reference,/dimension),type=size(image_reference,/type))
FOR f_i=0,n_freq-1 DO image_cube[f_i]=Ptr_new(init_arr)

n_taylor_terms=N_Elements(spectral_index_powers_arr)
spectral_index_prefactor=deriv_coefficients(n_taylor_terms)

FOR n=0.,n_taylor_terms-1 DO FOR f_i=0,n_freq-1 DO $
    *image_cube[f_i]+=*spectral_index_powers_arr[n]*spectral_index_prefactor[n]*image_reference*((freq_arr[f_i]/freq_ref-1.)^(n+1))/factorial(n+1)    

RETURN,image_cube
END