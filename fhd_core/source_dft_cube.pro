FUNCTION source_dft_cube,image_reference,spectral_index_powers_arr=spectral_index_powers_arr,freq_arr=freq_arr,freq_ref=freq_ref,no_fft=no_fft
;image_reference - image at freq_ref
;spectral_index_powers_arr - 1D pointer array, each containing the image_reference multiplied by the spectral_index^n 

n_freq=N_Elements(freq_arr)
image_cube=Ptrarr(n_freq)
FOR f_i=0,n_freq-1 DO image_cube[f_i]=Ptr_new(image_reference)

n_taylor_terms=N_Elements(spectral_index_powers_arr)
spectral_index_prefactor=deriv_coefficients(n_taylor_terms)

FOR n=0.,n_taylor_terms-1 DO FOR f_i=0,n_freq-1 DO $
    *image_cube[f_i]+=*spectral_index_powers_arr[n]*spectral_index_prefactor[n]*((freq_arr[f_i]/freq_ref-1.)^(n+1))/factorial(n+1)    

IF not Keyword_Set(no_fft) THEN FOR f_i=0,n_freq-1 DO *image_cube[f_i]=fft_shift(FFT(fft_shift(*image_cube[f_i]),/inverse))
RETURN,image_cube
END