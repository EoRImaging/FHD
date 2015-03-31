FUNCTION spectral_expand,obs,flux_arr=flux_arr,spectral_index_arr=spectral_index_arr,freq_arr=freq_arr,freq_ref

n_vec=N_Elements(flux_arr)
n_freq=N_Elements(freq_arr)
flux_cube=Ptrarr(n_freq)
FOR f_i=0,n_freq-1 DO flux_cube[f_i]=Ptr_new(Fltarr(n_vec))

n_alpha=N_Elements(spectral_index_arr)
spectral_index_prefactor=deriv_coefficients(n_alpha)


FOR n=0,n_taylor_terms-1 DO BEGIN  
    IF ptr_flag THEN flux_use=*flux_arr[n] ELSE flux_use=flux_arr
    
    
    
    
ENDFOR

RETURN,flux_cube
END