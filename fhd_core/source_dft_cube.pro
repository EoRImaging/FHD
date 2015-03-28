FUNCTION source_dft_cube,obs,image_arr=image_arr,spectral_index_powers_arr=spectral_index_powers_arr

freq_ref=obs.freq_center

freq_bin_i=(*obs.baseline_info).fbin_i
bin_use=Uniq(freq_bin_i,sort(freq_bin_i))
nf_bin=N_Elements(bin_use)

freq_arr_full=(*obs.baseline_info).freq
freq_arr=Fltarr(nf_bin)
FOR f_i=0L,nf_bin-1 DO freq_arr[f_i]=Mean(freq_arr_full[where(freq_bin_i EQ freq_bin_i[bin_use[f_i]])])

n_vec=N_Elements(flux_arr)
n_freq=N_Elements(freq_arr)
image_cube=Ptrarr(n_freq)
FOR f_i=0,n_freq-1 DO image_cube[f_i]=Ptr_new(Fltarr(n_vec))

n_taylor_terms=N_Elements(spectral_index_powers_arr)
spectral_index_prefactor=deriv_coefficients(n_alpha)


FOR n=0.,n_taylor_terms-1 DO BEGIN  
;    IF ptr_flag THEN flux_use=*flux_arr[n] ELSE flux_use=flux_arr
    
    FOR f_i=0,n_freq-1 DO *image_cube[f_i]+=*spectral_index_powers_arr[n]*spectral_index_prefactor[n]*image_use*((freq_ref-freq_arr[f_i])^(n+1))/factorial(n+1)
    
    
    
ENDFOR


RETURN,image_cube
END