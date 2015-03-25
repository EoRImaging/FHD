FUNCTION spectral_expand,obs,flux_arr=flux_arr,spectral_index_arr=spectral_index_arr


IF size(flux_arr,/type) EQ 10 THEN BEGIN ;check if pointer type
    n_talyor_terms=N_Elements(flux_arr)
    
    ptr_flag=1
ENDIF ELSE BEGIN
    n_taylor_terms=1
    ptr_flag=0
ENDELSE

binomial

n_alpha=N_Elements(spectral_index_arr)
spectral_index_prefactor=Fltarr(n_alpha)+1.
FOR n=1.,n_taylor_terms DO BEGIN ;it is important for the loop variable to be floating point, and indexed from 1 
    IF ptr_flag THEN flux_use=*flux_arr[n-1] ELSE flux_use=flux_arr
    spectral_index_prefactor*=spectral_index_arr-(n-1.) 
    
ENDFOR

RETURN,flux_cube
END