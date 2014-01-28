PRO vis_noise_calc

;; A simple script to calculate the noise in the visibilities
;  
;  IF n_elements(ntile) eq 0 then ntile=total((*obs.baseline_info).tile_use)
;  nbase = ntile*(ntile-1)/2+ntile ; includes autos. Will drop later
;  
;  ; only use imaginary part
;  data_array=imaginary(data_array)
;  
;  IF ~Keyword_Set(n_avg) THEN n_avg=1
;  nfreq_chunks = (size(data_array,/dimension))[0]/n_avg
;  noise_arr = fltarr(nfreq_chunks)
;  
;  time_bins=round((params.time-min(params.time))/(params.time[nbase]-min(params.time)))
;  even_ind=where(time_bins mod 2 EQ 0)
;  odd_ind=where(time_bins mod 2 EQ 1)
;    
;  tile_a = (*obs.baseline_info).tile_a[even_ind] ; there's an assumption that the baselines are in the same order for each time
;  tile_b = (*obs.baseline_info).tile_b[even_ind]
;  
;  ; split even and odd
;  data_even=data_array[*,even_ind]
;  flag_even=flag_array[*,even_ind]
;  data_odd=data_array[*,odd_ind]
;  flag_odd=flag_array[*,odd_ind]
;
;  ; combine
;  data_diff = data_even-data_odd
;  flag_diff = (flag_odd>0)*(flag_even>0)
;  
;  FOR freq_chunk=0,nfreq_chunks-1 DO BEGIN
;    print,freq_chunk
;    nsamp = 0.
;    
;    temp_data = data_diff[n_avg*freq_chunk:n_avg*(freq_chunk+1)-1,*]
;    temp_flag = flag_diff[n_avg*freq_chunk:n_avg*(freq_chunk+1)-1,*]
;    ind=where(temp_flag > 0)
;    if n_elements(ind) gt 1 then begin
;      noise_arr[freq_chunk] = stddev(temp_data[ind])/sqrt(2.)
;    endif
;  endfor

END