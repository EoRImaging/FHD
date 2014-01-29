PRO vis_noise_calc,obs,vis_arr,flag_arr,noise_arr=noise_arr
;; A simple script to calculate the noise in the visibilities

bin_start=(*obs.baseline_info).bin_offset
nt=N_Elements(bin_start)
nb=(size(*flag_arr[0],/dimension))[1]
bin_end=fltarr(nt)
bin_end[0:nt-2]=bin_start[1:nt-1]-1
bin_end[nt-1]=nb-1
bin_i=lonarr(nb)-1
nt2=Floor(nt/2)
FOR t_i=0,2*nt2-1 DO bin_i[bin_start[t_i]:bin_end[t_i]]=t_i
bi_n=findgen(nb)
even_bi_use=where(bin_i mod 2 EQ 0,n_even)
odd_bi_use=where(bin_i mod 2 EQ 1,n_odd)
n_use=n_even<n_odd
even_bi_use=even_bi_use[0:n_use-1]
odd_bi_use=odd_bi_use[0:n_use-1]

n_pol=obs.n_pol
n_freq=Long(obs.n_freq)
noise_arr=fltarr(n_freq)
FOR pol_i=0,n_pol-1 DO BEGIN
    data_diff = (*vis_arr[pol_i])[*,even_bi_use]-(*vis_arr[pol_i])[*,odd_bi_use]
    flag_diff = ((*flag_arr[pol_i])[*,even_bi_use]>0)*((*flag_arr[pol_i])[*,odd_bi_use]>0)
    FOR fi=0L,n_freq-1 DO BEGIN
        ind_use=where(flag_diff[fi,*],n_use)
        IF n_use GT 1 THEN noise_arr[fi]=Stddev(data_diff[fi,ind_use])/Sqrt(2.)
    ENDFOR
ENDFOR
IF Tag_exist(obs,'vis_noise') THEN obs.vis_noise=noise_arr
END
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