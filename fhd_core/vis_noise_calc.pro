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
noise_arr=fltarr(n_pol,n_freq)
FOR pol_i=0,n_pol-1 DO BEGIN
    data_diff =Imaginary( (*vis_arr[pol_i])[*,even_bi_use])-Imaginary((*vis_arr[pol_i])[*,odd_bi_use]) ; only use imaginary part
    flag_diff = ((*flag_arr[pol_i])[*,even_bi_use]>0)*((*flag_arr[pol_i])[*,odd_bi_use]>0)
    FOR fi=0L,n_freq-1 DO BEGIN
        ind_use=where(flag_diff[fi,*],n_use)
        IF n_use GT 1 THEN noise_arr[pol_i,fi]=Stddev(data_diff[fi,ind_use])/Sqrt(2.)
    ENDFOR
ENDFOR
IF Tag_exist(obs,'vis_noise') THEN obs.vis_noise=Ptr_new(noise_arr)
END