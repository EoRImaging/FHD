PRO vis_noise_calc,obs,vis_arr,flag_arr,noise_arr=noise_arr,bi_use=bi_use
;; A simple script to calculate the noise in the visibilities

n_pol=obs.n_pol
n_freq=Long(obs.n_freq)
noise_arr=fltarr(n_pol,n_freq)

IF N_Elements(bi_use) NE 2 THEN flag_arr_use=split_vis_flags(obs,flag_arr,bi_use=bi_use,/preserve_flags) 

FOR pol_i=0,n_pol-1 DO BEGIN
    data_diff =Imaginary( (*vis_arr[pol_i])[*,*bi_use[0]])-Imaginary((*vis_arr[pol_i])[*,*bi_use[1]]) ; only use imaginary part
    flag_diff = ((*flag_arr[pol_i])[*,*bi_use[0]]>0)*((*flag_arr[pol_i])[*,*bi_use[1]]>0)
    FOR fi=0L,n_freq-1 DO BEGIN
        ind_use=where(flag_diff[fi,*],n_use)
        IF n_use GT 1 THEN noise_arr[pol_i,fi]=Stddev(data_diff[fi,ind_use])/Sqrt(2.)
    ENDFOR
ENDFOR
obs.vis_noise=Ptr_new(noise_arr)
END