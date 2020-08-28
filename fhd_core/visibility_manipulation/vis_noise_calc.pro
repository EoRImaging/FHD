PRO vis_noise_calc,obs,vis_arr,vis_weights,noise_arr=noise_arr,bi_use=bi_use,_Extra=extra
;; A simple script to calculate the noise in the visibilities

n_pol=obs.n_pol
n_freq=Long(obs.n_freq)
noise_arr=dblarr(n_pol,n_freq)

IF obs.n_time LT 2 THEN RETURN ;exit if not enough data to calculate noise
IF N_Elements(bi_use) NE 2 THEN BEGIN
    vis_weights_use=split_vis_weights(obs,vis_weights,bi_use=bi_use,/preserve_weights,_Extra=extra) 
ENDIF ELSE vis_weights_use = vis_weights

FOR pol_i=0,n_pol-1 DO BEGIN
    data_diff =Imaginary( (*vis_arr[pol_i])[*,*bi_use[0]])-Imaginary((*vis_arr[pol_i])[*,*bi_use[1]]) ; only use imaginary part
    vis_weight_diff = ((*vis_weights_use[pol_i])[*,*bi_use[0]]>0)*((*vis_weights_use[pol_i])[*,*bi_use[1]]>0)
    FOR fi=0L,n_freq-1 DO BEGIN
        ind_use=where(vis_weight_diff[fi,*],n_use)
        IF n_use GT 1 THEN noise_arr[pol_i,fi]=Stddev(data_diff[fi,ind_use])/Sqrt(2.)
    ENDFOR
ENDFOR

obs.vis_noise=Ptr_new(noise_arr)
END
