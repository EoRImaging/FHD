pro vis_add_noise_simulation, cal_sim_input, vis_arr, obs_id,obs

  ;Clear important variables
  undefine, vis_ptr, vis_xx, vis_yy, random_arr, real_noise, imaginary_noise, noisy_vis_xx, noisy_vis_yy
  
  ;Restore the obs structure to get the noise standard deviation per visibility

  vis_noise_full=*obs.vis_noise
  vis_noise=vis_noise_full
  vis_noise[*,*]=vis_noise_full[0,6]/4 ;A non-flagged index, since I really don't want any flagging influence.

  undefine, vis_noise_full, obs
  
  visibility_num=size(*vis_arr[0])
  
  ;Get an array of 2,2,384,largenumber (real/imaginary, pol, freq, visibilities) of random numbers based off the machine time tag
  random_arr= randomn(systime(/seconds),2,2,384,visibility_num[2]) ;real/imaginary, pol, freq, visibilities
  
  ;Set up an array of pol,freq,visibilities. The real and imaginary parts have the same visibility noise. Each visibility
  ;apparently has the same noise standard deviation...divide by the sq root of 30000, which is about 1000hrs of observation
  vis_noise_large=DBLARR(2,384,visibility_num[2])
  for i=0, visibility_num[2]-1 do vis_noise_large[*,*,i]=vis_noise[*,*];/10.
  
  ;Get the real and imaginary noise, which are uncorrelated.
  real_noise=vis_noise_large*random_arr[0,*,*,*]
  imaginary_noise=vis_noise_large*random_arr[1,*,*,*]

  undefine, vis_noise_large, random_arr
  
  ;Add noise to each visibility based on real/imaginary and polarization
  ;*vis_arr[0]=(real_part(*vis_arr[0])+real_noise[0,*,*])+Complex(0,1)*(imaginary(*vis_arr[0])+imaginary_noise[0,*,*])
  ;*vis_arr[1]=(real_part(*vis_arr[1])+real_noise[1,*,*])+Complex(0,1)*(imaginary(*vis_arr[1])+imaginary_noise[1,*,*])
  
  vis_noise_xx=real_noise[0,*,*]+Complex(0,1)*imaginary_noise[0,*,*]
  vis_noise_yy=real_noise[1,*,*]+Complex(0,1)*imaginary_noise[1,*,*]
  
  undefine, imaginary_noise, real_noise
  
  vis_noise=PTRARR(2,/allocate)
  *vis_noise[0] = vis_noise_xx
  *vis_noise[1] = vis_noise_yy

  save, vis_noise, filename='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_sim_noise/'+obs_id+'_noise.sav'
  
;Save the visibilities in pointers (like the input format) and save in seperate xx and yy savefiles.
;vis_model_ptr=ptr_new(/allocate_heap)
;*vis_arr[0]=noisy_vis_xx
;save, vis_model_ptr, filename='/data4/MWA/FHD_Aug23/fhd_nb_bubbles_low_noise_retry/vis_data/1061316296_vis_model_XX.sav'
;*vis_arr[1]=noisy_vis_yy
;save, vis_model_ptr, filename='/data4/MWA/FHD_Aug23/fhd_nb_bubbles_low_noise_retry/vis_data/1061316296_vis_model_YY.sav'
  
  
end