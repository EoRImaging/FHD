function vis_add_noise_simulation, cal_sim_input, vis_arr, obs_id, obs, n_pol=n_pol , fhd_file_list=fhd_file_list

	if ~keyword_set(n_pol) then n_pol=2
	
	;Clear important variables
	undefine, random_arr, real_noise, imaginary_noise
	
	;Restore the obs structure to get the noise standard deviation per visibility, and set it to the mean
	vis_noise  =*obs.vis_noise
	freq_use = where((*obs.baseline_info).freq_use)
	tile_use = where((*obs.baseline_info).tile_use)
	n_freq = obs.n_freq
	vis_noise[*,*] = mean(vis_noise[freq_use,tile_use])
	
	undefine, obs
	
	visibility_num=(size(*vis_arr[0]))[2]
	
	;Get an array of 2,2,384,largenumber (real/imaginary, pol, freq, visibilities) of random numbers based off the machine time tag
	random_arr = randomn(systime(/seconds),2,n_pol,n_freq,visibility_num) ;real/imaginary, pol, freq, visibilities
	
	;Get the real and imaginary noise, which are uncorrelated.
	real_noise=DBLARR(n_pol,n_freq,visibility_num)
	imaginary_noise=DBLARR(n_pol,n_freq,visibility_num)
	for vis_i=0, visibility_num-1 do begin
		real_noise[*,*,vis_i] = vis_noise*reform(random_arr[0,*,*,vis_i])
		imaginary_noise[*,*,vis_i] = vis_noise*reform(random_arr[1,*,*,vis_i])
	endfor
	
	undefine,random_arr
	
	vis_noise=PTRARR(n_pol,/allocate)
	for pol_i=0,n_pol-1 do $
		*vis_noise[pol_i]=real_noise[pol_i,*,*]+Complex(0,1)*imaginary_noise[pol_i,*,*]
		
	undefine, imaginary_noise, real_noise
	
	if keyword_set(fhd_file_list) then begin
		save, vis_noise, filename=file_dirname(fhd_file_list) +'/sim_outputs/'+obs_id+'_noise.sav'
	endif
	
	return, vis_noise
end