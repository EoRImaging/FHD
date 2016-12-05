FUNCTION test_eor_sim

; Test for the eor simulations.
; Generate a eor uvf cube for 256x256 UV grid and 100 frequency channels.
; Load a psf
; Build an obs structure with dimension=256, elements=256, kbinsize=0.5, etc.

; Visibility degrid requires -- obs, psf, image_uv, and vis_weight

instrument='paper'
n_pol=2

;hdr=uvfits_header_simulate(hdr_in,n_pol=n_pol, instrument=instrument)
;params=uvfits_params_simulate(hdr)

;obs=fhd_struct_init_obs(file_path_vis,hdr,params,n_pol=n_pol,instrument=instrument)

;get obs, and modify if necessary
restore, '/users/alanman/fhd_out/fhd_sim_paper128_9hour/metadata/PAPER128_22_obs.sav'
obs.dimension=256
obs.elements=256
obs.kpix=0.5
n_samples=obs.n_time
n_freq=203
;(*obs.baseline_info).freq=findgen(101)*1e6+1e6     ;101 freq chans
freq_arr = (*obs.baseline_info).freq
;help, freq_arr

hdr=uvfits_header_simulate(instrument=instrument)
params=uvfits_params_simulate(hdr)

vis_weights=Ptrarr(n_pol,/allocate)
n_param=N_Elements(params.uu)
FOR pol_i=0,n_pol-1 DO BEGIN
  *vis_weights[pol_i]=Replicate(1.,n_freq,n_param)
  IF n_freq<n_param EQ 1 THEN *vis_weights[pol_i]=Reform(*vis_weights[pol_i],n_freq,n_param) ;need to make sure all dimensions are there even if n_freq=1 or n_param=1
ENDFOR

vis_dimension=n_param*n_samples
uv_arr = (findgen(obs.dimension)-obs.dimension/2)*obs.kpix

psf=beam_setup(obs)

eor_uvf_cube = eor_sim(uv_arr, uv_arr, freq_arr)

model_uvf_arr=Ptrarr(n_pol,/allocate)
for pol_i=0,n_pol-1 do *model_uvf_arr[pol_i]=Complexarr(obs.dimension,obs.elements, n_freq)


;(*model_uvf_arr[3])[*]=0.
;(*model_uvf_arr[2])[*]=0.
(*model_uvf_arr[1])[*]=eor_uvf_cube/2.
(*model_uvf_arr[0])[*]=eor_uvf_cube/2.


vis_model_arr = Ptrarr(n_pol,/allocate)
for pol_i=0,n_pol-1 do *vis_model_arr[pol_i]=Complexarr(n_freq,vis_dimension)

for fi=0, n_freq-1 do begin

	this_vis_weight_ptr = Ptrarr(n_pol,/allocate)
	this_model_uv = Ptrarr(n_pol,/allocate)
	for pol_i=0,n_pol-1 do begin
	  *this_vis_weight_ptr[pol_i]=intarr(n_freq, n_param)
	  (*this_vis_weight_ptr[pol_i])[fi,*] = (*vis_weights[pol_i])[fi,*]
	  *this_model_uv[pol_i] = (*model_uvf_arr[pol_i])[*,*,fi]
          this_model_ptr=visibility_degrid((*model_uvf_arr[pol_i])[*,*,fi], this_vis_weight_ptr[pol_i],obs,psf,params, polarization=pol_i)
	endfor

	for pol_i=0,n_pol-1 do (*vis_model_arr[pol_i])[fi,*] = (*this_model_ptr[pol_i])[fi,*]

	undefine_fhd, this_vis_weight_ptr, this_model_ptr, this_model_uv
endfor

return, vis_model_arr

END



