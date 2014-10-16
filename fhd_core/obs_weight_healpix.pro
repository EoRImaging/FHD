FUNCTION obs_weight_healpix,obs_arr,beam_arr,jones_arr,hpx_ind_map,hpx_cnv_arr,hpx_inds,$
    obs_weight_arr=obs_weight_arr,beam_mask_arr=beam_mask_arr,_Extra=extra

n_obs=N_Elements(obs_arr)
n_pol=obs_arr[0].n_pol
n_hpx=N_Elements(hpx_inds)
beam_square=Ptrarr(n_pol)

IF Arg_present(obs_weight_arr) THEN BEGIN
    weight_return_flag=1
    obs_weight_arr=Ptrarr(n_obs)
ENDIF
mask_flag=Min(Ptr_valid(beam_mask_arr))
weight_hpx=Fltarr(n_hpx)
FOR obs_i=0L,n_obs-1 DO BEGIN
    FOR pol_i=0,n_pol-1 DO beam_square[pol_i]=Ptr_new((*beam_arr[pol_i,obs_i])^2.)
    
    beam_stokes_arr=Stokes_cnv(beam_square,jones_arr[obs_i],obs_arr[obs_i],_Extra=extra)
    obs_weight_single=*beam_stokes_arr[0]
    Ptr_free,beam_stokes_arr
    obs_weight_single/=(2.<n_pol)
;    IF mask_flag THEN obs_weight_single*=*beam_mask_arr[obs_i]
    
    weight_hpx[*hpx_ind_map[obs_i]]+=healpix_cnv_apply(obs_weight_single,hpx_cnv_arr[obs_i])
    IF Keyword_Set(weight_return_flag) THEN obs_weight_arr[obs_i]=Ptr_new(Temporary(obs_weight_single))
ENDFOR


RETURN,weight_hpx
END