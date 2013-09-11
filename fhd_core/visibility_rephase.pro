FUNCTION visibility_rephase,obs,params,vis_arr,preserve_visibilities=preserve_visibilities

icomp=Complex(0,1)
n_pol=obs.n_pol
dimension=obs.dimension
elements=obs.elements
dx=obs.obsx-dimension
dy=obs.obsy-elements
freq_arr=(*obs.baseline_info).freq
kx_arr=params.uu#freq_arr
ky_arr=params.vv#freq_arr

IF Keyword_Set(preserve_visibilities) THEN BEGIN
    vis_arr_out=Ptrarr(n_pol,/allocate)
    FOR pol_i=0,n_pol-1 DO *vis_arr_out[pol_i]=*vis_arr[pol_i]
ENDIF ELSE vis_arr_out=vis_arr

phase_shift=Exp(icomp*2.*!Pi*(dx*kx_arr/dimension+dy*ky_arr/elements))
FOR pol_i=0,n_pol-1 DO *vis_arr_out[pol_i]=Temporary(*vis_arr_out[pol_i]*phase_shift)

obs.obsx-=dx
obs.obsy-=dy
obs.zenx-=dx
obs.zeny-=dy
obs.phasera=obs.obsra
obs.phasedec=obs.obsdec
RETURN,vis_arr_out
END