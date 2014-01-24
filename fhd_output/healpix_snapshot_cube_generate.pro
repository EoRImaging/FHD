PRO healpix_snapshot_cube_generate,obs,vis_arr,vis_model_ptr=vis_model_ptr,$
    file_path_fhd=file_path_fhd,ps_dimension=ps_dimension,ps_fov=ps_fov,ps_degpix=ps_degpix,$
    ps_kbinsize=ps_kbinsize,ps_kspan=ps_kspan,ps_beam_threshold=ps_beam_threshold,$
    rephase_weights=rephase_weights,n_avg=n_avg,_Extra=extra


n_pol=obs.n_pol
n_freq=obs.n_freq
IF N_Elements(n_avg) EQ 0 THEN n_avg=Float(Round(n_freq/48.)) ;default of 48 output frequency bins
n_freq_use=Floor(n_freq/n_avg)
IF Keyword_Set(ps_beam_threshold) THEN beam_threshold=ps_beam_threshold ELSE beam_threshold=0.2

IF Keyword_Set(ps_kbinsize) THEN kbinsize=ps_kbinsize ELSE $
    IF Keyword_Set(ps_fov) THEN kbinsize=!RaDeg/ps_FoV ELSE kbinsize=obs.kpix
FoV_use=!RaDeg/kbinsize
 
IF Keyword_Set(ps_kspan) THEN dimension_use=ps_kspan/kbinsize ELSE $
    IF Keyword_Set(ps_dimension) THEN dimension_use=ps_dimension ELSE $
    IF Keyword_Set(ps_degpix) THEN dimension_use=FoV_use/ps_degpix ELSE dimension_use=FoV_use/obs.degpix

degpix_use=FoV_use/dimension_use
pix_sky=4.*!Pi*!RaDeg^2./degpix_use^2.
Nside_chk=2.^(Ceil(ALOG(Sqrt(pix_sky/12.))/ALOG(2))) ;=1024. for 0.1119 degrees/pixel
IF ~Keyword_Set(nside) THEN nside_use=Nside_chk
nside_use=nside_use>Nside_chk
IF Keyword_Set(nside) THEN nside_use=nside ELSE nside=nside_use
END