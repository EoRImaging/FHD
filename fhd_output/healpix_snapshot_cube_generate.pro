PRO healpix_snapshot_cube_generate,obs_in,psf_in,vis_arr,vis_model_ptr=vis_model_ptr,$
    file_path_fhd=file_path_fhd,ps_dimension=ps_dimension,ps_fov=ps_fov,ps_degpix=ps_degpix,$
    ps_kbinsize=ps_kbinsize,ps_kspan=ps_kspan,ps_beam_threshold=ps_beam_threshold,$
    rephase_weights=rephase_weights,n_avg=n_avg,_Extra=extra


n_pol=obs_in.n_pol
n_freq=obs_in.n_freq
IF N_Elements(n_avg) EQ 0 THEN n_avg=Float(Round(n_freq/48.)) ;default of 48 output frequency bins
n_freq_use=Floor(n_freq/n_avg)
IF Keyword_Set(ps_beam_threshold) THEN beam_threshold=ps_beam_threshold ELSE beam_threshold=0.2

IF Keyword_Set(ps_kbinsize) THEN kbinsize=ps_kbinsize ELSE $
    IF Keyword_Set(ps_fov) THEN kbinsize=!RaDeg/ps_FoV ELSE kbinsize=obs_in.kpix
FoV_use=!RaDeg/kbinsize
 
IF Keyword_Set(ps_kspan) THEN dimension_use=ps_kspan/kbinsize ELSE $
    IF Keyword_Set(ps_dimension) THEN dimension_use=ps_dimension ELSE $
    IF Keyword_Set(ps_degpix) THEN dimension_use=FoV_use/ps_degpix ELSE dimension_use=FoV_use/obs_in.degpix

obs_out=vis_struct_update_obs(obs_in,n_pol=n_pol,nfreq_avg=n_avg,FoV=FoV_use,dimension=dimension_use)
degpix_use=FoV_use/dimension_use
pix_sky=4.*!Pi*!RaDeg^2./degpix_use^2.
Nside_chk=2.^(Ceil(ALOG(Sqrt(pix_sky/12.))/ALOG(2))) ;=1024. for 0.1119 degrees/pixel
IF ~Keyword_Set(nside) THEN nside_use=Nside_chk
nside_use=nside_use>Nside_chk
IF Keyword_Set(nside) THEN nside_use=nside ELSE nside=nside_use

IF N_Elements(psf_in) EQ 0 THEN psf_in=beam_setup(obs_in,file_path_fhd,/no_save,/silent)
ps_psf_resolution=Round(psf_in.resolution*obs_out.kpix/obs_in.kpix)
psf_out=beam_setup(obs_out,file_path_fhd,/no_save,psf_resolution=ps_psf_resolution,/silent)

hpx_cnv=healpix_cnv_generate(obs_out,file_path_fhd=file_path_fhd,nside=nside,$
    mask=beam_mask,radius=radius,restore_last=0,/no_save,hpx_radius=FoV_use/2.,_Extra=extra)



END