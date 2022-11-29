FUNCTION load_skyh5_source_catalog, catalog_path, $
  skyh5_name_stokes=skyh5_name_stokes, skyh5_name_nsources=skyh5_name_nsources, skyh5_name_ra=skyh5_name_ra, skyh5_name_dec=skyh5_name_dec,$
  skyh5_name_freqs=skyh5_name_freqs, skyh5_name_alphas=skyh5_name_alphas
  ; Restore a source catalog from a .skyh5 file
  
  if ~keyword_set(skyh5_name_stokes) then skyh5_name_stokes='/Data/stokes'
  if ~keyword_set(skyh5_name_nsources) then skyh5_name_nsources='/Header/Ncomponents'
  if ~keyword_set(skyh5_name_ra) then skyh5_name_ra= '/Header/lon'
  if ~keyword_set(skyh5_name_dec) then skyh5_name_dec='/Header/lat'
  if ~keyword_set(skyh5_name_freqs) then skyh5_name_freqs='/Header/reference_frequency'
  if ~keyword_set(skyh5_name_alphas) then skyh5_name_alphas='/Header/spectral_index'
  
  file_id = H5F_OPEN(catalog_path) ;Get file ID
  
  stokes_id = H5D_OPEN(file_id, skyh5_name_stokes)
  stokes = H5D_READ(stokes_id)
  H5D_CLOSE, stokes_id
  
  nsources_id = H5D_OPEN(file_id, skyh5_name_nsources)
  nsources = H5D_READ(nsources_id)
  H5D_CLOSE, nsources_id
  
  ras_id = H5D_OPEN(file_id, skyh5_name_ra)
  source_ras = H5D_READ(ras_id)
  H5D_CLOSE, ras_id
  
  decs_id = H5D_OPEN(file_id, skyh5_name_dec)
  source_decs = H5D_READ(decs_id)
  H5D_CLOSE, decs_id
  
  freqs_id = H5D_OPEN(file_id, skyh5_name_freqs)
  source_freqs = H5D_READ(freqs_id)
  H5D_CLOSE, freqs_id
  
  alphas_id = H5D_OPEN(file_id, skyh5_name_alphas)
  source_alphas = H5D_READ(alphas_id)
  H5D_CLOSE, alphas_id

  H5F_CLOSE, file_id
  
  source_fluxes_I = float(stokes[*,*,0])
  source_fluxes_Q = float(stokes[*,*,1])
  source_fluxes_U = float(stokes[*,*,2])
  source_fluxes_V = float(stokes[*,*,3])
  
  flux_struct={flux,xx:0.,yy:0.,xy:Complex(0.),yx:Complex(0.),I:0.,Q:0.,U:0.,V:0.}
  flux_struct_arr=Replicate(flux_struct,nsources>1)
  for ind=0,nsources-1 do begin
    flux_struct_arr[ind].I = source_fluxes_I[ind]
    flux_struct_arr[ind].Q = source_fluxes_Q[ind]
    flux_struct_arr[ind].U = source_fluxes_U[ind]
    flux_struct_arr[ind].V = source_fluxes_V[ind]
  endfor
  
  source_cat = source_comp_init(n_sources=nsources,frequency=source_freqs,$
    ra=source_ras,dec=source_decs,flux=flux_struct_arr,alpha=source_alphas)

  RETURN, source_cat

END
