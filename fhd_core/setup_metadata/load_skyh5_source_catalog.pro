FUNCTION load_skyh5_source_catalog, catalog
  ; Restore a source catalog from a .skyh5 file
  
  file_id = H5F_OPEN(catalog)
  dataset_id = H5D_OPEN(file_id, '/Data/stokes')
  stokes = H5D_READ(dataset_id)

  nsources = H5D_READ(H5D_OPEN(file_id, '/Header/Ncomponents'))

  source_ras = H5D_READ(H5D_OPEN(file_id, '/Header/lon'))
  source_decs = H5D_READ(H5D_OPEN(file_id, '/Header/lat'))
  source_freqs = H5D_READ(H5D_OPEN(file_id, '/Header/reference_frequency'))
  source_alphas = H5D_READ(H5D_OPEN(file_id, '/Header/spectral_index'))
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
