; Script to read .skyh5 files output by pyradiosky in an FHD-compatible way
; Written by Dara Storer, 5/24/21

pro skyh5_to_fhd, filename, filename_sav
  file_id = H5F_OPEN(filename)
  dataset_id = H5D_OPEN(file_id, '/Data/stokes')
  stokes = H5D_READ(dataset_id)
  
  nsources = H5D_READ(H5D_OPEN(file_id, '/Header/Ncomponents'))

  flux_struct={flux,xx:0.,yy:0.,xy:Complex(0.),yx:Complex(0.),I:0.,Q:0.,U:0.,V:0.}
  ;flux order is 0-3: xx, yy, xy, yx in apparent brightness; 4-7: I, Q, U, V in sky brightness
  ;flag type codes are 0: no flag, 1: low confidence 2: sidelobe contamination
  struct_base={id:'',x:0.,y:0.,ra:0.,dec:0.,ston:0.,freq:180.,alpha:0.,gain:1.,flag:0,extend:Ptr_new(),flux:flux_struct}
  
  source_ids = H5D_READ(H5D_OPEN(file_id, '/Header/name'))
  source_ras = H5D_READ(H5D_OPEN(file_id, '/Header/ra'))
  source_decs = H5D_READ(H5D_OPEN(file_id, '/Header/dec'))
  source_freqs = H5D_READ(H5D_OPEN(file_id, '/Header/reference_frequency'))
  source_alphas = H5D_READ(H5D_OPEN(file_id, '/Header/spectral_index'))
  source_fluxes_I = float(stokes[*,*,0])
  source_fluxes_Q = float(stokes[*,*,1])
  source_fluxes_U = float(stokes[*,*,2])
  source_fluxes_V = float(stokes[*,*,3])
  
  skyh5_catalog=Replicate(struct_base,nsources>1)
  for ind=0,nsources-1 do begin
    skyh5_catalog[ind].id = source_ids[ind]
    skyh5_catalog[ind].ra = source_ras[ind]
    skyh5_catalog[ind].dec = source_decs[ind]
    skyh5_catalog[ind].freq = source_freqs[ind]
    skyh5_catalog[ind].flux.I = source_fluxes_I[ind]
    skyh5_catalog[ind].flux.Q = source_fluxes_Q[ind]
    skyh5_catalog[ind].flux.U = source_fluxes_U[ind]
    skyh5_catalog[ind].flux.V = source_fluxes_V[ind]
    skyh5_catalog[ind].alpha = source_alphas[ind]
  endfor
  
  save, skyh5_catalog, filename=filename_sav
  ;return, skyh5_catalog
  
end