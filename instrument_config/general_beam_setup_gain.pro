FUNCTION general_beam_setup_gain,obs,antenna,file_path_fhd=file_path_fhd,$
  za_arr=za_arr,az_arr=az_arr,psf_image_dim=psf_image_dim,Jdate_use=Jdate_use,$
  import_pyuvdata_beam_filepath=import_pyuvdata_beam_filepath,_Extra=extra
  
  if ~keyword_set(import_pyuvdata_beam_filepath) then import_pyuvdata_beam_filepath=extra.import_pyuvdata_beam_filepath

  n_ant_pol=Max(antenna.n_pol)
  nfreq_bin=Max(antenna.nfreq_bin)
  pix_use = *antenna[0].pix_use
  IF N_Elements(file_path_fhd) EQ 0 THEN file_path_fhd=''
  n_tile=obs.n_tile
  beam_model_version=Max(antenna.model_version)
  xvals_interp=za_arr*Sin(az_arr*!DtoR)/obs.degpix+obs.dimension/2.
  yvals_interp=za_arr*Cos(az_arr*!DtoR)/obs.degpix+obs.elements/2.
  freq_center=antenna[0].freq ;all need to be identical, so just use the first
  pyuvdata_filepath=import_pyuvdata_beam_filepath

  ;calculate group identifications (used to set pointers to identical models)
  FOR pol_i=0,n_ant_pol-1 DO BEGIN
    gi=0
    n_ungrouped=n_tile
    ungrouped_i=where(antenna.group_id[pol_i] EQ -1,n_ungrouped)
    WHILE n_ungrouped GT 0 DO BEGIN
      ref_i=ungrouped_i[0]
      antenna[ref_i].group_id[pol_i]=gi
      FOR ug_i=1L,n_ungrouped-1 DO IF Total(*antenna[ungrouped_i[ug_i]].gain[pol_i] - *antenna[ref_i].gain[pol_i]) EQ 0 THEN antenna[ungrouped_i[ug_i]].group_id[pol_i]=gi
      ungrouped_i=where(antenna.group_id[pol_i] EQ -1,n_ungrouped)
      gi+=1
    ENDWHILE
  ENDFOR
  
  ;get the instrumental pol Jones matrix
  print,"Reading in: " + pyuvdata_filepath
  Jones_matrix = pyuvdata_beam_import(obs, antenna, pyuvdata_filepath,$
    za_arr=za_arr, az_arr=az_arr, psf_image_dim=psf_image_dim, pix_use=pix_use)
  
  antenna.jones=Jones_matrix

  RETURN,antenna
END
