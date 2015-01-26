PRO array_simulator,error=error,instrument=instrument,_Extra=extra
file_path_vis,export_images=export_images,cleanup=cleanup,recalculate_all=recalculate_all,$
    beam_recalculate=beam_recalculate,grid_recalculate=grid_recalculate,$
    n_pol=n_pol,silent=silent,$
    healpix_recalculate=healpix_recalculate,tile_flag_list=tile_flag_list,$
    file_path_fhd=file_path_fhd,freq_start=freq_start,freq_end=freq_end,error=error, $
    eor_sim=eor_sim, flat_sigma = flat_sigma, no_distrib = no_distrib, delta_power = delta_power, delta_uv_loc = delta_uv_loc, $
    include_catalog_sources = include_catalog_sources, source_list=source_list, catalog_file_path=catalog_file_path, $
    model_uvf_cube=model_uvf_cube, model_image_cube=model_image_cube, $
    weights_grid=weights_grid,save_visibilities=save_visibilities,save_uvf=save_uvf,save_imagecube=save_imagecube,$
    snapshot_healpix_export=snapshot_healpix_export,n_avg=n_avg,_Extra=extra
    
compile_opt idl2,strictarrsubs
except=!except
!except=0 ;System variable that controls when math errors are printed. Set to 0 to disable.
error=0
heap_gc
t0=Systime(1)
IF N_Elements(recalculate_all) EQ 0 THEN recalculate_all=0
IF N_Elements(beam_recalculate) EQ 0 THEN beam_recalculate=recalculate_all
IF N_Elements(grid_recalculate) EQ 0 THEN grid_recalculate=recalculate_all
IF N_Elements(healpix_recalculate) EQ 0 THEN healpix_recalculate=recalculate_all
IF N_Elements(save_visibilities) EQ 0 THEN save_visibilities=1
  
fhd_save_io,status_str,file_path_fhd=file_path_fhd
  
print,'Simulating: ',file_path_vis
print,systime()
print,'Output file_path:',file_path_fhd

array_simulator_init,obs,params,error=error,instrument=instrument,_Extra=extra



;Read in or construct a new beam model. Also sets up the structure PSF
print,'Calculating beam model'
psf=beam_setup(obs,status_str,file_path_fhd=file_path_fhd,restore_last=0,silent=silent,timing=t_beam,no_save=0,_Extra=extra)
IF Keyword_Set(t_beam) THEN print,'Beam modeling time: ',t_beam

flag_arr=vis_flag_basic(flag_arr,obs,params,n_pol=n_pol,n_freq=n_freq,freq_start=freq_start,$
    freq_end=freq_end,tile_flag_list=tile_flag_list,_Extra=extra)
vis_flag_update,flag_arr,obs,psf,params
;print informational messages
obs_status,obs
END