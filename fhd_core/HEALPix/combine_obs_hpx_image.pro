PRO combine_obs_hpx_image,file_list,status_arr,hpx_inds,obs_arr,n_obs_hpx=n_obs_hpx,stokes_dirty_hpx=stokes_dirty_hpx,$
    stokes_model_hpx=stokes_model_hpx,weights_hpx=weights_hpx,stokes_sources_hpx=stokes_sources_hpx,nside=nside,$
    output_path=output_path,image_filter_fn=image_filter_fn,color_table=color_table,$
    high_dirty=high_dirty,high_source=high_source,high_residual=high_residual,$
    low_dirty=low_dirty,low_source=low_source,low_residual=low_residual,$
    no_hpx_fits=no_hpx_fits,no_hpx_png=no_hpx_png,no_hpx_ps=no_hpx_ps,_Extra=extra

except=!except
!except=0 
heap_gc

;IF ~Keyword_Set(color_table) THEN color_table=0.1 ;written intentionally to overwrite color_table=0
;
;IF N_Elements(map_projection) EQ 0 THEN map_projection='orth' ELSE map_projection=StrLowCase(map_projection)
;proj_list=['mollweide','orthographic','gnomic','cartesian']
;proj_name_list=['MOLLVIEW', 'GNOMVIEW', 'CARTVIEW', 'ORTHVIEW']
;
;proj_i=where(strpos(proj_list,map_projection) GE 0,n_match_proj)
;IF n_match_proj EQ 0 THEN proj_routine='orthview' ELSE proj_routine=proj_name_list[proj_i]

IF N_Elements(output_path) EQ 0 THEN output_path='Combined_obs'
IF N_Elements(file_list) GT 1 THEN  save_path_base=output_path+'_'+file_basename(file_list[0])+'-'+file_basename(file_list[N_Elements(file_list)-1]) $
    ELSE IF N_Elements(file_list) EQ 1 THEN  save_path_base=output_path+'_'+file_basename(file_list[0]) $
        ELSE save_path_base=output_path

IF N_Elements(weight_threshold) EQ 0 THEN weight_threshold_use=0.2 $
    ELSE weight_threshold_use=weight_threshold

pol_names=['I','Q','U','V']
n_hpx=nside2npix(nside)
hpx_degpix=Sqrt(4.*!Pi/n_hpx)*!Radeg
n_pix_use=N_Elements(hpx_inds)
n_obs=N_Elements(obs_arr)
n_pol=Min(obs_arr.n_pol)<2
IF N_Elements(n_obs_hpx) NE n_pix_use THEN n_obs_hpx=1

IF n_pol EQ 1 THEN pol2=pol1 ;hack to get the factor of two in all the right places

residual_flag=Min(obs_arr.residual)
source_flag=Min(Ptr_valid(stokes_sources_hpx))
model_flag=Min(Ptr_valid(stokes_model_hpx))
IF Keyword_Set(residual_flag) THEN model_flag=0
restored_flag=(residual_flag OR model_flag) AND source_flag
dirty_flag=~residual_flag

ring2nest, nside, hpx_inds, hpx_inds_nest ;external programs are much happier reading in Healpix fits files with the nested pixel ordering
FOR stk_i=0,n_pol-1 DO BEGIN
    file_path_residual=save_path_base+'_Stokes_'+pol_names[stk_i]+'_hpx_residual'
    file_path_weights=save_path_base+'_Stokes_'+pol_names[stk_i]+'_hpx_weights'
    file_path_restored=save_path_base+'_Stokes_'+pol_names[stk_i]+'_hpx_restored'
    file_path_sources=save_path_base+'_Stokes_'+pol_names[stk_i]+'_hpx_sources'
    file_path_dirty=save_path_base+'_Stokes_'+pol_names[stk_i]+'_hpx_dirty'
    file_path_rings=save_path_base+'_Stokes_'+pol_names[stk_i]+'_hpx_source_rings'
        
    title_img='Composite Stokes '+pol_names[stk_i]+' residual'
    title_wts='Composite Stokes '+pol_names[stk_i]+' weights'
        
    title_rst='Composite Stokes '+pol_names[stk_i]+' restored'
    title_src='Composite Stokes '+pol_names[stk_i]+' sources'
    title_dty='Composite Stokes '+pol_names[stk_i]+' dirty'
    
    
    IF model_flag AND not residual_flag THEN BEGIN
        stokes_residual=(*stokes_dirty_hpx[stk_i]-*stokes_model_hpx[stk_i])
    ENDIF ELSE BEGIN
        stokes_residual=(*stokes_dirty_hpx[stk_i])
    ENDELSE
    
    IF dirty_flag THEN stokes_dirty=(*stokes_dirty_hpx[stk_i])
    
    source_flag_use=source_flag
    IF source_flag THEN BEGIN
        stokes_sources=*stokes_sources_hpx[stk_i]
        IF Max(stokes_sources) EQ Min(stokes_sources) THEN source_flag_use=0
    ENDIF
    
    stokes_weights=*weights_hpx[stk_i]
    err_map=weight_invert(stokes_weights)
    
    IF ~Keyword_Set(no_hpx_fits) THEN BEGIN
        ;Must use NESTED healpix indices here
        write_fits_cut4,file_path_weights+'.fits',hpx_inds_nest,stokes_weights,n_obs_hpx,err_map,nside=nside,/nested,coord='C'
        IF (residual_flag OR model_flag) THEN write_fits_cut4,file_path_residual+'.fits',hpx_inds_nest,stokes_residual,n_obs_hpx,err_map,nside=nside,/nested,coord='C'
        IF restored_flag THEN write_fits_cut4,file_path_restored+'.fits',hpx_inds_nest,stokes_residual+stokes_sources,n_obs_hpx,err_map,nside=nside,/nested,coord='C'
        IF source_flag_use THEN write_fits_cut4,file_path_sources+'.fits',hpx_inds_nest,Stokes_sources,n_obs_hpx,err_map,nside=nside,/nested,coord='C'
        IF dirty_flag THEN write_fits_cut4,file_path_dirty+'.fits',hpx_inds_nest,Stokes_dirty,n_obs_hpx,err_map,nside=nside,/nested,coord='C'
    ENDIF
    IF ~Keyword_Set(no_hpx_png) THEN BEGIN
        ;Must use RING healpix indices here
        healpix_quickimage,stokes_weights,hpx_inds,nside,/png,savefile=file_path_weights
        IF (residual_flag OR model_flag) THEN healpix_quickimage,stokes_residual,hpx_inds,nside,/png,savefile=file_path_residual
        IF restored_flag THEN healpix_quickimage,stokes_residual+stokes_sources,hpx_inds,nside,/png,savefile=file_path_restored
        IF source_flag_use THEN healpix_quickimage,Stokes_sources,hpx_inds,nside,/png,savefile=file_path_sources
        IF dirty_flag THEN healpix_quickimage,Stokes_dirty,hpx_inds,nside,/png,savefile=file_path_dirty
    ENDIF
ENDFOR
END
