PRO combine_obs_hpx_image,fhd_file_list,hpx_inds,obs_arr,instr_dirty_hpx=instr_dirty_hpx,$
    instr_model_hpx=instr_model_hpx,weights_hpx=weights_hpx,instr_sources_hpx=instr_sources_hpx,$
    instr_rings_hpx=instr_rings_hpx,instr_catalog_hpx=instr_catalog_hpx,nside=nside,$
    output_path=healpix_path,image_filter_fn=image_filter_fn,color_table=color_table,$
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

save_path_base=output_path+'_'+file_basename(file_list[0])+'-'+file_basename(file_list[n_obs-1])

IF N_Elements(weight_threshold) EQ 0 THEN weight_threshold_use=0.2 $
    ELSE weight_threshold_use=weight_threshold

Stokes_images=Ptrarr(2,/allocate)
Stokes_weights=Ptrarr(2,/allocate)
Stokes_inds=Ptrarr(2,/allocate)
sign=[[1.,1.],[1.,-1.]]
pol_names=['XX','YY','XY','YX','I','Q','U','V']
n_hpx=nside2npix(nside)
n_pix_use=N_Elements(hpx_inds)
n_obs=N_Elements(obs_arr)

IF n_obs EQ 1 THEN BEGIN
    lon_avg=obs_arr.obsra
    lat_avg=obs_arr.obsdec
ENDIF ELSE BEGIN
    lon_arr=obs_arr.obsra
    lat_arr=obs_arr.obsdec
    lon_hist=histogram(Floor(lon_arr),min=0,/bin,max=359)
    lon_test=morph_distance(lon_hist,/background)
    test_max=max(lon_test,lon_branch)
    lon_use=lon_arr
    lon_mod_i=where(lon_arr LE lon_branch,n_branch)
    IF n_branch GT 0 THEN lon_use[lon_mod_i]+=360.
    lon_avg=Median(lon_use) & IF lon_avg GE 360. THEN lon_avg-=360.
    lat_avg=Mean(lat_arr)
ENDELSE

n_pol=Min(obs_arr.n_pol)<2
IF n_pol EQ 1 THEN sign*=2. ;hack to get the factor of two in all the right places

FOR pol_i=0,n_pol-1 DO BEGIN
    file_path_img=save_path_base+'_Stokes_'+pol_names[pol_i+4]+'_hpx_residual'
    file_path_wts=save_path_base+'_Stokes_'+pol_names[pol_i+4]+'_hpx_weights'
    file_path_rst=save_path_base+'_Stokes_'+pol_names[pol_i+4]+'_hpx_restored'
    file_path_src=save_path_base+'_Stokes_'+pol_names[pol_i+4]+'_hpx_sources'
    file_path_dty=save_path_base+'_Stokes_'+pol_names[pol_i+4]+'_hpx_dirty'
        
    title_img='Composite Stokes '+pol_names[pol_i+4]+' residual'
    title_wts='Composite Stokes '+pol_names[pol_i+4]+' weights'
        
    title_rst='Composite Stokes '+pol_names[pol_i+4]+' restored'
    title_src='Composite Stokes '+pol_names[pol_i+4]+' sources'
    title_dty='Composite Stokes '+pol_names[pol_i+4]+' dirty'
    title_mrc='Composite Stokes '+pol_names[pol_i+4]+' MRC rings'
;FOR stk_i=0,n_pol-1 DO BEGIN
;    Stokes_single=fltarr(npix)
;    Stokes_weights_single=fltarr(npix)
;    Stokes_sources=fltarr(npix)
;    Stokes_restored=fltarr(npix)
;    Stokes_dirty=fltarr(npix)
;    Stokes_smooth=fltarr(npix)
;    Stokes_MRC=fltarr(npix)
;    FOR pol_i=0,n_pol-1 DO BEGIN
;        stk_res0=*residual_hpx[pol_i]*weight_invert(*weights_hpx[pol_i])
;        Stokes_single[hpx_inds]+=stk_res0*sign[stk_i,pol_i]
;        Stokes_weights_single[hpx_inds]+=*weights_hpx[pol_i]
;        stk_src0=*sources_hpx[pol_i]*weight_invert(*weights_hpx[pol_i]);*2.
;        Stokes_sources[hpx_inds]+=stk_src0*sign[stk_i,pol_i]
;        IF mrc_flag THEN Stokes_MRC[hpx_inds]+=Stokes_sources[hpx_inds]+*mrc_hpx[pol_i]*weight_invert(*weights_hpx[pol_i])
;        Stokes_restored[hpx_inds]+=(stk_res0+stk_src0)*sign[stk_i,pol_i]
;        Stokes_dirty[hpx_inds]+=*dirty_hpx[pol_i]*weight_invert(*weights_hpx[pol_i])*sign[stk_i,pol_i]
;    ENDFOR
;    
;    hpx_ind_use=where(Stokes_weights_single GT weight_threshold_use,n_hpx,complement=i_cut,ncomplement=n_cut)
;    IF stk_i EQ 0 THEN BEGIN      
;        area=4.*!Pi*(!RaDeg^2.)*(Float(n_hpx)/npix)
;        print,"Observed area: "+Strn(area)+" degrees from "+Strn(n_obs)+" snapshot observations."
;    ENDIF
;    norm=1.;1./Max(Stokes_weights_single)^2.
;    
;    *Stokes_images[stk_i]=Stokes_single[hpx_ind_use]*norm
;    *Stokes_weights[stk_i]=Stokes_weights_single[hpx_ind_use]
;    *Stokes_inds[stk_i]=hpx_ind_use
;    Stokes_restored=Stokes_restored[hpx_ind_use]*norm
;    Stokes_sources=Stokes_sources[hpx_ind_use]*norm
;    Stokes_MRC=Stokes_MRC[hpx_ind_use]*norm
;    Stokes_dirty=Stokes_dirty[hpx_ind_use]*norm
;    
;    file_path_img=save_path_base+'_Stokes_'+Stk_nm[stk_i]+'_hpx_residual'
;    file_path_wts=save_path_base+'_Stokes_'+Stk_nm[stk_i]+'_hpx_weights'
;    file_path_rst=save_path_base+'_Stokes_'+Stk_nm[stk_i]+'_hpx_restored'
;    file_path_src=save_path_base+'_Stokes_'+Stk_nm[stk_i]+'_hpx_sources'
;    file_path_dty=save_path_base+'_Stokes_'+Stk_nm[stk_i]+'_hpx_dirty'
;    file_path_mrc=save_path_base+'_Stokes_'+Stk_nm[stk_i]+'_hpx_MRC_rings'
;    
;    write_healpix_fits,file_path_img,*Stokes_images[stk_i],*Stokes_inds[stk_i],nside=nside,weights=*Stokes_weights[stk_i]
;    write_healpix_fits,file_path_wts,*Stokes_weights[stk_i],*Stokes_inds[stk_i],nside=nside
;    write_healpix_fits,file_path_rst,Stokes_restored,*Stokes_inds[stk_i],nside=nside,weights=*Stokes_weights[stk_i]
;    write_healpix_fits,file_path_src,Stokes_sources,*Stokes_inds[stk_i],nside=nside,weights=*Stokes_weights[stk_i]
;    write_healpix_fits,file_path_dty,Stokes_dirty,*Stokes_inds[stk_i],nside=nside,weights=*Stokes_weights[stk_i]
;    
;    IF mrc_flag THEN write_healpix_fits,file_path_mrc,Stokes_MRC,*Stokes_inds[stk_i],nside=nside,weights=*Stokes_weights[stk_i]
;    Stokes_single=0
;    Stokes_weights_single=0
;    Stokes_sources=0
;    Stokes_restored=0
;    Stokes_dirty=0
;    Stokes_smooth=0
;    Stokes_MRC=0
;;        write_fits_cut4,file_path_img+'.fits',*Stokes_inds[stk_i],*Stokes_images[stk_i],/ring,Coords='C',nside=nside
;;        write_fits_cut4,file_path_wts+'.fits',*Stokes_inds[stk_i],*Stokes_weights[stk_i],/ring,Coords='C',nside=nside
;;        
;;        write_fits_cut4,file_path_rst+'.fits',*Stokes_inds[stk_i],Stokes_restored,/ring,Coords='C',nside=nside
;;        write_fits_cut4,file_path_src+'.fits',*Stokes_inds[stk_i],Stokes_sources,/ring,Coords='C',nside=nside
;;        write_fits_cut4,file_path_dty+'.fits',*Stokes_inds[stk_i],Stokes_dirty,/ring,Coords='C',nside=nside
;        
;;        write_fits_cut4,file_path_smt+'.fits',*Stokes_inds[stk_i],Stokes_smooth,/ring,Coords='C',nside=nside
;ENDFOR    
;;ENDIF
;
;image_scale=Sqrt(*Stokes_weights[0]/Mean(*Stokes_weights[0]))
;IF N_Elements(high_residual) EQ 0 THEN BEGIN
;    high_residual=Max(*Stokes_images[0]*image_scale)<(-Min(*Stokes_images[0]*image_scale))
;    dec_test=Alog10(high_residual)
;    high_residual=Ceil(high_residual/10.^Floor(dec_test))*10.^Floor(dec_test)
;ENDIF
;IF N_Elements(low_residual) EQ 0 THEN low_residual=-high_residual
;
;IF N_Elements(high_dirty) EQ 0 THEN high_dirty=8.*high_residual
;IF N_Elements(high_source) EQ 0 THEN high_source=high_dirty
;IF N_Elements(low_dirty) EQ 0 THEN low_dirty=-high_dirty/2.
;IF N_Elements(low_source) EQ 0 THEN low_source=0.
;IF N_Elements(fraction_polarized) EQ 0 THEN fraction_polarized=0.5
;
;;free memory
;undefine_fhd,Stokes_images,Stokes_weights,Stokes_inds
;
;FOR stk_i=0,n_pol-1 DO BEGIN
;    CASE stk_i OF
;        0:cnorm=1.
;        1:cnorm=fraction_polarized
;    ENDCASE
;    
;    file_path_img=save_path_base+'_Stokes_'+Stk_nm[stk_i]+'_hpx_residual'
;    file_path_wts=save_path_base+'_Stokes_'+Stk_nm[stk_i]+'_hpx_weights'
;    file_path_rst=save_path_base+'_Stokes_'+Stk_nm[stk_i]+'_hpx_restored'
;    file_path_src=save_path_base+'_Stokes_'+Stk_nm[stk_i]+'_hpx_sources'
;    file_path_dty=save_path_base+'_Stokes_'+Stk_nm[stk_i]+'_hpx_dirty'
;    file_path_mrc=save_path_base+'_Stokes_'+Stk_nm[stk_i]+'_hpx_MRC_rings'
;;    file_path_smt=save_path_base+'_Stokes_'+Stk_nm[stk_i]+'_hpx_smooth'
;        
;    title_img='Composite Stokes '+Stk_nm[stk_i]+' residual'
;    title_wts='Composite Stokes '+Stk_nm[stk_i]+' weights'
;    healpix_image,file_path_img,map_projection=map_projection,ps_write=0,png_write=1,silent=1,$
;        title=title_img,lon=lon_avg,lat=lat_avg,min=low_residual*cnorm,max=high_residual*cnorm,/half,color_table=color_table
;    healpix_image,file_path_wts,map_projection=map_projection,ps_write=0,png_write=1,silent=1,$
;        title=title_wts,lon=lon_avg,lat=lat_avg,/half,color_table=color_table
;        
;    title_rst='Composite Stokes '+Stk_nm[stk_i]+' restored'
;    title_src='Composite Stokes '+Stk_nm[stk_i]+' sources'
;    title_dty='Composite Stokes '+Stk_nm[stk_i]+' dirty'
;    title_mrc='Composite Stokes '+Stk_nm[stk_i]+' MRC rings'
;    
;;    title_smt='Composite Stokes '+Stk_nm[stk_i]+' smooth'
;    healpix_image,file_path_rst,map_projection=map_projection,ps_write=0,png_write=1,silent=1,$
;        title=title_rst,lon=lon_avg,lat=lat_avg,min=low_dirty*cnorm,max=high_dirty*cnorm,/half,color_table=color_table
;    healpix_image,file_path_src,map_projection=map_projection,ps_write=0,png_write=1,silent=1,$
;        title=title_src,lon=lon_avg,lat=lat_avg,min=0,max=high_dirty*cnorm,/half,color_table=color_table
;    healpix_image,file_path_dty,map_projection=map_projection,ps_write=0,png_write=1,silent=1,$
;        title=title_dty,lon=lon_avg,lat=lat_avg,min=low_dirty*cnorm,max=high_dirty*cnorm,/half,color_table=color_table
;    IF mrc_flag THEN healpix_image,file_path_mrc,map_projection=map_projection,ps_write=0,png_write=1,silent=1,$
;        title=title_mrc,lon=lon_avg,lat=lat_avg,min=0,max=5.,/half,color_table=color_table
;;        
;;    healpix_image,file_path_smt,map_projection=map_projection,ps_write=0,png_write=1,silent=1,$
;;        title=title_smt,lon=lon_avg,lat=lat_avg,min=low_residual*cnorm/2.,max=high_residual*cnorm/2.,/half,color_table=color_table
;ENDFOR

END
