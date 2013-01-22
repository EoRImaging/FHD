PRO combine_obs_hpx_image,hpx_inds,residual_hpx,weights_hpx,dirty_hpx,sources_hpx,restored_hpx,smooth_hpx,$
    nside=nside,restore_last=restore_last,weight_threshold=weight_threshold,version=version,data_directory=data_directory,$
    obs_arr=obs_arr,$
    color_table=color_table,high_dirty=high_dirty,high_source=high_source,high_residual=high_residual,$
    fraction_polarized=fraction_polarized,low_dirty=low_dirty,low_source=low_source,low_residual=low_residual

except=!except
!except=0 
heap_gc

IF not Keyword_Set(color_table) THEN color_table=0.1 ;written intentionally to overwrite color_table=0

IF N_Elements(high_dirty) EQ 0 THEN high_dirty=20.
IF N_Elements(high_source) EQ 0 THEN high_source=20.
IF N_Elements(high_residual) EQ 0 THEN high_residual=3.
IF N_Elements(low_dirty) EQ 0 THEN low_dirty=-high_dirty/2.
IF N_Elements(low_source) EQ 0 THEN low_source=0.
IF N_Elements(low_residual) EQ 0 THEN low_residual=-high_residual
IF N_Elements(fraction_polarized) EQ 0 THEN fraction_polarized=0.5

IF not Keyword_Set(data_directory) THEN vis_path_default,data_directory ;set default if not supplied

IF N_Elements(version) EQ 0 THEN version=0 ELSE version=Fix(version)
version_name='v'+strn(version)
version_dirname='fhd_'+version_name
output_dir=filepath('',root=data_directory,sub=['Combined_obs',version_dirname])
IF file_test(rootdir('mwa')+output_dir) EQ 0 THEN file_mkdir,rootdir('mwa')+output_dir   
output_path=filepath('Healpix'+'v'+strn(version)+'.sav',root_dir=rootdir('mwa')+output_dir)


Stokes_images=Ptrarr(2,/allocate)
Stokes_weights=Ptrarr(2,/allocate)
Stokes_inds=Ptrarr(2,/allocate)
sign=[[1.,1.],[1.,-1.]]
Stk_nm=['I','Q']
npix=nside2npix(nside)
;IF not Keyword_Set(nside) THEN nside=npix2nside(npix)
lon_arr=obs_arr.obsra
lat_arr=obs_arr.obsdec
n_files=N_Elements(lon_arr)
lon_hist=histogram(Floor(lon_arr),min=0,/bin,max=359)
lon_test=morph_distance(lon_hist,/background)
test_max=max(lon_test,lon_branch)
lon_use=lon_arr
lon_mod_i=where(lon_arr LE lon_branch,n_branch)
IF n_branch GT 0 THEN lon_use[lon_mod_i]+=360.
lon_avg=Median(lon_use) & IF lon_avg GE 360. THEN lon_avg-=360.


IF not Keyword_Set(restore_last) THEN BEGIN
    FOR stk_i=0,1 DO BEGIN
        Stokes_single=fltarr(npix)
        Stokes_weights_single=fltarr(npix)
        Stokes_sources=fltarr(npix)
        Stokes_restored=fltarr(npix)
        Stokes_dirty=fltarr(npix)
;        Stokes_mrc=fltarr(npix)
        Stokes_smooth=fltarr(npix)
        FOR pol_i=0,1 DO BEGIN
            stk_res0=*residual_hpx[pol_i]*weight_invert(*weights_hpx[pol_i])
            Stokes_single+=stk_res0*sign[stk_i,pol_i]
            Stokes_smooth+=*smooth_hpx[pol_i]*weight_invert(*weights_hpx[pol_i])*sign[stk_i,pol_i]
            Stokes_weights_single+=*weights_hpx[pol_i]
    ;        weights2=fltarr(npix)
    ;        weights2[where(*weights_hpx[pol_i])]=1
            stk_src0=*sources_hpx[pol_i]*weight_invert(*weights_hpx[pol_i])*2.
            Stokes_sources+=stk_src0*sign[stk_i,pol_i]
;            Stokes_mrc+=*mrc_hpx[pol_i]*weight_invert(*weights_hpx[pol_i])*sign[stk_i,pol_i]
    ;        Stokes_restored+=*restored_hpx[pol_i]*weight_invert(*weights_hpx[pol_i])*sign[stk_i,pol_i]
            Stokes_restored+=(stk_res0+stk_src0)*sign[stk_i,pol_i]
            Stokes_dirty+=*dirty_hpx[pol_i]*weight_invert(*weights_hpx[pol_i])*sign[stk_i,pol_i]
        ENDFOR
        
        hpx_ind_use=where(Stokes_weights_single GT weight_threshold,n_hpx,complement=i_cut,ncomplement=n_cut)
        IF stk_i EQ 0 THEN BEGIN      
;            n_hpx=N_Elements(hpx_inds)
            area=4.*!Pi*(!RaDeg^2.)*(Float(n_hpx)/npix)
            print,"Observed area: "+Strn(area)+" degrees from "+Strn(n_files)+" snapshot observations."
        ENDIF
        norm=1.;1./Max(Stokes_weights_single)
;        
        *Stokes_images[stk_i]=Stokes_single[hpx_ind_use]*norm
        *Stokes_weights[stk_i]=Stokes_weights_single[hpx_ind_use]
        *Stokes_inds[stk_i]=hpx_inds[hpx_ind_use]
        Stokes_restored=Stokes_restored[hpx_ind_use]*norm
        Stokes_sources=Stokes_sources[hpx_ind_use]*norm
        Stokes_dirty=Stokes_dirty[hpx_ind_use]*norm
;        Stokes_mrc=Stokes_mrc[hpx_ind_use]*norm+Stokes_sources
        Stokes_smooth=Stokes_smooth[hpx_ind_use]*norm
        
;        *Stokes_images[stk_i]=Stokes_single*norm
;        *Stokes_weights[stk_i]=Stokes_weights_single
;        *Stokes_inds[stk_i]=hpx_inds
;        Stokes_restored=Stokes_restored*norm
;        Stokes_sources=Stokes_sources*norm
;        Stokes_dirty=Stokes_dirty*norm
;        Stokes_mrc=Stokes_mrc*norm+Stokes_sources
;        Stokes_smooth=Stokes_smooth*norm
        
        file_path_img=rootdir('mwa')+output_dir+'Composite_Stokes_'+Stk_nm[stk_i]+'_hpx_residual'
        file_path_wts=rootdir('mwa')+output_dir+'Composite_Stokes_'+Stk_nm[stk_i]+'_hpx_weights'
        file_path_rst=rootdir('mwa')+output_dir+'Composite_Stokes_'+Stk_nm[stk_i]+'_hpx_restored'
        file_path_src=rootdir('mwa')+output_dir+'Composite_Stokes_'+Stk_nm[stk_i]+'_hpx_sources'
        file_path_dty=rootdir('mwa')+output_dir+'Composite_Stokes_'+Stk_nm[stk_i]+'_hpx_dirty'
;        file_path_mrc=rootdir('mwa')+output_dir+'Composite_Stokes_'+Stk_nm[stk_i]+'_hpx_MRCrings'
        file_path_smt=rootdir('mwa')+output_dir+'Composite_Stokes_'+Stk_nm[stk_i]+'_hpx_smooth'
        
        write_fits_cut4,file_path_img+'.fits',*Stokes_inds[stk_i],*Stokes_images[stk_i],/ring,Coords='C',nside=nside
        write_fits_cut4,file_path_wts+'.fits',*Stokes_inds[stk_i],*Stokes_weights[stk_i],/ring,Coords='C',nside=nside
        
        write_fits_cut4,file_path_rst+'.fits',*Stokes_inds[stk_i],Stokes_restored,/ring,Coords='C',nside=nside
        write_fits_cut4,file_path_src+'.fits',*Stokes_inds[stk_i],Stokes_sources,/ring,Coords='C',nside=nside
        write_fits_cut4,file_path_dty+'.fits',*Stokes_inds[stk_i],Stokes_dirty,/ring,Coords='C',nside=nside
        
;        write_fits_cut4,file_path_mrc+'.fits',*Stokes_inds[stk_i],Stokes_mrc,/ring,Coords='C',nside=nside
        write_fits_cut4,file_path_smt+'.fits',*Stokes_inds[stk_i],Stokes_smooth,/ring,Coords='C',nside=nside
    ENDFOR    
ENDIF

FOR stk_i=0,1 DO BEGIN
    CASE stk_i OF
        0:cnorm=1.
        1:cnorm=fraction_polarized
    ENDCASE
    
    file_path_img=rootdir('mwa')+output_dir+'Composite_Stokes_'+Stk_nm[stk_i]+'_hpx_residual'
    file_path_wts=rootdir('mwa')+output_dir+'Composite_Stokes_'+Stk_nm[stk_i]+'_hpx_weights'
    file_path_rst=rootdir('mwa')+output_dir+'Composite_Stokes_'+Stk_nm[stk_i]+'_hpx_restored'
    file_path_src=rootdir('mwa')+output_dir+'Composite_Stokes_'+Stk_nm[stk_i]+'_hpx_sources'
    file_path_dty=rootdir('mwa')+output_dir+'Composite_Stokes_'+Stk_nm[stk_i]+'_hpx_dirty'
;    file_path_mrc=rootdir('mwa')+output_dir+'Composite_Stokes_'+Stk_nm[stk_i]+'_hpx_MRCrings'
    file_path_smt=rootdir('mwa')+output_dir+'Composite_Stokes_'+Stk_nm[stk_i]+'_hpx_smooth'
        
    title_img='Composite Stokes '+Stk_nm[stk_i]+' residual'
    title_wts='Composite Stokes '+Stk_nm[stk_i]+' weights'
    healpix_image,file_path_img,moll=1,cart=0,gnom=0,orth=1,ps_write=0,png_write=1,silent=1,$
        title=title_img,lon=lon_avg,lat=Median(lat_arr),min=low_residual*cnorm,max=high_residual*cnorm,/half,color_table=color_table
    healpix_image,file_path_wts,moll=1,cart=0,gnom=0,orth=1,ps_write=0,png_write=1,silent=1,$
        title=title_wts,lon=lon_avg,lat=Median(lat_arr),/half,color_table=color_table
        
    title_rst='Composite Stokes '+Stk_nm[stk_i]+' restored'
    title_src='Composite Stokes '+Stk_nm[stk_i]+' sources'
    title_dty='Composite Stokes '+Stk_nm[stk_i]+' dirty'
    
;    title_mrc='Composite Stokes '+Stk_nm[stk_i]+' MRC rings'
    title_smt='Composite Stokes '+Stk_nm[stk_i]+' smooth'
    healpix_image,file_path_rst,moll=1,cart=0,gnom=0,orth=1,ps_write=0,png_write=1,silent=1,$
        title=title_rst,lon=lon_avg,lat=Median(lat_arr),min=low_dirty*cnorm,max=high_dirty*cnorm,/half,color_table=color_table
    healpix_image,file_path_src,moll=1,cart=0,gnom=0,orth=1,ps_write=0,png_write=1,silent=1,$
        title=title_src,lon=lon_avg,lat=Median(lat_arr),min=0,max=high_dirty*cnorm,/half,color_table=color_table
    healpix_image,file_path_dty,moll=1,cart=0,gnom=0,orth=1,ps_write=0,png_write=1,silent=1,$
        title=title_dty,lon=lon_avg,lat=Median(lat_arr),min=low_dirty*cnorm,max=high_dirty*cnorm,/half,color_table=color_table
        
;    healpix_image,file_path_mrc,moll=1,cart=0,gnom=0,orth=1,ps_write=0,png_write=1,silent=1,$
;        title=title_mrc,lon=lon_avg,lat=Median(lat_arr),min=0,max=high_residual*cnorm,/half,color_table=-color_table
        
    healpix_image,file_path_smt,moll=1,cart=0,gnom=0,orth=1,ps_write=0,png_write=1,silent=1,$
        title=title_smt,lon=lon_avg,lat=Median(lat_arr),min=low_residual*cnorm/2.,max=high_residual*cnorm/2.,/half,color_table=color_table
ENDFOR

END
