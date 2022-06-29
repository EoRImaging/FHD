PRO fhd_quickview,obs,status_str,psf,cal,jones,skymodel,fhd_params,image_uv_arr=image_uv_arr,weights_arr=weights_arr,$
    model_uv_arr=model_uv_arr,file_path_fhd=file_path_fhd,silent=silent,show_grid=show_grid,$
    gridline_image_show=gridline_image_show,pad_uv_image=pad_uv_image,image_filter_fn=image_filter_fn,$
    grid_spacing=grid_spacing,reverse_image=reverse_image,show_obsname=show_obsname,mark_zenith=mark_zenith,$
    no_fits=no_fits,no_png=no_png,ring_radius=ring_radius,zoom_low=zoom_low,zoom_high=zoom_high,zoom_radius=zoom_radius,$
    instr_low=instr_low,instr_high=instr_high,stokes_low=stokes_low,stokes_high=stokes_high,$
    use_pointing_center=use_pointing_center,galaxy_model_fit=galaxy_model_fit,beam_arr=beam_arr,$
    allow_sidelobe_image_output=allow_sidelobe_image_output,beam_output_threshold=beam_output_threshold,beam_threshold=beam_threshold,$
    beam_diff_image=beam_diff_image,output_residual_histogram=output_residual_histogram,show_beam_contour=show_beam_contour,$
    image_mask_horizon=image_mask_horizon,write_healpix_fits=write_healpix_fits,nside=nside,$
    model_recalculate=model_recalculate,map_fn_arr=map_fn_arr,$
    min_restored_beam_width=min_restored_beam_width, max_restored_beam_width=max_restored_beam_width,_Extra=extra
t0=Systime(1)

basename=file_basename(file_path_fhd)
dirpath=file_dirname(file_path_fhd)
IF not Keyword_Set(silent) THEN print,'Exporting (quickview): ',basename
output_path=filepath(basename,root=dirpath,sub='output_data')
output_dir=file_dirname(output_path)

image_path=filepath(basename,root=dirpath,sub='output_images')
image_dir=file_dirname(image_path)
IF file_test(image_dir) EQ 0 THEN file_mkdir,image_dir
IF file_test(output_dir) EQ 0 THEN file_mkdir,output_dir
IF Keyword_Set(show_obsname) OR (N_Elements(show_obsname) EQ 0) THEN title_fhd=basename
IF N_Elements(show_grid) EQ 0 THEN show_grid=1
IF N_Elements(beam_threshold) EQ 0 THEN beam_threshold=0.05
IF N_Elements(beam_output_threshold) EQ 0 THEN beam_output_threshold=beam_threshold/2.
IF N_Elements(image_mask_horizon) EQ 0 THEN image_mask_horizon=1
IF status_str.fhd EQ 0 THEN model_recalculate=0
IF ~keyword_set(image_filter_fn) THEN BEGIN
  print, 'WARNING: image_filter_fn not set. Using "filter_uv_uniform."'
  image_filter_fn='filter_uv_uniform'
ENDIF

grid_spacing=10.
offset_lat=grid_spacing/2;15. paper 10 memo
offset_lon=grid_spacing/2.;15. paper 10 memo
reverse_image=1   ;1: reverse x axis, 2: y-axis, 3: reverse both x and y axes
map_reverse=reverse_image;1 paper 3 memo
label_spacing=1.

IF N_Elements(obs) EQ 0 THEN fhd_save_io,status_str,obs,var='obs',/restore,file_path_fhd=file_path_fhd,_Extra=extra
IF N_Elements(psf) EQ 0 THEN fhd_save_io,status_str,psf,var='psf',/restore,file_path_fhd=file_path_fhd,_Extra=extra
IF N_Elements(cal) EQ 0 THEN fhd_save_io,status_str,cal,var='cal',/restore,file_path_fhd=file_path_fhd,_Extra=extra
IF N_Elements(jones) EQ 0 THEN fhd_save_io,status_str,jones,var='jones',/restore,file_path_fhd=file_path_fhd,_Extra=extra
IF N_Elements(skymodel) EQ 0 THEN fhd_save_io,status_str,skymodel,var='skymodel',/restore,file_path_fhd=file_path_fhd,_Extra=extra

n_pol=obs.n_pol
dimension_uv=obs.dimension
pol_names=obs.pol_names
residual_flag=obs.residual
IF N_Elements(galaxy_model_fit) EQ 0 THEN galaxy_model_fit=0

IF N_Elements(image_uv_arr) EQ 0 THEN BEGIN
    image_uv_arr=Ptrarr(n_pol,/allocate)
    FOR pol_i=0,n_pol-1 DO BEGIN
        fhd_save_io,status_str,grid_uv,var='grid_uv',/restore,file_path_fhd=file_path_fhd,obs=obs,pol_i=pol_i,_Extra=extra
        *image_uv_arr[pol_i]=grid_uv
    ENDFOR
ENDIF

weights_flag=1
IF N_Elements(weights_arr) EQ 0 THEN BEGIN
    weights_arr=Ptrarr(n_pol,/allocate)
    FOR pol_i=0,n_pol-1 DO BEGIN
        fhd_save_io,status_str,weights_uv,var='weights_uv',/restore,file_path_fhd=file_path_fhd,obs=obs,pol_i=pol_i,_Extra=extra
        *weights_arr[pol_i]=weights_uv
    ENDFOR
ENDIF
IF Min(Ptr_valid(weights_arr)) EQ 0 THEN BEGIN
    FOR pol_i=0,n_pol-1 DO weights_arr[pol_i]=Ptr_new(Abs(*image_uv_arr[pol_i]))
    weights_flag=0
ENDIF
FOR pol_i=0,n_pol-1 DO IF Total(Abs(*weights_arr[pol_i])) EQ 0 THEN BEGIN
    weights_arr[pol_i]=Ptr_new(Abs(*image_uv_arr[pol_i]))
    weights_flag=0
ENDIF

model_flag=Keyword_Set(skymodel)
source_flag = Keyword_Set(skymodel) ? Keyword_Set(skymodel.n_sources):0
IF N_Elements(model_uv_arr) EQ 0 THEN BEGIN
    IF Min(status_str.grid_uv_model[0:n_pol-1]) GT 0 THEN BEGIN
        model_uv_arr=Ptrarr(n_pol,/allocate)
        FOR pol_i=0,n_pol-1 DO BEGIN
            fhd_save_io,status_str,grid_uv_model,var='grid_uv_model',/restore,file_path_fhd=file_path_fhd,obs=obs,pol_i=pol_i,_Extra=extra
            *model_uv_arr[pol_i]=grid_uv_model
        ENDFOR
    ENDIF ELSE IF status_str.fhd GT 0 THEN BEGIN
        fhd_save_io,var='fhd',file_path_fhd=file_path_fhd,path_use=fhd_sav_filepath,/no_save,_Extra=extra
        model_uv_arr=getvar_savefile(fhd_sav_filepath+'.sav','model_uv_holo')
    ENDIF ELSE model_flag=0
ENDIF

IF residual_flag THEN model_flag=0

IF Keyword_Set(image_filter_fn) THEN BEGIN
    dummy_img=Call_function(image_filter_fn,fltarr(2,2),name=filter_name,/return_name_only,_Extra=extra)
    IF Keyword_Set(filter_name) THEN filter_name='_'+filter_name ELSE filter_name=''
ENDIF ELSE filter_name=''

IF Keyword_Set(pad_uv_image) THEN obs_out=fhd_struct_update_obs(obs,dimension=obs.dimension*pad_uv_image,kbin=obs.kpix) $
    ELSE obs_out=obs

IF N_Elements(min_restored_beam_width) EQ 0 THEN min_restored_beam_width=0.75
IF N_Elements(max_restored_beam_width) EQ 0 THEN max_restored_beam_width=5.
restored_beam_width=beam_width_calculate(obs_out,min_restored_beam_width=min_restored_beam_width, max_restored_beam_width=max_restored_beam_width)
dimension=obs_out.dimension
elements=obs_out.elements
degpix=obs_out.degpix
astr_out=obs_out.astr

horizon_mask=fltarr(dimension,elements)+1.
;IF Keyword_Set(image_mask_horizon) THEN BEGIN
    ;set /ignore_refraction for speed since we're just finding pixels to mask
    apply_astrometry, obs_out, x_arr=meshgrid(dimension,elements,1), y_arr=meshgrid(dimension,elements,2), ra_arr=ra_arr, dec_arr=dec_arr, /xy2ad, /ignore_refraction
    horizon_test=where(Finite(ra_arr,/nan),n_horizon_mask)
    IF n_horizon_mask GT 0 THEN horizon_mask[horizon_test]=0
;ENDIF

beam_mask=fltarr(dimension,elements)+1
beam_avg=fltarr(dimension,elements)
beam_base_out=Ptrarr(n_pol,/allocate)
beam_correction_out=Ptrarr(n_pol,/allocate)
IF N_Elements(beam_arr) EQ 0 THEN BEGIN
    beam_arr=Ptrarr(n_pol,/allocate)
    FOR pol_i=0,n_pol-1 DO *beam_arr[pol_i]=beam_image(psf,obs,pol_i=pol_i,square=0)
ENDIF
FOR pol_i=0,n_pol-1 DO BEGIN
    *beam_base_out[pol_i]=Rebin(*beam_arr[pol_i],dimension,elements)*horizon_mask ;should be fine even if pad_uv_image is not set
    *beam_correction_out[pol_i]=weight_invert(*beam_base_out[pol_i],1e-3)
    IF pol_i GT 1 THEN CONTINUE
    beam_mask_test=*beam_base_out[pol_i]
    IF Keyword_Set(allow_sidelobe_image_output) THEN beam_i=where(beam_mask_test GE beam_output_threshold) ELSE $
        beam_i=region_grow(beam_mask_test,dimension/2.+dimension*elements/2.,threshold=[beam_output_threshold,Max(beam_mask_test)])
    beam_mask0=fltarr(dimension,elements) & beam_mask0[beam_i]=1.
    beam_avg+=*beam_base_out[pol_i]^2.
    beam_mask*=beam_mask0
ENDFOR
;beam_mask[0:dimension/4.-1,*]=0 & beam_mask[3.*dimension/4.:dimension-1,*]=0 
;beam_mask[*,0:elements/4.-1]=0 & beam_mask[*,3.*elements/4.:elements-1]=0 
beam_avg/=(n_pol<2)
beam_avg=Sqrt(beam_avg>0)*beam_mask
beam_i=where(beam_mask)
jones_out=fhd_struct_init_jones(obs_out,status_str,jones,file_path_fhd=file_path_fhd,mask=beam_mask,/update)

IF Keyword_Set(model_recalculate) THEN BEGIN
    IF N_Elements(map_fn_arr) EQ 0 THEN map_fn_arr=Ptrarr(n_pol)
    FOR pol_i=0,n_pol-1 DO BEGIN
        IF Ptr_valid(map_fn_arr[pol_i]) EQ 0 THEN BEGIN
            fhd_save_io,status_str,map_fn,var='map_fn',file_path_fhd=file_path_fhd,pol_i=pol_i,$
                transfer=transfer_mapfn,/no_save,path_use=path_use,obs=obs,_Extra=extra
            IF file_test(path_use+'.sav') EQ 0 THEN BEGIN
                print,'No mapping function supplied, and .sav files not found! Model not recalculated'
                print,path_use+'.sav'
                model_recalculate=0
                pol_i=n_pol ;skip any remaining polarizations if even one is missing
                CONTINUE
            ENDIF
            RESTORE,path_use+'.sav' ;map_fn
            map_fn_arr[pol_i]=Ptr_new(map_fn,/no_copy)
        ENDIF
    ENDFOR
ENDIF

IF Keyword_Set(model_recalculate) THEN IF model_recalculate GT 0 THEN BEGIN
    ;set model_recalculate=-1 to force the map_fn to be restored if the file exists, but not actually recalculate the point source model
    uv_mask=fltarr(dimension,elements)
    IF model_flag EQ 0 THEN BEGIN
        model_uv_arr=Ptrarr(n_pol,/allocate)
        model_flag=1
    ENDIF
    fhd_save_io,var='fhd',file_path_fhd=file_path_fhd,path_use=fhd_sav_filepath,/no_save,_Extra=extra
    IF N_Elements(fhd_params) EQ 0 THEN fhd_save_io,status_str,fhd_params,var='fhd_params',/restore,file_path_fhd=file_path_fhd,_Extra=extra
    component_array=getvar_savefile(fhd_sav_filepath+'.sav','component_array')
    FOR pol_i=0,n_pol-1 DO uv_mask[where(*weights_arr[pol_i])]=1
    noise_map=fhd_params.convergence*rebin(weight_invert(beam_avg),dimension,elements)
    component_array=component_array[0:fhd_params.n_components-1]
    source_array=Components2Sources(component_array,obs,fhd_params,noise_map=noise_map,source_mask=source_mask,_Extra=extra)
    IF Keyword_Set(no_condense_sources) THEN $
        model_uv_full=source_dft_model(obs,jones,component_array,t_model=t_model,uv_mask=uv_mask,sigma_threshold=0,_extra=extra) $
        ELSE model_uv_full=source_dft_model(obs,jones,source_array,t_model=t_model,uv_mask=uv_mask,sigma_threshold=fhd_params.sigma_cut,_extra=extra)
    FOR pol_i=0,n_pol-1 DO BEGIN
        *model_uv_arr[pol_i]=holo_mapfn_apply(*model_uv_full[pol_i],map_fn_arr[pol_i],_Extra=extra,/indexed)
    ENDFOR
ENDIF
heap_gc

IF Keyword_Set(write_healpix_fits) THEN BEGIN
    FoV_use=!RaDeg/obs_out.kpix
    hpx_cnv=healpix_cnv_generate(obs_out,file_path_fhd=file_path_fhd,nside=nside,restore_last=0,/no_save,$
        mask=beam_mask,hpx_radius=FoV_use/sqrt(2.),restrict_hpx_inds=restrict_hpx_inds,_Extra=extra)
    ring2nest, nside, hpx_cnv.inds, hpx_inds_nest ;external programs are much happier reading in Healpix fits files with the nested pixel ordering
ENDIF

source_flag=0 
IF model_flag THEN IF skymodel.n_sources GT 0 THEN BEGIN
    source_flag=1
    source_array=skymodel.source_list
    source_arr_out=source_array
    
    apply_astrometry, obs_out, ra_arr=source_array.ra, dec_arr=source_array.dec, x_arr=sx, y_arr=sy, /ad2xy
    source_arr_out.x=sx & source_arr_out.y=sy
    
    extend_test=where(Ptr_valid(source_arr_out.extend),n_extend)
    IF n_extend GT 0 THEN BEGIN
        FOR ext_i=0L,n_extend-1 DO BEGIN
            component_array_out=*source_array[extend_test[ext_i]].extend
            apply_astrometry, obs_out, ra_arr=component_array_out.ra, dec_arr=component_array_out.dec, x_arr=cx, y_arr=cy, /ad2xy
            component_array_out.x=cx & component_array_out.y=cy
            
            IF Total(component_array_out.flux.(0)) EQ 0 THEN BEGIN
                component_array_out.flux.(0)=(*beam_base_out[0])[component_array_out.x,component_array_out.y]*(component_array_out.flux.I+component_array_out.flux.Q)/2.
                component_array_out.flux.(1)=(*beam_base_out[1])[component_array_out.x,component_array_out.y]*(component_array_out.flux.I-component_array_out.flux.Q)/2.
        ;        component_array_out.flux.(2)=(*beam_base_out[2])[component_array_out.x,component_array_out.y]*(component_array_out.flux.Q+component_array_out.flux.U)/2.
        ;        component_array_out.flux.(3)=(*beam_base_out[3])[component_array_out.x,component_array_out.y]*(component_array_out.flux.Q-component_array_out.flux.U)/2.
            ENDIF
            source_arr_out[extend_test[ext_i]].extend=Ptr_new(/allocate)
            *source_arr_out[extend_test[ext_i]].extend=component_array_out
        ENDFOR
    ENDIF
    source_arr_out=stokes_cnv(source_arr_out,jones_out,beam=beam_base_out,/inverse,_Extra=extra)
ENDIF
IF model_flag THEN instr_model_arr=Ptrarr(n_pol)

cal_source_flag=0
IF size(cal, /type) EQ 8 THEN IF cal.skymodel.n_sources GT 0 THEN BEGIN
    cal_source_flag = 1
    cal_source_array = cal.skymodel.source_list
    cal_source_arr_out=cal_source_array
    
    apply_astrometry, obs_out, ra_arr=cal_source_array.ra, dec_arr=cal_source_array.dec, x_arr=sx, y_arr=sy, /ad2xy
    cal_source_arr_out.x=sx & cal_source_arr_out.y=sy

    extend_test=where(Ptr_valid(cal_source_arr_out.extend),n_extend)
    IF n_extend GT 0 THEN BEGIN
        FOR ext_i=0L,n_extend-1 DO BEGIN
            component_array_out=*cal_source_array[extend_test[ext_i]].extend
            apply_astrometry, obs_out, ra_arr=component_array_out.ra, dec_arr=component_array_out.dec, x_arr=cx, y_arr=cy, /ad2xy
            component_array_out.x=cx & component_array_out.y=cy
            
            cal_source_arr_out[extend_test[ext_i]].extend=Ptr_new(/allocate)
            *cal_source_arr_out[extend_test[ext_i]].extend=component_array_out
        ENDFOR
    ENDIF
ENDIF

gal_model_img=Ptrarr(n_pol)
IF Keyword_Set(galaxy_model_fit) THEN BEGIN
    gal_model_uv=fhd_galaxy_model(obs,file_path_fhd=file_path_fhd,/uv_return,_Extra=extra)
    
    FOR pol_i=0,n_pol-1 DO gal_model_img[pol_i]=Ptr_new(dirty_image_generate(*gal_model_uv[pol_i],degpix=degpix,/antialias,$
        image_filter_fn='',pad_uv_image=pad_uv_image,beam_ptr=beam_base_out[pol_i],_Extra=extra)*(*beam_base_out[pol_i]))
    
    gal_name='_galfit'
ENDIF ELSE BEGIN
    gal_name=''
ENDELSE

instr_dirty_arr=Ptrarr(n_pol)
instr_sources=Ptrarr(n_pol)
instr_rings=Ptrarr(n_pol)
filter_arr=Ptrarr(n_pol,/allocate) 
FOR pol_i=0,n_pol-1 DO BEGIN
    complex_flag = pol_i GT 1 ? 1 : 0
    instr_dirty_arr[pol_i]=Ptr_new(dirty_image_generate(*image_uv_arr[pol_i],degpix=degpix,weights=*weights_arr[pol_i],/antialias,$
        image_filter_fn=image_filter_fn,pad_uv_image=pad_uv_image,file_path_fhd=file_path_fhd,filter=filter_arr[pol_i],$
        beam_ptr=beam_base_out[pol_i],no_real=complex_flag,_Extra=extra))
    IF model_flag THEN instr_model_arr[pol_i]=Ptr_new(dirty_image_generate(*model_uv_arr[pol_i],degpix=degpix,weights=*weights_arr[pol_i],/antialias,$
        image_filter_fn=image_filter_fn,pad_uv_image=pad_uv_image,file_path_fhd=file_path_fhd,filter=filter_arr[pol_i],$
        beam_ptr=beam_base_out[pol_i],no_real=complex_flag,_Extra=extra))
    IF source_flag THEN BEGIN
        IF Keyword_Set(ring_radius) THEN instr_rings[pol_i]=Ptr_new(source_image_generate(source_arr_out,obs_out,pol_i=pol_i,resolution=16,$
            dimension=dimension,restored_beam_width=restored_beam_width,ring_radius=ring_radius,_Extra=extra))
        instr_sources[pol_i]=Ptr_new(source_image_generate(source_arr_out,obs_out,pol_i=pol_i,resolution=16,$
            dimension=dimension,restored_beam_width=restored_beam_width,_Extra=extra))
    ENDIF
ENDFOR

IF Keyword_Set(cal_source_flag) THEN cal_sources = source_image_generate(cal_source_arr_out,obs_out,pol_i=4,resolution=16,$
    dimension=dimension,restored_beam_width=restored_beam_width,_Extra=extra)

renorm_factor = get_image_renormalization(obs_out,weights_arr=weights_arr,beam_base=beam_base_out,filter_arr=filter_arr,$
  image_filter_fn=image_filter_fn,pad_uv_image=pad_uv_image,degpix=degpix,/antialias)
for pol_i=0,n_pol-1 do begin
  *instr_dirty_arr[pol_i]*=renorm_factor[pol_i]
  IF model_flag THEN *instr_model_arr[pol_i]*=renorm_factor[pol_i]
  IF source_flag THEN BEGIN
    *instr_sources[pol_i]*=renorm_factor[pol_i]
    IF Keyword_Set(ring_radius) THEN *instr_rings[pol_i]*=renorm_factor[pol_i]
  ENDIF
endfor

stokes_dirty_arr=stokes_cnv(instr_dirty_arr,jones_out,beam=beam_base_out,/square,_Extra=extra)
IF model_flag THEN BEGIN
    instr_residual_arr=Ptrarr(n_pol,/allocate)
    FOR pol_i=0,n_pol-1 DO *instr_residual_arr[pol_i]=*instr_dirty_arr[pol_i]-*instr_model_arr[pol_i]
    stokes_residual_arr=stokes_cnv(instr_residual_arr,jones_out,beam=beam_base_out,/square,_Extra=extra)
    stokes_model_arr=stokes_cnv(instr_model_arr,jones_out,beam=beam_base_out,/square,_Extra=extra)
ENDIF ELSE BEGIN
    instr_residual_arr=instr_dirty_arr
    stokes_residual_arr=stokes_dirty_arr
ENDELSE

weights_use = Pointer_copy(weights_arr)
; The cross-polarization XY and YX images are both complex, but are conjugate mirrors of each other
; To make images of these, we simply take the real and imaginary parts separately
crosspol_split_real_imaginary, instr_dirty_arr, pol_names=pol_names
crosspol_split_real_imaginary, instr_model_arr
crosspol_split_real_imaginary, instr_residual_arr
crosspol_split_real_imaginary, weights_use

IF source_flag THEN BEGIN
    stokes_sources=stokes_cnv(instr_sources,jones_out,beam=beam_base_out,_Extra=extra) ;returns null pointer if instr_sources is a null pointer 
    IF Keyword_Set(ring_radius) THEN stokes_rings=stokes_cnv(instr_rings,jones_out,beam=beam_base_out,_Extra=extra) 
ENDIF    

IF source_flag THEN source_array_export,source_arr_out,obs_out,beam=beam_avg,stokes_images=stokes_residual_arr,file_path=output_path+'_source_list'

; plot calibration solutions, export to png if allowed
IF ~Keyword_Set(no_png) THEN BEGIN
    IF size(cal,/type) EQ 8 THEN BEGIN
        IF cal.skymodel.n_sources GT 0 THEN BEGIN
            IF file_test(file_path_fhd+'_cal_hist.sav') THEN BEGIN
                vis_baseline_hist=getvar_savefile(file_path_fhd+'_cal_hist.sav','vis_baseline_hist')
                plot_cals,cal,obs,file_path_base=image_path,vis_baseline_hist=vis_baseline_hist
            ENDIF ELSE BEGIN
                plot_cals,cal,obs,file_path_base=image_path,_Extra=extra
            ENDELSE
        ENDIF ELSE plot_cals,cal,obs,file_path_base=image_path,_Extra=extra
    ENDIF
ENDIF

;Build a fits header
mkhdr,fits_header,*instr_dirty_arr[0]
putast, fits_header, astr_out;, cd_type=1

fits_header_Jy=fits_header
sxaddpar,fits_header_Jy,'BUNIT','Jy/sr'

fits_header_apparent=fits_header
sxaddpar,fits_header_apparent,'BUNIT','Jy/sr (apparent)'

mkhdr,fits_header_uv,Abs(*weights_arr[0])
sxaddpar,fits_header_uv,'CD1_1',obs.kpix,'Wavelengths / Pixel'
sxaddpar,fits_header_uv,'CD2_1',0.,'Wavelengths / Pixel'
sxaddpar,fits_header_uv,'CD1_2',0.,'Wavelengths / Pixel'
sxaddpar,fits_header_uv,'CD2_2',obs.kpix,'Wavelengths / Pixel'
sxaddpar,fits_header_uv,'CRPIX1',dimension/2+1,'Reference Pixel in X'
sxaddpar,fits_header_uv,'CRPIX2',elements/2+1,'Reference Pixel in Y'
sxaddpar,fits_header_uv,'CRVAL1',0.,'Wavelengths (u)'
sxaddpar,fits_header_uv,'CRVAL2',0.,'Wavelengths (v)'
sxaddpar,fits_header_uv,'MJD-OBS',astr_out.MJDOBS,'Modified Julian day of observation'
sxaddpar,fits_header_uv,'DATE-OBS',astr_out.DATEOBS,'Date of observation'

x_inc=beam_i mod dimension
y_inc=Floor(beam_i/dimension)
IF N_Elements(zoom_radius) GT 0 THEN BEGIN
    zoom_radius_use=zoom_radius<dimension
    zoom_low=dimension/2-zoom_radius_use/2
    zoom_high=dimension/2+zoom_radius_use/2-1
ENDIF
IF N_Elements(zoom_low) EQ 0 THEN zoom_low=min(x_inc)<min(y_inc)
IF N_Elements(zoom_high) EQ 0 THEN zoom_high=max(x_inc)>max(y_inc)
astr_out2=astr_out
astr_out2.crpix-=zoom_low
astr_out2.naxis=[zoom_high-zoom_low+1,zoom_high-zoom_low+1]

beam_contour_arr=Ptrarr(n_pol)
beam_contour_arr2=Ptrarr(n_pol)
beam_contour_stokes=Ptr_new()
IF Keyword_Set(show_beam_contour) THEN BEGIN
    FOR pol_i=0,n_pol-1 DO BEGIN
        IF Keyword_Set(gridline_image_show) THEN beam_contour_arr2[pol_i]=Ptr_new((*beam_base_out[pol_i])[zoom_low:zoom_high,zoom_low:zoom_high]) $
            ELSE beam_contour_arr[pol_i]=Ptr_new((*beam_base_out[pol_i])[zoom_low:zoom_high,zoom_low:zoom_high])
    ENDFOR
    beam_contour_stokes=Ptr_new(beam_avg[zoom_low:zoom_high,zoom_low:zoom_high])
ENDIF 

IF Keyword_Set(beam_diff_image) AND Keyword_Set(source_flag) THEN BEGIN
    source_res_arr=source_residual_image(obs_out,source_arr_out,instr_residual_arr,beam_arr=beam_base_out,$
        jones=jones_out,source_residual_flux_threshold=1.,beam_power=2,_Extra=extra)
    source_res_stks=stokes_cnv(source_res_arr,jones_out,_Extra=extra)
    beam_diff_low_use=0
    beam_diff_high_use=0
    FOR pol_i=0,n_pol-1 DO beam_diff_low_use=beam_diff_low_use<((Median((*source_res_arr[pol_i])[beam_i])-3.*Stddev((*source_res_arr[pol_i])[beam_i]))>Min((*source_res_arr[pol_i])[beam_i]))
    FOR pol_i=0,n_pol-1 DO beam_diff_high_use=beam_diff_high_use>((Median((*source_res_arr[pol_i])[beam_i])+3.*Stddev((*source_res_arr[pol_i])[beam_i]))<Max((*source_res_arr[pol_i])[beam_i]))
    IF N_Elements(beam_diff_low) GT 0 THEN beam_diff_low_use=beam_diff_low
    IF N_Elements(beam_diff_high) GT 0 THEN beam_diff_high_use=beam_diff_high
    
    mark_thick=1.
    mark_length=6.
    IF Keyword_Set(mark_zenith) AND (Floor(obs_out.zenx) GT mark_length) AND (Floor(obs_out.zenx) LT dimension-mark_length) $
        AND (Floor(obs_out.zeny) GT mark_length) AND (Floor(obs_out.zeny) LT elements-mark_length) THEN BEGIN 
        mark_image=fltarr(dimension,elements)
        mark_amp=beam_diff_low_use
        mark_image[Floor(obs_out.zenx)-mark_length:Floor(obs_out.zenx)+mark_length,Floor(obs_out.zeny)-mark_thick:Floor(obs_out.zeny)+mark_thick]=mark_amp
        mark_image[Floor(obs_out.zenx)-mark_thick:Floor(obs_out.zenx)+mark_thick,Floor(obs_out.zeny)-mark_length:Floor(obs_out.zeny)+mark_length]=mark_amp
        mark_image=mark_image[zoom_low:zoom_high,zoom_low:zoom_high]
    ENDIF ELSE mark_image=0.
    FOR pol_i=0,n_pol-1 DO BEGIN
        IF ~Keyword_Set(no_png) THEN BEGIN
            Imagefast,(*source_res_arr[pol_i])[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path+'_Beam_diff_'+pol_names[pol_i],$
                /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,show_grid=show_grid,$
                low=beam_diff_low_use,high=beam_diff_high_use,title=title_fhd,astr=astr_out2,contour_image=beam_contour_arr[pol_i],_Extra=extra
            Imagefast,(*source_res_stks[pol_i])[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path+'_Beam_diff_'+pol_names[pol_i+4],$
                /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,show_grid=show_grid,$
                low=beam_diff_low_use*2.,high=beam_diff_high_use*2.,title=title_fhd,astr=astr_out2,_Extra=extra
        ENDIF
        IF ~Keyword_Set(no_fits) THEN FitsFast,*source_res_arr[pol_i],fits_header,/write,file_path=output_path+'_Beam_diff_'+pol_names[pol_i]
    ENDFOR
ENDIF

IF (residual_flag EQ 0) AND (model_flag EQ 0) THEN res_name='_Dirty_' ELSE res_name='_Residual_'

FOR pol_i=0,n_pol-1 DO BEGIN
    instr_residual=*instr_residual_arr[pol_i]*(*beam_correction_out[pol_i])
    instr_dirty=*instr_dirty_arr[pol_i]*(*beam_correction_out[pol_i])
    IF model_flag THEN BEGIN
        instr_model=*instr_model_arr[pol_i]*(*beam_correction_out[pol_i])
        stokes_model=(*stokes_model_arr[pol_i])*beam_mask
        stokes_dirty=(*stokes_dirty_arr[pol_i])*beam_mask
    ENDIF
    stokes_residual=(*stokes_residual_arr[pol_i])*beam_mask
    IF source_flag THEN BEGIN
        instr_source=*instr_sources[pol_i]
        instr_restored=instr_residual+(Keyword_Set(ring_radius) ? *instr_rings[pol_i]:instr_source)
        stokes_source=(*stokes_sources[pol_i])
        stokes_restored=stokes_residual+(Keyword_Set(ring_radius) ? *stokes_rings[0]:stokes_source) ;use stokes I sources only if using rings
    ENDIF
    beam_use=*beam_base_out[pol_i]
    
    IF N_Elements(instr_low) EQ 0 THEN instr_low_use=Min(instr_residual[beam_i])>(-5.*Stddev(instr_residual[beam_i])) ELSE instr_low_use=instr_low
    IF N_Elements(instr_high) EQ 0 THEN instr_high_use=Max(instr_residual[beam_i])<(10.*Stddev(instr_residual[beam_i])) ELSE instr_high_use=instr_high
    
    instr_low_use=instr_low_use>(-instr_high_use)    
    IF N_Elements(stokes_low) EQ 0 THEN stokes_low_use=Min((stokes_residual*Sqrt(beam_avg>0))[beam_i])>(-5.*Stddev((stokes_residual*Sqrt(beam_avg>0))[beam_i])) $ 
        ELSE stokes_low_use=stokes_low
    IF N_Elements(stokes_high) EQ 0 THEN stokes_high_use=Max((stokes_residual*Sqrt(beam_avg>0))[beam_i])<(10.*Stddev((stokes_residual*Sqrt(beam_avg>0))[beam_i])) $
        ELSE stokes_high_use=stokes_high 
    stokes_low_use=stokes_low_use>(-stokes_high_use)
    log_dirty=0
    log_source=1
    
    mark_image=0
    mark_thick=1.
    mark_length=6.
    IF Keyword_Set(mark_zenith) AND (Floor(obs_out.zenx) GT mark_length) AND (Floor(obs_out.zenx) LT dimension-mark_length) $
        AND (Floor(obs_out.zeny) GT mark_length) AND (Floor(obs_out.zeny) LT elements-mark_length) THEN BEGIN 
        mark_image=fltarr(dimension,elements)
        mark_amp=(stokes_low_use<instr_low_use<(-100.))
        mark_image[Floor(obs_out.zenx)-mark_length:Floor(obs_out.zenx)+mark_length,Floor(obs_out.zeny)-mark_thick:Floor(obs_out.zeny)+mark_thick]=mark_amp
        mark_image[Floor(obs_out.zenx)-mark_thick:Floor(obs_out.zenx)+mark_thick,Floor(obs_out.zeny)-mark_length:Floor(obs_out.zeny)+mark_length]=mark_amp
        mark_image=mark_image[zoom_low:zoom_high,zoom_low:zoom_high]
    ENDIF
    
    IF ~Keyword_Set(no_png) THEN BEGIN
        IF weights_flag THEN Imagefast,Abs(*weights_use[pol_i])*obs.n_vis,file_path=image_path+'_UV_weights_'+pol_names[pol_i],$
            /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,/log,$
            low=Min(Abs(*weights_use[pol_i])*obs.n_vis),high=Max(Abs(*weights_use[pol_i])*obs.n_vis),_Extra=extra
        IF model_flag THEN BEGIN
            Imagefast,instr_dirty[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path+filter_name+'_Dirty_'+pol_names[pol_i],$
                /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,low=instr_low_use,high=instr_high_use,$
                title=title_fhd,show_grid=show_grid,astr=astr_out2,contour_image=beam_contour_arr[pol_i],_Extra=extra
            Imagefast,instr_model[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path+filter_name+'_Model_'+pol_names[pol_i],$
                /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,low=instr_low_use,high=instr_high_use,$
                title=title_fhd,show_grid=show_grid,astr=astr_out2,contour_image=beam_contour_arr[pol_i],_Extra=extra
            Imagefast,stokes_dirty[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path+filter_name+'_Dirty_'+pol_names[pol_i+4],$
                /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,low=stokes_low_use*2,high=stokes_high_use*2,$
                lat_center=obs_out.obsdec,lon_center=obs_out.obsra,rotation=0,grid_spacing=grid_spacing,degpix=degpix,$
                offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=show_grid,$
                title=title_fhd,/sphere,astr=astr_out2,contour_image=beam_contour_stokes,_Extra=extra
            Imagefast,stokes_model[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path+filter_name+'_Model_'+pol_names[pol_i+4],$
                /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,low=stokes_low_use*2,high=stokes_high_use*2,$
                lat_center=obs_out.obsdec,lon_center=obs_out.obsra,rotation=0,grid_spacing=grid_spacing,degpix=degpix,$
                offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=show_grid,$
                title=title_fhd,/sphere,astr=astr_out2,contour_image=beam_contour_stokes,_Extra=extra
        ENDIF
        Imagefast,instr_residual[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path+filter_name+res_name+pol_names[pol_i],$
            /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,low=instr_low_use,high=instr_high_use,$
            title=title_fhd,show_grid=show_grid,astr=astr_out2,contour_image=beam_contour_arr[pol_i],_Extra=extra
        Imagefast,beam_use[zoom_low:zoom_high,zoom_low:zoom_high]*100.+mark_image,file_path=image_path+'_Beam_'+pol_names[pol_i],/log,$
            /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,show_grid=show_grid,$
            low=min(beam_use[zoom_low:zoom_high,zoom_low:zoom_high]*100)>0,high=max(beam_use[zoom_low:zoom_high,zoom_low:zoom_high]*100),$
            title=title_fhd,/invert,astr=astr_out2,contour_image=beam_contour_arr[pol_i],_Extra=extra
        Imagefast,stokes_residual[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path+filter_name+res_name+pol_names[pol_i+4],$
            /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,low=stokes_low_use,high=stokes_high_use,$
            lat_center=obs_out.obsdec,lon_center=obs_out.obsra,rotation=0,grid_spacing=grid_spacing,degpix=degpix,$
            offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=show_grid,$
            title=title_fhd,/sphere,astr=astr_out2,contour_image=beam_contour_stokes,_Extra=extra
        IF Keyword_Set(galaxy_model_fit) THEN BEGIN
            gal_img=*gal_model_img[pol_i]
            gal_low_use=Min(gal_img[beam_i])
            gal_high_use=Max(gal_img[beam_i])
            Imagefast,gal_img[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path+filter_name+'_GalModel_'+pol_names[pol_i],$
                /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log_source,low=gal_low_use,high=gal_high_use,$
                lat_center=obs_out.obsdec,lon_center=obs_out.obsra,rotation=0,grid_spacing=grid_spacing,degpix=degpix,$
                offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=show_grid,$
                title=title_fhd,/sphere,astr=astr_out2,_Extra=extra
        ENDIF
    ENDIF
    IF ~Keyword_Set(no_fits) THEN BEGIN
        FitsFast,stokes_residual,fits_header_Jy,/write,file_path=output_path+filter_name+res_name+pol_names[pol_i+4]
        IF model_flag THEN BEGIN
            FitsFast,instr_dirty,fits_header_apparent,/write,file_path=output_path+filter_name+'_Dirty_'+pol_names[pol_i]
            FitsFast,instr_model,fits_header_apparent,/write,file_path=output_path+filter_name+'_Model_'+pol_names[pol_i]
            FitsFast,stokes_model,fits_header_apparent,/write,file_path=output_path+filter_name+'_Model_'+pol_names[pol_i+4]
            FitsFast,stokes_dirty,fits_header_apparent,/write,file_path=output_path+filter_name+'_Dirty_'+pol_names[pol_i+4]
        ENDIF
        FitsFast,instr_residual,fits_header_apparent,/write,file_path=output_path+filter_name+res_name+pol_names[pol_i]
        FitsFast,beam_use,fits_header,/write,file_path=output_path+'_Beam_'+pol_names[pol_i]
        IF weights_flag THEN FitsFast,Abs(*weights_use[pol_i])*obs.n_vis,fits_header_uv,/write,file_path=output_path+'_UV_weights_'+pol_names[pol_i]
        IF Keyword_Set(galaxy_model_fit) THEN FitsFast,*gal_model_img[pol_i],fits_header_apparent,/write,file_path=output_path+'_GalModel_'+pol_names[pol_i]
    ENDIF
    
    IF pol_i EQ 0 THEN log_source=1 ELSE log_source=0
    IF pol_i EQ 0 THEN log=0 ELSE log=0
    IF source_flag THEN BEGIN
        IF Keyword_Set(ring_radius) THEN restored_name='_Restored_rings_' ELSE restored_name='_Restored_'
        IF ~Keyword_Set(no_fits) THEN BEGIN
    ;        FitsFast,instr_source,fits_header_apparent,/write,file_path=output_path+filter_name+'_Sources_'+pol_names[pol_i]
            FitsFast,instr_residual+instr_source,fits_header_apparent,/write,file_path=output_path+filter_name+restored_name+pol_names[pol_i]
    ;        FitsFast,stokes_source,fits_header_Jy,/write,file_path=output_path+'_Sources_'+pol_names[pol_i+4]
            FitsFast,stokes_residual+stokes_source,fits_header_Jy,/write,file_path=output_path+filter_name+restored_name+pol_names[pol_i+4]
        ENDIF
        IF ~Keyword_Set(no_png) THEN BEGIN
            instrS_high=Max(instr_restored[beam_i])
            stokesS_high=Max((stokes_restored*Sqrt(beam_avg>0))[beam_i])
            IF pol_i EQ 0 THEN Imagefast,stokes_source[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path+filter_name+'_Sources_'+pol_names[pol_i+4],$
                /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log,low=0,high=stokes_high_use,/invert_color,$
                lat_center=obs_out.obsdec,lon_center=obs_out.obsra,rotation=0,grid_spacing=grid_spacing,degpix=degpix,$
                offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=show_grid,$
                title=title_fhd,/sphere,astr=astr_out2,contour_image=beam_contour_stokes,_Extra=extra
            Imagefast,stokes_restored[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path+filter_name+restored_name+pol_names[pol_i+4],$
                /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log,low=stokes_low_use,high=stokes_high_use,$
                lat_center=obs_out.obsdec,lon_center=obs_out.obsra,rotation=0,grid_spacing=grid_spacing,degpix=degpix,$
                offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=show_grid,$
                title=title_fhd,/sphere,astr=astr_out2,contour_image=beam_contour_stokes,_Extra=extra
            
    ;        Imagefast,instr_source[zoom_low:zoom_high,zoom_low:zoom_high],file_path=image_path+filter_name+'_Sources_'+pol_names[pol_i],$
    ;            /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log_source,low=0,high=instr_high_use,/invert_color,contour_image=contour_image,_Extra=extra
            Imagefast,instr_restored[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path+filter_name+restored_name+pol_names[pol_i],$
                /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log_dirty,low=instr_low_use,high=instr_high_use,$
                title=title_fhd,show_grid=show_grid,astr=astr_out2,contour_image=beam_contour_arr[pol_i],_Extra=extra
        ENDIF
    ENDIF
    IF ~Keyword_Set(no_png) AND Keyword_Set(cal_source_flag) AND (pol_i EQ 0) THEN BEGIN
            Imagefast,cal_sources[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path+filter_name+'_Calibration_Sources_'+pol_names[pol_i+4],$
                /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log,low=0,high=stokes_high_use,/invert_color,$
                lat_center=obs_out.obsdec,lon_center=obs_out.obsra,rotation=0,grid_spacing=grid_spacing,degpix=degpix,$
                offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=show_grid,$
                title=title_fhd,/sphere,astr=astr_out2,contour_image=beam_contour_stokes,_Extra=extra
    ENDIF
    
    IF Keyword_Set(write_healpix_fits) THEN BEGIN
        IF pol_i EQ 0 THEN err_map = healpix_cnv_apply(weight_invert(beam_avg),hpx_cnv)
        IF source_flag THEN BEGIN
            healpix_restored_path = output_path+filter_name+restored_name+pol_names[pol_i+4]+'_HEALPix.fits'
            healpix_restored_stokes = healpix_cnv_apply(stokes_restored,hpx_cnv)
            write_fits_cut4,healpix_restored_path,hpx_inds_nest,healpix_restored_stokes,n_obs_hpx,err_map,nside=nside,/nested,coord='C'
            healpix_source_path = output_path+filter_name+'_Sources_'+pol_names[pol_i+4]+'_HEALPix.fits'
            healpix_source_stokes = healpix_cnv_apply(stokes_source,hpx_cnv)
            write_fits_cut4,healpix_source_path,hpx_inds_nest,healpix_source_stokes,n_obs_hpx,err_map,nside=nside,/nested,coord='C'
        ENDIF
        IF model_flag THEN BEGIN
            healpix_model_path = output_path+filter_name+'_Model_'+pol_names[pol_i+4]+'_HEALPix.fits'
            healpix_model_stokes = healpix_cnv_apply(stokes_model,hpx_cnv)
            write_fits_cut4,healpix_model_path,hpx_inds_nest,healpix_model_stokes,n_obs_hpx,err_map,nside=nside,/nested,coord='C'
        ENDIF
        healpix_residual_path = output_path+filter_name+res_name+pol_names[pol_i+4]+'_HEALPix.fits'
        healpix_residual_stokes = healpix_cnv_apply(stokes_residual,hpx_cnv)
        write_fits_cut4,healpix_residual_path,hpx_inds_nest,healpix_residual_stokes,n_obs_hpx,err_map,nside=nside,/nested,coord='C'
    ENDIF
    
    IF Keyword_Set(gridline_image_show) THEN BEGIN
        IF pol_i EQ 0 THEN BEGIN
            Imagefast,fltarr(zoom_high-zoom_low+1,zoom_high-zoom_low+1),file_path=image_path+filter_name+'_Grid',$
                /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log,low=0,high=stokes_high_use,/invert_color,$
                lat_center=obs_out.obsdec,lon_center=obs_out.obsra,rotation=0,grid_spacing=grid_spacing,degpix=degpix,astr=astr_out2,$
                offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=1,/sphere,_Extra=extra
        ENDIF
        IF Max(Ptr_valid(beam_contour_arr2)) THEN BEGIN
            Imagefast,fltarr(zoom_high-zoom_low+1,zoom_high-zoom_low+1),file_path=image_path+filter_name+'_beam_contour_'+pol_names[pol_i],$
                /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,low=instr_low_use,high=instr_high_use,$
                title=title_fhd,show_grid=0,astr=astr_out2,contour_image=beam_contour_arr2[pol_i],/zero_white,_Extra=extra
        ENDIF
    ENDIF
ENDFOR
IF Keyword_Set(output_residual_histogram) AND ~Keyword_Set(no_png) THEN $
    residual_statistics,(*stokes_residual_arr[0])*beam_mask,obs_out,beam_base=beam_base_out,$
        /center,file_path_base=image_path+filter_name,_Extra=extra

undefine_fhd,beam_contour_arr,beam_contour_arr2,beam_correction_out,beam_base_out
undefine_fhd,instr_residual_arr,instr_dirty_arr,instr_sources,instr_rings
undefine_fhd,stokes_residual_arr,stokes_sources,stokes_rings
undefine_fhd,cal_source_array, cal_source_arr_out
undefine_fhd,gal_model_img

timing=Systime(1)-t0
IF ~Keyword_Set(silent) THEN print,'Image output timing (quickview): ',timing
END
