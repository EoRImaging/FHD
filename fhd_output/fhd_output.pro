;+
;NOTE!!!! This routine is no longer used. Use fhd_quickview instead!
; :Author: Ian Sullivan
;-
PRO fhd_output,obs,status_str,fhd_params,cal,jones,skymodel,file_path_fhd=file_path_fhd,version=version,map_fn_arr=map_fn_arr,$
    silent=silent,show_grid=show_grid,align=align,catalog_file_path=catalog_file_path,image_filter_fn=image_filter_fn,$
    pad_uv_image=pad_uv_image,galaxy_model_fit=galaxy_model_fit,model_recalculate=model_recalculate,$
    gridline_image_show=gridline_image_show,transfer_mapfn=transfer_mapfn,show_obsname=show_obsname,mark_zenith=mark_zenith,$
    image_uv_arr=image_uv_arr,weights_arr=weights_arr,beam_arr=beam,zoom_low=zoom_low,zoom_high=zoom_high,zoom_radius=zoom_radius,$
    instr_low=instr_low,instr_high=instr_high,stokes_low=stokes_low,stokes_high=stokes_high,$
    use_pointing_center=use_pointing_center,no_fits=no_fits,no_png=no_png,$
    allow_sidelobe_image_output=allow_sidelobe_image_output,beam_diff_image=beam_diff_image,$
    beam_output_threshold=beam_output_threshold,beam_threshold=beam_threshold,output_residual_histogram=output_residual_histogram,$
   show_beam_contour=show_beam_contour, image_mask_horizon=image_mask_horizon,no_condense_sources=no_condense_sources,_Extra=extra

compile_opt idl2,strictarrsubs  
heap_gc
t0a=Systime(1) & t0=0 & t1=0 & t2=0 & t3=0 & t4=0 & t5=0 & t6=0 & t7=0 & t8=0 & t9=0 & t10=0
IF N_Elements(silent) EQ 0 THEN silent=0
basename=file_basename(file_path_fhd)
dirpath=file_dirname(file_path_fhd)
print,'Exporting: ',basename
output_path=filepath(basename,root=dirpath,sub='output_data')
output_dir=file_dirname(output_path)

image_path=filepath(basename,root=dirpath,sub='output_images')
image_dir=file_dirname(image_path)
IF file_test(image_dir) EQ 0 THEN file_mkdir,image_dir
IF file_test(output_dir) EQ 0 THEN file_mkdir,output_dir

IF not Keyword_Set(obs) THEN fhd_save_io,status_str,obs,var='obs',/restore,file_path_fhd=file_path_fhd,_Extra=extra
IF not Keyword_Set(fhd_params) THEN fhd_save_io,status_str,fhd_params,var='fhd_params',/restore,file_path_fhd=file_path_fhd,_Extra=extra
IF N_Elements(jones) EQ 0 THEN fhd_save_io,status_str,jones,var='jones',/restore,file_path_fhd=file_path_fhd,_Extra=extra

IF N_Elements(image_mask_horizon) EQ 0 THEN image_mask_horizon=1
IF N_Elements(galaxy_model_fit) EQ 0 THEN galaxy_model_fit=0
IF tag_exist(fhd_params,'galaxy_subtract') THEN galaxy_model_fit=fhd_params.galaxy_subtract 
IF N_Elements(cal) GT 0 THEN IF cal.galaxy_cal THEN galaxy_model_fit=1
IF N_Elements(show_grid) EQ 0 THEN show_grid=1
stats_radius=10. ;degrees
pol_names=obs.pol_names

grid_spacing=10.
offset_lat=5.;15. paper 10 memo
offset_lon=5.;15. paper 10 memo
reverse_image=1   ;1: reverse x axis, 2: y-axis, 3: reverse both x and y axes
map_reverse=reverse_image;1 paper 3 memo
label_spacing=1.
IF Keyword_Set(show_obsname) OR (N_Elements(show_obsname) EQ 0) THEN title_fhd=basename
IF N_Elements(mark_zenith) EQ 0 THEN mark_zenith=1

IF Keyword_Set(image_filter_fn) THEN BEGIN
    dummy_img=Call_function(image_filter_fn,fltarr(2,2),name=filter_name,/return_name_only)
    IF Keyword_Set(filter_name) THEN filter_name='_'+filter_name ELSE filter_name=''
ENDIF ELSE filter_name=''

; *_fhd.sav contains:
;residual_array,dirty_array,image_uv_arr,source_array,comp_arr,model_uv_full,model_uv_holo,normalization_arr,weights_arr,$
;    source_mask=source_mask,beam_base,beam_correction,ra_arr,dec_arr,astr
fhd_save_io,status_str,var='fhd',file_path_fhd=file_path_fhd,path_use=path_use,/no_save,_Extra=extra
restore,path_use+'.sav'

n_pol=fhd_params.npol
dimension_uv=obs.dimension
IF Keyword_Set(pad_uv_image) THEN obs_out=fhd_struct_update_obs(obs,dimension=obs.dimension*pad_uv_image,kbin=obs.kpix) $
    ELSE obs_out=obs

IF N_Elements(beam_threshold) EQ 0 THEN beam_threshold=fhd_params.beam_threshold
IF N_Elements(beam_output_threshold) EQ 0 THEN beam_output_threshold=beam_threshold/2.
dimension=obs_out.dimension
elements=obs_out.elements
degpix=obs_out.degpix
astr_out=obs_out.astr
;pix_area_cnv=pixel_area(astr_out,dimension=dimension)/degpix^2.

t1a=Systime(1)
t0+=t1a-t0a

t2a=Systime(1)
t1+=t2a-t1a

beam_mask=fltarr(dimension,elements)+1
IF Keyword_Set(image_mask_horizon) THEN BEGIN
    xy2ad,meshgrid(dimension,elements,1),meshgrid(dimension,elements,2),astr_out,ra_arr,dec_arr
    horizon_test=where(Finite(ra_arr,/nan),n_horizon_mask)
    IF n_horizon_mask GT 0 THEN beam_mask[horizon_test]=0
ENDIF

beam_avg=fltarr(dimension,elements)
beam_base_out=Ptrarr(n_pol,/allocate)
beam_correction_out=Ptrarr(n_pol,/allocate)
FOR pol_i=0,n_pol-1 DO BEGIN
    *beam_base_out[pol_i]=Rebin(*beam_base[pol_i],dimension,elements) ;should be fine even if pad_uv_image is not set
    *beam_correction_out[pol_i]=weight_invert(*beam_base_out[pol_i],fhd_params.beam_threshold/10.)
    IF pol_i GT 1 THEN CONTINUE
    beam_mask_test=*beam_base_out[pol_i]
    IF Keyword_Set(allow_sidelobe_image_output) THEN beam_i=where(beam_mask_test GE beam_output_threshold) ELSE $
        beam_i=region_grow(beam_mask_test,dimension/2.+dimension*elements/2.,threshold=[beam_output_threshold,Max(beam_mask_test)])
    beam_mask0=fltarr(dimension,elements) & beam_mask0[beam_i]=1.
    beam_avg+=*beam_base_out[pol_i]
    beam_mask*=beam_mask0
ENDFOR
beam_avg/=(n_pol<2)
beam_i=where(beam_mask)

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
    FOR pol_i=0,n_pol-1 DO uv_mask[where(*model_uv_full[pol_i])]=1
    noise_map=fhd_params.convergence*rebin(weight_invert(beam_avg),dimension,elements)
    comp_arr=comp_arr[0:fhd_params.n_components-1]
    source_array=Components2Sources(comp_arr,obs,fhd_params,noise_map=noise_map,source_mask=source_mask,_Extra=extra)
    IF Keyword_Set(no_condense_sources) THEN $
        model_uv_full=source_dft_model(obs,jones,comp_arr,t_model=t_model,uv_mask=uv_mask,sigma_threshold=0,_extra=extra) $
        ELSE model_uv_full=source_dft_model(obs,jones,source_array,t_model=t_model,uv_mask=uv_mask,sigma_threshold=fhd_params.sigma_cut,_extra=extra)
    FOR pol_i=0,n_pol-1 DO BEGIN
        *model_uv_holo[pol_i]=holo_mapfn_apply(*model_uv_full[pol_i],map_fn_arr[pol_i],_Extra=extra,/indexed)
    ENDFOR
ENDIF
heap_gc

si_use=where(source_array.ston GE 0,ns_use)
source_arr=source_array[si_use]
source_arr_out=source_arr
comp_arr_out=comp_arr

ad2xy,source_arr.ra,source_arr.dec,astr_out,sx,sy
ad2xy,comp_arr.ra,comp_arr.dec,astr_out,cx,cy
source_arr_out.x=sx & source_arr_out.y=sy
comp_arr_out.x=cx & comp_arr_out.y=cy

extend_test=where(Ptr_valid(source_arr_out.extend),n_extend)
IF n_extend GT 0 THEN BEGIN
    FOR ext_i=0L,n_extend-1 DO BEGIN
        comp_arr_out1=*source_arr[extend_test[ext_i]].extend
        ad2xy,comp_arr_out1.ra,comp_arr_out1.dec,astr_out,cx,cy
        comp_arr_out1.x=cx & comp_arr_out1.y=cy
        source_arr_out[extend_test[ext_i]].extend=Ptr_new(/allocate)
        *source_arr_out[extend_test[ext_i]].extend=comp_arr_out1
    ENDFOR
ENDIF

;Build a fits header
mkhdr,fits_header,*residual_array[0]
putast, fits_header, astr_out;, cd_type=1

fits_header_Jy=fits_header
sxaddpar,fits_header_Jy,'BUNIT','Jy/beam'

fits_header_apparent=fits_header
sxaddpar,fits_header_apparent,'BUNIT','Jy/beam (apparent)'

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

jones_out=fhd_struct_init_jones(obs_out,0,jones,file_path_fhd=file_path_fhd,/update,/no_save,mask=beam_mask)

t3a=Systime(1)
t2+=t3a-t2a

dirty_images=Ptrarr(n_pol,/allocate)
instr_images=Ptrarr(n_pol,/allocate)
model_images=Ptrarr(n_pol,/allocate)
stokes_sources=Ptrarr(n_pol,/allocate)

model_uv_arr=Ptrarr(n_pol,/allocate)
model_holo_arr=Ptrarr(n_pol,/allocate)
res_uv_arr=Ptrarr(n_pol,/allocate)
filter_arr=Ptrarr(n_pol,/allocate) 

restored_beam_width=beam_width_calculate(obs_out,min_restored_beam_width=0.75)
;IF Keyword_Set(pad_uv_image) THEN restored_beam_width*=pad_uv_image
gal_model_img=Ptrarr(n_pol)
gal_holo_img=Ptrarr(n_pol)

FOR pol_i=0,n_pol-1 DO BEGIN
    filter_single=filter_arr[pol_i]
    *dirty_images[pol_i]=dirty_image_generate(*image_uv_arr[pol_i],degpix=degpix,weights=*weights_arr[pol_i],$
        image_filter_fn=image_filter_fn,pad_uv_image=pad_uv_image,file_path_fhd=file_path_fhd,filter=filter_single,/antialias,_Extra=extra);*(*beam_correction_out[pol_i])
    instr_img_uv=*image_uv_arr[pol_i]-*model_uv_holo[pol_i];-*gal_holo_uv[pol_i]
    *res_uv_arr[pol_i]=instr_img_uv
    *model_images[pol_i]=dirty_image_generate(*model_uv_holo[pol_i],degpix=degpix,weights=*weights_arr[pol_i],$
        image_filter_fn=image_filter_fn,pad_uv_image=pad_uv_image,file_path_fhd=file_path_fhd,filter=filter_single,/antialias,_Extra=extra)
    *instr_images[pol_i]=*dirty_images[pol_i]-*model_images[pol_i]
    IF Keyword_Set(no_condense_sources) THEN BEGIN
        *stokes_sources[pol_i]=source_image_generate(comp_arr_out,obs_out,pol_i=pol_i+4,resolution=16,$
            dimension=dimension,restored_beam_width=restored_beam_width)
    ENDIF ELSE BEGIN
        *stokes_sources[pol_i]=source_image_generate(source_arr_out,obs_out,pol_i=pol_i+4,resolution=16,$
            dimension=dimension,restored_beam_width=restored_beam_width)
    ENDELSE
    filter_arr[pol_i]=filter_single
ENDFOR

; renormalize based on weights
renorm_factor = get_image_renormalization(obs_out,weights_arr=weights_arr,beam_base=beam_base_out,filter_arr=filter_arr,$
  image_filter_fn=image_filter_fn,pad_uv_image=pad_uv_image,degpix=degpix,/antialias)
for pol_i=0,n_pol-1 do begin
  *dirty_images[pol_i]*=renorm_factor
  *res_uv_arr[pol_i]*=renorm_factor
  *model_images[pol_i]*=renorm_factor
  *instr_images[pol_i]*=renorm_factor
endfor

IF Keyword_Set(galaxy_model_fit) THEN BEGIN
    gal_model_base=fhd_galaxy_model(obs,file_path_fhd=file_path_fhd,gal_model_uv=gal_model_uv,_Extra=extra)
    IF Keyword_Set(pad_uv_image) THEN gal_model_base=Rebin(gal_model_base,dimension,elements)
    FOR pol_i=0,n_pol-1 DO gal_model_img[pol_i]=Ptr_new(gal_model_base*(*beam_base_out[pol_i]))
    gal_name='_galfit'
    IF Min(Ptr_valid(map_fn_arr)) GT 0 THEN FOR pol_i=0,n_pol-1 DO BEGIN
        gal_holo_uv=holo_mapfn_apply(*gal_model_uv[pol_i],map_fn_arr[pol_i],_Extra=extra,/indexed)
        gal_holo_img[pol_i]=Ptr_new(dirty_image_generate(gal_holo_uv,pad_uv_image=pad_uv_image,weights=*weights_arr[pol_i],$
            image_filter_fn=image_filter_fn,degpix=degpix,file_path_fhd=file_path_fhd,filter=filter_arr[pol_i],/antialias,_Extra=extra)$
            *(*beam_correction_out[pol_i])*renorm_factor)
    ENDFOR
ENDIF ELSE  gal_name=''

stokes_images=stokes_cnv(instr_images,jones_out,beam=beam_base_out,/square,_Extra=extra)
instr_sources=stokes_cnv(stokes_sources,jones_out,/inverse,beam=beam_base_out,/square,_Extra=extra) 

t4a=Systime(1)
t3+=t4a-t3a

IF Keyword_Set(catalog_file_path) AND file_test(catalog_file_path) EQ 1 THEN BEGIN
    mrc_cat=mrc_catalog_read(astr_out,file_path=catalog_file_path)
    mrc_i_use=where((mrc_cat.x GE 0) AND (mrc_cat.x LE dimension-1) AND (mrc_cat.y GE 0) AND (mrc_cat.y LE elements-1),n_mrc)
    
    IF Keyword_Set(align) THEN BEGIN 
        error=0
        source_array1_base=fltarr(6,ns_use)
        source_array1_base[0,*]=source_arr_out.x
        source_array1_base[1,*]=source_arr_out.y
        source_array1_base[4,*]=source_arr_out.flux.I
        source_array1_base[5,*]=Round(source_arr_out.x)+Round(source_arr_out.y)*dimension
        source_array2_base=fltarr(6,n_mrc)
        source_array2_base[0,*]=mrc_cat[mrc_i_use].x
        source_array2_base[1,*]=mrc_cat[mrc_i_use].y
        source_array2_base[4,*]=mrc_cat[mrc_i_use].flux.I
        source_array2_base[5,*]=Round(mrc_cat[mrc_i_use].x)+Round(mrc_cat[mrc_i_use].y)*dimension
        image_align,fltarr(dimension,elements),fltarr(dimension,elements),dx,dy,theta,scale,source_array1_base,$
            source_array2_base,source_array1_out,source_array2_out,radius=128.,sub_max_number=32,final_match=20,$
            binsize=1.,error=error,min_radius=32,fix_scale=1.,fix_theta=0
         
        IF not Keyword_Set(error) THEN BEGIN
        
            print,"Alignment success. Dx:",dx,' Dy:',dy,' Theta:',theta,' Scale:',scale

            IF (Abs(dx)>Abs(dy)) LE 50. AND (Abs(theta) LE 45.) THEN BEGIN            
                x1_vals=source_arr_out.x
                y1_vals=source_arr_out.y
                x1a_vals=Scale*(x1_vals*Cos(theta*!DtoR)-y1_vals*Sin(theta*!DtoR))+dx
                y1a_vals=Scale*(y1_vals*Cos(theta*!DtoR)+x1_vals*Sin(theta*!DtoR))+dy
                ra_vals=Interpolate(ra_arr,x1a_vals,y1a_vals)
                dec_vals=Interpolate(dec_arr,x1a_vals,y1a_vals)
                source_arr_out.ra=ra_vals
                source_arr_out.dec=dec_vals
                
                x2_vals=mrc_cat[mrc_i_use].x
                y2_vals=mrc_cat[mrc_i_use].y
                x2a_vals=(1./Scale)*((x2_vals-dx)*Cos(theta*!DtoR)+(y2_vals-dy)*Sin(theta*!DtoR))
                y2a_vals=(1./Scale)*((y2_vals-dy)*Cos(theta*!DtoR)-(x2_vals-dx)*Sin(theta*!DtoR))
                mrc_cat[mrc_i_use].x=x2a_vals
                mrc_cat[mrc_i_use].y=y2a_vals
            ENDIF
        ENDIF ELSE print,'Alignment failure on:',filename
    ENDIF
ENDIF ELSE n_mrc=0

t5a=Systime(1)
t4+=t5a-t4a

IF n_mrc GT 2 THEN BEGIN
    mrc_cat=mrc_cat[mrc_i_use]
    mrc_image=source_image_generate(mrc_cat,obs_out,pol_i=4,resolution=16,dimension=dimension,$
        restored_beam_width=pad_uv_image,ring=6.*pad_uv_image)
ENDIF
;write sources to a text file
source_array_export,source_arr_out,obs_out,beam=beam_avg,stokes_images=stokes_images,file_path=output_path+'_source_list2'
source_array_export,comp_arr_out,obs_out,beam=beam_avg,stokes_images=stokes_images,file_path=output_path+'_component_list2'

t6a=Systime(1)
t5+=t6a-t5a

; plot calibration solutions, export to png
IF N_Elements(cal) GT 0 THEN BEGIN
   IF cal.n_cal_src ne 0 THEN BEGIN
      IF file_test(file_path_fhd+'_cal_hist.sav') THEN BEGIN
         vis_baseline_hist=getvar_savefile(file_path_fhd+'_cal_hist.sav','vis_baseline_hist')
         plot_cals,cal,obs,file_path_base=image_path,vis_baseline_hist=vis_baseline_hist,_Extra=extra
      ENDIF ELSE BEGIN
         plot_cals,cal,obs,file_path_base=image_path,_Extra=extra
      ENDELSE
   ENDIF
ENDIF

;FREE MEMORY
undefine_fhd,map_fn_arr,image_uv_arr,model_uv_full,model_uv_holo,gal_model_uv
    
t7a=Systime(1)
IF Keyword_Set(t6a) THEN t6=t7a-t6a

x_inc=beam_i mod dimension
y_inc=Floor(beam_i/dimension)
IF N_Elements(zoom_radius) GT 0 THEN BEGIN
    zoom_radius_use=zoom_radius<dimension
    zoom_low=dimension/2-zoom_radius_use/2
    zoom_high=dimension/2+zoom_radius_use/2
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

IF Keyword_Set(beam_diff_image) THEN BEGIN
    source_res_arr=source_residual_image(obs_out,source_arr_out,instr_images,beam_arr=beam_base_out,$
        jones=jones_out,source_residual_radius=100.,source_residual_flux_threshold=1.,beam_power=2,_Extra=extra)
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
    ENDIF ELSE mark_image=0
    FOR pol_i=0,n_pol-1 DO BEGIN
        IF ~Keyword_Set(no_png) THEN BEGIN
            Imagefast,(*source_res_arr[pol_i])[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path+'_Beam_diff_'+pol_names[pol_i],$
                /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,show_grid=show_grid,$
                low=beam_diff_low_use,high=beam_diff_high_use,title=title_fhd,astr=astr_out2,_Extra=extra
            Imagefast,(*source_res_stks[pol_i])[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path+'_Beam_diff_'+pol_names[pol_i+4],$
                /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,show_grid=show_grid,$
                low=beam_diff_low_use*2.,high=beam_diff_high_use*2.,title=title_fhd,astr=astr_out2,_Extra=extra
        ENDIF
        IF ~Keyword_Set(no_fits) THEN FitsFast,*source_res_arr[pol_i],fits_header,/write,file_path=output_path+'_Beam_diff_'+pol_names[pol_i]
    ENDFOR
ENDIF

image_path_fg=image_path+filter_name;+gal_name
output_path_fg=output_path+filter_name;+gal_name

FOR pol_i=0,n_pol-1 DO BEGIN
    instr_residual=*instr_images[pol_i]*(*beam_correction_out[pol_i])
    instr_dirty=*dirty_images[pol_i]*(*beam_correction_out[pol_i])
    instr_model=*model_images[pol_i]*(*beam_correction_out[pol_i])
    instr_source=*instr_sources[pol_i]
    instr_restored=instr_residual+instr_source
    beam_use=*beam_base_out[pol_i]
    stokes_residual=(*stokes_images[pol_i])*beam_mask
    stokes_source=(*stokes_sources[pol_i])*beam_mask
    stokes_restored=stokes_residual+stokes_source
    
    IF N_Elements(instr_low) EQ 0 THEN instr_low_use=Min(instr_residual[beam_i])>(-5.*Stddev(instr_residual[beam_i])) ELSE instr_low_use=instr_low
    IF N_Elements(instr_high) EQ 0 THEN instr_high_use=Max(instr_residual[beam_i])<(10.*Stddev(instr_residual[beam_i])) ELSE instr_high_use=instr_high
    
    instr_low_use=instr_low_use>(-instr_high_use)    
    IF N_Elements(stokes_low) EQ 0 THEN stokes_low_use=Min((stokes_residual*Sqrt(beam_avg>0))[beam_i])>(-5.*Stddev((stokes_residual*Sqrt(beam_avg>0))[beam_i])) $ 
        ELSE stokes_low_use=stokes_low
    IF N_Elements(stokes_high) EQ 0 THEN stokes_high_use=Max((stokes_residual*Sqrt(beam_avg>0))[beam_i])<(10.*Stddev((stokes_residual*Sqrt(beam_avg>0))[beam_i])) $
        ELSE stokes_high_use=stokes_high 
    stokes_low_use=stokes_low_use>(-stokes_high_use)
    
    t8a=Systime(1)
    IF ~Keyword_Set(no_fits) THEN BEGIN
        FitsFast,instr_dirty,fits_header_apparent,/write,file_path=output_path+filter_name+'_Dirty_'+pol_names[pol_i]
        FitsFast,instr_model,fits_header_apparent,/write,file_path=output_path+filter_name+'_Model_'+pol_names[pol_i]
        FitsFast,instr_residual,fits_header_apparent,/write,file_path=output_path_fg+'_Residual_'+pol_names[pol_i]
        FitsFast,instr_source,fits_header_apparent,/write,file_path=output_path_fg+'_Sources_'+pol_names[pol_i]
        FitsFast,instr_restored,fits_header_apparent,/write,file_path=output_path_fg+'_Restored_'+pol_names[pol_i]
        FitsFast,beam_use,fits_header,/write,file_path=output_path+'_Beam_'+pol_names[pol_i]
        FitsFast,Abs(*weights_arr[pol_i])*obs.n_vis,fits_header_uv,/write,file_path=output_path+'_UV_weights_'+pol_names[pol_i]
        
        FitsFast,*stokes_images[pol_i],fits_header_Jy,/write,file_path=output_path_fg+'_Residual_'+pol_names[pol_i+4]
        FitsFast,*stokes_sources[pol_i],fits_header_Jy,/write,file_path=output_path+'_Sources_'+pol_names[pol_i+4]
        FitsFast,*stokes_images[pol_i]+*stokes_sources[pol_i],fits_header_Jy,/write,file_path=output_path_fg+'_Restored_'+pol_names[pol_i+4]
        IF Keyword_Set(galaxy_model_fit) THEN BEGIN
            FitsFast,*gal_model_img[pol_i],fits_header_apparent,/write,file_path=output_path+'_GalModel_'+pol_names[pol_i]
            IF Ptr_valid(gal_holo_img[pol_i]) THEN FitsFast,*gal_holo_img[pol_i],fits_header_apparent,/write,file_path=output_path+'_GalHolo_'+pol_names[pol_i]
        ENDIF
    ENDIF
    
    t9a=Systime(1)
    t8+=t9a-t8a
    
    IF ~Keyword_Set(no_png) THEN BEGIN
        Imagefast,Abs(*weights_arr[pol_i])*obs.n_vis,file_path=image_path+'_UV_weights_'+pol_names[pol_i],$
            /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,/log,$
            low=Min(Abs(*weights_arr[pol_i])*obs.n_vis),high=Max(Abs(*weights_arr[pol_i])*obs.n_vis),title=title_fhd,_Extra=extra
        
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
        
        instrS_high=Max(instr_restored[beam_i])
        log_dirty=0
        log_source=1
        Imagefast,instr_dirty[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path_fg+'_Dirty_'+pol_names[pol_i],$
            /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log_dirty,low=instr_low_use,high=instr_high_use,title=title_fhd,_Extra=extra
        Imagefast,instr_model[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path_fg+'_Model_'+pol_names[pol_i],$
            /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log_dirty,low=instr_low_use,high=instr_high_use,title=title_fhd,_Extra=extra
        Imagefast,instr_residual[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path_fg+'_Residual_'+pol_names[pol_i],$
            /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,low=instr_low_use,high=instr_high_use,title=title_fhd,_Extra=extra
        Imagefast,instr_source[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path_fg+'_Sources_'+pol_names[pol_i],$
            /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log_source,low=0,high=instr_high_use,/invert_color,title=title_fhd,_Extra=extra
        Imagefast,instr_restored[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path_fg+'_Restored_'+pol_names[pol_i],$
            /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log_dirty,low=instr_low_use,high=instr_high_use,title=title_fhd,_Extra=extra
        Imagefast,beam_use[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image*100.,file_path=image_path+'_Beam_'+pol_names[pol_i],/log,$
            /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,$
            low=min(beam_use[zoom_low:zoom_high,zoom_low:zoom_high]*100)>0,high=max(beam_use[zoom_low:zoom_high,zoom_low:zoom_high]*100),/invert,title=title_fhd,_Extra=extra
        
        t8b=Systime(1)
        
        IF Keyword_Set(galaxy_model_fit) THEN BEGIN
            gal_img=*gal_model_img[pol_i]
            IF Ptr_valid(gal_holo_img[pol_i]) THEN gal_holo=*gal_holo_img[pol_i] ELSE gal_holo=0
            gal_low_use=Min(gal_img[beam_i])
            gal_high_use=Max(gal_img[beam_i])  
            Imagefast,gal_img[zoom_low:zoom_high,zoom_low:zoom_high],file_path=image_path_fg+'_GalModel_'+pol_names[pol_i],$
                /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log_source,low=gal_low_use,high=gal_high_use,$
                lat_center=obs_out.obsdec,lon_center=obs_out.obsra,rotation=0,grid_spacing=grid_spacing,degpix=degpix,$
                offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=show_grid,$
                title=title_fhd,/sphere,astr=astr_out2,contour_image=beam_contour_arr[pol_i],_Extra=extra
            IF Keyword_Set(gal_holo) THEN Imagefast,gal_holo[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path_fg+'_GalHolo_'+pol_names[pol_i],$
                /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log_dirty,low=instr_low_use,high=instr_high_use,$
                offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=show_grid,$
                title=title_fhd,/sphere,astr=astr_out2,contour_image=beam_contour_arr[pol_i],_Extra=extra
        ENDIF
        t9+=t8b-t9a
        
        t9b=Systime(1)
        t8+=t9b-t8b
        
        stokesS_high=Max((stokes_restored*Sqrt(beam_avg>0))[beam_i])
        IF pol_i EQ 0 THEN log_source=1 ELSE log_source=0
        IF pol_i EQ 0 THEN log=0 ELSE log=0
        Imagefast,stokes_residual[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path_fg+'_Residual_'+pol_names[pol_i+4],$
            /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,low=stokes_low_use,high=stokes_high_use,$
            lat_center=obs_out.obsdec,lon_center=obs_out.obsra,rotation=0,grid_spacing=grid_spacing,degpix=degpix,$
            offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,$
            show_grid=show_grid,/sphere,title=title_fhd,astr=astr_out2,_Extra=extra
        Imagefast,stokes_source[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path_fg+'_Sources_'+pol_names[pol_i+4],$
            /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log,low=0,high=stokes_high_use,/invert_color,$
            lat_center=obs_out.obsdec,lon_center=obs_out.obsra,rotation=0,grid_spacing=grid_spacing,degpix=degpix,$
            offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,$
            show_grid=show_grid,/sphere,title=title_fhd,astr=astr_out2,_Extra=extra
        Imagefast,stokes_restored[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path_fg+'_Restored_'+pol_names[pol_i+4],$
            /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log,low=stokes_low_use,high=stokes_high_use,$
            lat_center=obs_out.obsdec,lon_center=obs_out.obsra,rotation=0,grid_spacing=grid_spacing,degpix=degpix,$
            offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,$
            show_grid=show_grid,/sphere,title=title_fhd,astr=astr_out2,_Extra=extra
        
        IF pol_i EQ 0 THEN BEGIN
            IF Keyword_Set(gridline_image_show) THEN BEGIN
                Imagefast,fltarr(zoom_high-zoom_low+1,zoom_high-zoom_low+1),file_path=image_path_fg+'_Grid',$
                    /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log,low=0,high=stokes_high_use,/invert_color,$
                    lat_center=obs_out.obsdec,lon_center=obs_out.obsra,rotation=0,grid_spacing=grid_spacing,degpix=degpix,astr=astr_out2,$
                    offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=show_grid,/sphere,_Extra=extra
                IF Max(Ptr_valid(beam_contour_arr2)) THEN BEGIN
                    Imagefast,fltarr(zoom_high-zoom_low+1,zoom_high-zoom_low+1),file_path=image_path+filter_name+'_beam_contour_'+pol_names[pol_i],$
                        /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,low=instr_low_use,high=instr_high_use,$
                        title=title_fhd,show_grid=0,astr=astr_out2,contour_image=beam_contour_arr2[pol_i],/zero_white,_Extra=extra
                ENDIF
            ENDIF
        
            IF Keyword_Set(mrc_image) THEN BEGIN
                mrc_comp=(stokes_source+mrc_image)[zoom_low:zoom_high,zoom_low:zoom_high]
                Imagefast,mrc_comp,file_path=image_path_fg+'_Sources_MRCrings_'+pol_names[pol_i+4],$
                    /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,low=0,high=5.,/invert_color,_Extra=extra 
            ENDIF
        ENDIF
    ENDIF
;    IF Keyword_Set(galaxy_model_fit) THEN BEGIN
;        IF ~Keyword_Set(no_fits) THEN BEGIN
;            FitsFast,*stokes_gal_model[pol_i],fits_header_Jy,/write,file_path=output_path+'_GalModel_'+pol_names[pol_i+4]
;            FitsFast,*gal_holo_img[pol_i],fits_header_apparent,/write,file_path=output_path+'_GalHolo_'+pol_names[pol_i]
;            FitsFast,*gal_model_img[pol_i],fits_header_apparent,/write,file_path=output_path+'_GalModel_'+pol_names[pol_i]
;        ENDIF
;        gal_low=0.
;        gal_high=Max((*gal_model_img[pol_i])[beam_i])
;        gal_high2=Max((*stokes_gal_model[pol_i])[beam_i])
;        gal_log=0
;        debug_gal=1
;        IF ~Keyword_Set(no_png) THEN BEGIN
;            while debug_gal GT 0 DO BEGIN
;                imagefast,(*stokes_gal_model[pol_i])[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path_fg+'_GalModel_'+pol_names[pol_i+4],$
;                    /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=gal_log,low=gal_low,high=gal_high*2.,$
;                    lat_center=obs_out.obsdec,lon_center=obs_out.obsra,rotation=0,grid_spacing=grid_spacing,degpix=degpix,$
;                    offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,$
;                    show_grid=show_grid,/sphere,title=title_fhd,_Extra=extra
;                imagefast,(*gal_holo_img[pol_i])[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path_fg+'_GalHolo_'+pol_names[pol_i],$
;                    /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=0,low=instr_low_use,high=instr_high_use,$
;                    lat_center=obs_out.obsdec,lon_center=obs_out.obsra,rotation=0,grid_spacing=grid_spacing,degpix=degpix,$
;                    offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,$
;                    show_grid=show_grid,/sphere,title=title_fhd,_Extra=extra
;                imagefast,(*gal_model_img[pol_i])[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path_fg+'_GalModel_'+pol_names[pol_i],$
;                    /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=gal_log,low=gal_low,high=gal_high,$
;                    lat_center=obs_out.obsdec,lon_center=obs_out.obsra,rotation=0,grid_spacing=grid_spacing,degpix=degpix,$
;                    offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,$
;                    show_grid=show_grid,/sphere,title=title_fhd,_Extra=extra
;                imagefast,(*gal_model_img[pol_i]+instr_source)[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path_fg+'_GalSources_'+pol_names[pol_i],$
;                    /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=gal_log,low=gal_low,high=gal_high,title=title_fhd,_Extra=extra
;                imagefast,(*gal_model_img[pol_i]+instr_restored)[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path_fg+'_GalRestored_'+pol_names[pol_i],$
;                    /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=gal_log,low=gal_low,high=gal_high,title=title_fhd,_Extra=extra
;                imagefast,(*stokes_gal_model[pol_i]+stokes_residual)[zoom_low:zoom_high,zoom_low:zoom_high]+mark_image,file_path=image_path_fg+'_GalResidual_'+pol_names[pol_i+4],$
;                    /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=gal_log,low=gal_low,high=gal_high2,title=title_fhd,_Extra=extra
;                debug_gal=0
;                dummyval=0
;            endwhile
;        ENDIF
;    ENDIF
    t10a=Systime(1)
    t9+=t10a-t9b
ENDFOR

t10b=Systime(1)

IF Keyword_Set(output_residual_histogram) THEN $
    residual_statistics,(*stokes_images[0])*beam_mask,obs_out,fhd_params,radius=stats_radius,beam_base=beam_base_out,$
        ston=fhd_params.sigma_cut,/center,file_path_base=image_path_fg,_Extra=extra

t10=Systime(1)-t10b
t00=Systime(1)-t0a
IF ~Keyword_Set(silent) THEN print,'Image output timing: ',t00
END
