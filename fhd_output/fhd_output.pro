PRO fhd_output,obs,fhd, file_path_fhd=file_path_fhd,version=version,$
    noise_calibrate=noise_calibrate,restore_last=restore_last,coord_debug=coord_debug,silent=silent,show_grid=show_grid,$
    fluxfix=fluxfix,align=align,catalog_file_path=catalog_file_path,image_filter_fn=image_filter_fn,$
    pad_uv_image=pad_uv_image,galaxy_model_fit=galaxy_model_fit,_Extra=extra

compile_opt idl2,strictarrsubs  
heap_gc
t0a=Systime(1)
t0=0
t1=0
t2=0
t3=0
t4=0
t5=0
t6=0
t7=0
t8=0
t9=0
t10=0

IF not Keyword_Set(obs) THEN restore,file_path_fhd+'_obs.sav'
IF not Keyword_Set(fhd) THEN restore,file_path_fhd+'_fhd_params.sav'
IF N_Elements(show_grid) EQ 0 THEN show_grid=1

basename=file_basename(file_path_fhd)
dirpath=file_dirname(file_path_fhd)
IF not Keyword_Set(silent) THEN print,'Exporting: ',basename
export_path=filepath(basename,root=dirpath,sub='export')
export_dir=file_dirname(export_path)

image_path=filepath(basename,root=dirpath,sub='images')
image_dir=file_dirname(image_path)
IF file_test(image_dir) EQ 0 THEN file_mkdir,image_dir
IF file_test(export_dir) EQ 0 THEN file_mkdir,export_dir

IF Keyword_Set(image_filter_fn) THEN BEGIN
    dummy_img=Call_function(image_filter_fn,fltarr(2,2),name=filter_name)
    IF Keyword_Set(filter_name) THEN filter_name='_'+filter_name ELSE filter_name=''
ENDIF ELSE filter_name=''

; *_fhd.sav contains:
;residual_array,dirty_array,image_uv_arr,source_array,comp_arr,model_uv_full,model_uv_holo,normalization_arr,weights_arr,$
;    beam_base,beam_correction,ra_arr,dec_arr,astr
restore,file_path_fhd+'_fhd.sav'

restore,file_path_fhd+'_params.sav' ;params
restore,file_path_fhd+'_hdr.sav' ;hdr

IF N_Elements(normalization_arr) GT 0 THEN normalization=Mean(normalization_arr)/2.
npol=fhd.npol
dimension_uv=obs.dimension

IF Keyword_Set(pad_uv_image) THEN BEGIN
    dimension_use=((obs.dimension>obs.elements)*pad_uv_image)>(obs.dimension>obs.elements)
    IF tag_exist(extra,'dimension') THEN extra.dimension=dimension_use
    obs_out=vis_struct_init_obs(hdr,params,n_pol=obs.n_pol,dimension=dimension_use,lon=obs.lon,lat=obs.lat,$
        kbinsize=obs.kpix,zenra=obs.zenra,zendec=obs.zendec,phasera=obs.phasera,phasedec=obs.phasedec,_Extra=extra)
ENDIF ELSE obs_out=obs
dimension=obs_out.dimension
elements=obs_out.elements
degpix=obs_out.degpix

zoom_radius=Round(18./(degpix)/16.)*16.
zoom_low=dimension/2.-zoom_radius
zoom_high=dimension/2.+zoom_radius-1
stats_radius=10. ;degrees
pol_names=['xx','yy','xy','yx','I','Q','U','V']

grid_spacing=10.
offset_lat=5.;15. paper 10 memo
offset_lon=5.;15. paper 10 memo
reverse_image=0   ;1: reverse x axis, 2: y-axis, 3: reverse both x and y axes
map_reverse=0;1 paper 3 memo
label_spacing=1.
astr_out=obs_out.astr
astr=obs.astr

si_use=where(source_array.ston GE fhd.sigma_cut,ns_use)
source_arr=source_array[si_use]
source_arr_out=source_arr
sx=(source_arr.x-obs.dimension/2.)*2.+obs_out.dimension/2.
sy=(source_arr.y-obs.elements/2.)*2.+obs_out.elements/2.
;ad2xy,source_arr.ra,source_arr.dec,astr_out,sx,sy
source_arr_out.x=sx & source_arr_out.y=sy

comp_arr_out=comp_arr
sx=(comp_arr.x-obs.dimension/2.)*2.+obs_out.dimension/2.
sy=(comp_arr.y-obs.elements/2.)*2.+obs_out.elements/2.
;ad2xy,comp_arr.ra,comp_arr.dec,astr_out,sx,sy
comp_arr_out.x=sx & comp_arr_out.y=sy

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

t1a=Systime(1)
t0+=t1a-t0a

;IF not Keyword_Set(restore_last) THEN BEGIN

;    map_fn_arr=Ptrarr(npol,/allocate)
    pol_names=['xx','yy','xy','yx','I','Q','U','V']
;    FOR pol_i=0,npol-1 DO BEGIN
;        file_name_base='_mapfn_'+pol_names[pol_i]
;        restore,file_path_fhd+file_name_base+'.sav' ;map_fn
;        *map_fn_arr[pol_i]=map_fn
;    ENDFOR
    psf=beam_setup(obs_out,file_path_fhd,/restore_last,/silent)
    t2a=Systime(1)
    t1+=t2a-t1a
    
    ;beam_threshold=0.05
    beam_mask=fltarr(dimension,elements)+1
    beam_avg=fltarr(dimension,elements)
    beam_base_out=Ptrarr(npol,/allocate)
    beam_correction_out=Ptrarr(npol,/allocate)
    FOR pol_i=0,(npol<2)-1 DO BEGIN
        *beam_base_out[pol_i]=Rebin(*beam_base[pol_i],dimension,elements) ;should be fine even if pad_uv_image is not set
        *beam_correction_out[pol_i]=weight_invert(*beam_base_out[pol_i],fhd.beam_threshold/100.)
        beam_mask_test=*beam_base_out[pol_i]
        beam_i=region_grow(beam_mask_test,dimension/2.+dimension*elements/2.,threshold=[fhd.beam_threshold,Max(beam_mask_test)])
        beam_mask0=fltarr(dimension,elements) & beam_mask0[beam_i]=1.
        beam_avg+=*beam_base_out[pol_i]
        beam_mask*=beam_mask0
    ENDFOR
    psf=0
    heap_gc
;    FOR pol_i=0,(npol<2)-1 DO *beam_correction_out[pol_i]*=beam_mask
    beam_avg/=(npol<2)
    beam_i=where(beam_mask)
    
    t3a=Systime(1)
    t2+=t3a-t2a
    
    dirty_images=Ptrarr(npol,/allocate)
    instr_images=Ptrarr(npol,/allocate)
    instr_sources=Ptrarr(npol,/allocate)
    
    model_uv_arr=Ptrarr(npol,/allocate)
    model_holo_arr=Ptrarr(npol,/allocate)
    
    source_uv_mask=fltarr(size(*image_uv_arr[0],/dimension))
    FOR pol_i=0,npol-1 DO BEGIN
        mask_i=where(*image_uv_arr[pol_i],n_mask_use)
        IF n_mask_use GT 0 THEN source_uv_mask[mask_i]=1
    ENDFOR
    IF Total(source_uv_mask) EQ 0 THEN source_uv_mask+=1
    
    ;factor of (2.*Sqrt(2.*Alog(2.))) is to convert FWHM and sigma of gaussian
    restored_beam_width=(!RaDeg/(obs.MAX_BASELINE/obs.KPIX)/obs.degpix)/(2.*Sqrt(2.*Alog(2.)))
    restored_beam_width=restored_beam_width>0.75
    IF Keyword_Set(pad_uv_image) THEN restored_beam_width*=pad_uv_image
;    restored_beam_width=pad_uv_image*(!RaDeg/(obs.MAX_BASELINE/obs.KPIX)/obs.degpix)/(Sqrt(2.*Alog(2.)))
    model_holo_arr=model_uv_holo
    IF Keyword_Set(galaxy_model_fit) THEN BEGIN
        gal_holo_uv=fhd_galaxy_deconvolve(obs,image_uv_arr,map_fn_arr=map_fn_arr,beam_base=beam_base,$
            galaxy_model_uv=galaxy_model_uv,file_path_fhd=file_path_fhd,restore=1,uv_return=1)
        gal_name='_galfit'
        gal_model_img=Ptrarr(npol)
        gal_holo_img=Ptrarr(npol)
        FOR pol_i=0,npol-1 DO BEGIN
            gal_model_img[pol_i]=Ptr_new(dirty_image_generate(*galaxy_model_uv[pol_i],pad_uv_image=pad_uv_image,$
                image_filter_fn='',degpix=degpix,_Extra=extra)*(*beam_base_out[pol_i])/(obs.degpix*obs.dimension)^2.)
            gal_holo_img[pol_i]=Ptr_new(dirty_image_generate(*gal_holo_uv[pol_i],pad_uv_image=pad_uv_image,$
                image_filter_fn=image_filter_fn,degpix=degpix,_Extra=extra)*(*beam_correction_out[pol_i]))
        ENDFOR
    ENDIF ELSE BEGIN
        gal_name=''
        gal_holo_uv=Ptrarr(npol)
        FOR pol_i=0,npol-1 DO gal_holo_uv[pol_i]=Ptr_new(0.)
    ENDELSE
    FOR pol_i=0,npol-1 DO BEGIN
;        *model_uv_arr[pol_i]=source_array_model(source_arr,pol_i=pol_i,dimension=dimension_uv,$
;            beam_correction=beam_correction,mask=source_uv_mask)
;        *model_holo_arr[pol_i]=holo_mapfn_apply(*model_uv_arr[pol_i],*map_fn_arr[pol_i],_Extra=extra)*normalization
        *dirty_images[pol_i]=dirty_image_generate(*image_uv_arr[pol_i],degpix=degpix,weights=*weights_arr[pol_i],$
            image_filter_fn=image_filter_fn,pad_uv_image=pad_uv_image,_Extra=extra)*(*beam_correction_out[pol_i])
        instr_img_uv=*image_uv_arr[pol_i]-*model_holo_arr[pol_i]-*gal_holo_uv[pol_i]
        *instr_images[pol_i]=dirty_image_generate(instr_img_uv,degpix=degpix,weights=*weights_arr[pol_i],$
            image_filter_fn=image_filter_fn,pad_uv_image=pad_uv_image,_Extra=extra)*(*beam_correction_out[pol_i])
        *instr_sources[pol_i]=source_image_generate(comp_arr_out,obs_out,pol_i=pol_i,resolution=16,$
            dimension=dimension,width=restored_beam_width,pad_uv_image=pad_uv_image)
;        IF Keyword_Set(galaxy_model_fit) THEN BEGIN
;            *instr_sources[pol_i]+=*gal_model_img[pol_i]
;;            *instr_images[pol_i]-=*gal_holo_img[pol_i]
;        ENDIF
    ENDFOR
    
    stokes_images=stokes_cnv(instr_images,beam=beam_base_out)
    stokes_sources=stokes_cnv(instr_sources,beam=beam_base_out)
    IF Keyword_Set(galaxy_model_fit) THEN BEGIN
        stokes_gal_holo=stokes_cnv(gal_holo_img,beam=beam_base_out)
        stokes_gal_model=stokes_cnv(gal_model_img,beam=beam_base_out)
    ENDIF

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
    ;            temp_out=Strarr(15)
    ;            temp_out[0]=filename
    ;            temp_out[1:*]=[degpix,obs_out.obs_outra,obs_out.obs_outdec,obs_out.zenra,obs_out.zendec,$
    ;               obs_out.obsx,obs_out.obsy,obs_out.zenx,obs_out.zeny,obs_out.rotation,dx,dy,theta,scale]
    ;            textfast,temp_out,filename='alignment'+'v'+strn(version),data_dir=data_directory,/append,/write
                
    ;            RETURN
    
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
            ENDIF ELSE BEGIN
                print,'Alignment failure on:',filename
    ;            temp_out=Strarr(15)
    ;            temp_out[0]=filename
    ;            temp_out[1:*]=[degpix,obs_out.obsra,obs_out.obsdec,obs_out.zenra,obs_out.zendec,obs_out.obsx,$
    ;               obs_out.obsy,obs_out.zenx,obs_out.zeny,obs_out.rotation,'NAN','NAN','NAN','NAN']
    ;            textfast,temp_out,filename='alignment'+'v'+strn(version),data_dir=data_directory,/append,/write
            ENDELSE
        ENDIF ELSE BEGIN
    ;        temp_out=Strarr(15)
    ;        temp_out[0]=filename
    ;        temp_out[1:*]=[degpix,obs_out.obsra,obs_out.obsdec,obs_out.zenra,obs_out.zendec,obs_out.obsx,$
    ;           obs_out.obsy,obs_out.zenx,obs_out.zeny,obs_out.rotation,'NAN','NAN','NAN','NAN']
    ;        textfast,temp_out,filename='alignment'+'v'+strn(version),data_dir=data_directory,/append,/write
        ENDELSE
    ENDIF ELSE n_mrc=0
    
    t5a=Systime(1)
    t4+=t5a-t4a
    
    IF n_mrc GT 2 THEN BEGIN
        mrc_cat=mrc_cat[mrc_i_use]
        mrc_image=source_image_generate(mrc_cat,obs_out,pol_i=4,resolution=16,dimension=dimension,$
            width=pad_uv_image,ring=6.*pad_uv_image)
        mrc_image*=Median(source_arr_out.flux.I)/median(mrc_cat.flux.I)
    ENDIF
    
    IF Keyword_Set(noise_calibrate) THEN BEGIN
        res_I=*instr_images[0]+*instr_images[1]
        res_Is=(res_I-median(res_I,5))
        noise_floor=noise_calibrate
        calibration=noise_floor/Stddev(res_Is[beam_i])
        FOR pol_i=0,npol-1 DO BEGIN
            *instr_images[pol_i]*=calibration
            *dirty_images[pol_i]*=calibration
            *stokes_images[pol_i]*=calibration
            *instr_sources[pol_i]*=calibration
            *stokes_sources[pol_i]*=calibration
        ENDFOR
        IF n_mrc GT 0 THEN mrc_image*=calibration
    ENDIF ELSE calibration=1.
    
    t6a=Systime(1)
    t5+=t6a-t5a
    
    ;FREE MEMORY
    IF Max(Ptr_valid(map_fn_arr)) THEN Ptr_free,map_fn_arr
    Ptr_free,image_uv_arr
    Ptr_free,model_uv_full
    Ptr_free,model_uv_holo
    
;    save,mrc_cat,mrc_image,beam_mask,beam_avg,instr_images,dirty_images,stokes_images,instr_sources,stokes_sources,$
;        model_uv_arr,model_holo_arr,calibration,filename=file_path_fhd+'_output.sav'
;    
;ENDIF ELSE restore,file_path_fhd+'_output.sav'

t7a=Systime(1)
IF Keyword_Set(t6a) THEN t6=t7a-t6a

x_inc=beam_i mod dimension
y_inc=Floor(beam_i/dimension)
zoom_low=min(x_inc)<min(y_inc)
zoom_high=max(x_inc)>max(y_inc)
    
FOR pol_i=0,npol-1 DO BEGIN
    instr_residual=*instr_images[pol_i]
    instr_dirty=*dirty_images[pol_i]
    instr_source=*instr_sources[pol_i]
    instr_restored=instr_residual+instr_source
    beam_use=*beam_base_out[pol_i]
    stokes_residual=(*stokes_images[pol_i])*beam_mask
    stokes_source=(*stokes_sources[pol_i])*beam_mask
    stokes_restored=stokes_residual+stokes_source
    
    t8a=Systime(1)
    export_path_fg=export_path+filter_name+gal_name
    FitsFast,instr_dirty,fits_header,/write,file_path=export_path+filter_name+'_Dirty_'+pol_names[pol_i]
    FitsFast,instr_residual,fits_header,/write,file_path=export_path_fg+'_Residual_'+pol_names[pol_i]
    FitsFast,instr_source,fits_header,/write,file_path=export_path_fg+'_Sources_'+pol_names[pol_i]
    FitsFast,instr_restored,fits_header,/write,file_path=export_path_fg+'_Restored_'+pol_names[pol_i]
    FitsFast,beam_use,fits_header,/write,file_path=export_path+'_Beam_'+pol_names[pol_i]
    FitsFast,Abs(*weights_arr[pol_i])*obs.n_vis,fits_header,/write,file_path=export_path+'_UV_weights_'+pol_names[pol_i]
    
    t9a=Systime(1)
    t8+=t9a-t8a
    Imagefast,Abs(*weights_arr[pol_i])*obs.n_vis,file_path=image_path+'_UV_weights_'+pol_names[pol_i],$
        /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,/log,$
        low=Min(Abs(*weights_arr[pol_i])*obs.n_vis),high=Max(Abs(*weights_arr[pol_i])*obs.n_vis),_Extra=extra
    
    instr_low=Min(instr_residual[beam_i])
    instr_high=Max(instr_residual[beam_i])
    instr_low=instr_low>(-instr_high)
    instrS_high=Max(instr_restored[beam_i])
    log_dirty=0
    log_source=1
    image_path_fg=image_path+filter_name+gal_name
    Imagefast,instr_dirty[zoom_low:zoom_high,zoom_low:zoom_high],file_path=image_path_fg+'_Dirty_'+pol_names[pol_i],$
        /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log_dirty,low=instr_low,high=instr_high,_Extra=extra
    Imagefast,instr_residual[zoom_low:zoom_high,zoom_low:zoom_high],file_path=image_path_fg+'_Residual_'+pol_names[pol_i],$
        /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,low=instr_low,high=instr_high,_Extra=extra
    Imagefast,instr_source[zoom_low:zoom_high,zoom_low:zoom_high],file_path=image_path_fg+'_Sources_'+pol_names[pol_i],$
        /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log_source,low=0,high=instr_high,/invert_color,_Extra=extra
    Imagefast,instr_restored[zoom_low:zoom_high,zoom_low:zoom_high],file_path=image_path_fg+'_Restored_'+pol_names[pol_i],$
        /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log_dirty,low=instr_low,high=instr_high,_Extra=extra
    Imagefast,beam_use[zoom_low:zoom_high,zoom_low:zoom_high]*100.,file_path=image_path+'_Beam_'+pol_names[pol_i],/log,$
        /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,$
        low=min(beam_use[zoom_low:zoom_high,zoom_low:zoom_high]*100),high=max(beam_use[zoom_low:zoom_high,zoom_low:zoom_high]*100),/invert,_Extra=extra
    
    t8b=Systime(1)
    t9+=t8b-t9a
    
    FitsFast,*stokes_images[pol_i],fits_header,/write,file_path=export_path_fg+'_Residual_'+pol_names[pol_i+4]
    FitsFast,*stokes_sources[pol_i],fits_header,/write,file_path=export_path+'_Sources_'+pol_names[pol_i+4]
    FitsFast,*stokes_images[pol_i]+*stokes_sources[pol_i],fits_header,/write,file_path=export_path_fg+'_Restored_'+pol_names[pol_i+4]
    
    t9b=Systime(1)
    t8+=t9b-t8b
    
    stokes_low=Min((stokes_residual*Sqrt(beam_avg>0))[beam_i])
    stokes_high=Max((stokes_residual*Sqrt(beam_avg>0))[beam_i])
    stokes_low=stokes_low>(-stokes_high)
    stokesS_high=Max((stokes_restored*Sqrt(beam_avg>0))[beam_i])
    IF pol_i EQ 0 THEN log_source=1 ELSE log_source=0
    IF pol_i EQ 0 THEN log=0 ELSE log=0
    Imagefast,stokes_residual[zoom_low:zoom_high,zoom_low:zoom_high],file_path=image_path_fg+'_Residual_'+pol_names[pol_i+4],$
        /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,low=stokes_low,high=stokes_high,$
        lat_center=obs_out.obsdec,lon_center=obs_out.obsra,rotation=0,grid_spacing=grid_spacing,degpix=degpix,$
        offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=show_grid,/sphere,_Extra=extra
    Imagefast,stokes_source[zoom_low:zoom_high,zoom_low:zoom_high],file_path=image_path_fg+'_Sources_'+pol_names[pol_i+4],$
        /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log,low=0,high=stokes_high,/invert_color,$
        lat_center=obs_out.obsdec,lon_center=obs_out.obsra,rotation=0,grid_spacing=grid_spacing,degpix=degpix,$
        offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=show_grid,/sphere,_Extra=extra
    Imagefast,stokes_restored[zoom_low:zoom_high,zoom_low:zoom_high],file_path=image_path_fg+'_Restored_'+pol_names[pol_i+4],$
        /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log,low=stokes_low,high=stokes_high,$
        lat_center=obs_out.obsdec,lon_center=obs_out.obsra,rotation=0,grid_spacing=grid_spacing,degpix=degpix,$
        offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=show_grid,/sphere,_Extra=extra
    
    IF pol_i EQ 0 THEN BEGIN
        IF n_mrc GT 0 THEN BEGIN
            mrc_comp=(stokes_source+mrc_image)[zoom_low:zoom_high,zoom_low:zoom_high]
            Imagefast,mrc_comp,file_path=image_path_fg+'_Sources_MRCrings_'+pol_names[pol_i+4],$
                /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,low=0,high=stokes_high,/invert_color,_Extra=extra 
        ENDIF
    ENDIF
    IF Keyword_Set(galaxy_model_fit) THEN BEGIN
        FitsFast,*stokes_gal_model[pol_i],fits_header,/write,file_path=export_path+'_GalModel_'+pol_names[pol_i+4]
        FitsFast,*gal_holo_img[pol_i],fits_header,/write,file_path=export_path+'_GalHolo_'+pol_names[pol_i]
        FitsFast,*gal_model_img[pol_i],fits_header,/write,file_path=export_path+'_GalModel_'+pol_names[pol_i]
        gal_low=0.
        gal_high=Max((*gal_model_img[pol_i])[beam_i])
        gal_log=0
        debug_gal=1
        while debug_gal GT 0 DO BEGIN
            imagefast,(*stokes_gal_model[pol_i])[zoom_low:zoom_high,zoom_low:zoom_high],file_path=image_path_fg+'_GalModel_'+pol_names[pol_i+4],$
                /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=gal_log,low=gal_low,high=gal_high*2.,$
                lat_center=obs_out.obsdec,lon_center=obs_out.obsra,rotation=0,grid_spacing=grid_spacing,degpix=degpix,$
                offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=show_grid,/sphere,_Extra=extra
            imagefast,(*gal_holo_img[pol_i])[zoom_low:zoom_high,zoom_low:zoom_high],file_path=image_path_fg+'_GalHolo_'+pol_names[pol_i],$
                /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=0,low=instr_low,high=instr_high,$
                lat_center=obs_out.obsdec,lon_center=obs_out.obsra,rotation=0,grid_spacing=grid_spacing,degpix=degpix,$
                offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=show_grid,/sphere,_Extra=extra
            imagefast,(*gal_model_img[pol_i])[zoom_low:zoom_high,zoom_low:zoom_high],file_path=image_path_fg+'_GalModel_'+pol_names[pol_i],$
                /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=gal_log,low=gal_low,high=gal_high,$
                lat_center=obs_out.obsdec,lon_center=obs_out.obsra,rotation=0,grid_spacing=grid_spacing,degpix=degpix,$
                offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=show_grid,/sphere,_Extra=extra
            imagefast,(*gal_model_img[pol_i]+instr_source)[zoom_low:zoom_high,zoom_low:zoom_high],file_path=image_path_fg+'_GalCombinedSources_'+pol_names[pol_i],$
                /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=gal_log,low=gal_low,high=gal_high,_Extra=extra
            imagefast,(*gal_model_img[pol_i]+instr_restored)[zoom_low:zoom_high,zoom_low:zoom_high],file_path=image_path_fg+'_GalRestored_'+pol_names[pol_i],$
                /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=gal_log,low=gal_low,high=gal_high,_Extra=extra
            debug_gal=0
            dummyval=0
        endwhile
    ENDIF
    t10a=Systime(1)
    t9+=t10a-t9b
ENDFOR

t10b=Systime(1)
;write sources to a text file
radius=angle_difference(obs_out.obsdec,obs_out.obsra,source_arr_out.dec,source_arr_out.ra,/degree)
Ires=(Qres=fltarr(N_Elements(source_arr_out)))
cx=Round(source_arr_out.x) & cy=Round(source_arr_out.y)
ind_use=where((cx<cy GE 0) AND (cx>cy LE (obs_out.dimension<obs_out.elements)-1))  
Ires[ind_use]=(*stokes_images[0])[cx[ind_use],cy[ind_use]]
IF npol GT 1 THEN Qres[ind_use]=(*stokes_images[1])[cx[ind_use],cy[ind_use]]
source_array_export,source_arr_out,beam_avg,radius=radius,Ires=Ires,Qres=Qres,file_path=export_path+'_source_list2'

radius=angle_difference(obs_out.obsdec,obs_out.obsra,comp_arr.dec,comp_arr.ra,/degree)
Ires=(Qres=fltarr(N_Elements(comp_arr_out)))
cx=Round(comp_arr_out.x) & cy=Round(comp_arr_out.y)
ind_use=where((cx<cy GE 0) AND (cx>cy LE (obs_out.dimension<obs_out.elements)-1))  
Ires[ind_use]=(*stokes_images[0])[cx[ind_use],cy[ind_use]]
IF npol GT 1 THEN Qres[ind_use]=(*stokes_images[1])[cx[ind_use],cy[ind_use]]
source_array_export,comp_arr_out,beam_avg,radius=radius,Ires=Ires,Qres=Qres,file_path=export_path+'_component_list2'

residual_statistics,(*stokes_images[0])*beam_mask,obs_out,fhd,radius=stats_radius,beam_base=beam_base_out,ston=fhd.sigma_cut,/center,$
    file_path_base=image_path_fg,_Extra=extra

t10=Systime(1)-t10b
t00=Systime(1)-t0a

;print,'timing:',[t00,t0,t1],[t2,t3,t4],[t5,t6,t7],[t8,t9,t10]
END