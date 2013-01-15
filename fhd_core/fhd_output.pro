PRO fhd_output,obs,fhd, filename=filename,data_directory=data_directory,version=version,$
    noise_calibrate=noise_calibrate,restore_last=restore_last,coord_debug=coord_debug,silent=silent,show_grid=show_grid,_Extra=extra,$
    fluxfix=fluxfix,align=align

compile_opt idl2,strictarrsubs  
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


vis_path_default,data_directory,filename,file_path,obs=obs,version=version
IF not Keyword_Set(obs) THEN restore,file_path+'_obs.sav'
IF not Keyword_Set(fhd) THEN restore,file_path+'_fhd_params.sav'
version=obs.version
IF N_Elements(show_grid) EQ 0 THEN show_grid=1
version_name='v'+strn(version)
version_dirname='fhd_'+version_name

IF not Keyword_Set(silent) THEN print,'Exporting: ',filename
;output_internal_dir=filepath('',root=data_directory,sub=[version_dirname,'internal'])
export_dir=filepath('',root=data_directory,sub=[version_dirname,filename,'export'])
export_path=filepath(filename,root=rootdir('mwa'),sub=export_dir)

image_dir=filepath('',root=data_directory,sub=[version_dirname,filename,'Images'])
image_path=filepath(filename,root=rootdir('mwa'),sub=image_dir)
IF file_test(rootdir('mwa')+image_dir) EQ 0 THEN file_mkdir,rootdir('mwa')+image_dir
IF file_test(rootdir('mwa')+export_dir) EQ 0 THEN file_mkdir,rootdir('mwa')+export_dir

; *_fhd.sav contains:
;residual_array,dirty_array,image_uv_arr,source_array,comp_arr,model_uv_full,model_uv_holo,normalization_arr,weights_arr,$
;    beam_base,beam_correction,ra_arr,dec_arr,astr
restore,file_path+'_fhd.sav'

IF Keyword_Set(fluxfix) THEN BEGIN
    source_array.flux.I/=2
    comp_arr.flux.I/=2
    source_array.ston/=2
    
    save,residual_array,dirty_array,image_uv_arr,source_array,comp_arr,model_uv_full,model_uv_holo,normalization,weights_arr,$
        beam_base,beam_correction,ra_arr,dec_arr,astr,filename=file_path+'_fhd.sav'
ENDIF
;

IF N_Elements(normalization_arr) GT 0 THEN normalization=Mean(normalization_arr)/2.
npol=fhd.npol
dimension=obs.dimension
elements=obs.elements
zoom_radius=Round(18./(obs.degpix)/16.)*16.
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

si_use=where(source_array.ston GE fhd.sigma_cut,ns_use)
source_arr=source_array[si_use]

;Build a fits header
mkhdr,fits_header,*residual_array[0]
astr=obs.astr
putast, fits_header, astr, cd_type=1

t1a=Systime(1)
t0+=t1a-t0a

IF not Keyword_Set(restore_last) THEN BEGIN

    map_fn_arr=Ptrarr(npol,/allocate)
    FOR pol_i=0,npol-1 DO BEGIN
        holo_mapfn_generate,obs,/restore_last,map_fn=map_fn_single,polarization=pol_i
        *map_fn_arr[pol_i]=map_fn_single
    ENDFOR
    
    t2a=Systime(1)
    t1+=t2a-t1a
    
    ;beam_threshold=0.05
    beam_mask=fltarr(dimension,elements)+1
    beam_avg=fltarr(dimension,elements)
    FOR pol_i=0,(npol<2)-1 DO BEGIN
        beam_mask_test=*beam_base[pol_i];*(*p_map_simple[pol_i]);*(ps_not_used*2.)
        beam_i=region_grow(beam_mask_test,dimension/2.+dimension*elements/2.,threshold=[fhd.beam_threshold,Max(beam_mask_test)])
        beam_mask0=fltarr(dimension,elements) & beam_mask0[beam_i]=1.
        beam_avg+=*beam_base[pol_i]
        beam_mask*=beam_mask0
    ENDFOR
    beam_avg/=(npol<2)
    beam_i=where(beam_mask)
    
    t3a=Systime(1)
    t2+=t3a-t2a
    
    instr_images=Ptrarr(npol,/allocate)
    instr_sources=Ptrarr(npol,/allocate)
    
    model_uv_arr=Ptrarr(npol,/allocate)
    model_holo_arr=Ptrarr(npol,/allocate)
    
    source_uv_mask=fltarr(dimension,elements)
    FOR pol_i=0,npol-1 DO BEGIN
        mask_i=where(*image_uv_arr[pol_i],n_mask_use)
        IF n_mask_use GT 0 THEN source_uv_mask[mask_i]=1
    ENDFOR
    IF Total(source_uv_mask) EQ 0 THEN source_uv_mask+=1
    
    FOR pol_i=0,npol-1 DO BEGIN
        *model_uv_arr[pol_i]=source_array_model(source_arr,pol_i=pol_i,dimension=dimension,beam_correction=beam_correction,mask=source_uv_mask)
;        *model_uv_arr[pol_i]=source_array_model(source_arr,pol_i=pol_i,dimension=dimension)
        *model_holo_arr[pol_i]=holo_mapfn_apply(*model_uv_arr[pol_i],*map_fn_arr[pol_i])*normalization
        *instr_images[pol_i]=dirty_image_generate(*image_uv_arr[pol_i]-*model_holo_arr[pol_i])*(*beam_correction[pol_i])
        *instr_sources[pol_i]=source_image_generate(source_arr,obs,pol_i=pol_i,resolution=8,dimension=dimension,width=.75)
    ENDFOR
    
    stokes_images=stokes_cnv(instr_images,beam=beam_base)
    stokes_sources=stokes_cnv(instr_sources,beam=beam_base)

    t4a=Systime(1)
    t3+=t4a-t3a
    
    mrc_cat=mrc_catalog_read(astr)
    mrc_i_use=where((mrc_cat.x GE zoom_low) AND (mrc_cat.x LE zoom_high) AND (mrc_cat.y GE zoom_low) AND (mrc_cat.y LE zoom_high),n_mrc)
    
    IF Keyword_Set(align) THEN BEGIN 
        error=0
        source_array1_base=fltarr(6,ns_use)
        source_array1_base[0,*]=source_arr.x
        source_array1_base[1,*]=source_arr.y
        source_array1_base[4,*]=source_arr.flux.I
        source_array1_base[5,*]=Round(source_arr.x)+Round(source_arr.y)*dimension
        source_array2_base=fltarr(6,n_mrc)
        source_array2_base[0,*]=mrc_cat[mrc_i_use].x
        source_array2_base[1,*]=mrc_cat[mrc_i_use].y
        source_array2_base[4,*]=mrc_cat[mrc_i_use].flux.I
        source_array2_base[5,*]=Round(mrc_cat[mrc_i_use].x)+Round(mrc_cat[mrc_i_use].y)*dimension
        image_align,fltarr(dimension,elements),fltarr(dimension,elements),dx,dy,theta,scale,source_array1_base,source_array2_base,source_array1_out,source_array2_out,$
            radius=128.,sub_max_number=32,final_match=20,binsize=1.,error=error,min_radius=32,fix_scale=1.,fix_theta=0
         
        IF not Keyword_Set(error) THEN BEGIN
        
            print,"Alignment success. Dx:",dx,' Dy:',dy,' Theta:',theta,' Scale:',scale
            temp_out=Strarr(15)
            temp_out[0]=filename
            temp_out[1:*]=[obs.degpix,obs.obsra,obs.obsdec,obs.zenra,obs.zendec,obs.obsx,obs.obsy,obs.zenx,obs.zeny,obs.rotation,dx,dy,theta,scale]
            textfast,temp_out,filename='alignment'+'v'+strn(version),data_dir=data_directory,/append,/write
            
;            RETURN

            IF (Abs(dx)>Abs(dy)) LE 50. AND (Abs(theta) LE 45.) THEN BEGIN            
                x1_vals=source_arr.x
                y1_vals=source_arr.y
                x1a_vals=Scale*(x1_vals*Cos(theta*!DtoR)-y1_vals*Sin(theta*!DtoR))+dx
                y1a_vals=Scale*(y1_vals*Cos(theta*!DtoR)+x1_vals*Sin(theta*!DtoR))+dy
                ra_vals=Interpolate(ra_arr,x1a_vals,y1a_vals)
                dec_vals=Interpolate(dec_arr,x1a_vals,y1a_vals)
                source_arr.ra=ra_vals
                source_arr.dec=dec_vals
                
                x2_vals=mrc_cat[mrc_i_use].x
                y2_vals=mrc_cat[mrc_i_use].y
                x2a_vals=(1./Scale)*((x2_vals-dx)*Cos(theta*!DtoR)+(y2_vals-dy)*Sin(theta*!DtoR))
                y2a_vals=(1./Scale)*((y2_vals-dy)*Cos(theta*!DtoR)-(x2_vals-dx)*Sin(theta*!DtoR))
                mrc_cat[mrc_i_use].x=x2a_vals
                mrc_cat[mrc_i_use].y=y2a_vals
            ENDIF
        ENDIF ELSE BEGIN
            print,'Alignment failure on:',filename
            temp_out=Strarr(15)
            temp_out[0]=filename
            temp_out[1:*]=[obs.degpix,obs.obsra,obs.obsdec,obs.zenra,obs.zendec,obs.obsx,obs.obsy,obs.zenx,obs.zeny,obs.rotation,'NAN','NAN','NAN','NAN']
            textfast,temp_out,filename='alignment'+'v'+strn(version),data_dir=data_directory,/append,/write
        ENDELSE
    ENDIF ELSE BEGIN
        temp_out=Strarr(15)
        temp_out[0]=filename
        temp_out[1:*]=[obs.degpix,obs.obsra,obs.obsdec,obs.zenra,obs.zendec,obs.obsx,obs.obsy,obs.zenx,obs.zeny,obs.rotation,'NAN','NAN','NAN','NAN']
        textfast,temp_out,filename='alignment'+'v'+strn(version),data_dir=data_directory,/append,/write
    ENDELSE
    
    t5a=Systime(1)
    t4+=t5a-t4a
;    RETURN
    
    IF n_mrc GT 0 THEN BEGIN
        mrc_cat=mrc_cat[mrc_i_use]
        mrc_image=source_image_generate(mrc_cat,obs,pol_i=4,resolution=8,dimension=dimension,width=1,ring=6.)
        mrc_image*=Median(source_arr.flux.I)/median(mrc_cat.flux.I)
    ENDIF
    
    IF Keyword_Set(noise_calibrate) THEN BEGIN
        res_I=*instr_images[0]+*instr_images[1]
        res_Is=(res_I-median(res_I,5))
        noise_floor=noise_calibrate
        calibration=noise_floor/Stddev(res_Is[beam_i])
        FOR pol_i=0,npol-1 DO BEGIN
            *instr_images[pol_i]*=calibration
            *stokes_images[pol_i]*=calibration
            *instr_sources[pol_i]*=calibration
            *stokes_sources[pol_i]*=calibration
        ENDFOR
        IF n_mrc GT 0 THEN mrc_image*=calibration
    ENDIF ELSE calibration=1.
    
    t6a=Systime(1)
    t5+=t6a-t5a
;    beam_est=Ptrarr(npol,/allocate)
;    FOR pol_i=0,npol-1 DO BEGIN
;        beam_est_single=beam_estimate(*instr_images[pol_i],radius=20.,nsigma=3,beam_model=*beam_base[pol_i])
;        *beam_est[pol_i]=beam_est_single
;    ENDFOR
    
    
    save,mrc_cat,mrc_image,beam_mask,beam_avg,instr_images,stokes_images,instr_sources,stokes_sources,$
        beam_est,model_uv_arr,model_holo_arr,calibration,p_map_simple,p_corr_simple,filename=file_path+'_output.sav'
    
ENDIF ELSE restore,file_path+'_output.sav'

t7a=Systime(1)
IF Keyword_Set(t6a) THEN t6=t7a-t6a

IF N_Elements(beam_est) EQ 0 THEN beam_est_flag=0 ELSE beam_est_flag=1

Imagefast,dec_arr,filename=filename+' Dec',$
    data_dir=image_dir,/right,sig=2,color_table=0,back='white',reverse_image=reverse_image,low=Min(dec_arr),high=Max(dec_arr),$
    lat_center=obs.obsdec,lon_center=obs.obsra,rotation=0,grid_spacing=grid_spacing,degpix=obs.degpix,$
    offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,/show,/sphere
Imagefast,ra_arr,filename=filename+' RA',$
    data_dir=image_dir,/right,sig=2,color_table=0,back='white',reverse_image=reverse_image,low=Min(ra_arr),high=Max(ra_arr),$
    lat_center=obs.obsdec,lon_center=obs.obsra,rotation=0,grid_spacing=grid_spacing,degpix=obs.degpix,$
    offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,/show,/sphere
    
FOR pol_i=0,npol-1 DO BEGIN
;    IF Keyword_Set(coord_debug) THEN BEGIN
;;        instr_residual=Reverse(reverse(*instr_images[pol_i],1),2)
;;        instr_source=Reverse(reverse(*instr_sources[pol_i],1),2)
;;        instr_restored=Reverse(reverse(instr_residual+instr_source,1),2)
;;        beam_use=Reverse(reverse(*beam_base[pol_i],1),2)
;;        beam_est_use=Reverse(reverse(*beam_est[pol_i],1),2)
;;        beam_diff=beam_use-beam_est_use
;;        stokes_residual=Reverse(reverse((*stokes_images[pol_i])*beam_mask,1),2)
;;        stokes_source=Reverse(reverse((*stokes_sources[pol_i])*beam_mask,1),2)
;;        stokes_restored=Reverse(reverse(stokes_residual+stokes_source,1),2)
;;        mrc_image=Reverse(reverse(mrc_image,1),2)
;        instr_residual=reverse(*instr_images[pol_i],1)
;        instr_source=reverse(*instr_sources[pol_i],1)
;        instr_restored=reverse(instr_residual+instr_source,1)
;;        beam_use=reverse(*beam_base[pol_i]*(*p_map_simple[pol_i]),1)*2.
;        beam_use=reverse(*beam_base[pol_i],1)
;        IF beam_est_flag THEN BEGIN
;            beam_est_use=reverse(*beam_est[pol_i],1)
;            beam_diff=beam_use-beam_est_use
;        ENDIF
;        stokes_residual=reverse((*stokes_images[pol_i])*beam_mask,1)
;        stokes_source=reverse((*stokes_sources[pol_i])*beam_mask,1)
;        stokes_restored=reverse(stokes_residual+stokes_source,1)
;        mrc_image=reverse(mrc_image,1)
;    ENDIF ELSE BEGIN
        instr_residual=*instr_images[pol_i]
        instr_source=*instr_sources[pol_i]
        instr_restored=instr_residual+instr_source
        beam_use=*beam_base[pol_i];*(*p_map_simple[pol_i])*2.
        IF beam_est_flag THEN BEGIN
            beam_est_use=*beam_est[pol_i]
            beam_diff=beam_use-beam_est_use
        ENDIF
        stokes_residual=(*stokes_images[pol_i])*beam_mask
        stokes_source=(*stokes_sources[pol_i])*beam_mask
        stokes_restored=stokes_residual+stokes_source
;    ENDELSE
    
    t8a=Systime(1)
    
    FitsFast,instr_residual,fits_header,/write,filename=filename+'_Residual_'+pol_names[pol_i],data_dir=export_dir
    FitsFast,instr_source,fits_header,/write,filename=filename+'_Sources_'+pol_names[pol_i],data_dir=export_dir
    FitsFast,instr_restored,fits_header,/write,filename=filename+'_Restored_'+pol_names[pol_i],data_dir=export_dir
    FitsFast,beam_use,fits_header,/write,filename=filename+'_Beam_'+pol_names[pol_i],data_dir=export_dir
    IF beam_est_flag THEN FitsFast,beam_est_use,fits_header,/write,filename=filename+'_Beam_estimate_'+pol_names[pol_i],data_dir=export_dir
    
    t9a=Systime(1)
    t8+=t9a-t8a
    
    instr_low=Min(instr_residual[where(beam_mask)])
    instr_high=Max(instr_residual[where(beam_mask)])
    instrS_high=Max(instr_restored[where(beam_mask)])
    Imagefast,instr_residual,filename=filename+'_Residual_'+pol_names[pol_i],$
        data_dir=image_dir,/right,sig=2,color_table=0,back='white',reverse_image=reverse_image,low=instr_low,high=instr_high;,$
;        lat_center=obs.obsdec,lon_center=obs.obsra,rotation=0,grid_spacing=grid_spacing,degpix=obs.degpix,$
;        offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=show_grid,/sphere
    Imagefast,instr_source,filename=filename+'_Sources_'+pol_names[pol_i],$
        data_dir=image_dir,/right,sig=2,color_table=0,back='white',reverse_image=reverse_image,/log,low=0,high=instrS_high,/invert_color;,$
;        lat_center=obs.obsdec,lon_center=obs.obsra,rotation=0,grid_spacing=grid_spacing,degpix=obs.degpix,$
;        offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=show_grid,/sphere
    Imagefast,instr_restored,filename=filename+'_Restored_'+pol_names[pol_i],$
        data_dir=image_dir,/right,sig=2,color_table=0,back='white',reverse_image=reverse_image,/log,low=instr_low,high=instrS_high;,$
;        lat_center=obs.obsdec,lon_center=obs.obsra,rotation=0,grid_spacing=grid_spacing,degpix=obs.degpix,$
;        offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=show_grid,/sphere
;    Imagefast,beam_use,filename=filename+'_Beam_'+pol_names[pol_i],/log,$
;        data_dir=image_dir,/right,sig=2,color_table=0,back='white',reverse_image=reverse_image,low=min(beam_use),high=max(beam_use)
    Imagefast,beam_use*100.,filename=filename+'_Beam_'+pol_names[pol_i],/log,$
        data_dir=image_dir,/right,sig=2,color_table=0,back='white',reverse_image=reverse_image,$
        low=min(beam_use*100),high=max(beam_use*100),/invert
        
    IF beam_est_flag THEN BEGIN
        Imagefast,beam_est_use*100.,filename=filename+'_Beam_estimate_'+pol_names[pol_i],/log,$
            data_dir=image_dir,/right,sig=2,color_table=0,back='white',reverse_image=reverse_image,$
            low=min(beam_est_use*100),high=max(beam_est_use*100),/invert
        Imagefast,beam_diff*100.,filename=filename+'_Beam_diff_'+pol_names[pol_i],log=0,$
            data_dir=image_dir,/right,sig=2,color_table=0,back='white',reverse_image=reverse_image,$
            low=min(beam_diff*100)>(-10.),high=max(beam_diff*100)<10,/invert
    ENDIF
    
    t8b=Systime(1)
    t9+=t8b-t9a
    
    FitsFast,*stokes_images[pol_i],fits_header,/write,filename=filename+'_Residual_'+pol_names[pol_i+4],data_dir=export_dir
    FitsFast,*stokes_sources[pol_i],fits_header,/write,filename=filename+'_Sources_'+pol_names[pol_i+4],data_dir=export_dir
    FitsFast,*stokes_images[pol_i]+*stokes_sources[pol_i],fits_header,/write,filename=filename+'_Restored_'+pol_names[pol_i+4],data_dir=export_dir
    
    t9b=Systime(1)
    t8+=t9b-t8b
    
    stokes_low=Min((stokes_residual*Sqrt(beam_avg>0))[where(beam_mask)])
    stokes_high=Max((stokes_residual*Sqrt(beam_avg>0))[where(beam_mask)])
    stokesS_high=Max(stokes_restored[where(beam_mask)])
    IF pol_i EQ 0 THEN log=1 ELSE log=0
    Imagefast,stokes_residual[zoom_low:zoom_high,zoom_low:zoom_high],filename=filename+'_Residual_'+pol_names[pol_i+4],$
        data_dir=image_dir,/right,sig=2,color_table=0,back='white',reverse_image=reverse_image,low=stokes_low,high=stokes_high,$
        lat_center=obs.obsdec,lon_center=obs.obsra,rotation=0,grid_spacing=grid_spacing,degpix=obs.degpix,$
        offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=show_grid,/sphere
    Imagefast,stokes_source[zoom_low:zoom_high,zoom_low:zoom_high],filename=filename+'_Sources_'+pol_names[pol_i+4],$
        data_dir=image_dir,/right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log,low=0,high=stokesS_high,/invert_color,$
        lat_center=obs.obsdec,lon_center=obs.obsra,rotation=0,grid_spacing=grid_spacing,degpix=obs.degpix,$
        offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=show_grid,/sphere
    Imagefast,stokes_restored[zoom_low:zoom_high,zoom_low:zoom_high],filename=filename+'_Restored_'+pol_names[pol_i+4],$
        data_dir=image_dir,/right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log,low=stokes_low,high=stokesS_high,$
        lat_center=obs.obsdec,lon_center=obs.obsra,rotation=0,grid_spacing=grid_spacing,degpix=obs.degpix,$
        offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=show_grid,/sphere
    
    IF pol_i EQ 0 THEN BEGIN
        n_mrc=(size(mrc_cat,/dimension))[0]
        IF n_mrc GT 0 THEN BEGIN
            mrc_comp=(stokes_source+mrc_image)[zoom_low:zoom_high,zoom_low:zoom_high]
            Imagefast,mrc_comp,filename=filename+'_Sources_MRCrings_'+pol_names[pol_i+4],$
                data_dir=image_dir,/right,sig=2,color_table=0,back='white',reverse_image=reverse_image,low=0,high=stokes_high,/invert_color    
        ENDIF
    ENDIF
    
    t10a=Systime(1)
    t9+=t10a-t9b
ENDFOR

t10b=Systime(1)
;write sources to a text file
radius=angle_difference(obs.obsdec,obs.obsra,source_arr.dec,source_arr.ra,/degree)
source_array_export,source_arr,beam_avg,radius,filename=filename+'_source_list',data_directory=export_dir

;old .sav files had source_array_full instead of comp_arr, so check for that here
IF N_Elements(comp_arr) EQ 0 THEN comp_arr=source_array_full
radius=angle_difference(obs.obsdec,obs.obsra,comp_arr.dec,comp_arr.ra,/degree)
source_array_export,comp_arr,beam_avg,radius,filename=filename+'_component_list',data_directory=export_dir

residual_statistics,(*stokes_images[0])*beam_mask,obs,fhd,radius=stats_radius,beam_base=beam_base,ston=fhd.sigma_cut,/center,$
    data_directory=image_dir,filename=filename

t10=Systime(1)-t10b
t00=Systime(1)-t0a

;print,'timing:',[t00,t0,t1],[t2,t3,t4],[t5,t6,t7],[t8,t9,t10]
END