PRO fhd_quickview,obs,psf,cal,image_uv_arr=image_uv_arr,weights_arr=weights_arr,source_array=source_array,$
    model_uv_arr=model_uv_arr,file_path_fhd=file_path_fhd,silent=silent,$
    gridline_image_show=gridline_image_show,pad_uv_image=pad_uv_image,image_filter_fn=image_filter_fn,$
    zoom_low=zoom_low,zoom_high=zoom_high,grid_spacing=grid_spacing,reverse_image=reverse_image,$
    no_fits=no_fits,no_png=no_png,ring_radius=ring_radius,_Extra=extra


basename=file_basename(file_path_fhd)
dirpath=file_dirname(file_path_fhd)
IF not Keyword_Set(silent) THEN print,'Exporting (quickview): ',basename
export_path=filepath(basename,root=dirpath,sub='export')
export_dir=file_dirname(export_path)

image_path=filepath(basename,root=dirpath,sub='images')
image_dir=file_dirname(image_path)
IF file_test(image_dir) EQ 0 THEN file_mkdir,image_dir
IF file_test(export_dir) EQ 0 THEN file_mkdir,export_dir


IF N_Elements(obs) EQ 0 THEN RESTORE,file_path_fhd+'_obs.sav' 
IF N_Elements(psf) EQ 0 THEN IF file_test(file_path_fhd+'_beams.sav') THEN RESTORE,file_path_fhd+'_beams.sav' ELSE $
    psf=beam_setup(obs,file_path_fhd,silent=silent,timing=t_beam,_Extra=extra)
IF N_Elements(cal) EQ 0 THEN IF file_test(file_path_fhd+'_cal.sav') THEN RESTORE,file_path_fhd+'_cal.sav'
    
n_pol=obs.n_pol
dimension_uv=obs.dimension
astr=obs.astr
restored_beam_width=(!RaDeg/(obs.MAX_BASELINE/obs.KPIX)/obs.degpix)/(2.*Sqrt(2.*Alog(2.)))
restored_beam_width=restored_beam_width>0.75
pol_names=['xx','yy','xy','yx','I','Q','U','V']

IF N_Elements(image_uv_arr) EQ 0 THEN BEGIN
    image_uv_arr=Ptrarr(n_pol,/allocate)
    FOR pol_i=0,n_pol-1 DO *image_uv_arr[pol_i]=getvar_savefile(file_path_fhd+'_uv_'+pol_names[pol_i]+'.sav','dirty_uv');*obs.cal[pol_i]
ENDIF
IF N_Elements(weights_arr) EQ 0 THEN BEGIN
    weights_arr=Ptrarr(n_pol)
    IF file_test(file_path_fhd+'_uv_'+pol_names[0]+'.sav') THEN $
        FOR pol_i=0,n_pol-1 DO weights_arr[pol_i]=Ptr_new(getvar_savefile(file_path_fhd+'_uv_'+pol_names[pol_i]+'.sav','weights_grid'))
ENDIF 

IF Min(Ptr_valid(weights_arr)) EQ 0 THEN FOR pol_i=0,n_pol-1 DO weights_arr[pol_i]=Ptr_new(Abs(*image_uv_arr[pol_i]))
FOR pol_i=0,n_pol-1 DO IF Total(Abs(*weights_arr[pol_i])) EQ 0 THEN weights_arr[pol_i]=Ptr_new(Abs(*image_uv_arr[pol_i]))

IF Keyword_Set(image_filter_fn) THEN BEGIN
    dummy_img=Call_function(image_filter_fn,fltarr(2,2),name=filter_name)
    IF Keyword_Set(filter_name) THEN filter_name='_'+filter_name ELSE filter_name=''
ENDIF ELSE filter_name=''

IF Keyword_Set(pad_uv_image) THEN BEGIN
    pad_uv_image=pad_uv_image>1.
    
    restored_beam_width*=pad_uv_image
    
    obs_out=obs
    astr_out=astr
    astr_out.cdelt/=pad_uv_image
    astr_out.crpix*=pad_uv_image
    
    obs_out.astr=astr_out
    obs_out.dimension*=pad_uv_image
    obs_out.elements*=pad_uv_image
    obs_out.obsx*=pad_uv_image
    obs_out.obsy*=pad_uv_image
    obs_out.zenx*=pad_uv_image
    obs_out.zeny*=pad_uv_image
    obs_out.degpix/=pad_uv_image
ENDIF ELSE obs_out=obs
dimension=obs_out.dimension
elements=obs_out.elements
degpix=obs_out.degpix
astr_out=obs_out.astr

;stats_radius=10. ;degrees

grid_spacing=10.
offset_lat=5.;15. paper 10 memo
offset_lon=5.;15. paper 10 memo
reverse_image=0   ;1: reverse x axis, 2: y-axis, 3: reverse both x and y axes
map_reverse=0;1 paper 3 memo
label_spacing=1.

beam_mask=fltarr(dimension,elements)+1
beam_avg=fltarr(dimension,elements)
beam_base_out=Ptrarr(n_pol,/allocate)
beam_correction_out=Ptrarr(n_pol,/allocate)
FOR pol_i=0,n_pol-1 DO BEGIN
    beam_base=Sqrt(beam_image(psf,obs,pol_i=pol_i,/square)>0.)
    *beam_base_out[pol_i]=Rebin(beam_base,dimension,elements) ;should be fine even if pad_uv_image is not set
    *beam_correction_out[pol_i]=weight_invert(*beam_base_out[pol_i],1e-2)
    IF pol_i GT 1 THEN CONTINUE
    beam_mask_test=*beam_base_out[pol_i]
    beam_i=region_grow(beam_mask_test,dimension/2.+dimension*elements/2.,threshold=[1e-2,Max(beam_mask_test)])
    beam_mask0=fltarr(dimension,elements) & beam_mask0[beam_i]=1.
    beam_avg+=*beam_base_out[pol_i]^2.
    beam_mask*=beam_mask0
ENDFOR
beam_mask[0:dimension/4.-1,*]=0 & beam_mask[3.*dimension/4.:dimension-1,*]=0 
beam_mask[*,0:elements/4.-1]=0 & beam_mask[*,3.*elements/4.:elements-1]=0 
beam_i=where(beam_mask)
beam_avg/=(n_pol<2)
beam_avg=Sqrt(beam_avg>0)*beam_mask

IF N_Elements(source_array) GT 0 THEN BEGIN
    source_flag=1
    source_arr_out=source_array
    sx=(source_array.x-obs.dimension/2.)*2.+obs_out.dimension/2.
    sy=(source_array.y-obs.elements/2.)*2.+obs_out.elements/2.
    source_arr_out.x=sx & source_arr_out.y=sy
    IF Total(source_arr_out.flux.(0)) EQ 0 THEN BEGIN
        source_arr_out.flux.(0)=(*beam_base_out[0])[source_arr_out.x,source_arr_out.y]*(source_arr_out.flux.I+source_arr_out.flux.Q)/2.
        source_arr_out.flux.(1)=(*beam_base_out[1])[source_arr_out.x,source_arr_out.y]*(source_arr_out.flux.I-source_arr_out.flux.Q)/2.
;        source_arr_out.flux.(2)=(*beam_base_out[2])[source_arr_out.x,source_arr_out.y]*(source_arr_out.flux.Q+source_arr_out.flux.U)/2.
;        source_arr_out.flux.(3)=(*beam_base_out[3])[source_arr_out.x,source_arr_out.y]*(source_arr_out.flux.Q-source_arr_out.flux.U)/2.
    ENDIF
    
    extend_test=where(Ptr_valid(source_arr_out.extend),n_extend)
    IF n_extend GT 0 THEN BEGIN
        FOR ext_i=0L,n_extend-1 DO BEGIN
            comp_arr_out=*source_array[extend_test[ext_i]].extend
            ad2xy,comp_arr_out.ra,comp_arr_out.dec,astr_out,cx,cy
            comp_arr_out.x=cx & comp_arr_out.y=cy
            
            IF Total(comp_arr_out.flux.(0)) EQ 0 THEN BEGIN
                comp_arr_out.flux.(0)=(*beam_base_out[0])[comp_arr_out.x,comp_arr_out.y]*(comp_arr_out.flux.I+comp_arr_out.flux.Q)/2.
                comp_arr_out.flux.(1)=(*beam_base_out[1])[comp_arr_out.x,comp_arr_out.y]*(comp_arr_out.flux.I-comp_arr_out.flux.Q)/2.
        ;        comp_arr_out.flux.(2)=(*beam_base_out[2])[comp_arr_out.x,comp_arr_out.y]*(comp_arr_out.flux.Q+comp_arr_out.flux.U)/2.
        ;        comp_arr_out.flux.(3)=(*beam_base_out[3])[comp_arr_out.x,comp_arr_out.y]*(comp_arr_out.flux.Q-comp_arr_out.flux.U)/2.
            ENDIF
            source_arr_out[extend_test[ext_i]].extend=Ptr_new(/allocate)
            *source_arr_out[extend_test[ext_i]].extend=comp_arr_out
        ENDFOR
    ENDIF
ENDIF ELSE source_flag=0

instr_images=Ptrarr(n_pol)
instr_sources=Ptrarr(n_pol)
instr_rings=Ptrarr(n_pol)
FOR pol_i=0,n_pol-1 DO BEGIN
    instr_images[pol_i]=Ptr_new(dirty_image_generate(*image_uv_arr[pol_i],degpix=degpix,weights=*weights_arr[pol_i],$
        image_filter_fn=image_filter_fn,pad_uv_image=pad_uv_image,_Extra=extra)*(*beam_correction_out[pol_i]))
    IF source_flag THEN BEGIN
        IF Keyword_Set(ring_radius) THEN instr_rings[pol_i]=Ptr_new(source_image_generate(source_arr_out,obs_out,pol_i=pol_i,resolution=16,$
            dimension=dimension,width=restored_beam_width,ring_radius=ring_radius,_Extra=extra))
        instr_sources[pol_i]=Ptr_new(source_image_generate(source_arr_out,obs_out,pol_i=pol_i,resolution=16,$
            dimension=dimension,width=restored_beam_width,_Extra=extra))
    ENDIF
ENDFOR
stokes_images=stokes_cnv(instr_images,beam=beam_base_out)
IF source_flag THEN BEGIN
    stokes_sources=stokes_cnv(instr_sources,beam=beam_base_out) ;returns null pointer if instr_sources is a null pointer 
    IF Keyword_Set(ring_radius) THEN stokes_rings=stokes_cnv(instr_rings,beam=beam_base_out) 
ENDIF    

;Build a fits header
mkhdr,fits_header,*instr_images[0]
putast, fits_header, astr_out;, cd_type=1

x_inc=beam_i mod dimension
y_inc=Floor(beam_i/dimension)
IF N_Elements(zoom_low) EQ 0 THEN zoom_low=min(x_inc)<min(y_inc)
IF N_Elements(zoom_high) EQ 0 THEN zoom_high=max(x_inc)>max(y_inc)

FOR pol_i=0,n_pol-1 DO BEGIN
    instr_residual=*instr_images[pol_i]
    stokes_residual=(*stokes_images[pol_i])*beam_mask
    IF source_flag THEN BEGIN
        instr_source=*instr_sources[pol_i]
        instr_restored=instr_residual+(Keyword_Set(ring_radius) ? *instr_rings[pol_i]:instr_sourc)
        stokes_source=(*stokes_sources[pol_i])*beam_mask
        stokes_restored=stokes_residual+(Keyword_Set(ring_radius) ? *stokes_rings[pol_i]:stokes_source)
    ENDIF
    beam_use=*beam_base_out[pol_i]
    
    instr_low=Min(instr_residual[beam_i])>(-5.*Stddev(instr_residual[beam_i]))
    instr_high=Max(instr_residual[beam_i])<(10.*Stddev(instr_residual[beam_i]))
    
    instr_low=instr_low>(-instr_high)    
    stokes_low=Min((stokes_residual*Sqrt(beam_avg>0))[beam_i])>(-5.*Stddev((stokes_residual*Sqrt(beam_avg>0))[beam_i]))
    stokes_high=Max((stokes_residual*Sqrt(beam_avg>0))[beam_i])<(10.*Stddev((stokes_residual*Sqrt(beam_avg>0))[beam_i]))
    stokes_low=stokes_low>(-stokes_high)
    log_dirty=0
    log_source=1
    
    IF ~Keyword_Set(no_png) THEN BEGIN
;        Imagefast,Abs(*weights_arr[pol_i])*obs.n_vis,file_path=image_path+'_UV_weights_'+pol_names[pol_i],$
;            /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,/log,$
;            low=Min(Abs(*weights_arr[pol_i])*obs.n_vis),high=Max(Abs(*weights_arr[pol_i])*obs.n_vis),_Extra=extra
        
        Imagefast,instr_residual[zoom_low:zoom_high,zoom_low:zoom_high],file_path=image_path+filter_name+'_Residual_'+pol_names[pol_i],$
            /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,low=instr_low,high=instr_high,show_grid=show_grid,_Extra=extra
        Imagefast,beam_use[zoom_low:zoom_high,zoom_low:zoom_high]*100.,file_path=image_path+'_Beam_'+pol_names[pol_i],/log,$
            /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,show_grid=show_grid,$
            low=min(beam_use[zoom_low:zoom_high,zoom_low:zoom_high]*100),high=max(beam_use[zoom_low:zoom_high,zoom_low:zoom_high]*100),/invert,_Extra=extra
        Imagefast,stokes_residual[zoom_low:zoom_high,zoom_low:zoom_high],file_path=image_path+filter_name+'_Residual_'+pol_names[pol_i+4],$
            /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,low=stokes_low,high=stokes_high,$
            lat_center=obs_out.obsdec,lon_center=obs_out.obsra,rotation=0,grid_spacing=grid_spacing,degpix=degpix,$
            offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=show_grid,/sphere,_Extra=extra
    ENDIF
    IF ~Keyword_Set(no_fits) THEN BEGIN
        FitsFast,stokes_residual,fits_header,/write,file_path=export_path+filter_name+'_Residual_'+pol_names[pol_i+4]
        FitsFast,instr_residual,fits_header,/write,file_path=export_path+filter_name+'_Residual_'+pol_names[pol_i]
        FitsFast,beam_use,fits_header,/write,file_path=export_path+'_Beam_'+pol_names[pol_i]
;        FitsFast,Abs(*weights_arr[pol_i])*obs.n_vis,fits_header,/write,file_path=export_path+'_UV_weights_'+pol_names[pol_i]
    ENDIF
    
    IF pol_i EQ 0 THEN log_source=1 ELSE log_source=0
    IF pol_i EQ 0 THEN log=0 ELSE log=0
    IF source_flag THEN BEGIN
        IF Keyword_Set(ring_radius) THEN restored_name='_Restored_rings_' ELSE restored_name='_Restored_'
        IF ~Keyword_Set(no_fits) THEN BEGIN
    ;        FitsFast,instr_source,fits_header,/write,file_path=export_path+filter_name+'_Sources_'+pol_names[pol_i]
            FitsFast,instr_residual+instr_source,fits_header,/write,file_path=export_path+filter_name+restored_name+pol_names[pol_i]
    ;        FitsFast,stokes_source,fits_header,/write,file_path=export_path+'_Sources_'+pol_names[pol_i+4]
            FitsFast,stokes_residual+stokes_source,fits_header,/write,file_path=export_path+filter_name+restored_name+pol_names[pol_i+4]
        ENDIF
        IF ~Keyword_Set(no_png) THEN BEGIN
            instrS_high=Max(instr_restored[beam_i])
            stokesS_high=Max((stokes_restored*Sqrt(beam_avg>0))[beam_i])
            IF pol_i EQ 0 THEN Imagefast,stokes_source[zoom_low:zoom_high,zoom_low:zoom_high],file_path=image_path+filter_name+'_Sources_'+pol_names[pol_i+4],$
                /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log,low=0,high=stokes_high,/invert_color,$
                lat_center=obs_out.obsdec,lon_center=obs_out.obsra,rotation=0,grid_spacing=grid_spacing,degpix=degpix,$
                offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=show_grid,/sphere,_Extra=extra
            Imagefast,stokes_restored[zoom_low:zoom_high,zoom_low:zoom_high],file_path=image_path+filter_name+restored_name+pol_names[pol_i+4],$
                /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log,low=stokes_low,high=stokes_high,$
                lat_center=obs_out.obsdec,lon_center=obs_out.obsra,rotation=0,grid_spacing=grid_spacing,degpix=degpix,$
                offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=show_grid,/sphere,_Extra=extra
            
    ;        Imagefast,instr_source[zoom_low:zoom_high,zoom_low:zoom_high],file_path=image_path+filter_name+'_Sources_'+pol_names[pol_i],$
    ;            /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log_source,low=0,high=instr_high,/invert_color,_Extra=extra
            Imagefast,instr_restored[zoom_low:zoom_high,zoom_low:zoom_high],file_path=image_path+filter_name+restored_name+pol_names[pol_i],$
                /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log_dirty,low=instr_low,high=instr_high,show_grid=show_grid,_Extra=extra
        ENDIF
    ENDIF
    IF pol_i EQ 0 THEN BEGIN
        IF Keyword_Set(gridline_image_show) THEN Imagefast,fltarr(zoom_high-zoom_low+1,zoom_high-zoom_low+1),file_path=image_path+filter_name+'_Grid',$
            /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log,low=0,high=stokes_high,/invert_color,$
            lat_center=obs_out.obsdec,lon_center=obs_out.obsra,rotation=0,grid_spacing=grid_spacing,degpix=degpix,$
            offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=1,/sphere,_Extra=extra
    ENDIF
ENDFOR

IF source_flag THEN BEGIN
    ;write sources to a text file
    radius=angle_difference(obs_out.obsdec,obs_out.obsra,source_arr_out.dec,source_arr_out.ra,/degree)
    Ires=(Qres=fltarr(N_Elements(source_arr_out)))
    cx=Round(source_arr_out.x) & cy=Round(source_arr_out.y)
    ind_use=where((cx<cy GE 0) AND (cx>cy LE (obs_out.dimension<obs_out.elements)-1))  
    Ires[ind_use]=(*stokes_images[0])[cx[ind_use],cy[ind_use]]
    IF n_pol GT 1 THEN Qres[ind_use]=(*stokes_images[1])[cx[ind_use],cy[ind_use]]
    source_array_export,source_arr_out,beam_avg,radius=radius,Ires=Ires,Qres=Qres,file_path=export_path+'_source_list'
ENDIF

; plot calibration solutions, export to png
IF N_Elements(cal) GT 0 THEN BEGIN
   IF file_test(file_path_fhd+'_cal_hist.sav') THEN BEGIN
      restore,file_path_fhd+'_cal_hist.sav'
      plot_cals,cal,obs,phase_filename=image_path+'_cal_phase.png',amp_filename=image_path+'_cal_amp.png',$
                vis_baseline_hist=vis_baseline_hist,vis_hist_filename=image_path+'_cal_hist.png'
   ENDIF ELSE BEGIN
      plot_cals,cal,obs,phase_filename=image_path+'_cal_phase.png',amp_filename=image_path+'_cal_amp.png'
   ENDELSE
ENDIF


END