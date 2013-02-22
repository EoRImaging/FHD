PRO fhd_quickview,fhd,obs,residual_array,source_array,comp_arr,beam_base,file_path_fhd=file_path_fhd,image_filter_fn=image_filter_fn,_Extra=extra

IF N_Elements(show_grid) EQ 0 THEN show_grid=1
;version_name='v'+strn(version)
;version_dirname='fhd_'+version_name

np=N_Params() 
IF N_Elements(beam_base) GT 0 THEN np=3 ELSE np=np<2
SWITCH np OF
    0:restore,file_path_fhd+'_fhd_params.sav'
    1:restore,file_path_fhd+'_obs.sav'
    2:restore,file_path_fhd+'_fhd.sav'
    3:
ENDSWITCH

basename=file_basename(file_path_fhd)
dirpath=file_dirname(file_path_fhd)
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


;IF N_Elements(normalization_arr) GT 0 THEN normalization=Mean(normalization_arr)/2.
npol=fhd.npol
dimension=obs.dimension
elements=obs.elements

stats_radius=10. ;degrees
pol_names=['xx','yy','xy','yx','I','Q','U','V']

grid_spacing=10.
offset_lat=5.;15. paper 10 memo
offset_lon=5.;15. paper 10 memo
reverse_image=0   ;1: reverse x axis, 2: y-axis, 3: reverse both x and y axes
map_reverse=0;1 paper 3 memo
label_spacing=1.

instr_images=Ptrarr(npol,/allocate)
restored_beam_width=(!RaDeg/(obs.MAX_BASELINE/obs.KPIX)/obs.degpix)/2.
FOR pol_i=0,npol-1 DO BEGIN
    *instr_images[pol_i]=dirty_image_generate(*residual_array[pol_i],image_filter_fn=image_filter_fn,$
        _Extra=extra)*weight_invert(*beam_base[pol_i])
    *instr_sources[pol_i]=source_image_generate(source_array,obs,pol_i=pol_i,resolution=16,$
        dimension=dimension,width=restored_beam_width)
ENDFOR

stokes_images=stokes_cnv(instr_images,beam=beam_base)
stokes_sources=stokes_cnv(instr_sources,beam=beam_base)


beam_mask=fltarr(dimension,elements)+1
beam_avg=fltarr(dimension,elements)
FOR pol_i=0,(npol<2)-1 DO BEGIN
    beam_mask_test=fltarr(dimension,elements)
    beam_i=region_grow(*beam_base[pol_i],dimension/2.+dimension*elements/2.,threshold=[fhd.beam_threshold/2.,Max(*beam_base[pol_i])])
    beam_avg+=*beam_base[pol_i]/(2.<npol)
    beam_mask_test[beam_i]=1.
    beam_mask*=beam_mask_test
ENDFOR
x_inc=where(beam_mask) mod dimension
y_inc=Floor(where(beam_mask)/dimension)
zoom_low=min(x_inc)<min(y_inc)
zoom_high=max(x_inc)>max(y_inc)

FOR pol_i=0,npol-1 DO BEGIN
    stokes_residual=(*stokes_images[pol_i])*beam_mask
    stokes_source=(*stokes_sources[pol_i])*beam_mask
    stokes_restored=stokes_residual+stokes_source
    
    stokes_low=Min((stokes_residual*Sqrt(beam_avg>0))[where(beam_mask)])
    stokes_high=Max((stokes_residual*Sqrt(beam_avg>0))[where(beam_mask)])
    stokesS_high=Max(stokes_restored[where(beam_mask)])
    IF pol_i EQ 0 THEN log=1 ELSE log=0
    IF pol_i EQ 0 THEN Imagefast,stokes_residual[zoom_low:zoom_high,zoom_low:zoom_high],file_path=image_path+filter_name+'_Residual_'+pol_names[pol_i+4],$
        /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,low=stokes_low,high=stokes_high,$
        lat_center=obs.obsdec,lon_center=obs.obsra,rotation=0,grid_spacing=grid_spacing,degpix=obs.degpix,$
        offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=show_grid,/sphere,/no_ps
    IF pol_i EQ 1 THEN Imagefast,stokes_restored[zoom_low:zoom_high,zoom_low:zoom_high],file_path=image_path+filter_name+'_Restored_'+pol_names[pol_i+4],$
        /right,sig=2,color_table=0,back='white',reverse_image=reverse_image,log=log,low=stokes_low,high=stokesS_high,$
        lat_center=obs.obsdec,lon_center=obs.obsra,rotation=0,grid_spacing=grid_spacing,degpix=obs.degpix,$
        offset_lat=offset_lat,offset_lon=offset_lon,label_spacing=label_spacing,map_reverse=map_reverse,show_grid=show_grid,/sphere,/no_ps
ENDFOR

;write sources to a text file
Ires=(*stokes_images[0])[source_array.x,source_array.y]
Qres=(*stokes_images[1])[source_array.x,source_array.y]
radius=angle_difference(obs.obsdec,obs.obsra,source_array.dec,source_array.ra,/degree)
source_array_export,source_array,beam_avg,radius=radius,Ires=Ires,Qres=Qres,file_path=export_path+'_source_list'

Ires=(*stokes_images[0])[comp_arr.x,comp_arr.y]
Qres=(*stokes_images[1])[comp_arr.x,comp_arr.y]
radius=angle_difference(obs.obsdec,obs.obsra,comp_arr.dec,comp_arr.ra,/degree)
source_array_export,comp_arr,beam_avg,radius=radius,Ires=Ires,Qres=Qres,file_path=export_path+'_component_list'

END