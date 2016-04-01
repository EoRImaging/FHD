FUNCTION source_residual_image,obs,source_arr,image_arr,jones=jones,source_residual_stokes=source_residual_stokes,beam_arr=beam_arr,beam_power=beam_power,$
    image_path=image_path,source_residual_radius=source_residual_radius,source_residual_ston_threshold=source_residual_ston_threshold,$
    source_residual_flux_threshold=source_residual_flux_threshold,source_residual_beam_threshold=source_residual_beam_threshold,$
    beam_threshold=beam_threshold,source_residual_include_sidelobe=source_residual_include_sidelobe,smooth_iter=smooth_iter

source_arr_use=source_arr

n_pol=obs.n_pol
dimension=obs.dimension
elements=obs.elements
pol_names=obs.pol_names
IF N_Elements(beam_threshold) EQ 0 THEN beam_threshold=0.05
IF N_Elements(beam_power) EQ 0 THEN beam_power=1.
IF Keyword_Set(source_residual_stokes) THEN pol_offset=4 ELSE pol_offset=0
beam_flag=Min(Ptr_valid(beam_arr))
restored_beam_width=beam_width_calculate(obs,min_restored_beam_width=1.)
smooth_width=5.*restored_beam_width 
IF N_Elements(smooth_iter) EQ 0 THEN smooth_iter=4.

IF Keyword_Set(source_residual_stokes) THEN BEGIN
    IF Max(source_arr_use.flux.I) EQ 0 THEN BEGIN
        IF N_Elements(jones) EQ 0 THEN jones=fhd_struct_init_jones(obs,/no_save,restore=0,mask=beam_mask)
        source_arr_use=stokes_cnv(source_arr_use,jones,obs,beam_arr=beam_arr,square=0)
    ENDIF 
ENDIF ELSE BEGIN
    IF Max(source_arr_use.flux.(0)) EQ 0 THEN BEGIN
        IF N_Elements(jones) EQ 0 THEN jones=fhd_struct_init_jones(obs,/no_save,restore=0,mask=beam_mask)
        source_arr_use=stokes_cnv(source_arr_use,jones,obs,beam_arr=beam_arr,square=0,/inverse)
    ENDIF 
ENDELSE

IF Keyword_Set(source_residual_flux_threshold) THEN BEGIN
    si_use=where(source_arr_use.flux.I GE source_residual_flux_threshold,n_use)
    IF n_use GT 0 THEN source_arr_use=source_arr_use[si_use]
ENDIF
IF Keyword_Set(source_residual_ston_threshold) THEN BEGIN
    IF Stddev(source_arr_use.ston) GT 0 THEN BEGIN ;check if valid signal to noise has been calculated for the source list. 
        si_use=where(source_arr_use.ston GE source_residual_ston_threshold,n_use)
        IF n_use GT 0 THEN source_arr_use=source_arr_use[si_use]
    ENDIF
ENDIF
n_use=N_Elements(source_arr_use)

IF beam_flag THEN BEGIN
    beam_avg=fltarr(dimension,elements)
    FOR pol_i=0,(n_pol<2)-1 DO beam_avg+=Abs(*beam_arr[pol_i])^2
    beam_avg=Sqrt(beam_avg/(n_pol<2))
    
    beam_mask=Fltarr(dimension,elements)
    IF Keyword_Set(source_residual_include_sidelobe) THEN beam_i=where((beam_avg GE beam_threshold) AND (beam_avg GT 0),n_i_use) $
        ELSE beam_i=Region_grow(beam_avg,dimension/2.+dimension*elements/2.,threshold=[beam_threshold,Max(beam_avg)])
    
    IF beam_i[0] EQ -1 THEN beam_flag=0 ELSE beam_mask[beam_i]=1. 
    
    IF Keyword_Set(source_residual_beam_threshold) THEN BEGIN
        si_use=where(beam_avg[source_arr_use.sx,source_arr_use.sy] GE source_residual_beam_threshold,n_use)
        IF n_use GT 0 THEN source_arr_use=source_arr_use[si_use]
    ENDIF
ENDIF ELSE beam_mask=Fltarr(dimension,elements)+1.

IF N_Elements(source_residual_radius) EQ 0 THEN radius=Ceil(Sqrt(N_Elements(where(beam_mask))/(n_use/4.)/!Pi))>11. $ ; want an average of 4 sources per smooth box
    ELSE radius=source_residual_radius

residual_arr=Ptrarr(n_pol)
sx=source_arr_use.x
sy=source_arr_use.y

;;IF (N_Elements(zoom_low) EQ 0) OR (N_Elements(zoom_high) EQ 0) THEN BEGIN
;;    IF beam_flag THEN BEGIN
;;        beam_xv=beam_i mod dimension
;;        beam_yv=Floor(beam_i/dimension)
;;        IF N_Elements(zoom_low) EQ 0 THEN zoom_low=Min(beam_xv)<Min(beam_yv)
;;        IF N_Elements(zoom_high) EQ 0 THEN zoom_high=Max(beam_xv)>Max(beam_yv)
;;    ENDIF ELSE BEGIN
;;        IF N_Elements(zoom_low) EQ 0 THEN zoom_low=Min(sx)<Min(sy)
;;        IF N_Elements(zoom_high) EQ 0 THEN zoom_high=Max(sx)>Max(sy)
;;    ENDELSE
;;ENDIF
;IF N_Elements(zoom_low) EQ 0 THEN zoom_low=0.
;IF N_Elements(zoom_high) EQ 0 THEN zoom_high=dimension-1.

res_mask=fltarr(dimension,elements)
res_mask[sx,sy]=1
source_arr_ones=source_arr_use
FOR pol_i=0,7 DO source_arr_ones.flux.(pol_i)=1.
FOR pol_i=0,n_pol-1 DO BEGIN
    flux=source_arr_use.flux.(pol_offset+pol_i)
    res=Fltarr(dimension,elements)
;    res[sx,sy]=(*image_arr[pol_i])[sx,sy]*weight_invert(flux)
    source_img=source_image_generate(source_arr_use,obs,pol_i=pol_offset+pol_i,resolution=16,dimension=dimension,restored_beam_width=restored_beam_width,_Extra=extra)
    source_weights=source_image_generate(source_arr_ones,obs,pol_i=pol_offset+pol_i,resolution=16,dimension=dimension,restored_beam_width=restored_beam_width,_Extra=extra)<1.
    source_mask=Fltarr(dimension,elements)
    smask_i=where(source_weights,n_i_use)
    IF n_i_use GT 0 THEN source_mask[smask_i]=1.
    IF beam_flag THEN BEGIN
        IF Keyword_Set(source_residual_stokes) THEN BEGIN
            res_img=*image_arr[pol_i]*beam_avg
            res_img_smooth=Median(res_img,smooth_width)
            res_img-=res_img_smooth
            res_img2=res_img*source_img*weight_invert(beam_avg)
;            res_img*=weight_invert(beam_avg)
;            res[sx,sy]=res_img[sx,sy]*weight_invert(flux)
        ENDIF ELSE BEGIN
            res_img=*image_arr[pol_i]*weight_invert(*beam_arr[pol_i])^((beam_power-1.)>0.)
            res_img_smooth=Median(res_img,smooth_width)
            res_img-=res_img_smooth
            res_img2=res_img*source_img*weight_invert(*beam_arr[0])
;            res[sx,sy]=res_img[sx,sy]*weight_invert(flux)
;            IF beam_power GT 0 THEN res*=weight_invert(*beam_arr[pol_i])
        ENDELSE
    ENDIF ELSE BEGIN
        res_img=*image_arr[pol_i]
        res_img_smooth=Median(res_img,smooth_width)
        res_img-=res_img_smooth
        res_img2=res_img*source_img
;        res[sx,sy]=res_img[sx,sy]*weight_invert(flux)
    ENDELSE
;    
;    res_x_max=(Max(sx)+restored_beam_width/2)<(dimension-1)
;    res_x_min=(Min(sx)-restored_beam_width/2)>0 
;    res_y_max=(Max(sy)+restored_beam_width/2)<(elements-1)
;    res_y_min=(Min(sy)-restored_beam_width/2)>0
    
    res_img2=Smooth(res_img*source_weights,radius,/edge)
    source_img2=Smooth(source_img,radius,/edge)
    FOR i=0L,smooth_iter-1 DO BEGIN
        res_img2=Smooth(res_img2,radius,/edge)
        source_img2=Smooth(source_img2,radius,/edge)
    ENDFOR
    ;power of 3 intentional: after 1 iteration a source will be smeared at 1/radius^2 in all surrounding pixels. 
    ;Want to pick up one more iteration out, and the final row of radius pixels will be smeared at an additional 1/r^2 for a total of 1/r^3
    res_test=res_img2*weight_invert(source_img2,min(source_arr_use.flux.I)/radius^3.)
;    res_test2=Smooth((res_img*source_weights)[res_x_min:res_x_max,res_y_min:res_y_max],radius,/edge)*$
;        weight_invert(Smooth(source_img[res_x_min:res_x_max,res_y_min:res_y_max],radius,/edge),min(source_arr_use.flux.I)/radius^2.)
    
;    res_fill=max_filter(res,radius,/median,/circle,mask=res_mask,missing=0)
    residual_arr[pol_i]=Ptr_new(res_test)
ENDFOR

RETURN,residual_arr
END