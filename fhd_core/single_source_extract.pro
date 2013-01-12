FUNCTION single_source_extract,image,beam_correction,radius=radius,gain_factor=gain_factor,$
    image_correct=image_correct,absolute=absolute,error=error,median_remove=median_remove,source_i=source_i,$
    ra_arr=ra_arr,dec_arr=dec_arr,source_mask=source_mask,no_beam=no_beam

IF N_Elements(radius) EQ 0 THEN radius=3.
IF N_Elements(gain_factor) EQ 0 THEN gain_factor=1.
IF N_Elements(source_mask) EQ 0 THEN source_mask=fltarr(dimension,elements)+1.
source_mask_i=where(source_mask,n_use)
n_use2=Sqrt(n_use)

dimension=(size(image,/dimension))[0]
elements=(size(image,/dimension))[1]
IF Keyword_Set(no_beam) THEN image_use=image ELSE image_use=image*beam_correction;^2.
IF Keyword_Set(median_remove) THEN image_use-=Median(image_use,radius*5.)

error=1
iter=0
WHILE error EQ 1 DO BEGIN
    iter+=1
    IF Keyword_Set(absolute) THEN BEGIN
        dummy_flux=Max(Abs(image_use*source_mask),source_i)
        source_flux=image_use[source_i]
    ENDIF ELSE source_flux=Max(image_use*source_mask,source_i)
    sx=(source_i mod dimension)
    sy=Floor(source_i/dimension)
    gcntrd,image_use,sx,sy,xcen,ycen,radius,/silent,/keepcenter
    
    IF (xcen EQ -1) OR (ycen EQ -1) THEN BEGIN
        error=1 
        source_mask[source_i]=0
        xcen=sx
        ycen=sy
    ENDIF ELSE error=0
    IF iter GT n_use2 THEN BREAK
ENDWHILE

xcen2=xcen-dimension/2.
ycen2=ycen-elements/2.
IF Keyword_Set(no_beam) THEN source_flux_corr1=source_flux ELSE $
    source_flux_corr1=source_flux*beam_correction[source_i]
source_flux_corr2=source_flux_corr1*gain_factor
IF N_Elements(ra_arr) EQ 0 THEN ra=0 ELSE ra=Interpolate(ra_arr,xcen,ycen)
IF N_Elements(dec_arr) EQ 0 THEN dec=0 ELSE dec=Interpolate(dec_arr,xcen,ycen)

;columns of source_array are: 0:x, 1:y, 2:RA, 3:Dec, 4:co-added flux, 5:pixel index, 6:amplitude in UV
single_source_array=[xcen,ycen,ra,dec,source_flux_corr1,source_i,source_flux_corr2]

RETURN,single_source_array
END