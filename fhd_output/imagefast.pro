PRO imagefast,Image,astr=astr,file_path=file_path, eps=eps, png=png,$
    no_colorbar=no_colorbar,vertical_colorbar=vertical_colorbar,colorbar_title=colorbar_title,color_table=color_table,$
    right_colorbar=right_colorbar,left_colorbar=left_colorbar,$
    layout=layout,margin=margin,background=background,over_plot=over_plot,charsize=charsize,$
    transparent=transparent,title=title,reverse_image=reverse_image,$
    significant_figures=significant_figures,mask=mask,resize=resize,hist_equal=hist_equal,$
    tick_spacing=tick_spacing,logarithmic_color=logarithmic_color,sqrt_color=sqrt_color,invert_color=invert_color,$
    color_center=color_center,low=low,high=high,$
    show_grid=show_grid,projection=projection,grid_spacing=grid_spacing,degpix=degpix,sphere=sphere,$
    lat_center=lat_center,lon_center=lon_center,zenith_ra=zenith_ra,zenith_dec=zenith_dec,$
    rotation=rotation,offset_lat=offset_lat,offset_lon=offset_lon,$
    label_spacing=label_spacing,map_reverse=map_reverse,zero_black=zero_black,zero_white=zero_white,$
    contour_image=contour_image,contour_levels=contour_levels,contour_nlevels=contour_nlevels,contour_color=contour_color,_EXTRA=extra
    
;use _Extra to pass keywords such as:
;   bold,cmyk,encapsulated,font (type!),narrow,oblique,preview,xoffset,xsize,yoffset,ysize
;
;Unsupported keywords: position,

        
;new imaging program to use cgImage
;   Will generate both a .png and high-resolution .eps by default
;
;Standard color tables:
; 0-        B-W LINEAR   14-             STEPS   28-         Hardcandy
; 1-        BLUE/WHITE   15-     STERN SPECIAL   29-            Nature
; 2-   GRN-RED-BLU-WHT   16-              Haze   30-             Ocean
; 3-   RED TEMPERATURE   17- Blue - Pastel - R   31-        Peppermint
; 4- BLUE/GREEN/RED/YE   18-           Pastels   32-            Plasma
; 5-      STD GAMMA-II   19- Hue Sat Lightness   33-          Blue-Red
; 6-             PRISM   20- Hue Sat Lightness   34-           Rainbow
; 7-        RED-PURPLE   21-   Hue Sat Value 1   35-        Blue Waves
; 8- GREEN/WHITE LINEA   22-   Hue Sat Value 2   36-           Volcano
; 9- GRN/WHT EXPONENTI   23- Purple-Red + Stri   37-             Waves
;10-        GREEN-PINK   24-             Beach   38-         Rainbow18
;11-          BLUE-RED   25-         Mac Style   39-   Rainbow + white
;12-          16 LEVEL   26-             Eos A   40-   Rainbow + black
;13-           RAINBOW   27-             Eos B
;
;Map projections:
;1: Stereographic  2: Orthographic 3: Lambert Conic 4: Lambert Azimuthal 5: Gnomonic 6: Azimuthal Equidistant 
;7: Satellite 8: Cylindrical 9: Mercator 10: Mollweide 11: Sinusoidal 12: Aitoff 

IF N_Elements(png) EQ 0 AND N_Elements(eps) EQ 0 THEN png=1
IF Keyword_Set(png) AND cgHasImageMagick() EQ 0 THEN BEGIN
    print,"Imagemagick not found! Install from http://www.imagemagick.org/ to use this program."
    RETURN
ENDIF

IF N_Elements(file_path) EQ 0 THEN BEGIN
    file_path_use=expand_path('ImageFast')
    print,"Path not specified. File put here: ", file_path_use 
ENDIF ELSE file_path_use=file_path

IF N_Elements(significant_figures) EQ 0 THEN sigfig=2 ELSE sigfig=significant_figures
IF N_Elements(background) EQ 0 THEN background='white'
IF Keyword_Set(over_plot) THEN noerase=1 ELSE noerase=0
IF Keyword_Set(hist_equal) THEN scale=1 ELSE scale=0

IF N_Elements(grid_spacing) EQ 0 THEN grid_spacing=10. ;degrees
IF N_Elements(label_spacing) EQ 0 THEN label_spacing=1

IF N_Elements(reverse_image) EQ 0 THEN reverse_image=0
CASE reverse_image OF
    0:Image_use=image
    1:Image_use=Reverse(Image,1)
    2:Image_use=Reverse(Image,2)
    3:Image_use=Reverse(Reverse(Image,1),2)
ENDCASE
IF N_Elements(mask) EQ N_Elements(image_use) THEN BEGIN
    CASE reverse_image OF
        0:mask_use*=mask
        1:mask_use*=Reverse(mask,1)
        2:mask_use*=Reverse(mask,2)
        3:mask_use*=Reverse(Reverse(mask,1),2)
    ENDCASE    
ENDIF

IF Keyword_Set(zero_black) OR Keyword_Set(zero_white) THEN BEGIN
    zero_i=Keyword_Set(mask_use) ? where(mask_use EQ 0,n_zero):where(image_use EQ 0,n_zero)
ENDIF ELSE n_zero=0

dimension=Float((size(Image_use,/dimension))[0])
elements=Float((size(Image_use,/dimension))[1])
IF N_Elements(charsize) EQ 0 THEN charsize=(Alog10(dimension)-1.)>1
IF N_Elements(high) EQ 0 THEN high=Max(image_use)
IF N_Elements(low) EQ 0 THEN low=Min(image_use)
image_use=image_use>low
image_use=image_use<high

cb_divisions=Floor(4.*charsize)
cb_label_vals=findgen(cb_divisions+1)*256/cb_divisions
IF N_Elements(color_center) EQ 0 THEN color_center=low
CASE 1 OF
    Keyword_Set(logarithmic_color):BEGIN
        image_use2=image_use
        high_i=where(image_use GE color_center,n_high)
        low_i=where(image_use LT color_center,n_low)
        IF n_high GT 0 THEN image_use2[high_i]=Alog10(image_use2[high_i]-color_center+1)
        IF n_low GT 0 THEN image_use2[low_i]=-Alog10(color_center-image_use2[low_i]+1)
        image_val_shift=-Min(image_use2)
        image_rescale=256./(Max(image_use2)-Min(image_use2))
        image_use2+=image_val_shift
        image_use2*=image_rescale
        image_use=image_use2
        
        cb_divisions2=cb_divisions*32
        cb_label_vals=(high-low)*findgen(cb_divisions*32+1)/(cb_divisions*32.)+low
        cb_label_vals2=cb_label_vals
        high_i2=where(cb_label_vals GE color_center,n_high2)
        low_i2=where(cb_label_vals LT color_center,n_low2)
        IF n_high2 GT 0 THEN cb_label_vals2[high_i2]=Alog10(cb_label_vals[high_i2]-color_center+1)
        IF n_low2 GT 0 THEN cb_label_vals2[low_i2]=-Alog10(color_center-cb_label_vals[low_i2]+1)
        cb_label_vals2+=image_val_shift
        cb_label_vals2*=256./Max(cb_label_vals2)
        cb_label_ii=indgen(cb_divisions+1)*256/cb_divisions
        cb_label_vals=Interpol(cb_label_vals,cb_label_vals2,cb_label_ii)
    END
    
    Keyword_Set(sqrt_color): BEGIN
        image_use2=image_use
        high_i=where(image_use GE color_center,n_high)
        low_i=where(image_use LT color_center,n_low)
        IF n_high GT 0 THEN image_use2[high_i]=Sqrt(image_use2[high_i]-color_center+1)
        IF n_low GT 0 THEN image_use2[low_i]=-Sqrt(color_center-image_use2[low_i]+1)
        image_val_shift=-Min(image_use2)
        image_rescale=256./(Max(image_use2)-Min(image_use2))
        image_use2+=image_val_shift
        image_use2*=image_rescale
        image_use=image_use2
        
        cb_divisions2=cb_divisions*32
        cb_label_vals=(high-low)*findgen(cb_divisions*32+1)/(cb_divisions*32.)+low
        cb_label_vals2=cb_label_vals
        high_i2=where(cb_label_vals GE color_center,n_high2)
        low_i2=where(cb_label_vals LT color_center,n_low2)
        IF n_high2 GT 0 THEN cb_label_vals2[high_i2]=Sqrt(cb_label_vals[high_i2]-color_center+1)
        IF n_low2 GT 0 THEN cb_label_vals2[low_i2]=-Sqrt(color_center-cb_label_vals[low_i2]+1)
        cb_label_vals2+=image_val_shift
        cb_label_vals2*=256./Max(cb_label_vals2)
        cb_label_ii=indgen(cb_divisions+1)*256/cb_divisions
        cb_label_vals=Interpol(cb_label_vals,cb_label_vals2,cb_label_ii)
        
    END
    
    ELSE: BEGIN
        image_use-=low
        image_use*=256./(high-low)
        cb_label_vals*=(high-low)/256.
        cb_label_vals+=low
    ENDELSE
ENDCASE
IF Keyword_Set(mask_use) THEN image_use*=mask_use

cgLoadCT, color_table, REVERSE=invert_color, RGB_TABLE=palette

IF (Keyword_Set(zero_black) OR Keyword_Set(zero_white)) AND n_zero GT 0 THEN BEGIN
    image_use[zero_i]=0
    CASE 1 OF
        Keyword_Set(zero_black):palette[0,*]=cgColor('black',/triple)
        Keyword_Set(zero_white):palette[0,*]=cgColor('white',/triple)
    ENDCASE
ENDIF

back_color_val=cgColor(background)
contrast_color=cgColor('white')-back_color_val
CASE background OF
    'white':annotate_color='black'
    'black':annotate_color='white'
    ELSE: annotate_color=(contrast_color GE cgColor('white')/2.) ? 'black':'white'
ENDCASE

xsize=Float(dimension)
ysize=Float(elements)
colorbar_width=20; pixel width of the colorbar, excluding labels

cb_labels=Strarr(cb_divisions+1)
cb_digit_arr=intarr(cb_divisions+1)
cb_decimal_arr=intarr(cb_divisions+1)
cb_div_arr=Fltarr(cb_divisions+1)
FOR li=0,cb_divisions DO BEGIN
    sig_div_digit=(cb_label_vals[li] NE 0) ? (Floor(Alog10(Abs(cb_label_vals[li])))>0):0
    precision_digit=sig_div_digit-sigfig
    cb_digit_arr[li]=sig_div_digit+1
    IF precision_digit LT 0 THEN BEGIN
        cb_digit_arr[li]+=2
        cb_decimal_arr[li]=Abs(precision_digit)
    ENDIF
    IF cb_label_vals[li] LT 0 THEN cb_digit_arr[li]+=1
    cb_div_arr[li]=10.^precision_digit
ENDFOR
cb_digits=Max(cb_digit_arr)
cb_label_format=Strarr(cb_divisions+1)
 
FOR li=0,cb_divisions DO BEGIN
    label_format=String(format='("(F",I0,".",I0,")")',cb_digits,cb_decimal_arr[li])
    cb_labels[li]=String(format=label_format,Round(cb_label_vals[li]/cb_div_arr[li])*cb_div_arr[li])
ENDFOR
iter=0   
nstar=Total(strmatch(cb_labels,'*\*'))
WHILE nstar GT 0 DO BEGIN
    FOR li=0,cb_divisions DO BEGIN
        label_format=String(format='("(F",I0,".",I0,")")',cb_digits,cb_decimal_arr[li])
        cb_labels[li]=String(format=label_format,Round(cb_label_vals[li]/cb_div_arr[li])*cb_div_arr[li])
    ENDFOR
    cb_digits+=1
    nstar=Total(strmatch(cb_labels,'*\*'))
    IF iter++ GT 8 THEN BREAK
ENDWHILE

colorbar_label_width=charsize*cb_digits*6.
IF Keyword_Set(colorbar_title) THEN colorbar_label_width+=charsize*16.
colorbar_pad=0.05; normalized coordinates 
IF Keyword_Set(title) THEN ysize+=charsize*20.
IF not Keyword_Set(no_colorbar) THEN IF (Keyword_Set(right_colorbar) OR Keyword_Set(left_colorbar)) THEN $
    xsize+=colorbar_width+colorbar_label_width ELSE ysize+=colorbar_width+colorbar_label_width
xstart=0.
ystart=0.

CASE 1 OF
    Keyword_Set(no_colorbar):
    Keyword_Set(right_colorbar):BEGIN
        cb_vertical=1
        cbl_right=1
    END    
    Keyword_Set(left_colorbar):BEGIN
        cb_vertical=1
        xstart+=colorbar_width+colorbar_label_width
    END    
    ELSE:ystart+=colorbar_width+colorbar_label_width
ENDCASE

pixres=72.
xsize2=xsize/pixres
ysize2=ysize/pixres

cgPS_Open,filename=file_path_use+'.eps',/quiet,/nomatch,charsize=charsize,xsize=xsize2,ysize=ysize2,/encapsulated

image_position=[xstart/xsize,ystart/ysize,(xstart+dimension)/xsize,(ystart+elements)/ysize]
cgImage,image_use,/keep_aspect,background=background,layout=layout,margin=margin,noerase=noerase,$
    palette=palette,oposition=oposition,position=image_position,$
    scale=scale,title=title,/axes,axkeywords={XSTYLE:4,YSTYLE:4}

IF Keyword_Set(show_grid) THEN BEGIN
    astr_use=astr
    xvals=meshgrid(dimension,elements,1)
    yvals=meshgrid(dimension,elements,2)
    CASE reverse_image OF
        0:cd_mod=[1.,1.]
        1:BEGIN
            cd_mod=[-1.,1.]
            astr_use.crpix=[dimension+1-(astr_use.crpix)[0],(astr_use.crpix)[1]]
        END
        2:BEGIN
            cd_mod=[1.,-1.]
            astr_use.crpix=[(astr_use.crpix)[0],elements+1-(astr_use.crpix)[1]]
        END
        3:BEGIN
            cd_mod=[-1.,-1.]
            astr_use.crpix=[dimension+1-(astr_use.crpix)[0],elements+1-(astr_use.crpix)[1]]
        END
    ENDCASE
    astr_use.cdelt=astr_use.cdelt*cd_mod
    ;don't use apply_astrometry here, since we're only calculating grid lines and have modified the astr structure
    xy2ad,xvals,yvals,astr_use,ra_arr,dec_arr
    ra_levels=indgen(360./grid_spacing)*grid_spacing
    dec_levels=indgen(1+180./grid_spacing)*grid_spacing-90
    ra_cut=where((ra_arr GT 360.-0.75*grid_spacing) AND (ra_arr LT 360.-0.25*grid_spacing),n_ra_cut)
    ra_mod=where(ra_arr GE 360.-0.25*grid_spacing,n_ra_mod)
    IF n_ra_mod GT 0 THEN ra_arr[ra_mod]-=360.
    IF n_ra_cut GT 0 THEN ra_arr[ra_cut]=!Values.F_NAN
    cgcontour,ra_arr,levels=ra_levels,/overplot,/noerase,position=oposition,/onimage
    cgcontour,dec_arr,levels=dec_levels,/overplot,/noerase,position=oposition,/onimage
ENDIF  

IF Keyword_Set(contour_image) THEN BEGIN
    IF Ptr_valid(contour_image[0]) THEN contour_image_use=*contour_image[0] ELSE contour_image_use=contour_image
    CASE reverse_image OF
        0:contour_image_use=contour_image_use
        1:contour_image_use=Reverse(contour_image_use,1)
        2:contour_image_use=Reverse(contour_image_use,2)
        3:contour_image_use=Reverse(Reverse(contour_image_use,1),2)
    ENDCASE
    cgcontour,contour_image_use,levels=contour_levels,nlevels=contour_nlevels,/overplot,/noerase,position=oposition,/onimage,color=contour_color
ENDIF

IF not Keyword_Set(no_colorbar) THEN BEGIN
    CASE 1 OF
        Keyword_Set(right_colorbar):cb_position=[oposition[2],ystart/ysize+colorbar_pad,oposition[2]+colorbar_width/xsize,oposition[3]-colorbar_pad]
        Keyword_Set(left_colorbar):cb_position=[0,0+colorbar_pad,colorbar_width/xsize,oposition[3]-colorbar_pad]
        ELSE:cb_position=[0+colorbar_pad,oposition[1]-colorbar_width/ysize,oposition[2]-colorbar_pad,oposition[1]]
    ENDCASE
    cgColorbar,FORMAT=format,ANNOTATECOLOR=annotate_color,palette=palette,vertical=cb_vertical,right=cbl_right,$
        position=cb_position,title=colorbar_title,divisions=cb_divisions,ticknames=cb_labels;,/fit ;format default is '(I0)'
ENDIF

delete_ps = ~Keyword_Set(eps)
cgPS_Close,Density=75,Resize=100.,png=png,delete_ps=delete_ps,/allow_transparent,/nomessage

END
