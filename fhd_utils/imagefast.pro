PRO imagefast,Image,astr=astr,file_path=file_path,no_ps=no_ps,$
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
    label_spacing=label_spacing,map_reverse=map_reverse,zero_black=zero_black,zero_white=zero_white,_EXTRA=extra
    
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

;
IF cgHasImageMagick() EQ 0 THEN BEGIN
    print,"Imagemagick not found! Install from http://www.imagemagick.org/ to use this program."
    RETURN
ENDIF

IF N_Elements(file_path) EQ 0 THEN BEGIN
    file_path_use=expand_path('ImageFast')
    print,"Path not specified. File put here: ", file_path_use 
ENDIF ELSE file_path_use=file_path
;IF N_Elements(project_name) EQ 0 THEN project_name='mwa'
;RootDirectory=rootdir(project_name)
;IF Keyword_Set(data_directory) THEN filename_full=filepath(filename_use,Root_dir=RootDirectory,subdir=data_directory) $
;    ELSE filename_full=filepath(filename_use,Root_dir=RootDirectory)

IF N_Elements(significant_figures) EQ 0 THEN sigfig=2 ELSE sigfig=significant_figures
IF N_Elements(background) EQ 0 THEN background='white'
IF Keyword_Set(over_plot) THEN noerase=1 ELSE noerase=0
IF Keyword_Set(hist_equal) THEN scale=1 ELSE scale=0

;OBSOLETE KEYWORDS! Supply astr structure instead
;IF N_Elements(map_reverse) EQ 0 THEN map_reverse=1
;IF N_Elements(projection) EQ 0 THEN projection='orthographic' ;2
;IF N_Elements(rotation) EQ 0 THEN rotation=0
;IF N_Elements(lat_center) EQ 0 THEN lat_center=0
;IF N_Elements(lon_center) EQ 0 THEN lon_center=0
;IF N_Elements(offset_lat) EQ 0 THEN offset_lat=0
;IF N_Elements(offset_lon) EQ 0 THEN offset_lon=0
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

dimension=(size(Image_use,/dimension))[0]
elements=(size(Image_use,/dimension))[1]
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
;;        cb_label_vals
        
;        cb_label_vals/=image_rescale
;        cb_label_vals-=image_val_shift
;        cb_lab_high_i=where(cb_label_vals GE 0,n_high)
;        cb_lab_low_i=where(cb_label_vals LT 0,n_low)
;        IF n_high GT 0 THEN cb_label_vals[cb_lab_high_i]=color_center-1+10.^cb_label_vals[cb_lab_high_i]
;        IF n_low GT 0 THEN cb_label_vals[cb_lab_low_i]=1-color_center-10.^(Abs(cb_label_vals[cb_lab_low_i]))
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


;cb_digits=Max(Abs(Alog10(abs(cb_label_vals))))
;IF low LT 0 THEN cb_digits+=1
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
;cb_format_test='(I0)'
;cb_labels_test=String(format='(I0)',cb_label_vals)
;cb_digits=Max(Strlen(cb_labels_test))

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

PS_Start,filename=file_path_use+'.ps',/quiet,/nomatch,charsize=charsize,xsize=xsize2,ysize=ysize2
;position_default=[x0, y0, x1, y1]
image_position=[xstart/xsize,ystart/ysize,(xstart+dimension)/xsize,(ystart+elements)/ysize]
cgImage,image_use,/keep_aspect,background=background,layout=layout,margin=margin,noerase=noerase,$
    palette=palette,oposition=oposition,position=image_position,$
    scale=scale,title=title,/axes,axkeywords={XSTYLE:4,YSTYLE:4}

IF Keyword_Set(show_grid) THEN BEGIN
    IF Keyword_Set(astr) THEN BEGIN
        astr_use=astr
        xvals=meshgrid(dimension,elements,1)
        yvals=meshgrid(dimension,elements,2)
        CASE reverse_image OF
            0:cd_mod=[1.,1.]
            1:BEGIN
                cd_mod=[-1.,1.]
                astr.crpix=[dimension+1-(astr.crpix)[0],(astr.crpix)[1]]
            END
            2:BEGIN
                cd_mod=[1.,-1.]
                astr.crpix=[(astr.crpix)[0],elements+1-(astr.crpix)[1]]
            END
            3:BEGIN
                cd_mod=[-1.,-1.]
                astr.crpix=[dimension+1-(astr.crpix)[0],elements+1-(astr.crpix)[1]]
            END
        ENDCASE
        astr_use.cdelt=astr_use.cdelt*cd_mod
        xy2ad,xvals,yvals,astr_use,ra_arr,dec_arr
        cgcontour,ra_arr,levels=indgen(360./grid_spacing)*grid_spacing,/overplot,/noerase
        cgcontour,dec_arr,levels=indgen(1+180./grid_spacing)*grid_spacing-90,/overplot,/noerase
    ENDIF ELSE BEGIN
        ;Obsolete
    ENDELSE
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

;;IF !version.os_family EQ 'unix' THEN BEGIN
;    IF Keyword_Set(no_ps) THEN PS_End,Density=300,Resize=25.,/png,/DELETE_PS,/allow_transparent,/nomessage $
;        ELSE PS_End,Density=300,Resize=25.,/png,/allow_transparent,/nomessage
;;ENDIF ELSE BEGIN
;;    IF Keyword_Set(no_ps) THEN PS_End,Density=300,Resize=25.,/png,/DELETE_PS $
;;        ELSE PS_End,Density=300,Resize=25.,/png,/NoWait
;;ENDELSE
    IF Keyword_Set(no_ps) THEN PS_End,Density=75,Resize=100.,/png,/DELETE_PS,/allow_transparent,/nomessage $
        ELSE PS_End,Density=75,Resize=100.,/png,/allow_transparent,/nomessage

END