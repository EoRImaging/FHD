PRO source_counts_compare,obs,source_array,filename=filename,data_directory=data_directory,version=version

;NEEDED: beam_i,beam_mask

vis_path_default,data_directory,filename,file_path,obs=obs,version=version
IF N_Elements(source_array) EQ 0 THEN restore,file_path+'_fhd.sav'
IF N_Elements(obs) EQ 0 THEN restore,file_path+'_obs.sav'

sx=source_array.x
sy=source_array.y
dimension=obs.dimension
elements=obs.elements
degpix=obs.degpix
source_dist=Sqrt((dimension/2.-sx)^2.+(elements/2.-sy)^2.)
Npix_center=N_Elements(*beam_i[0])
beam_mask_use=*beam_mask[0]*(*beam_mask[1])

zoom_area=N_Elements(where(beam_mask_use GT 0))*degpix^2.

flux_I=source_array.flux.I

ston=source_array.ston
hist_ston=histogram(ston,min=0,binsize=1)
ston_tot=reverse(Total(reverse(hist_ston),/cumulative))
ston_vals=(Strtrim(Sindgen(100),2))[[1,2,3,5,10]]
n_ston=N_Elements(ston_vals)
ston_string=Strarr(n_ston)
FOR ston_i=0,n_ston-1 DO ston_string[ston_i]=StrTrim(String(format='(I," sources above ",A," s/n")',ston_tot[ston_vals[ston_i]],ston_vals[ston_i]),2)
print,ston_string

binsize=0.15
hist_I=histogram(Alog10(flux_I),locations=loc_I,binsize=binsize,omin=omin,max=max(ALog10(flux_I))+binsize,omax=omax)
total_hist=Total(reverse(hist_I),/cumulative)

loc_I+=binsize/2.
flux_I_bin=10.^loc_I
binsize_use=(10.^(loc_I+binsize/2.)-10.^(loc_I-binsize/2.))

hist_I_norm=hist_I/binsize_use/zoom_area
error_I=Sqrt(hist_I/2.)/binsize_use/zoom_area
error_I_up=error_I+hist_I_norm
error_I_down=(hist_I_norm-error_I);>(1./(zoom_area*Max(binsize_use))/10.)
xrange=[min(flux_I_bin)>1e-2,max(flux_I_bin)]
yrange=[1./(zoom_area*Max(binsize_use)),max(hist_I_norm)*10.]

plot,flux_I_bin,hist_I_norm,/xlog,/ylog,xtitle='Source flux (~ Jy)',ytitle='d(Number/degree^2)/d(flux)',$
    xrange=xrange,yrange=yrange,background=!white,color=!black,xmargin=[12,1],ymargin=[4,1],psym=1
XYoutS,400,450,"+ Stokes I",color=!black,/device
plot_image=tvread(true=0)
wdelete
write_png,filepath_short2+' sourcecounts.png',plot_image

beam_radius=sqrt((meshgrid(dimension,elements,1)-dimension/2.)^2+(meshgrid(dimension,elements,2)-elements/2.)^2)*beam_mask_use-(1.-beam_mask_use)
n_radius_bin0=3.
beam_range=21.
add_small=0.
n_radius_bin=n_radius_bin0+add_small
radius_bin_npix=Npix_center/n_radius_bin0
radius_arr=fltarr(n_radius_bin+1)
radius_arr[1+add_small:*]=(findgen(n_radius_bin0)+1.)*(beam_range/degpix)/n_radius_bin0
IF Keyword_Set(add_small) THEN radius_arr[1:add_small]=radius_arr[add_small+1]*(findgen(add_small)+1)/(add_small+1)

hist_theory=source_counts_theory(flux_I_bin,g1=1.75)
source_counts_6c_catalog,hist_6C,flux_bin_6C,binsize=binsize,min_use=omin

loadct,39
lstyle_list=[2,1,3,5,4]
leg_x=400 ;360
leg_y=460 ;480
leg_y2=leg_y+4
i_use=where(flux_I_bin GE 3)
plot,flux_bin_6C,hist_6C,/xlog,/ylog,xtitle='Source flux (Jy)',ytitle='dN(S)/dS (Number/deg!E2!N/Jy)',psym=10,$
    xrange=xrange,yrange=yrange,background=!white,color=!black,xmargin=[12,1],ymargin=[6,1],thick=2,xcharsize=1.5,ycharsize=1.5
plots,[leg_x-30,leg_x],[leg_y2,leg_y2],/device,color=!black,thick=2
XYoutS,leg_x,leg_y,"  6C source counts.",color=!black,/device,charsize=1.5;,charthick=2
count=1        

oplot,flux_I_bin[i_use],hist_I_norm[i_use],color=!red,thick=2,linestyle=0,psym=10
plots,[leg_x-30,leg_x],[leg_y2,leg_y2]-20*(count),/device,color=!red,line=0,thick=2;,psym=1
XYoutS,leg_x,leg_y-20*(count),"  Full "+filename_short2+" field.",color=!black,/device,charsize=1.5;,charthick=2
count+=1


errplot,flux_I_bin[i_use],error_I_down[i_use],error_I_up[i_use],color=!red,thick=2;,psym=1
FOR i=0,n_radius_bin-1 DO BEGIN
    IF Keyword_Set(add_small) AND i EQ add_small THEN CONTINUE
    IF Keyword_Set(add_small) THEN BEGIN
        IF i EQ add_small THEN BEGIN
            si_use=where((source_dist GE radius_arr[0]) AND (source_dist LT radius_arr[i+1]),n_si_use)
            zoom_area2=N_Elements(where((beam_radius GE radius_arr[0]) AND (beam_radius LT radius_arr[i+1]) AND (beam_mask_use GT 0)))*degpix^2.
        ENDIF
        IF i LT add_small THEN BEGIN
            si_use=where((source_dist GE radius_arr[i]) AND (source_dist LT radius_arr[i+1]),n_si_use)
            zoom_area2=N_Elements(where((beam_radius GE radius_arr[i]) AND (beam_radius LT radius_arr[i+1]) AND (beam_mask_use GT 0)))*degpix^2.
        ENDIF
        IF i GT add_small THEN BEGIN
            si_use=where((source_dist GE radius_arr[i]) AND (source_dist LT radius_arr[i+1]),n_si_use)
            zoom_area2=N_Elements(where((beam_radius GE radius_arr[i]) AND (beam_radius LT radius_arr[i+1]) AND (beam_mask_use GT 0)))*degpix^2.
        ENDIF
    ENDIF ELSE BEGIN
        si_use=where((source_dist GE radius_arr[i]) AND (source_dist LT radius_arr[i+1]),n_si_use)
        zoom_area2=N_Elements(where((beam_radius GE radius_arr[i]) AND (beam_radius LT radius_arr[i+1]) AND (beam_mask_use GT 0)))*degpix^2.
    ENDELSE
    IF n_si_use EQ 0 THEN CONTINUE
    linestyle=1+(i mod 5)
    psym_style=2+(i mod 5)
    IF psym_style GE 3 THEN psym_style+=1
    hist_I1=histogram(Alog10(flux_I[si_use]),binsize=binsize,min=omin,max=omax)
    hist_I1_norm=hist_I1/binsize_use/zoom_area2
    error_I1=Sqrt(hist_I1/2.)/binsize_use/zoom_area2
    error_I1_up=error_I1+hist_I1_norm
    error_I1_down=(hist_I1_norm-error_I1)
    
    i_use=where((hist_I1_norm NE 0) AND (flux_I_bin LE 4))
    
    oplot,flux_I_bin[i_use],hist_I1_norm[i_use],color=!red,linestyle=lstyle_list[i],thick=2,psym=10
    plots,[leg_x-30,leg_x],[leg_y2,leg_y2]-20*(count),/device,color=!red,line=lstyle_list[i],thick=2
    IF i EQ 0 OR i EQ add_small THEN $
        string1=String(format='("  Central ",G4.3,"!Eo!N.")',radius_arr[i+1]*degpix) $;"  Field center" $
    ELSE string1=String(format='("  ",G4.3,"!Eo!N to ",G4.3,"!Eo!N ring.")',radius_arr[i]*degpix,radius_arr[i+1]*degpix)
    XYoutS,leg_x,leg_y-20*(count),string1,color=!black,/device,charsize=1.5;,charthick=2
    count+=1
    
    beam_val0=(Sqrt((*beam_correction2[0])*(*beam_correction2[1])))[dimension/2.,dimension/2.+radius_arr[i]]
    pol_val0=(*polarization_correction[0,0])[dimension/2.,dimension/2.+radius_arr[i]]
    beam_val1=(Sqrt((*beam_correction2[0])*(*beam_correction2[1])))[dimension/2.,dimension/2.+radius_arr[i+1]]
    pol_val1=(*polarization_correction[0,0])[dimension/2.,dimension/2.+radius_arr[i+1]]
    print,"Range of beam in ring:",beam_val0*pol_val0,beam_val1*pol_val1
ENDFOR

plot_image=tvread(true=1)
wdelete
write_png,filepath_short2+'Isourcecounts.png',plot_image,/verbose

loadct,color_table

END