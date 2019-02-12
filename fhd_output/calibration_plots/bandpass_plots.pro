PRO bandpass_plots,obs,bandpass_arr,file_path_fhd=file_path_fhd,$
    cable_length_ref=cable_length_ref,tile_use_arr=tile_use_arr

n_pol=obs.n_pol < 2
freq_arr=(*obs.baseline_info).freq
freq_use=where((*obs.baseline_info).freq_use)

basename=file_basename(file_path_fhd)
dirpath=file_dirname(file_path_fhd)
image_path=filepath(basename,root=dirpath,sub='output_images')
IF file_test(file_dirname(image_path),/directory) EQ 0 THEN file_mkdir,file_dirname(image_path)
export_path=filepath(basename,root=dirpath,sub='calibration')
IF file_test(file_dirname(export_path),/directory) EQ 0 THEN file_mkdir,file_dirname(export_path)
Textfast,bandpass_arr,/write,file_path=export_path+'_bandpass'

freq=freq_arr/1E6
xtickv=[ceil(min(freq)/10)*10,floor(max(freq)/10)*10]
xtickname=strtrim(round(xtickv),2)
range_pad=.125
xrange=[min(freq)-(max(freq)-min(freq))*range_pad,max(freq)+(max(freq)-min(freq))*range_pad]
axiscolor='black'
;check if there different bandpasses for different cable types
IF (size(bandpass_arr,/dimension))[0] EQ (n_pol+1) THEN BEGIN ;first column of bandpass_arr is always frequency
    yrange=[min(bandpass_arr[1:n_pol,freq_use]),max(bandpass_arr[1:n_pol,freq_use])]
    ytickv=[yrange[0],mean(yrange),yrange[1]]
    cgPS_Open,image_path+'_bandpass.png',/quiet,/nomatch
    cgplot,freq[freq_use],bandpass_arr[1,freq_use],color='blue',title=basename,xtitle='Frequency [MHz]',ytitle='Gain',$
        yrange=yrange,xrange=xrange,/noerase,axiscolor=axiscolor,psym=2,symsize=0.2
    IF n_pol GT 1 THEN cgoplot,freq[freq_use],bandpass_arr[2,freq_use],color='red',psym=2,symsize=0.2
    cgPS_Close,/png,Density=300,Resize=100.,/allow_transparent,/nomessage
ENDIF ELSE BEGIN
    
    n_cable=N_Elements(cable_length_ref)    
    yrange=[min(bandpass_arr[1:(n_pol*n_cable),freq_use]),max(bandpass_arr[1:(n_pol*n_cable),freq_use])]
    ytickv=[yrange[0],mean(yrange),yrange[1]]
    ;PRINT, n_cable 
    ;n_cable=N_Elements(cable_length_ref)
    Title=Strarr(n_cable)
    FOR cable_i=0,n_cable-1 DO Title[cable_i]=String(format='(A,"m cables (",A,")")',Strn(Round(cable_length_ref[cable_i])),Strn(N_Elements(*tile_use_arr[cable_i])))
    color_arr=['blue','red','green','purple','yellow','black','cyan','orange']
    
    cgPS_Open,image_path+'_bandpass_xx.png',/quiet,/nomatch
    cgplot,freq[freq_use],bandpass_arr[1,freq_use],color=color_arr[0],title=basename + ' xx',xtitle='Frequency [MHz]',ytitle='Gain',$
        yrange=yrange,xrange=xrange,/noerase,axiscolor=axiscolor,psym=2,symsize=0.2
    FOR cable_i=1,n_cable-1 DO cgoplot,freq[freq_use],bandpass_arr[2*cable_i+1,freq_use],color=color_arr[cable_i],psym=2,symsize=0.2     
    cgLegend, Title=Title, Color=color_arr[0:(n_cable-1)],Psym=Replicate(2,n_cable),Length=0.0,Location=[0.55,0.85]
    cgPS_Close,/png,Density=300,Resize=100.,/allow_transparent,/nomessage
    
    IF n_pol GT 1 THEN BEGIN
        cgPS_Open,image_path+'_bandpass_yy.png',/quiet,/nomatch
        cgplot,freq[freq_use],bandpass_arr[2,freq_use],color=color_arr[0],title=basename + ' yy',xtitle='Frequency [MHz]',ytitle='Gain',$
            yrange=yrange,xrange=xrange,/noerase,axiscolor=axiscolor,psym=2,symsize=0.2
        FOR cable_i=1,n_cable-1 DO cgoplot,freq[freq_use],bandpass_arr[2*cable_i+2,freq_use],color=color_arr[cable_i],psym=2,symsize=0.2
        cgLegend, Title=Title, Color=color_arr[0:(n_cable-1)],Psym=Replicate(2,n_cable),Length=0.0,Location=[0.55,0.85]
        cgPS_Close,/png,Density=300,Resize=100.,/allow_transparent,/nomessage
    ENDIF
    
    ; cgPS_Close,/png,Density=300,Resize=100.,/allow_transparent,/nomessage
ENDELSE
END
