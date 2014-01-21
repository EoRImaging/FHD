FUNCTION vis_cal_bandpass,cal,obs,cal_remainder=cal_remainder,file_path_fhd=file_path_fhd

gain_arr_ptr=cal.gain
n_pol=cal.n_pol
n_freq=cal.n_freq
n_tile=cal.n_tile
IF N_Elements(obs) GT 0 THEN freq_use=where((*obs.baseline_info).freq_use) ELSE freq_use=lindgen(n_freq)
nf_use=N_Elements(freq_use)
freq_arr=cal.freq
IF N_Elements(obs) GT 0 THEN tile_use=where((*obs.baseline_info).tile_use) ELSE tile_use=lindgen(n_tile)
nt_use=N_Elements(tile_use)

n_pol=N_Elements(gain_arr_ptr)

gain_arr_ptr2=Ptrarr(n_pol,/allocate)
gain_arr_ptr3=Ptrarr(n_pol,/allocate)
bandpass_arr=Fltarr(n_pol+1,n_freq)
bandpass_arr[0,*]=freq_arr
FOR pol_i=0,n_pol-1 DO BEGIN
    gain=*gain_arr_ptr[pol_i] ;n_freq x n_tile element complex array
    gain_use=extract_subarray(gain,freq_use,tile_use)
    amp=Abs(gain_use)
    phase=Atan(gain_use,/phase)
    amp2=fltarr(nf_use,nt_use)
    FOR tile_i=0,nt_use-1 DO amp2[*,tile_i]=(Median(amp[*,tile_i]) EQ 0) ? 0:(amp[*,tile_i]/Median(amp[*,tile_i]))
    bandpass_single=Median(amp2,dimension=2)
    bandpass_arr[pol_i+1,freq_use]=bandpass_single
    gain2=Complexarr(size(gain,/dimension))
    FOR tile_i=0,n_tile-1 DO gain2[freq_use,tile_i]=bandpass_single
    *gain_arr_ptr2[pol_i]=gain2
    gain3=gain
    FOR tile_i=0,n_tile-1 DO gain3[freq_use,tile_i]/=bandpass_single
    *gain_arr_ptr3[pol_i]=gain3
ENDFOR
cal_bandpass=cal
cal_bandpass.gain=gain_arr_ptr2
cal_remainder=cal
cal_remainder.gain=gain_arr_ptr3

IF Keyword_Set(file_path_fhd) THEN BEGIN
    basename=file_basename(file_path_fhd)
    dirpath=file_dirname(file_path_fhd)
    image_path=filepath(basename,root=dirpath,sub='images')
    IF file_test(file_dirname(image_path),/directory) EQ 0 THEN file_mkdir,file_dirname(image_path)
    export_path=filepath(basename,root=dirpath,sub='export')
    IF file_test(file_dirname(export_path),/directory) EQ 0 THEN file_mkdir,file_dirname(export_path)
    Textfast,bandpass_arr,/write,file_path=export_path+'_bandpass'
    
    freq=freq_arr/1E6
    xtickv=[ceil(min(freq)/10)*10,floor(max(freq)/10)*10]
    xtickname=strtrim(round(xtickv),2)
    xrange=[min(freq)-(max(freq)-min(freq))/8,max(freq)+(max(freq)-min(freq))/8]
    yrange=[min(bandpass_arr[1:n_pol,freq_use]),max(bandpass_arr[1:n_pol,freq_use])]
    ytickv=[yrange[0],mean(yrange),yrange[1]]
    axiscolor='black'
    cgPS_Open,image_path+'_bandpass.png',/quiet,/nomatch
    cgplot,freq[freq_use],bandpass_arr[1,freq_use],color='blue',title=obs.obsname,xtitle='Frequency [MHz]',ytitle='Gain',$
        yrange=yrange,xrange=xrange,/noerase,axiscolor=axiscolor,psym=10
    IF n_pol GT 1 THEN cgoplot,freq[freq_use],bandpass_arr[2,freq_use],color='red',psym=10
    cgPS_Close,/png,Density=75,Resize=100.,/allow_transparent,/nomessage
ENDIF

RETURN,cal_bandpass
END