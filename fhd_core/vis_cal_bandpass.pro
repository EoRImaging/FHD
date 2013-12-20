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
    
    PS_START,image_path+'_bandpass.png',scale_factor=2,/quiet,/nomatch
    cgplot,freq,abs(gains0[*,tile_i]),color='blue',title=strtrim(tile_name,2),$
        xticks=1,xtickv=xtickv,xtickname=xtickname,yticks=2,ytickv=ytickv,position=plot_pos[tile_i,*],$
        yticklen=0.04,yrange=yrange,xrange=xrange,charsize=.5,/noerase,axiscolor=axiscolor,psym=3
    cgtext,.4,max(plot_pos[*,3]+height/4),obs.obsname,/normal
    PS_END,/png,Density=75,Resize=100.,/allow_transparent,/nomessage
ENDIF

RETURN,cal_bandpass
END