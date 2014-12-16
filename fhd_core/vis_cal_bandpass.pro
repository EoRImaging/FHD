FUNCTION vis_cal_bandpass,cal,obs,cal_remainder=cal_remainder,file_path_fhd=file_path_fhd
;This function is version 1 of calibrating each group of tiles with similar cable lengths per observation.  


;Extract needed elements from the input structures
gain_arr_ptr=cal.gain
n_pol=cal.n_pol
n_freq=cal.n_freq
n_tile=cal.n_tile
IF N_Elements(obs) GT 0 THEN freq_use=where((*obs.baseline_info).freq_use) ELSE freq_use=lindgen(n_freq)
freq_arr=cal.freq
IF N_Elements(obs) GT 0 THEN tile_use=where((*obs.baseline_info).tile_use) ELSE tile_use=lindgen(n_tile)

;Find element numbers for loops later
nf_use=N_Elements(freq_use)
nt_use=N_Elements(tile_use)
n_pol=N_Elements(gain_arr_ptr)

;Initialize arrays
gain_arr_ptr2=Ptrarr(n_pol,/allocate)
gain_arr_ptr3=Ptrarr(n_pol,/allocate)

;n_freq x 13 array. columns are frequency, 90m xx, 90m yy, 150m xx, 150m yy, 230m xx, 230m yy, 320m xx, 320m yy, 400m xx, 400m yy, 524m xx, 524m yy
bandpass_arr=Fltarr((n_pol)*6+1,n_freq)
bandpass_arr[0,*]=freq_arr
bandpass_col_count=0

;Using preexisting file to extract information about which tiles have which cable length
mode_filepath=filepath(obs.instrument+'_cable_reflection_coefficients.txt',root=rootdir('FHD'),subdir='instrument_config')
textfast,data_array,/read,file_path=mode_filepath,first_line=1
cable_len=Reform(data_array[2,*])

;Taking tile information and cross-matching it with the nonflagged tiles array, resulting in nonflagged tile arrays
;grouped by cable length
tile_use_90=where((*obs.baseline_info).tile_use AND cable_len EQ 90)
tile_use_150=where((*obs.baseline_info).tile_use AND cable_len EQ 150)
tile_use_230=where((*obs.baseline_info).tile_use AND cable_len EQ 230)
tile_use_320=where((*obs.baseline_info).tile_use AND cable_len EQ 320)
tile_use_400=where((*obs.baseline_info).tile_use AND cable_len EQ 400)
tile_use_524=where((*obs.baseline_info).tile_use AND cable_len EQ 524)

;Main gain calculation loop
FOR cable_i=0,5 DO BEGIN
    
    IF cable_i EQ 0 THEN tile_use_cable=tile_use_90
    IF cable_i EQ 1 THEN tile_use_cable=tile_use_150
    IF cable_i EQ 2 THEN tile_use_cable=tile_use_230
    IF cable_i EQ 3 THEN tile_use_cable=tile_use_320
    IF cable_i EQ 4 THEN tile_use_cable=tile_use_400
    IF cable_i EQ 5 THEN tile_use_cable=tile_use_524
    
    ;This is an option to calibrate over all tiles to find the 'global' bandpass. It will be looped over by the number
    ;of cable lengths, and will redo the same calculation everytime. It is inefficient, but effective.
    IF Keyword_set(normal_bp_cal) THEN tile_use_cable=tile_use
    
    nt_use_cable=N_Elements(tile_use_cable)

    FOR pol_i=0,n_pol-1 DO BEGIN

         gain=*gain_arr_ptr[pol_i] ;n_freq x n_tile element complex array
         
         ;gain2 is a temporary variable used in place of the gain array for an added layer of safety
         IF cable_i EQ 0 THEN gain2=Complexarr(size(gain,/dimension))
         
         ;Only use gains from unflagged tiles and frequencies, and calculate the amplitude and phase
         gain_use=extract_subarray(gain,freq_use,tile_use_cable)
         amp=Abs(gain_use)
         phase=Atan(gain_use,/phase)
         
         ;amp2 is a temporary variable used in place of the amp array for an added layer of safety
         amp2=fltarr(nf_use,nt_use_cable)
    
         ;This is the normalization loop for each tile. If the median of gain amplitudes over all frequencies is nonzero, then divide
         ;the gain amplitudes by that number, otherwise make the gain amplitudes zero. It is unclear at this point in time whether the 
         ;median is better/worse than a resistant mean with a 2sigma cut. 
         FOR tile_i=0,nt_use_cable-1 DO amp2[*,tile_i]=(Median(amp[*,tile_i]) EQ 0) ? 0:(amp[*,tile_i]/Median(amp[*,tile_i]))
         
         ;This finds the normalized gain amplitude median per frequency over all tiles, which is the final bandpass per cable group. 
         bandpass_single=Median(amp2,dimension=2)
         
         ;Want iterative to start at 1 (to not overwrite freq) and store final bandpass per cable group.
         bandpass_col_count = bandpass_col_count+1
         bandpass_arr[bandpass_col_count,freq_use]=bandpass_single
         
         ;Fill temporary variable gain2, set equal to final bandpass per cable group for each tile that will use that bandpass.
         ;As cables are looped through, gain2 will fill up with the correct bandpass per tile.
         FOR tile_i=0,N_elements(tile_use_cable)-1 DO gain2[freq_use,tile_use_cable[tile_i]]=bandpass_single   
        
         ;Execute last bit at the end of the cable loop
         IF cable_i EQ 5 THEN BEGIN
             gain3=gain
             
             ;Set what will be passed back as the output gain as the final bandpass per cable type
             *gain_arr_ptr2[pol_i]=gain2
             
             ;Set what will be passed back as the residual as the input gain divided by the final bandpass per cable type.
             FOR tile_i=0,n_tile-1 DO gain3[freq_use,tile_i]/=gain2[freq_use,tile_i]
             *gain_arr_ptr3[pol_i]=gain3
         ENDIF
    ENDFOR ;end pol for
    undefine, tile_use_cable, nt_use_cable, gain_use, amp, phase, amp2, bandpass_single
ENDFOR ; end cable for

;Return the final bandpass per cable type as the cal_bandpass.gain. Return the residual (input gain/global bp) as cal_remainder.gain.
cal_bandpass=cal
cal_bandpass.gain=gain_arr_ptr2
cal_remainder=cal
cal_remainder.gain=gain_arr_ptr3

;Begin plotting

;This is where regular plots of each cable group's bandpass per xx and yy are plotted.
IF Keyword_Set(file_path_fhd) THEN BEGIN
    basename=file_basename(file_path_fhd)
    dirpath=file_dirname(file_path_fhd)
    image_path='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_cable_cal_v2/output_images/'
    ;image_path=filepath(basename,root=dirpath,sub='output_images')
    IF file_test(file_dirname(image_path),/directory) EQ 0 THEN file_mkdir,file_dirname(image_path)
    export_path='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_cable_cal_v2/calibration/'
    ;export_path=filepath(basename,root=dirpath,sub='calibration')
    IF file_test(file_dirname(export_path),/directory) EQ 0 THEN file_mkdir,file_dirname(export_path)
    Textfast,bandpass_arr,/write,file_path=export_path+obs.obsname+'_bandpass'
    
    freq=freq_arr/1E6
    xtickv=[ceil(min(freq)/10)*10,floor(max(freq)/10)*10]
    xtickname=strtrim(round(xtickv),2)
    xrange=[min(freq)-(max(freq)-min(freq))/8,max(freq)+(max(freq)-min(freq))/8]
    
    yrange=[min(bandpass_arr[1:(n_pol*6),freq_use]),max(bandpass_arr[1:(n_pol*6),freq_use])]
    
    ytickv=[yrange[0],mean(yrange),yrange[1]]
    axiscolor='black'
    
    cgPS_Open,image_path+obs.obsname+'_bandpass_xx.png',/quiet,/nomatch
    cgplot,freq[freq_use],bandpass_arr[1,freq_use],color='blue',title=obs.obsname + ' xx',xtitle='Frequency [MHz]',ytitle='Gain',$
        yrange=yrange,xrange=xrange,/noerase,axiscolor=axiscolor,psym=2,symsize=0.2
    cgoplot,freq[freq_use],bandpass_arr[3,freq_use],color='red',psym=2,symsize=0.2
    cgoplot,freq[freq_use],bandpass_arr[5,freq_use],color='green',psym=2,symsize=0.2
    cgoplot,freq[freq_use],bandpass_arr[7,freq_use],color='purple',psym=2,symsize=0.2
    cgoplot,freq[freq_use],bandpass_arr[9,freq_use],color='yellow',psym=2,symsize=0.2
    cgoplot,freq[freq_use],bandpass_arr[11,freq_use],color='black',psym=2,symsize=0.2
    cgLegend, Title=['90m cables ('+ Strtrim(String(N_elements(tile_use_90)),1) +')','150m cables ('+ Strtrim(String(N_elements(tile_use_150)),1) +')', $
        '230m cables ('+ Strtrim(String(N_elements(tile_use_230)),1) +')', '320m cables ('+ Strtrim(String(N_elements(tile_use_320)),1) +')', $
        '400m cables ('+ Strtrim(String(N_elements(tile_use_400)),1) +')','524m cables ('+ Strtrim(String(N_elements(tile_use_524)),1) +')'], $
        Color=['blue','red','green','purple','yellow','black'],Psym=[2,2,2,2,2,2], $
        Length=0.0,Location=[0.55,0.85]
    cgPS_Close,/png,Density=300,Resize=100.,/allow_transparent,/nomessage
    
    cgPS_Open,image_path+obs.obsname+'_bandpass_yy.png',/quiet,/nomatch
    cgplot,freq[freq_use],bandpass_arr[2,freq_use],color='blue',title=obs.obsname + ' yy',xtitle='Frequency [MHz]',ytitle='Gain',$
        yrange=yrange,xrange=xrange,/noerase,axiscolor=axiscolor,psym=2,symsize=0.2
    cgoplot,freq[freq_use],bandpass_arr[4,freq_use],color='red',psym=2,symsize=0.2
    cgoplot,freq[freq_use],bandpass_arr[6,freq_use],color='green',psym=2,symsize=0.2
    cgoplot,freq[freq_use],bandpass_arr[8,freq_use],color='purple',psym=2,symsize=0.2
    cgoplot,freq[freq_use],bandpass_arr[10,freq_use],color='yellow',psym=2,symsize=0.2
    cgoplot,freq[freq_use],bandpass_arr[12,freq_use],color='black',psym=2,symsize=0.2
    cgLegend, Title=['90m cables ('+ Strtrim(String(N_elements(tile_use_90)),1) +')','150m cables ('+ Strtrim(String(N_elements(tile_use_150)),1) +')', $
        '230m cables ('+ Strtrim(String(N_elements(tile_use_230)),1) +')', '320m cables ('+ Strtrim(String(N_elements(tile_use_320)),1) +')', $
        '400m cables ('+ Strtrim(String(N_elements(tile_use_400)),1) +')','524m cables ('+ Strtrim(String(N_elements(tile_use_524)),1) +')'], $
        Color=['blue','red','green','purple','yellow','black'],Psym=[2,2,2,2,2,2], $
        Length=0.0,Location=[0.55,0.85]
    cgPS_Close,/png,Density=300,Resize=100.,/allow_transparent,/nomessage
ENDIF


;This is where a bandpass fractional change is calculated if a directory is supplied. Below is an example. It is assumed that the bandpass
;supplied is a single, 'global' bandpass. Uncomment cgLegend below if a legend is wanted.
bp_change_dir='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_apb_std_Dec2014b/calibration/'
IF Keyword_Set(bp_change_dir) THEN BEGIN
    bandpass_arr_change=bandpass_arr
    filename=bp_change_dir + obs.obsname + '_bandpass.txt'
    
    ;Change the next three lines if the bandpass to calculate the fractional change from is different from a single, 'global' bandpass.
    readcol, filename, freq_old, old_xx, old_yy
    FOR i=1, 6 DO bandpass_arr_change[i*2-1,*]=(bandpass_arr[i*2-1,*]-old_xx[*])/old_xx[*]
    FOR i=1, 6 DO bandpass_arr_change[i*2,*]=(bandpass_arr[i*2,*]-old_yy[*])/old_yy[*]

    basename=file_basename(file_path_fhd)
    dirpath=file_dirname(file_path_fhd)
    image_path='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_cable_cal_v2/output_images/'
    ;image_path=filepath(basename,root=dirpath,sub='output_images')
    IF file_test(file_dirname(image_path),/directory) EQ 0 THEN file_mkdir,file_dirname(image_path)
    export_path='/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_cable_cal_v2/calibration/'
    ;export_path=filepath(basename,root=dirpath,sub='calibration')
    IF file_test(file_dirname(export_path),/directory) EQ 0 THEN file_mkdir,file_dirname(export_path)
    Textfast,bandpass_arr_change,/write,file_path=export_path+obs.obsname+'_bandpass_change'
    
    freq=freq_arr/1E6
    xtickv=[ceil(min(freq)/10)*10,floor(max(freq)/10)*10]
    xtickname=strtrim(round(xtickv),2)
    xrange=[min(freq)-(max(freq)-min(freq))/8,max(freq)+(max(freq)-min(freq))/8]
    yrange=[min(bandpass_arr_change[1:(n_pol*6),freq_use]),max(bandpass_arr_change[1:(n_pol*6),freq_use])]
    ytickv=[yrange[0],mean(yrange),yrange[1]]
    axiscolor='black'
    
    cgPS_Open,image_path+obs.obsname+'_bandpass_change_xx.png',/quiet,/nomatch
    cgplot,freq[freq_use],bandpass_arr_change[1,freq_use],color='blue',title=obs.obsname + ': (cable-orig)/orig xx',xtitle='Frequency [MHz]',ytitle='Gain',$
        yrange=yrange,xrange=xrange,/noerase,axiscolor=axiscolor,psym=2,symsize=0.2
    cgoplot,freq[freq_use],bandpass_arr_change[3,freq_use],color='red',psym=2,symsize=0.2
    cgoplot,freq[freq_use],bandpass_arr_change[5,freq_use],color='green',psym=2,symsize=0.2
    cgoplot,freq[freq_use],bandpass_arr_change[7,freq_use],color='purple',psym=2,symsize=0.2
    cgoplot,freq[freq_use],bandpass_arr_change[9,freq_use],color='yellow',psym=2,symsize=0.2
    cgoplot,freq[freq_use],bandpass_arr_change[11,freq_use],color='black',psym=2,symsize=0.2
    ;cgLegend, Title=['90m cables ('+ Strtrim(String(N_elements(tile_use_90)),1) +')','150m cables ('+ Strtrim(String(N_elements(tile_use_150)),1) +')', $
    ;    '230m cables ('+ Strtrim(String(N_elements(tile_use_230)),1) +')', '320m cables ('+ Strtrim(String(N_elements(tile_use_320)),1) +')', $
    ;    '400m cables ('+ Strtrim(String(N_elements(tile_use_400)),1) +')','524m cables ('+ Strtrim(String(N_elements(tile_use_524)),1) +')'], $
    ;    Color=['blue','red','green','purple','yellow','black'],Psym=[2,2,2,2,2,2], $
    ;    Length=0.0,Location=[0.55,0.85]  
    cgPS_Close,/png,Density=300,Resize=100.,/allow_transparent,/nomessage
    
    cgPS_Open,image_path+obs.obsname+'_bandpass_change_yy.png',/quiet,/nomatch
    cgplot,freq[freq_use],bandpass_arr_change[2,freq_use],color='blue',title=obs.obsname + ': (cable-orig)/orig yy',xtitle='Frequency [MHz]',ytitle='Gain',$
        yrange=yrange,xrange=xrange,/noerase,axiscolor=axiscolor,psym=2,symsize=0.2
    cgoplot,freq[freq_use],bandpass_arr_change[4,freq_use],color='red',psym=2,symsize=0.2
    cgoplot,freq[freq_use],bandpass_arr_change[6,freq_use],color='green',psym=2,symsize=0.2
    cgoplot,freq[freq_use],bandpass_arr_change[8,freq_use],color='purple',psym=2,symsize=0.2
    cgoplot,freq[freq_use],bandpass_arr_change[10,freq_use],color='yellow',psym=2,symsize=0.2
    cgoplot,freq[freq_use],bandpass_arr_change[12,freq_use],color='black',psym=2,symsize=0.2
    ;cgLegend, Title=['90m cables ('+ Strtrim(String(N_elements(tile_use_90)),1) +')','150m cables ('+ Strtrim(String(N_elements(tile_use_150)),1) +')', $
    ;    '230m cables ('+ Strtrim(String(N_elements(tile_use_230)),1) +')', '320m cables ('+ Strtrim(String(N_elements(tile_use_320)),1) +')', $
    ;    '400m cables ('+ Strtrim(String(N_elements(tile_use_400)),1) +')','524m cables ('+ Strtrim(String(N_elements(tile_use_524)),1) +')'], $
    ;    Color=['blue','red','green','purple','yellow','black'],Psym=[2,2,2,2,2,2], $
    ;    Length=0.0,Location=[0.55,0.85]
    cgPS_Close,/png,Density=300,Resize=100.,/allow_transparent,/nomessage
ENDIF

RETURN,cal_bandpass
END
