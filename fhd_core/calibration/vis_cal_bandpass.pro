FUNCTION vis_cal_bandpass,cal,obs,params,cal_remainder=cal_remainder,file_path_fhd=file_path_fhd,cable_bandpass_fit=cable_bandpass_fit,$
    bandpass_directory=bandpass_directory,tile_use=tile_use,calibration_bandpass_cable_exclude=calibration_bandpass_cable_exclude,$
    cal_bp_transfer=cal_bp_transfer,uvfits_version=uvfits_version,uvfits_subversion=uvfits_subversion,auto_ratio_calibration=auto_ratio_calibration,Extra=extra
  ;This function is version 1 of calibrating each group of tiles with similar cable lengths per observation.
    
  ;Extract needed elements from the input structures
  gain_arr_ptr=cal.gain
  n_pol=cal.n_pol
  n_freq=cal.n_freq
  n_tile=cal.n_tile
  IF N_Elements(obs) GT 0 THEN freq_use=where((*obs.baseline_info).freq_use) ELSE freq_use=lindgen(n_freq)
  freq_arr=cal.freq
  IF N_Elements(obs) GT 0 THEN tile_use=where((*obs.baseline_info).tile_use) ELSE tile_use=lindgen(n_tile)
  
  IF Keyword_Set(calibration_bandpass_cable_exclude) THEN BEGIN
    mode_filepath=filepath(obs.instrument+'_cable_reflection_coefficients.txt',root=rootdir('FHD'),subdir='instrument_config')
    textfast,data_array,/read,file_path=mode_filepath,first_line=1
    cable_len=Reform(data_array[2,*])
    FOR cable_i=0,N_Elements(calibration_bandpass_cable_exclude)-1 DO tile_use=tile_use[where(cable_len[tile_use] NE calibration_bandpass_cable_exclude[cable_i])]
  ENDIF
  ;Find element numbers for loops later
  nf_use=N_Elements(freq_use)
  nt_use=N_Elements(tile_use)
  n_pol=N_Elements(gain_arr_ptr)
  
  ;restoring saved bandpass
  If keyword_set(cal_bp_transfer) then begin
    RETURN, transfer_bandpass(cal_bp_transfer,cal,obs,params,cal_remainder=cal_remainder,silent=silent,_Extra=extra)
  endif
  
  ;Initialize arrays
  gain_arr_ptr2=Ptrarr(n_pol,/allocate)
  gain_arr_ptr3=Ptrarr(n_pol,/allocate)
  
  IF Keyword_Set(cable_bandpass_fit) THEN BEGIN
  
  
    ;Using preexisting file to extract information about which tiles have which cable length
    mode_filepath=filepath(obs.instrument+'_cable_reflection_coefficients.txt',root=rootdir('FHD'),subdir='instrument_config')
    textfast,data_array,/read,file_path=mode_filepath,first_line=1
    cable_len=Reform(data_array[2,*])
    
    ;Taking tile information and cross-matching it with the nonflagged tiles array, resulting in nonflagged tile arrays
    ;grouped by cable length
    cable_length_ref=cable_len[Uniq(cable_len,Sort(cable_len))]
    n_cable=N_Elements(cable_length_ref)
    tile_use_arr=Ptrarr(n_cable)
    FOR cable_i=0,n_cable-1 DO BEGIN
       tile_use_arr[cable_i]=Ptr_new(where((*obs.baseline_info).tile_use AND cable_len EQ cable_length_ref[cable_i]))
       if (N_elements(*tile_use_arr[cable_i]) EQ 0) AND ~keyword_set(cal_bp_transfer) then begin
          print, 'WARNING: Too many flagged tiles to implement bandpass cable averaging. Using global bandpass.'
          normal_bp_cal=1
       endif 
    ENDFOR
    
    ;n_freq x 13 array. columns are frequency, 90m xx, 90m yy, 150m xx, 150m yy, 230m xx, 230m yy, 320m xx, 320m yy, 400m xx, 400m yy, 524m xx, 524m yy
    bandpass_arr=Fltarr((n_pol)*n_cable+1,n_freq)
    bandpass_arr[0,*]=freq_arr
    bandpass_col_count=0
    IF Keyword_set(auto_ratio_calibration) THEN BEGIN
      normal_bp_cal=1
      print, 'auto_ratio_calibration is set, using global bandpass'
    ENDIF
    ;Main gain calculation loop
    FOR cable_i=0,n_cable-1 DO BEGIN
      tile_use_cable=*tile_use_arr[cable_i]
      
      ;This is an option to calibrate over all tiles to find the 'global' bandpass. It will be looped over by the number
      ;of cable lengths, and will redo the same calculation everytime. It is inefficient, but effective.
      IF Keyword_set(normal_bp_cal) THEN tile_use_cable=tile_use
      
      nt_use_cable=N_Elements(tile_use_cable)
      
      FOR pol_i=0,n_pol-1 DO BEGIN
        gain=*gain_arr_ptr[pol_i] ;n_freq x n_tile element complex array
        
        ;gain2 is a temporary variable used in place of the gain array for an added layer of safety
        IF cable_i EQ 0 AND pol_i EQ 0 THEN gain2=Complexarr(n_pol,(size(gain))[1],(size(gain))[2])
        
        ;Only use gains from unflagged tiles and frequencies, and calculate the amplitude and phase
        gain_use=extract_subarray(gain,freq_use,tile_use_cable)
        amp=Abs(gain_use)
        phase=Atan(gain_use,/phase)
        
        ;amp2 is a temporary variable used in place of the amp array for an added layer of safety
        amp2=fltarr(nf_use,nt_use_cable)
        
        ;This is the normalization loop for each tile. If the mean of gain amplitudes over all frequencies is nonzero, then divide
        ;the gain amplitudes by that number, otherwise make the gain amplitudes zero.
        FOR tile_i=0,nt_use_cable-1 DO BEGIN
          resistant_mean,amp[*,tile_i],2,res_mean
          IF res_mean NE 0 THEN amp2[*,tile_i]=amp[*,tile_i]/res_mean ELSE amp2[*,tile_i]=0.
        ENDFOR
        
        ;This finds the normalized gain amplitude mean per frequency over all tiles, which is the final bandpass per cable group.
        
        bandpass_single=Fltarr(nf_use)
        FOR f_i=0L,nf_use-1 DO BEGIN
          resistant_mean,amp2[f_i,*],2,res_mean
          bandpass_single[f_i]=res_mean
        ENDFOR
        
        ;Override calc with saved run for pointing
        If keyword_set(cal_bp_transfer) then bandpass_single=bandpass_saved_sol[1+(cable_i*2)+(pol_i),freq_use]
        
        ;Want iterative to start at 1 (to not overwrite freq) and store final bandpass per cable group.
        bandpass_col_count += 1
        bandpass_arr[bandpass_col_count,freq_use]=bandpass_single
        
        ;Fill temporary variable gain2, set equal to final bandpass per cable group for each tile that will use that bandpass.
        ;As cables are looped through, gain2 will fill up with the correct bandpass per tile.
        FOR tile_i=0,N_elements(tile_use_cable)-1 DO gain2[pol_i,freq_use,tile_use_cable[tile_i]]=bandpass_single
        
        ;Execute last bit at the end of the cable loop
        IF cable_i EQ n_cable-1 THEN BEGIN
          ;Set gain3 to the input gains (safe from overwrite if referencing the orig gain pointer)
          gain3=*gain_arr_ptr[pol_i]
          
          ;Set what will be passed back as the output gain as the final bandpass per cable type.
          gain2_input = reform(gain2[pol_i,*,*])
          *gain_arr_ptr2[pol_i]=gain2_input
          
          ;Set what will be passed back as the residual as the input gain divided by the final bandpass per cable type.
          FOR tile_i=0,n_tile-1 DO gain3[freq_use,tile_i]/=gain2_input[freq_use,tile_i]
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
    
    ;Add the Levine memo bandpass to the gain solutions if applicable

;    IF keyword_set(uvfits_version) AND keyword_set(uvfits_subversion) then if (uvfits_version EQ 5) AND (uvfits_subversion EQ 1) then begin
;      for pol_i=0, n_pol-1 do for tile_i=0, n_tile-1 do cal_bandpass.gain[pol_i] = (cal_bandpass.gain[pol_i])[*,tile_i] * ave_bp_gains_fullband
;    endif

    
  ENDIF ELSE BEGIN
    bandpass_arr=Fltarr(n_pol+1,n_freq)
    bandpass_arr[0,*]=freq_arr
    FOR pol_i=0,n_pol-1 DO BEGIN
      gain=*gain_arr_ptr[pol_i] ;n_freq x n_tile element complex array
      gain_use=extract_subarray(gain,freq_use,tile_use)
      amp=Abs(gain_use)
      phase=Atan(gain_use,/phase)
      amp2=fltarr(nf_use,nt_use)
      
      FOR tile_i=0,nt_use-1 DO BEGIN
        resistant_mean,amp[*,tile_i],2,res_mean
        IF res_mean NE 0 THEN amp2[*,tile_i]=amp[*,tile_i]/res_mean ELSE amp2[*,tile_i]=0.
      ENDFOR
      bandpass_single=Fltarr(nf_use)
      FOR f_i=0L,nf_use-1 DO BEGIN
        resistant_mean,amp2[f_i,*],2,res_mean
        bandpass_single[f_i]=res_mean
      ENDFOR
      ;        bandpass_single=Median(amp2,dimension=2)
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
    
  ENDELSE
  
  IF Keyword_Set(file_path_fhd) THEN bandpass_plots,obs,bandpass_arr,file_path_fhd=file_path_fhd,$
    cable_length_ref=cable_length_ref,tile_use_arr=tile_use_arr
    
  RETURN,cal_bandpass
END
