function transfer_bandpass, cal_bp_transfer,cal,obs,params,silent=silent,cal_remainder=cal_remainder,_Extra=extra

    if cal_bp_transfer EQ 1 then begin
      cal_bp_transfer = filepath('mwa_eor0_highband_season1_cable_bandpass.fits',root=rootdir('FHD'),subdir='instrument_config')
      if ~keyword_set(silent) then print, 'cal_bp_transfer defaulting to ' + cal_bp_transfer
    endif else if file_test(cal_bp_transfer) EQ 0 THEN message, cal_bp_transfer + ' not found.'

    if ~keyword_set(silent) then print, 'Bandpass saved run activated, using ' + cal_bp_transfer
    CASE StrLowCase(Strmid(cal_bp_transfer[0],3,/reverse)) OF
    
      ;can use a cal sav file to restore a bandpass
      '.sav':BEGIN
        cal_bandpass=getvar_savefile(cal_bp_transfer,'cal')
        IF ~Keyword_Set(cal_bandpass.cal_origin) THEN cal_bandpass.cal_origin=cal_bp_transfer
        cal_bandpass=fhd_struct_init_cal(obs,params,calibration_origin=cal_bandpass.cal_origin,gain_arr_ptr=cal_bandpass.gain,_Extra=extra)
        cal_remainder=cal
        for pol_i=0, n_pol-1 do *cal_remainder.gain[pol_i]=(*cal.gain[pol_i])/(*cal_bandpass.gain[pol_i])
        Return, cal_bandpass
      END
        
      ;can use a txt file to read-in a gain array or a 13x384 cable bandpass (obsolete, please use cal fits)  
      '.txt':BEGIN
        textfast,gain_arr,/read,file_path=cal_bp_transfer
        if ~keyword_set(cable_bandpass_fit) AND (size(gain_arr))[1] NE 13 then begin
          textfast,gain_arr,/read,file_path=cal_bp_transfer
          gain_arr_ptr=Ptr_new(gain_arr)
          cal_bandpass=fhd_struct_init_cal(obs,params,calibration_origin=cal_bp_transfer,gain_arr_ptr=gain_arr_ptr,_Extra=extra)
          cal_remainder=cal
          for pol_i=0, n_pol-1 do *cal_remainder.gain[pol_i]=(*cal.gain[pol_i])/(*cal_bandpass.gain[pol_i])
          Return, cal_bandpass
        endif else begin 
          bandpass_saved_sol = gain_arr ;columns are: freq_arr_input, cable90xx, cable90yy, cable150xx, cable150yy, cable230xx, cable230yy, cable320xx, cable320yy, cable400xx, cable400yy, cable524xx, cable524yy
        endelse
      END
        
      ;can use a calfits file
      'fits':BEGIN
        cal_bandpass = calfits_read(cal_bp_transfer,obs,params,silent=silent,_Extra=extra)
        cal_remainder=cal
        for pol_i=0, n_pol-1 do *cal_remainder.gain[pol_i]=(*cal.gain[pol_i])/(*cal_bandpass.gain[pol_i])
        Return, cal_bandpass
      END
          
    ENDCASE
  
end