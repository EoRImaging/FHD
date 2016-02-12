PRO calibration_sim_setup, cal_sim_input, vis_arr, flag_arr, enhance_eor=enhance_eor, remove_eor=remove_eor,bubbles=bubbles, file_path_vis=file_path_vis, $
    add_sim_noise=add_sim_noise
    
  (*flag_arr[0])[*,*]=1.
  (*flag_arr[1])[*,*]=1.
  
  vis_arr=PTRARR(2,/allocate) ;correct pol format
  
  ;visibilities with flagged frequencies, no longer standard
  ;vis_XX_model = GETVAR_SAVEFILE('/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_sim_beamperchannel/vis_data/1061316176_vis_model_XX.sav', 'vis_model_ptr') ;restore array of calibrated visibilities
  ;vis_YY_model = GETVAR_SAVEFILE('/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_sim_beamperchannel/vis_data/1061316176_vis_model_YY.sav', 'vis_model_ptr')
  
  If ~keyword_set(cal_sim_input) then begin
    print, 'Please specify an input into the calibration simulation, e.g. fhd_nb_sim_beamperchannel_unflagged'
    exit
  endif
  
  obs_id = file_basename(file_path_vis, '.uvfits')
  
  ;restore model visibilities given the cal_sim_input
  vis_XX_model = GETVAR_SAVEFILE('/nfs/mwa-09/r1/djc/EoR2013/Aug23/'+cal_sim_input+'/vis_data/'+obs_id+'_vis_model_XX.sav', 'vis_model_ptr') ;restore array of calibrated visibilities
  vis_YY_model = GETVAR_SAVEFILE('/nfs/mwa-09/r1/djc/EoR2013/Aug23/'+cal_sim_input+'/vis_data/'+obs_id+'_vis_model_YY.sav', 'vis_model_ptr')
  
  ;vis_XX_model = GETVAR_SAVEFILE('/nfs/mwa-09/r1/djc/EoR2013/Aug23/'+cal_sim_input+'/vis_data/'+obs_id+'_vis_XX.sav', 'vis_ptr') ;restore array of calibrated visibilities
  ;vis_YY_model = GETVAR_SAVEFILE('/nfs/mwa-09/r1/djc/EoR2013/Aug23/'+cal_sim_input+'/vis_data/'+obs_id+'_vis_YY.sav', 'vis_ptr')
  
  
  ;restore EoR visibilities
  If ~keyword_set(bubbles) then begin
    ;Hash eor
    vis_XX_eor = GETVAR_SAVEFILE('/nfs/eor-00/h1/nbarry/'+obs_id+'_vis_XX.sav', 'vis_ptr') ;restore array of calibrated visibilities
    vis_YY_eor = GETVAR_SAVEFILE('/nfs/eor-00/h1/nbarry/'+obs_id+'_vis_YY.sav', 'vis_ptr')
  endif else begin
    ;Bubble eor from Adam Lidz
    vis_XX_eor = GETVAR_SAVEFILE('/nfs/eor-00/h1/nbarry/1061316176_vis_bubbles_XX.sav', 'vis_ptr') ;restore array of calibrated visibilities
    vis_YY_eor = GETVAR_SAVEFILE('/nfs/eor-00/h1/nbarry/1061316176_vis_bubbles_YY.sav', 'vis_ptr')
  endelse
  
  If keyword_set(enhance_eor) then begin
    If enhance_eor EQ 1 then begin
      *vis_XX_eor=*vis_XX_eor*1000.
      *vis_YY_eor=*vis_YY_eor*1000.
    endif
    
    If enhance_eor EQ 2 then begin
      *vis_XX_eor=*vis_XX_eor*100000.
      *vis_YY_eor=*vis_YY_eor*100000.
    endif
  endif
  
  If keyword_set(remove_eor) then begin
    *vis_XX_eor=0.
    *vis_YY_eor=0.
  endif
  
  ;Combine the calibrated visibilities in the correct format for the script
  *vis_arr[0] = *vis_XX_model+*vis_XX_eor
  *vis_arr[1] = *vis_YY_model+*vis_YY_eor
  
  If keyword_set(add_sim_noise) then begin
    vis_noise=getvar_savefile('/nfs/mwa-00/h1/nbarry/'+obs_id+'_noise.sav','vis_noise')
    *vis_arr[0] = *vis_arr[0]+*vis_noise[0]
    *vis_arr[1] = *vis_arr[1]+*vis_noise[1]
  endif
  
END
