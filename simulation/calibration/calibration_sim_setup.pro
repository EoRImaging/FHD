PRO calibration_sim_setup, cal_sim_input, vis_arr, flag_arr, enhance_eor=enhance_eor, remove_eor=remove_eor,bubbles=bubbles, file_path_vis=file_path_vis, $
    add_sim_noise=add_sim_noise,stokesV=stokesV
    
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
  
  If ~keyword_set(stokesV) then begin
    ;restore model visibilities given the cal_sim_input
    vis_XX_model = GETVAR_SAVEFILE('/nfs/mwa-09/r1/djc/EoR2013/Aug23/'+cal_sim_input+'/vis_data/'+obs_id+'_vis_model_XX.sav', 'vis_model_ptr') ;restore array of calibrated visibilities
    vis_YY_model = GETVAR_SAVEFILE('/nfs/mwa-09/r1/djc/EoR2013/Aug23/'+cal_sim_input+'/vis_data/'+obs_id+'_vis_model_YY.sav', 'vis_model_ptr')
  endif else begin
    vis_arr =  GETVAR_SAVEFILE('/nfs/mwa-03/r1/EoR2013/'+cal_sim_input+'/'+obs_id+'_vis_model_arr.sav', 'vis_model_arr')
    IF ~keyword_set(vis_arr) then message, '/nfs/mwa-03/r1/EoR2013/'+cal_sim_input+'/'+obs_id+'_vis_model_arr.sav not found or did not contain specified variable'
    Return
  endelse
  
  ;vis_XX_model = GETVAR_SAVEFILE('/nfs/mwa-09/r1/djc/EoR2013/Aug23/'+cal_sim_input+'/vis_data/'+obs_id+'_vis_XX.sav', 'vis_ptr') ;restore array of calibrated visibilities
  ;vis_YY_model = GETVAR_SAVEFILE('/nfs/mwa-09/r1/djc/EoR2013/Aug23/'+cal_sim_input+'/vis_data/'+obs_id+'_vis_YY.sav', 'vis_ptr')
  
  
  ;restore EoR visibilities
  If ~keyword_set(bubbles) then begin
    ;Hash eor
    plusone=['1061317272','1061317400','1061317520','1061317640','1061317760','1061317888','1061318008','1061318128','1061318248', $
      '1061318376','1061318496','1061318616','1061318736','1061318864','1061318984']
      
    zenith = ['1061315448','1061315568','1061315688','1061315808','1061315936','1061316056','1061316176','1061316296','1061316424', $
      '1061316544','1061316664','1061316784','1061316912','1061317032','1061317152']
      
    match_index=where(STRMATCH(plusone, obs_id),n_count)
    If n_count GT 0 then obs_temp = zenith[match_index] else obs_temp=obs_id
    
    vis_XX_eor = GETVAR_SAVEFILE('/nfs/eor-00/h1/nbarry/'+obs_temp+'_vis_XX.sav', 'vis_ptr') ;restore array of calibrated visibilities
    vis_YY_eor = GETVAR_SAVEFILE('/nfs/eor-00/h1/nbarry/'+obs_temp+'_vis_YY.sav', 'vis_ptr')
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
    vis_noise=getvar_savefile('/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_sim_noise/'+obs_id+'_noise.sav','vis_noise')
    zero_xx_i = where(real_part(*vis_arr[0]) NE 0)
    (*vis_arr[0])[zero_xx_i] = (*vis_arr[0])[zero_xx_i]+(*vis_noise[0])[zero_xx_i]
    zero_yy_i = where(real_part(*vis_arr[1]) NE 0)
    (*vis_arr[1])[zero_yy_i] = (*vis_arr[1])[zero_yy_i]+(*vis_noise[1])[zero_yy_i]
  endif
  
END
