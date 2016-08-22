PRO calibration_sim_setup, cal_sim_input, vis_arr, vis_weights, enhance_eor=enhance_eor, remove_eor=remove_eor,bubbles=bubbles, file_path_vis=file_path_vis, $
		add_sim_noise=add_sim_noise
		
	if ~keyword_set(n_pol) then n_pol=2
	if n_pol EQ 2 then pol_name=['XX','YY'] else pol_name=['XX','YY','XY','YX']
	
	for pol_i=0, n_pol-1 do (*vis_weights[pol_i])[*,*]=1.
	
	vis_arr=PTRARR(n_pol,/allocate)
	vis_model=PTRARR(n_pol,/allocate)
	obs_id = file_basename(file_path_vis, '.uvfits')
	
	;restore model visibilities given the cal_sim_input
	for pol_i=0, n_pol-1 do $
		vis_model[pol_i] = GETVAR_SAVEFILE(cal_sim_input+'/vis_data/'+obs_id+'_vis_model_'+pol_name[pol_i]+'.sav', 'vis_model_ptr')
		
	IF ~ptr_valid(vis_model) then begin
		print, "Read-in file not found/provided in cal_sim_input. Creating model"
		
		;Note: explicitly reference dft_threshold here to remove it from EXTRA, which would be passed on to lower-level routines
		obs=fhd_struct_init_obs(file_path_vis,hdr,params,n_pol=n_pol,dft_threshold=dft_threshold,_Extra=extra)
		n_pol=obs.n_pol
		n_freq=obs.n_freq
		
		IF Keyword_Set(deproject_w_term) THEN vis_arr=simple_deproject_w_term(obs,params,vis_arr,direction=deproject_w_term)
		
		;Read in or construct a new beam model. Also sets up the structure PSF
		psf=beam_setup(obs,status_str,antenna,file_path_fhd=file_path_fhd,restore_last=0,silent=silent,timing=t_beam,no_save=no_save,_Extra=extra)
		jones=fhd_struct_init_jones(obs,status_str,file_path_fhd=file_path_fhd,restore=0,mask=beam_mask,_Extra=extra)
		
		vis_weights=vis_flag_basic(vis_weights,obs,params,n_pol=n_pol,n_freq=n_freq,freq_start=freq_start,$
			freq_end=freq_end,tile_flag_list=tile_flag_list,vis_ptr=vis_arr,unflag_all=1,no_frequency_flagging=1,_Extra=extra)
		vis_weights_update,vis_weights,obs,psf,params,_Extra=extra
		
		IF Keyword_Set(calibration_catalog_file_path) THEN catalog_use=calibration_catalog_file_path
		IF ~Keyword_Set(calibration_source_list) THEN $
			calibration_source_list=generate_source_cal_list(obs,psf,catalog_path=catalog_use,_Extra=extra)
		skymodel_cal=fhd_struct_init_skymodel(obs,source_list=calibration_source_list,catalog_path=catalog_use,return_cal=1,diffuse_model=diffuse_calibrate,_Extra=extra)
		cal=fhd_struct_init_cal(obs,params,skymodel_cal,source_list=calibration_source_list,$
			catalog_path=catalog_use,transfer_calibration=transfer_calibration,_Extra=extra)
			
		vis_model_arr=vis_source_model(cal.skymodel,obs,status_str,psf,params,vis_weights,cal,jones,model_uv_arr=model_uv_arr,fill_model_vis=1,$
			timing=model_timing,silent=silent,error=error,/calibration_flag,spectral_model_uv_arr=spectral_model_uv_arr,_Extra=extra)
			
		vis_export,obs,status_str,vis_model_arr,vis_weights,file_path_fhd=file_path_fhd,/compress,/model
		
		vis_XX_model = PTR_NEW(vis_model_arr[0])
		vis_YY_model = PTR_NEW(vis_model_arr[1])
		
		undefine, vis_model_arr, psf, jones, skymodel_cal, cal, calibration_source_list
		
	endif
	
	
	;restore EoR visibilities
	If ~keyword_set(bubbles_eor) then begin
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
		print, "Enhancing input EoR by "+enhance_eor+"x"
		for pol_i=0,n_pol-1 do *vis_eor[pol_i]=*vis_eor[pol_i]*enhance_eor
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
