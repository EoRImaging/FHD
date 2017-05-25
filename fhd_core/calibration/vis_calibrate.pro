FUNCTION vis_calibrate,vis_ptr,cal,obs,status_str,psf,params,jones,vis_weight_ptr=vis_weight_ptr,model_uv_arr=model_uv_arr,$
    transfer_calibration=transfer_calibration,timing=timing,file_path_fhd=file_path_fhd,$
    n_cal_iter=n_cal_iter,error=error,preserve_visibilities=preserve_visibilities,$
    debug=debug,gain_arr_ptr=gain_arr_ptr,calibration_flag_iterate=calibration_flag_iterate,$
    return_cal_visibilities=return_cal_visibilities,silent=silent,initial_calibration=initial_calibration,$
    calibration_visibilities_subtract=calibration_visibilities_subtract,vis_baseline_hist=vis_baseline_hist,$
    flag_calibration=flag_calibration,vis_model_arr=vis_model_arr,calibration_bandpass_iterate=calibration_bandpass_iterate,$
    calibration_auto_fit=calibration_auto_fit,debug_selected_cal=debug_selected_cal,zero_debug_cal=zero_debug_cal,$
    over_calibrate=over_calibrate,phase_longrun=phase_longrun,perf_calibrate=perf_calibrate,firstpass_model_recalculate=firstpass_model_recalculate,ave_ref=ave_ref,_Extra=extra
  t0_0=Systime(1)
  error=0
  timing=-1
  heap_gc
  IF N_Elements(flag_calibration) EQ 0 THEN flag_calibration=1
  
  IF Keyword_Set(transfer_calibration) THEN BEGIN
    IF size(transfer_calibration,/type) EQ 7 THEN BEGIN
      cal_file_use=transfer_calibration
      IF file_test(cal_file_use,/directory) THEN BEGIN
        fhd_save_io,file_path_fhd=file_path_fhd,transfer=transfer_calibration,var='cal',path_use=cal_file_use2,_Extra=extra
        cal_file_use2+='.sav'
        IF file_test(cal_file_use2) THEN cal_file_use=cal_file_use2 ELSE BEGIN
          print,'File:'+cal_file_use2+' not found!'
          error=1
          RETURN,vis_ptr
        ENDELSE
      ENDIF ELSE BEGIN
        IF file_test(cal_file_use) EQ 0 THEN BEGIN
          fhd_save_io,file_path_fhd=cal_file_use,var='cal',path_use=cal_file_use2,_Extra=extra
          cal_file_use2+='.sav'
          IF file_test(cal_file_use2) THEN cal_file_use=cal_file_use2 ELSE BEGIN
            print,'File:'+cal_file_use2+' not found!'
            error=1
            RETURN,vis_ptr
          ENDELSE
        ENDIF
      ENDELSE
      CASE StrLowCase(Strmid(cal_file_use[0],3,/reverse)) OF
        '.sav':BEGIN
          cal=getvar_savefile(cal_file_use,'cal')
          gain_arr_ptr=cal.gain
          IF ~Keyword_Set(cal.cal_origin) THEN cal.cal_origin=cal_file_use
          cal=fhd_struct_init_cal(obs,params,calibration_origin=cal.cal_origin,gain_arr_ptr=cal.gain,_Extra=extra)
        END
        '.txt':BEGIN
          textfast,gain_arr,/read,file_path=cal_file_use
          gain_arr_ptr=Ptr_new(gain_arr)
          cal=fhd_struct_init_cal(obs,params,calibration_origin=cal_file_use,gain_arr_ptr=gain_arr_ptr,_Extra=extra)
        END
        '.npz':BEGIN
          gain_arr=read_numpy(cal_file_use)
          gain_arr_ptr=Ptr_new(gain_arr)
          cal=fhd_struct_init_cal(obs,params,calibration_origin=cal_file_use,gain_arr_ptr=gain_arr_ptr,_Extra=extra)
        END
        '.npy':BEGIN
          gain_arr=read_numpy(cal_file_use)
          gain_arr_ptr=Ptr_new(gain_arr)
          cal=fhd_struct_init_cal(obs,params,calibration_origin=cal_file_use,gain_arr_ptr=gain_arr_ptr,_Extra=extra)
        END
        ELSE: BEGIN
          print,'Unknown file format: ',cal_file_use
          error=1
          RETURN,vis_ptr
        ENDELSE
      ENDCASE
    ENDIF
;    IF Keyword_Set(flag_calibration) THEN vis_calibration_flag,obs,cal,_Extra=extra
;    nc_pol=cal.n_pol
;    cal_base=cal & FOR pol_i=0,nc_pol-1 DO cal_base.gain[pol_i]=Ptr_new(*cal.gain[pol_i])
;    IF tag_exist(cal,'bandpass') THEN bandpass_calibrate=cal.bandpass
;    IF tag_exist(cal,'polyfit') THEN calibration_polyfit=cal.polyfit
;
;    IF Keyword_Set(bandpass_calibrate) THEN BEGIN
;        cal_bandpass=vis_cal_bandpass(cal,obs,cal_remainder=cal_remainder,file_path_fhd=file_path_fhd)
;        IF Keyword_Set(calibration_polyfit) THEN BEGIN
;            cal_polyfit=vis_cal_polyfit(cal_remainder,obs,degree=calibration_polyfit)
;            cal=vis_cal_combine(cal_polyfit,cal_bandpass)
;        ENDIF ELSE cal=cal_bandpass
;    ENDIF ELSE IF Keyword_Set(calibration_polyfit) THEN cal=vis_cal_polyfit(cal,obs,degree=calibration_polyfit)
;    vis_cal=vis_calibration_apply(vis_ptr,cal)
;    cal_res=vis_cal_subtract(cal_base,cal)
;
;    IF Keyword_Set(return_cal_visibilities) OR Keyword_Set(calibration_visibilities_subtract) THEN BEGIN
;
;    ENDIF
    vis_cal=vis_calibration_apply(vis_ptr,cal)
    timing=Systime(1)-t0_0
    RETURN,vis_cal
  ENDIF

fill_model_vis=1
if N_elements(firstpass_model_recalculate) EQ 0 then firstpass_model_recalculate=1
if keyword_set(firstpass_model_recalculate) then begin
vis_model_arr=vis_source_model(cal.skymodel,obs,status_str,psf,params,vis_weight_ptr,cal,jones,model_uv_arr=model_uv_arr,fill_model_vis=fill_model_vis,$
  timing=model_timing,silent=silent,error=error,/calibration_flag,spectral_model_uv_arr=spectral_model_uv_arr,_Extra=extra)
  endif else begin
    vis_model_arr=PTRARR(2,/allocate)
    pol_names=['XX','YY']
    for pol_i=0, 1 do $
    vis_model_arr[pol_i] = getvar_savefile(file_dirname(file_path_fhd) + '/vis_data/' + obs.obsname + '_vis_model_'+pol_names[pol_i]+'.sav','vis_model_ptr')
  endelse
t1=Systime(1)-t0_0

vis_auto=vis_extract_autocorr(obs,vis_arr = vis_ptr,/time_average,auto_tile_i=auto_tile_i)
IF Keyword_Set(cal.auto_initialize) THEN BEGIN
  IF Keyword_Set(vis_auto) THEN $
    initial_calibration=vis_cal_auto_init(obs,psf,cal,vis_arr=vis_ptr,vis_model_arr=vis_model_arr,_Extra=extra) $
  ELSE print,"calibration_auto_initialize is set, but autocorrelation visibilities are missing. Skipping."
ENDIF

IF Keyword_Set(fill_model_vis) THEN vis_auto_model=vis_extract_autocorr(obs,vis_arr = vis_model_arr,/time_average,auto_tile_i=auto_tile_i)

CASE size(initial_calibration,/type) OF
  0:;do nothing if undefined
  
  7:BEGIN ;type code 7 is string
  file_path_use=initial_calibration
  IF StrLowCase(Strmid(file_path_use,2,3,/reverse_offset)) NE 'sav' THEN file_path_use+='.sav'
  IF file_test(file_path_use) EQ 0 THEN file_path_use=filepath(file_path_use,root=file_dirname(file_path_fhd))
  IF file_test(file_path_use) THEN BEGIN
    cal_init=getvar_savefile(file_path_use,'cal')
    cal.gain=cal_init.gain
    print,'Using initial calibration solution from '+initial_calibration
  ENDIF else print, 'Initial calibration file not found'
END
8:cal.gain=initial_calibration.gain ;type code 8 is structure
10:cal.gain=initial_calibration ;type code 10 is pointer
ELSE:IF Keyword_Set(initial_calibration) THEN initial_calibration=file_path_fhd+'_cal' ;if set to a numeric type, assume this calibration solution will be wanted for future iterations
ENDCASE

IF Keyword_Set(error) THEN BEGIN
  timing=Systime(1)-t0_0
  RETURN,vis_ptr
ENDIF
pol_names=obs.pol_names

;extract information from the structures
n_pol=obs.n_pol
nc_pol=cal.n_pol
n_freq=cal.n_freq
n_tile=cal.n_tile
n_time=cal.n_time

bandpass_calibrate=cal.bandpass
calibration_polyfit=cal.polyfit

tile_A_i=cal.tile_A-1
tile_B_i=cal.tile_B-1
freq_arr=cal.freq
bin_offset=cal.bin_offset
n_baselines=obs.nbaselines
tile_A_i=tile_A_i[0:n_baselines-1]
tile_B_i=tile_B_i[0:n_baselines-1]

IF N_Elements(vis_weight_ptr) EQ 0 THEN BEGIN
  flag_init=Replicate(1.,n_freq,n_baselines*Float(n_time))
  vis_weight_ptr=Ptrarr(n_pol,/allocate)
  FOR pol_i=0,n_pol-1 DO *vis_weight_ptr[pol_i]=flag_init
ENDIF

;calibration loop
IF N_Elements(preserve_visibilities) EQ 0 THEN preserve_visibilities=0
IF Keyword_Set(calibration_visibilities_subtract) OR Keyword_Set(vis_baseline_hist) $
  OR Keyword_Set(return_cal_visibilities) THEN preserve_visibilities=1
IF N_Elements(calibration_flag_iterate) EQ 0 THEN calibration_flag_iterate=0
;    IF Keyword_Set(flag_calibration) THEN calibration_flag_iterate=1 ELSE calibration_flag_iterate=0

t2=0
FOR iter=0,calibration_flag_iterate DO BEGIN
  t2_a=Systime(1)
  IF iter LT calibration_flag_iterate THEN preserve_flag=1 ELSE preserve_flag=preserve_visibilities
  cal=vis_calibrate_subroutine(vis_ptr,vis_model_arr,vis_weight_ptr,obs,cal,$
    preserve_visibilities=preserve_flag,_Extra=extra)
  if keyword_set(ave_ref) then begin
    ref_avg = getvar_savefile('/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_2013longrun/longrun_gain_ave/longrun_gain_dig_poi_refave.sav','ref_avg')
    obsids = getvar_savefile('/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_2013longrun/longrun_gain_ave/longrun_gain_dig_poi_refave.sav','obsids')
    gain0 = (*cal.gain[0])
    gain1 = (*cal.gain[1])
    obs_id = where(obs.obsname EQ obsids)
    print, obs_id
    unwrapped_phase = phunwrap(atan((*cal.gain[0]),/phase))
    for tile_i=0,127 do (*cal.gain[0])[*,tile_i] = abs((*cal.gain[0])[*,tile_i]) * ((exp(Complex(0,1)*unwrapped_phase[*,tile_i])) / (exp(Complex(0,1)*reform(ref_avg[0,obs_id,*]))))
    unwrapped_phase = phunwrap(atan((*cal.gain[1]),/phase))
    for tile_i=0,127 do (*cal.gain[1])[*,tile_i] = abs((*cal.gain[1])[*,tile_i]) * ((exp(Complex(0,1)*unwrapped_phase[*,tile_i])) / (exp(Complex(0,1)*reform(ref_avg[1,obs_id,*]))))

  endif  
  t3_a=Systime(1)
  t2+=t3_a-t2_a
  
  ;if keyword_set(debug_selected_cal) then cal=getvar_savefile('/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_2013longrun/selected_cal/1061316296_selected_cal.sav','cal')
  if keyword_set(debug_selected_cal) then begin
    if debug_selected_cal EQ 1 then begin
      cal_selected=getvar_savefile('/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_2013longrun/selected_cal/cableave_obsave.sav','cal_sel')
      pointing_num=mwa_get_pointing_number(obs,/string)
      pointing_val = ['-2','-1','0','1','2','3']
      match, pointing_num, pointing_val, suba, subb
      
      mode_filepath=filepath(obs.instrument+'_cable_reflection_coefficients.txt',root=rootdir('FHD'),subdir='instrument_config')
      textfast,data_array,/read,file_path=mode_filepath,first_line=1
      cable_len=Reform(data_array[2,*])
      
      ;Taking tile information and cross-matching it with the nonflagged tiles array, resulting in nonflagged tile arrays
      ;grouped by cable length
      cable_length_ref=cable_len[Uniq(cable_len,Sort(cable_len))]
      n_cable=N_Elements(cable_length_ref)
      
      freq_use = (*obs.baseline_info).freq_use
      freq_use = where(freq_use)
      
      cal_old = getvar_savefile('/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_notimeavg_cal/calibration/'+obs.obsname+'_cal.sav','cal')
      gain_fit = FLTARR(2,384,128)
      for pol_i=0, 1 do for tile_i=0,127 do $
        if (*cal_old.amp_params[pol_i,tile_i]) NE !NULL then $
        FOR di=0L,2 DO gain_fit[pol_i,*,tile_i]+=(*cal_old.amp_params[pol_i,tile_i])[di]*findgen(n_freq)^di
        
      FOR cable_i=0,n_cable-1 DO begin
        cable_tiles = where(cable_len EQ cable_length_ref[cable_i])
        for pol_i=0,1 do begin
        
          ;mean_cal = FLTARR(N_elements(freq_use))
          ;for freq_i=0, N_elements(freq_use)-1 do mean_cal[freq_i] = mean(abs((*cal.gain[pol_i])[freq_use[freq_i],cable_tiles]))
          ;mean_cal = mean(abs(gain[freq_use,cable_tiles]),dim=2)
          ;fit_params=poly_fit(freq_use,mean_cal,2)
          ;gain_fit=fltarr(n_freq)
          ;FOR di=0L,2 DO gain_fit+=fit_params[di]*findgen(n_freq)^di
        
        
          (*cal.gain[pol_i])[*,cable_tiles] = rebin(reform(abs(cal_selected[pol_i,subb,*,cable_i])),384,N_elements(cable_tiles))/mean(abs(cal_selected[pol_i,subb,freq_use,cable_i]))$
            * reform(gain_fit[pol_i,*,cable_tiles]) * exp(Complex(0,1)*atan((*cal.gain[pol_i])[*,cable_tiles],/phase))
        endfor
      endfor
      
    endif
    if debug_selected_cal EQ 2 then begin
      ;cal_selected=getvar_savefile('/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_2013longrun/selected_cal/noave.sav','cal_sel')
      cal_selected=getvar_savefile('/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_2013longrun/selected_cal/noave_zeroed.sav','updated_cal')
      filename2='/nfs/eor-00/h1/nbarry/MWA/IDL_code/obs_list/Aug23.txt'
      readcol, filename2, obs_ids, format='A', /silent
      obs_id_current = where(obs_ids EQ obs.obsname)
      for pol_i=0,1 do (*cal.gain[pol_i]) = reform(abs(cal_selected[pol_i,obs_id_current,*,*])) * exp(Complex(0,1)*atan((*cal.gain[pol_i]),/phase))
    endif
  endif
  
  if keyword_set(zero_debug_cal) then cal=getvar_savefile('/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_2013longrun/selected_cal/1061316296_selected_cal_zero.sav','cal')
  
  IF Keyword_Set(flag_calibration) THEN vis_calibration_flag,obs,cal,n_tile_cut=n_tile_cut,_Extra=extra
  IF Keyword_Set(n_tile_cut) THEN BREAK
  
ENDFOR
cal_base=Pointer_copy(cal)

IF Keyword_Set(bandpass_calibrate) THEN BEGIN
  cal_bandpass=vis_cal_bandpass(cal,obs,cal_remainder=cal_remainder,file_path_fhd=file_path_fhd,_Extra=extra)
  IF Keyword_Set(calibration_polyfit) THEN BEGIN
    IF Keyword_Set(calibration_bandpass_iterate) THEN BEGIN
      cal_remainder.amp_degree = 1 & cal_remainder.phase_degree=0
      cal_polyfit=vis_cal_polyfit(cal_remainder,obs,_Extra=extra)
      cal_poly_sub=vis_cal_divide(cal_base,cal_polyfit)
      cal_bandpass2=vis_cal_bandpass(cal_poly_sub,obs,file_path_fhd=file_path_fhd,_Extra=extra)
      cal_remainder2=vis_cal_divide(cal_base,cal_bandpass2)
      cal_polyfit2=vis_cal_polyfit(cal_remainder2,obs,_Extra=extra)
      cal=vis_cal_combine(cal_polyfit2,cal_bandpass2)
    ENDIF ELSE BEGIN
      cal_polyfit=vis_cal_polyfit(cal_remainder,obs,_Extra=extra)
      cal=vis_cal_combine(cal_polyfit,cal_bandpass)
    ENDELSE
  ENDIF ELSE cal=cal_bandpass
ENDIF ELSE IF Keyword_Set(calibration_polyfit) THEN cal=vis_cal_polyfit(cal,obs,_Extra=extra)
IF Keyword_Set(vis_auto_model) THEN cal_auto=vis_cal_auto_fit(obs,cal,vis_auto=vis_auto,vis_model_auto=vis_auto_model,auto_tile_i=auto_tile_i)
IF Keyword_Set(calibration_auto_fit) THEN cal_res=vis_cal_subtract(cal_base,cal_auto) ELSE cal_res=vis_cal_subtract(cal_base,cal)
basename=file_basename(file_path_fhd)
dirpath=file_dirname(file_path_fhd)
image_path=filepath(basename,root=dirpath,sub='output_images')
;make sure to plot both, if autocorrelations are used for the calibration solution
plot_cals,cal,obs,cal_res=cal_res,cal_auto=cal_auto,file_path_base=image_path,_Extra=extra

if keyword_set(over_calibrate) then begin
*cal.gain[0] = (*cal_base.gain[0]); + atan(*cal.gain[0],/phase)
*cal.gain[1] = (*cal_base.gain[1]); + atan(*cal.gain[1],/phase)
endif
if keyword_set(perf_calibrate) then begin
*cal.gain[0] = 1.
*cal.gain[1] = 1.
endif

if keyword_set(phase_longrun) then begin
  ;longrun_gain  = getvar_savefile('/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_2013longrun/longrun_gain_ave/longrun_gain_plus_phase_dig_poi.sav', 'longrun_gain')
  longrun_gain = getvar_savefile('/nfs/mwa-09/r1/djc/EoR2013/Aug23/fhd_nb_2013longrun/longrun_gain_ave/longrun_gain_dig_poi_refave.sav','longrun_gain')
      pointing_num=mwa_get_pointing_number(obs,/string)
      poi_name = ['-2','-1','0','1','2']
      poi = where(pointing_num EQ poi_name)
*cal.gain[0] = abs(*cal.gain[0])*exp(Complex(0,1)*( atan((*cal.gain[0]),/phase)+atan(reform(longrun_gain[0,poi,*,*]),/phase)))
*cal.gain[1] = abs(*cal.gain[1])*exp(Complex(0,1)*( atan((*cal.gain[1]),/phase)+atan(reform(longrun_gain[1,poi,*,*]),/phase)))
endif


IF Keyword_Set(calibration_auto_fit) THEN cal=cal_auto
vis_cal=vis_calibration_apply(vis_ptr,cal)
cal.gain_residual=cal_res.gain
undefine_fhd,cal_base



IF Keyword_Set(vis_baseline_hist) THEN $
  vis_baseline_hist,obs,params,vis_arr=vis_cal,vis_model_arr=vis_model_arr,file_path_fhd=file_path_fhd
  
IF ~Keyword_Set(return_cal_visibilities) THEN preserve_visibilities=0
IF Keyword_Set(calibration_visibilities_subtract) THEN BEGIN
  FOR pol_i=0,n_pol-1 DO *vis_cal[pol_i]-=*vis_model_arr[pol_i]
  IF tag_exist(obs,'residual') THEN obs.residual=1
ENDIF
IF ~Keyword_Set(return_cal_visibilities) THEN undefine_fhd,vis_model_arr

cal_gain_avg=Fltarr(nc_pol)
cal_res_avg=Fltarr(nc_pol)
cal_res_restrict=Fltarr(nc_pol)
cal_res_stddev=Fltarr(nc_pol)
FOR pol_i=0,nc_pol-1 DO BEGIN
  tile_use_i=where((*obs.baseline_info).tile_use,n_tile_use)
  freq_use_i=where((*obs.baseline_info).freq_use,n_freq_use)
  IF n_tile_use EQ 0 OR n_freq_use EQ 0 THEN CONTINUE
  gain_ref=extract_subarray(*cal.gain[pol_i],freq_use_i,tile_use_i)
  gain_res=extract_subarray(*cal_res.gain[pol_i],freq_use_i,tile_use_i)
  cal_gain_avg[pol_i]=Mean(Abs(gain_ref))
  cal_res_avg[pol_i]=Mean(Abs(gain_res))
  resistant_mean,Abs(gain_res),2,res_mean
  cal_res_restrict[pol_i]=res_mean
  cal_res_stddev[pol_i]=Stddev(Abs(gain_res))
ENDFOR
IF Tag_exist(cal,'Mean_gain') THEN cal.mean_gain=cal_gain_avg
IF Tag_exist(cal,'Mean_gain_residual') THEN cal.mean_gain_residual=cal_res_avg
IF Tag_exist(cal,'Mean_gain_restrict') THEN cal.mean_gain_restrict=cal_res_restrict
IF Tag_exist(cal,'Stddev_gain_residual') THEN cal.stddev_gain_residual=cal_res_stddev

t3=Systime(1)-t3_a
timing=Systime(1)-t0_0
IF ~Keyword_Set(silent) THEN print,timing,t1,t2,t3
RETURN,vis_cal
END
