function poly_cal_freq_mode, mode, val, mask

  dims = size(mask, /dim)
  
  case n_elements(dims) of
    1: begin
      n_freq = dims[0]
      n_val = 1
    end
    
    2: begin
      n_freq = dims[0]
      n_val = dims[1]
    end
    else: stop
  end
  
  if n_elements(val) ne n_val then stop
  
  val_arr = matrix_multiply(dblarr(n_freq)+1, val)
  x_arr = (dindgen(n_freq)/(n_freq-1))*2-1
  if n_val gt 1 then x_arr = rebin(x_arr, n_freq, n_val,/sample)
  
  case mode of
    0: freq_arr = val_arr
    1: freq_arr = val_arr * x_arr
    2: freq_arr = val_arr * (3*x_arr^2. - 1)/2.
  endcase
  
  ;freq_arr = sqrt((2*mode+1)/2.)*freq_arr
  
  return, freq_arr*mask
end

function poly_cal_gain, gain_modes, mode_types, mode_num, mask, amp=amp, phase=phase

  n_modes = n_elements(mode_types)
  dims_gain = size(gain_modes, /dimension)
  dims_mask = size(mask, /dimension)
  
  if n_elements(dims_gain) gt 1 then if dims_gain[1] ne dims_mask[1] then message, 'second dimension of gain_modes and mask must match'
  
  if dims_gain[0] ne n_modes then message, 'first dimension of gain_modes must match length of mode_types'
  
  
  amp = dblarr(dims_mask)
  phase = dblarr(dims_mask)
  for fi=0L,n_modes-1 do begin
    if mode_types[fi] eq 'amp' then amp += poly_cal_freq_mode(mode_num[fi], reform(gain_modes[fi,*]), mask) $
    else phase += poly_cal_freq_mode(mode_num[fi], reform(gain_modes[fi,*]), mask)
  endfor
  
  if total(abs(phase)) gt 0 then gain = amp * exp(complex(0,1)*phase) else gain = amp
  
  return, gain
end

function poly_cal_chi2, params

  common poly_cal_data, model_vis, data_vis, poly_cal_gain_B, f_arr, mode_types, mode_num, mask, ab_switch
  
  dims = size(mask, /dimension)
  if n_elements(dims) gt 1 then n_vis = dims[1] else n_vis = 1
  
  if n_vis gt 1 then p = rebin(params, n_elements(params), n_vis) else p=params
  gain_A = poly_cal_gain(p, mode_types, mode_num, mask)
  
  if ab_switch then data_use = Conj(data_vis) else data_use = data_vis
  
  diff = model_vis*Conj(poly_cal_gain_B)*gain_A - data_use
  
  return, total(abs(diff)^2.)
  
end

function poly_cal_grad, params

  common poly_cal_data, model_vis, data_vis, poly_cal_gain_B, f_arr, mode_types, mode_num, mask, ab_switch
  
  dims = size(mask, /dimension)
  if n_elements(dims) gt 1 then n_vis = dims[1] else n_vis = 1
  
  if n_vis gt 1 then p = rebin(params, n_elements(params), n_vis) else p=params
  gain_A = poly_cal_gain(p, mode_types, mode_num, mask, phase=phase_A)
  
  if ab_switch then data_use = Conj(data_vis) else data_use = data_vis
  
  amp_grad_factor = 2.*abs(model_vis)^2.*abs(poly_cal_gain_B)^2.*abs(gain_A) - 2*Real_part(model_vis*Conj(poly_cal_gain_B)*Conj(data_use)*exp(complex(0,1)*phase_A))
  phase_grad_factor = 2*Imaginary(model_vis*gain_A*Conj(poly_cal_gain_B)*Conj(data_use))
  
  
  grad = params*0
  for i=0, n_elements(mode_types)-1 do begin
    if mode_types[i] eq 'amp' then grad[i] = total(amp_grad_factor*f_arr^(mode_num[i])) $
    else grad[i] = total(phase_grad_factor*f_arr^(mode_num[i]))
    
  endfor
  
  return, grad
  
end


FUNCTION vis_calibrate_subroutine,vis_ptr,vis_model_ptr,flag_ptr,obs,params,cal,preserve_visibilities=preserve_visibilities,$
    calib_freq_func=calib_freq_func,_Extra=extra
    
  IF N_Elements(cal) EQ 0 THEN cal=vis_struct_init_cal(obs,params,_Extra=extra)
  reference_tile=cal.ref_antenna
  min_cal_baseline=cal.min_cal_baseline
  max_cal_baseline=cal.max_cal_baseline
  min_cal_solutions=cal.min_solns ;minimum number of calibration equations needed to solve for the gain of one baseline
  time_average=cal.time_avg
  max_cal_iter=cal.max_iter
  IF max_cal_iter LT 5 THEN print,'Warning! At least 5 calibration iterations recommended. Using '+Strn(Floor(max_cal_iter))
  conv_thresh=cal.conv_thresh
  
  n_pol=cal.n_pol
  n_freq=cal.n_freq
  n_tile=cal.n_tile
  n_time=cal.n_time
  
  flag_ptr_use=flag_ptr ;flags WILL be over-written!
  tile_A_i=cal.tile_A-1
  tile_B_i=cal.tile_B-1
  freq_arr=cal.freq
  bin_offset=cal.bin_offset
  n_baselines=bin_offset[1]
  
  kbinsize=obs.kpix
  
  cal_return=cal
  FOR pol_i=0,n_pol-1 DO cal_return.gain[pol_i]=Ptr_new(*cal.gain[pol_i])
  
  FOR pol_i=0,n_pol-1 DO BEGIN
    gain_arr=*cal.gain[pol_i]
    
    IF Keyword_Set(time_average) THEN BEGIN
      ;average over time
      ;the visibilities have dimension nfreq x (n_baselines x n_time),
      ; which can be reformed to nfreq x n_baselines x n_time
      tile_A_i=tile_A_i[0:n_baselines-1]
      tile_B_i=tile_B_i[0:n_baselines-1]
      flag_use=0>Reform(*flag_ptr_use[pol_i],n_freq,n_baselines,n_time)<1
      IF Keyword_Set(preserve_visibilities) THEN vis_model=Reform(*vis_model_ptr[pol_i],n_freq,n_baselines,n_time) $
      ELSE vis_model=Reform(Temporary(*vis_model_ptr[pol_i]),n_freq,n_baselines,n_time)
      vis_model=Total(Temporary(vis_model)*flag_use,3)
      vis_measured=Reform(*vis_ptr[pol_i],n_freq,n_baselines,n_time)
      vis_avg=Total(Temporary(vis_measured)*flag_use,3)
      weight=Total(Temporary(flag_use),3)
      
      kx_arr=cal.uu[0:n_baselines-1]/kbinsize ;ignore slight variation with time
      ky_arr=cal.vv[0:n_baselines-1]/kbinsize
      kr_arr=Sqrt(kx_arr^2.+ky_arr^2.)
      dist_arr=(freq_arr#kr_arr)*kbinsize
      flag_dist_cut=where((dist_arr LT min_cal_baseline) OR (dist_arr GT max_cal_baseline),n_dist_cut)
    ENDIF ELSE BEGIN
      flag_use=0>*flag_ptr_use[pol_i]<1
      IF Keyword_Set(preserve_visibilities) THEN vis_model=*vis_model_ptr[pol_i] $
      ELSE vis_model=Temporary(*vis_model_ptr[pol_i])
      vis_model=Temporary(vis_model)*flag_use
      vis_avg=*vis_ptr[pol_i]*flag_use
      weight=Temporary(flag_use)
      
      kx_arr=cal.uu/kbinsize
      ky_arr=cal.vv/kbinsize
      kr_arr=Sqrt(kx_arr^2.+ky_arr^2.)
      dist_arr=(freq_arr#kr_arr)*kbinsize
      flag_dist_cut=where((dist_arr LT min_cal_baseline) OR (dist_arr GT max_cal_baseline),n_dist_cut)
    ENDELSE
    
    IF n_dist_cut GT 0 THEN weight[flag_dist_cut]=0.
    vis_avg*=weight_invert(weight)
    vis_model*=weight_invert(weight)
    
    tile_use_flag=(*obs.baseline_info).tile_use
    freq_use_flag=(*obs.baseline_info).freq_use
    
    freq_weight=Total(weight,2)
    baseline_weight=Total(weight,1)
    freq_use=where((freq_weight GT 0) AND (freq_use_flag GT 0),n_freq_use)
    baseline_use=where(baseline_weight,n_baseline_use)
    hist_tile_A=histogram(tile_A_i[baseline_use],min=0,/bin,max=n_tile-1,reverse_ind=riA)
    hist_tile_B=histogram(tile_B_i[baseline_use],min=0,/bin,max=n_tile-1,reverse_ind=riB)
    tile_use=where(((hist_tile_A+hist_tile_B) GT 0) AND (tile_use_flag GT 0),n_tile_use)
    
    tile_A_i_use=Lonarr(n_baseline_use)
    tile_B_i_use=Lonarr(n_baseline_use)
    FOR tile_i=0L,n_tile_use-1 DO BEGIN
      IF hist_tile_A[tile_use[tile_i]] GT 0 THEN tile_A_i_use[riA[riA[tile_use[tile_i]]:riA[tile_use[tile_i]+1]-1]]=tile_i
      IF hist_tile_B[tile_use[tile_i]] GT 0 THEN tile_B_i_use[riB[riB[tile_use[tile_i]]:riB[tile_use[tile_i]+1]-1]]=tile_i
    ENDFOR
    
    ref_tile_use=Min(where(reference_tile EQ tile_use))
    IF ref_tile_use EQ -1 THEN BEGIN
      ref_tile_use=0L
      cal.ref_antenna=tile_use[ref_tile_use]
      cal.ref_antenna_name=(*obs.baseline_info).tile_names[cal.ref_antenna]
    ENDIF
    nan_i=where(Finite(vis_avg,/nan),n_nan)
    IF n_nan GT 0 THEN vis_model[nan_i]=(vis_avg[nan_i]=0)
    
    if not keyword_set(calib_freq_func) then begin
      FOR fii=0L,n_freq_use-1 DO BEGIN
        fi=freq_use[fii]
        gain_curr=Reform(gain_arr[fi,tile_use])
        ;Reuse same gain solution between successive frequency channels IF input gains are default values
        ;        IF fii EQ 0 THEN gain_curr=Reform(gain_arr[fi,tile_use])
        ;        IF Stddev(gain_arr[fi,tile_use]) GT 0 THEN gain_curr=Reform(gain_arr[fi,tile_use])
        vis_data2=Reform(vis_avg[fi,baseline_use]) & vis_data2=[vis_data2,Conj(vis_data2)]
        vis_model2=Reform(vis_model[fi,baseline_use]) & vis_model2=[vis_model2,Conj(vis_model2)]
        weight2=Reform(weight[fi,baseline_use]) & weight2=[weight2,weight2]
        
        b_i_use=where(weight2 GT 0,n_baseline_use2)
        weight2=weight2[b_i_use]
        vis_data2=vis_data2[b_i_use];*weight_invert(weight2)
        vis_model2=vis_model2[b_i_use];*weight_invert(weight2)
        
        A_ind=[tile_A_i_use,tile_B_i_use] & A_ind=A_ind[b_i_use]
        B_ind=[tile_B_i_use,tile_A_i_use] & B_ind=B_ind[b_i_use]
        
        A_ind_arr=Ptrarr(n_tile_use,/allocate)
        n_arr=Fltarr(n_tile_use)
        FOR tile_i=0L,n_tile_use-1 DO BEGIN
          ;should be set up so that using where is okay
          inds=where(A_ind EQ tile_i,n1)
          IF n1 GT 1 THEN *A_ind_arr[tile_i]=Reform(inds,1,n1) ELSE *A_ind_arr[tile_i]=-1
          n_arr[tile_i]=n1 ;NEED SOMETHING MORE IN CASE INDIVIDUAL TILES ARE FLAGGED FOR ONLY A FEW FREQUENCIES!!
        ENDFOR
        
        phase_fit_iter=Floor(max_cal_iter/4.)
        
        gain_new=Complexarr(n_tile_use)
        conv_test=fltarr(max_cal_iter)
        FOR i=0L,(max_cal_iter-1)>1 DO BEGIN
          vis_use=vis_data2
          
          vis_model_matrix=vis_model2*Conj(gain_curr[B_ind])
          FOR tile_i=0L,n_tile_use-1 DO IF n_arr[tile_i] GE min_cal_solutions THEN $
            gain_new[tile_i]=LA_Least_Squares(vis_model_matrix[*A_ind_arr[tile_i]],vis_use[*A_ind_arr[tile_i]],method=2)
            
          IF Total(Abs(gain_new)) EQ 0 THEN BEGIN
            gain_curr=gain_new
            BREAK
          ENDIF
          IF phase_fit_iter-i GT 0 THEN gain_new*=weight_invert(Abs(gain_new)) ;fit only phase at first
          IF (2.*phase_fit_iter-i GT 0) AND (phase_fit_iter-i LE 0) THEN $
            gain_new*=Mean(Abs(gain_new[where(gain_new)]))*weight_invert(Abs(gain_new)) ;then fit only average amplitude
          gain_old=gain_curr
          gain_curr=(gain_new+gain_old)/2.
          dgain=Abs(gain_curr)*weight_invert(Abs(gain_old))
          diverge_i=where(dgain LT Abs(gain_old)/2.,n_diverge)
          IF n_diverge GT 0 THEN gain_curr[diverge_i]=(gain_new[diverge_i]+gain_old[diverge_i]*2.)/3.
          IF nan_test(gain_curr) GT 0 THEN gain_curr[where(Finite(gain_curr,/nan))]=gain_old[where(Finite(gain_curr,/nan))]
          gain_curr*=Conj(gain_curr[ref_tile_use])/Abs(gain_curr[ref_tile_use])
          conv_test[i]=Max(Abs(gain_curr-gain_old)*weight_invert(Abs(gain_old)))
          IF i GE 1 THEN $
            IF conv_test[i] LE conv_thresh THEN BREAK $
          ELSE IF Abs(conv_test[i]-conv_test[i-1]) LE conv_thresh/2. THEN BREAK
        ENDFOR
        Ptr_free,A_ind_arr
        gain_arr[fi,tile_use]=gain_curr
      ENDFOR
    endif else begin
      ;; calibrate using polynomials in frequency rather than fitting each frequency independently
    
      common poly_cal_data, model_vis, data_vis, poly_cal_gain_B, f_arr, mode_type, mode_num, mask, ab_switch
      
      phase_modes = 2
      amp_modes = 3
      n_mode = phase_modes + amp_modes
      if phase_modes gt 0 then begin
        mode_type = [strarr(phase_modes) + 'phase', strarr(amp_modes) + 'amp']
        mode_num = [indgen(phase_modes), indgen(amp_modes)]
      endif else begin
        mode_type = strarr(amp_modes) + 'amp'
        mode_num = indgen(amp_modes)
      endelse
      gain_arr_mode = dblarr(n_mode, n_tile)
      gain_arr_mode[where(mode_type eq 'amp' and mode_num eq 0),*] = 1.
      
      ;; keep frequency direction around
      vis_data2=vis_avg[*,baseline_use]
      vis_model2=vis_model[*,baseline_use]
      weight2=weight[*,baseline_use]
      
      b_i_use=where(total(weight2,1) GT 0,n_baseline_use2)
      weight2=weight2[*, b_i_use]
      vis_data2=vis_data2[*, b_i_use];*weight_invert(weight2)
      vis_model2=vis_model2[*, b_i_use];*weight_invert(weight2)
      
      hist_A = histogram(tile_A_i_use, min=0, max = n_tile-1, reverse_indices = ri_A)
      hist_B = histogram(tile_B_i_use, min=0, max = n_tile-1, reverse_indices = ri_B)
      for i=0, n_tile-1 do begin
        if hist_A[i] ne 0 then begin
          inds = ri_A[ri_A[i]:ri_A[i+1]-1]
          if hist_B[i] ne 0 then inds = [inds, ri_B[ri_B[i]:ri_B[i+1]-1]]
        endif else if hist_B[i] ne 0 then inds = ri_B[ri_B[i]:ri_B[i+1]-1] else continue
        
        if i eq 0 then vis_tile_inds = create_struct('t'+number_formatter(i), inds) $
        else vis_tile_inds = create_struct(vis_tile_inds, 't'+number_formatter(i), inds)
      endfor
      
      tile_freq_flag = bytarr(n_freq, n_tile_use)
      n_arr=Fltarr(n_tile_use)
      FOR tile_i=0L,n_tile_use-1 DO BEGIN
        inds=vis_tile_inds.(tile_i)
        IF n_elements(inds) GT 1 THEN begin
          tile_freq_flag[*,tile_i] = total(weight2[*, inds],2) gt 0
        endif
        n_arr[tile_i]=n_elements(inds) ;NEED SOMETHING MORE IN CASE INDIVIDUAL TILES ARE FLAGGED FOR ONLY A FEW FREQUENCIES!!
      ENDFOR
      
      
      vis_use=vis_data2
      gain_curr_mode=gain_arr_mode[*,tile_use]
      gain_curr = poly_cal_gain(gain_curr_mode, mode_type, mode_num, tile_freq_flag)
      
      gain_track = dblarr(n_mode, n_tile, max_cal_iter)
      ncalls_track = lonarr(max_cal_iter)
stop      
      time0 = systime(1)
      FOR i=0L,(max_cal_iter-1)>1 DO BEGIN
        phase_fit_iter=Floor(max_cal_iter/4.)
        
        gain_new_mode=dblarr(n_modes, n_tile_use)
        gain_track[*, *, i] = gain_curr_mode
        conv_test=fltarr(max_cal_iter)
        
        FOR fi=0L,n_modes-1 DO BEGIN
        
          gain_curr = poly_cal_gain(gain_curr_mode, mode_type, mode_num, tile_freq_flag)
          
          IF phase_fit_iter-i GT 0 then continue ;fit only phase at first
          
          vis_model_matrix=total(abs(vis_model2*Conj(gain_curr[*,B_ind])*exp(complex(0,1)*atan(gain_curr[*,A_ind],/phase)))*freq_func, 1)
          
          FOR tile_i=0L,n_tile-1 DO begin
            wh_A = where(tile_A_i eq tile_i, n_vis_A)
            
            if n_vis_A eq 0 then begin
              wh_B = where(tile_B_i eq tile_i, n_vis_B)
             
              f_arr = rebin((dindgen(n_freq)/(n_freq-1))*2-1, n_freq, n_vis_B)
              model_vis = vis_model1[*,wh_B]
              data_vis = vis_data1[*, wh_B]
              poly_cal_gain_B = gain_curr[*,tile_A_i[wh_B]]
              mask = tile_freq_flag[*,tile_B_i[wh_B]]
              ab_switch = 1
            endif else begin
            
              f_arr = rebin((dindgen(n_freq)/(n_freq-1))*2-1, n_freq, n_vis_A)
              model_vis = vis_model1[*,wh_A]
              data_vis = vis_data1[*, wh_A]
              poly_cal_gain_B = gain_curr[*,tile_B_i[wh_A]]
              mask = tile_freq_flag[*,tile_A_i[wh_A]]
              ab_switch=0
            endelse
            
            p=gain_curr_mode[*, tile_i]
            dfpmin, p, 1.0e-3, fval, 'poly_cal_chi2', 'poly_cal_grad', iter=ncalls
            gain_new_mode[*, tile_i]=p
          endfor
          ncalls_track[i] = ncalls
          
        endfor
        
        IF Total(Abs(gain_new_mode[where(mode_type eq 'amp')])) EQ 0 THEN BEGIN
          gain_curr_mode=gain_new_mode
          BREAK
        ENDIF
        
        gain_old_mode=gain_curr_mode
        gain_curr_mode=(gain_new_mode+gain_old_mode)/2.
        IF phase_fit_iter-i GT 0 then gain_curr_mode[where(mode_type eq 'amp'),*] = gain_old_mode[where(mode_type eq 'amp'),*]
        if min(gain_curr_mode[where(mode_type eq 'amp' and mode_num eq 0),*]) lt 0 then $
          gain_curr_mode[where(mode_type eq 'amp' and mode_num eq 0),*] = abs(gain_curr_mode[where(mode_type eq 'amp' and mode_num eq 0),*])
          
        gain_old = gain_curr
        gain_curr = poly_cal_gain(gain_curr_mode, mode_type, mode_num, tile_freq_flag)
        
        dgain=Abs(gain_curr)*weight_invert(Abs(gain_old))
        diverge_i=where(dgain LT Abs(gain_old)/2.,n_diverge)
        IF n_diverge GT 0 THEN gain_curr[diverge_i]=(gain_new[diverge_i]+gain_old[diverge_i]*2.)/3.
        IF nan_test(gain_curr) GT 0 THEN gain_curr[where(Finite(gain_curr,/nan))]=gain_old[where(Finite(gain_curr,/nan))]
        gain_curr*=Conj(gain_curr[ref_tile_use])/Abs(gain_curr[ref_tile_use])
        conv_test[i]=Max(Abs(gain_curr-gain_old)*weight_invert(Abs(gain_old)))
        IF i GE 1 THEN $
          IF conv_test[i] LE conv_thresh THEN BREAK $
        ELSE IF Abs(conv_test[i]-conv_test[i-1]) LE conv_thresh/2. THEN BREAK
        
      ENDFOR
      time1 = systime(1)
      print, 'polynomial cal loop time: ', time1-time0
      
      gain_arr_mode[*,tile_use]=gain_curr_mode
      Ptr_free,A_ind_arr
      
      gain_arr[*,tile_use] = gain_curr
stop      
    endelse
    
    nan_i=where(Finite(gain_arr,/nan),n_nan)
    IF n_nan GT 0 THEN BEGIN
      ;any gains with NANs -> all tiles for that freq will have NANs
      freq_nan_i=nan_i mod n_freq
      freq_nan_i=freq_nan_i[Uniq(freq_nan_i,Sort(freq_nan_i))]
      FOR pol_i2=0,n_pol-1 DO (*flag_ptr_use[pol_i2])[freq_nan_i,*]=0
      gain_arr[nan_i]=0.
      
    ENDIF
    *cal_return.gain[pol_i]=gain_arr
  ENDFOR
  
  vis_count_i=where(*flag_ptr_use[0],n_vis_cal)
  cal_return.n_vis_cal=n_vis_cal
  
  RETURN,cal_return
END