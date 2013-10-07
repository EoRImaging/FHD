function calib_freq_poly, mode, val, mask

  dims = size(mask, /dim)
  case n_elements(dims) of
    1: begin
      n_freq = dims
      n_val = 1
    end
    
    2: begin
      n_freq = dims[0]
      n_val = dims[1]
    end
    else: stop
  end
  
  if n_elements(val) ne n_val then stop
  
  if n_val eq 1 then val_arr = dblarr(n_freq) + val else val_arr = matrix_multiply(dblarr(n_freq)+1, val)
  x_arr = dindgen(n_freq)/(n_freq-1)-0.5
  if n_val gt 1 then x_arr = rebin(x_arr, n_freq, n_val)
  
  ;; use Legendre Polynomials for orthogonality
  case mode of
    0: freq_arr = val_arr
    1: freq_arr = val_arr * x_arr
    2: freq_arr = val_arr * (3.*x_arr^2. - 1.) / 2.
  endcase
  
  return, freq_arr*mask
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
      ;; calibrate using a 3rd order polynomial in frequency rather than fitting each frequency independently
    
      n_modes=3
      gain_arr_mode = complex(dblarr(n_modes, n_tile)+1)
      
      ;; keep frequency direction around
      vis_data2=vis_avg[*,baseline_use] & vis_data2=[[vis_data2],[Conj(vis_data2)]]
      vis_model2=vis_model[*,baseline_use] & vis_model2=[[vis_model2],[Conj(vis_model2)]]
      weight2=weight[*,baseline_use] & weight2=[[weight2],[weight2]]
      
      b_i_use=where(total(weight2,1) GT 0,n_baseline_use2)
      weight2=weight2[*, b_i_use]
      vis_data2=vis_data2[*, b_i_use];*weight_invert(weight2)
      vis_model2=vis_model2[*, b_i_use];*weight_invert(weight2)
      
      A_ind=[tile_A_i_use,tile_B_i_use] & A_ind=A_ind[b_i_use]
      B_ind=[tile_B_i_use,tile_A_i_use] & B_ind=B_ind[b_i_use]
      
      A_ind_arr=Ptrarr(n_tile_use,/allocate)
      n_arr=Fltarr(n_tile_use)
      tile_freq_flag = bytarr(n_freq, n_tile_use)
      FOR tile_i=0L,n_tile_use-1 DO BEGIN
        ;should be set up so that using where is okay
        inds=where(A_ind EQ tile_i,n1)
        IF n1 GT 1 THEN begin
          *A_ind_arr[tile_i]=Reform(inds,1,n1)
          tile_freq_flag[*,tile_i] = total(weight2[*, inds],2) gt 0
        endif ELSE *A_ind_arr[tile_i]=-1
        n_arr[tile_i]=n1 ;NEED SOMETHING MORE IN CASE INDIVIDUAL TILES ARE FLAGGED FOR ONLY A FEW FREQUENCIES!!
      ENDFOR
      
      FOR fi=0L,n_modes-1 DO BEGIN
        gain_curr=Reform(gain_arr_mode[fi,tile_use])
        
        phase_fit_iter=Floor(max_cal_iter/4.)
        
        gain_new=Complexarr(n_tile_use)
        conv_test=fltarr(max_cal_iter)
        FOR i=0L,(max_cal_iter-1)>1 DO BEGIN
          vis_use=vis_data2
          
          freq_func_B = calib_freq_poly(fi, reform(gain_curr[fi, B_ind]), weight2[*,B_ind] gt 0)
          vis_model_matrix=total(vis_model2*Conj(freq_func_B), 1)
          FOR tile_i=0L,n_tile_use-1 DO begin
            IF n_arr[tile_i] GE min_cal_solutions THEN begin
              freq_func = calib_freq_poly(fi, complex(dblarr(n_arr[tile_i])+1.), weight2[*,*A_ind_arr[tile_i]] gt 0)
              
              gain_new[tile_i]=LA_Least_Squares(vis_model_matrix[*A_ind_arr[tile_i]],total(vis_use[*, *A_ind_arr[tile_i]]*freq_func, 1),method=2)
            endif
          endfor
          
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
        gain_arr_mode[fi,tile_use]=gain_curr
      ENDFOR
      Ptr_free,A_ind_arr
      
      ;; make a new [n_freq, n_tile] gain array from gain_arr_model:[n_modes, n_baselines]
      temp = dblarr(n_freq, n_tile_use)
      for fi=0L,n_modes-1 do temp += calib_freq_poly(fi, reform(gain_arr_mode[fi,tile_use]), tile_freq_flag)
      
      gain_arr[*,tile_use] = temp
      
    endelse
    
;    ;need some error checking in case bad tile_use or freq_use
;    gain_freq_test=Median(Abs(gain_arr[*,tile_use]),dimension=2)
;    gain_tile_test=Median(Abs(gain_arr[freq_use,*]),dimension=1)
;    
;    sigma_threshold=10.
;    tile_mask=fltarr(n_tile) & tile_mask[tile_use]=1
;    freq_mask=fltarr(n_freq) & freq_mask[freq_use]=1
;    gain_arr_sub=extract_subarray(Abs(gain_arr),freq_use,tile_use)
;    gain_vals=gain_arr_sub[sort(gain_arr_sub)]
;    n_vals=N_Elements(gain_vals)
;    sigma_use=stddev(gain_vals[n_vals/4.:(3.*n_vals/4.)],/nan,/double)
;    tile_use=where((Abs(gain_tile_test-Median(gain_tile_test[tile_use])) LE sigma_threshold*sigma_use) AND tile_mask,$
;        n_tile_use,complement=tile_cut,ncomplement=n_tile_cut)
;    IF n_tile_cut GT 0 THEN tile_mask[tile_cut]=0
;    freq_use=where((Abs(gain_freq_test-Median(gain_freq_test[freq_use])) LE sigma_threshold*sigma_use) AND freq_mask,$
;        n_freq_use,complement=freq_cut,ncomplement=n_freq_cut)
;    IF n_freq_cut GT 0 THEN freq_mask[freq_cut]=0
;    
;    IF n_tile_cut GT 0 THEN BEGIN
;        gain_arr[*,tile_cut]=1.
;        tile_cut_full=tile_cut#Replicate(1.,n_time)+Replicate(1.,n_tile_cut)#bin_offset
;        FOR pol_i2=0,n_pol-1 DO (*flag_ptr_use[pol_i2])[*,tile_cut_full]=0
;    ENDIF
;    IF n_freq_cut GT 0 THEN BEGIN
;        gain_arr[freq_cut,*]=1.
;        FOR pol_i2=0,n_pol-1 DO (*flag_ptr_use[pol_i2])[freq_cut,*]=0
;    ENDIF
    
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