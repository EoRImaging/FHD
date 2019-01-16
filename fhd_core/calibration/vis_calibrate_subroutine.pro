FUNCTION vis_calibrate_subroutine,vis_ptr,vis_model_ptr,vis_weight_ptr,obs,cal,preserve_visibilities=preserve_visibilities,$
    calib_freq_func=calib_freq_func,calibration_weights=calibration_weights,no_ref_tile=no_ref_tile,_Extra=extra

  reference_tile=cal.ref_antenna
  min_baseline=obs.min_baseline
  max_baseline=obs.max_baseline
  dimension=obs.dimension
  elements=obs.elements
  double_precision=0
  IF Tag_Exist(obs, 'double_precision') THEN double_precision=obs.double_precision
  
  min_cal_baseline=cal.min_cal_baseline
  max_cal_baseline=cal.max_cal_baseline
  min_cal_solutions=cal.min_solns ;minimum number of calibration equations needed to solve for the gain of one baseline
  time_average=cal.time_avg ;average the visibilities across time steps before solving for the gains
  max_cal_iter=cal.max_iter ;maximum iterations to perform for the linear least-squares solver
  IF max_cal_iter LT 5 THEN print,'Warning! At least 5 calibration iterations recommended. Using '+Strn(Floor(max_cal_iter))
  conv_thresh=cal.conv_thresh
  
  n_pol=cal.n_pol
  n_freq=cal.n_freq
  n_tile=cal.n_tile
  n_time=cal.n_time
  
  vis_weight_ptr_use=vis_weight_ptr ;weights WILL be over-written! (Only for NAN gain solutions)
  tile_A_i=cal.tile_A-1 ;tile_A contribution indexed from 0
  tile_B_i=cal.tile_B-1 ;tile_B contribution indexed from 0
  freq_arr=cal.freq
  bin_offset=cal.bin_offset ;FLAG FOR DELETION
  n_baselines=obs.nbaselines
  IF Tag_exist(cal,'phase_iter') THEN phase_fit_iter=cal.phase_iter ELSE phase_fit_iter=Floor(max_cal_iter/4.)<4
  
  kbinsize=obs.kpix
  
  cal_return=cal
  FOR pol_i=0,n_pol-1 DO cal_return.gain[pol_i]=Ptr_new(*cal.gain[pol_i])
  
  FOR pol_i=0,n_pol-1 DO BEGIN
    convergence=Fltarr(n_freq,n_tile)
    gain_arr=*cal.gain[pol_i]
    
    ;***Average the visibilities over the time steps before solving for the gains solutions
    ;   This is not recommended, as longer baselines will be downweighted artifically.
    IF Keyword_Set(time_average) THEN BEGIN
      ;***The visibilities have dimension nfreq x (n_baselines x n_time),
      ;   which can be reformed to nfreq x n_baselines x n_time
      tile_A_i=tile_A_i[0:n_baselines-1]
      tile_B_i=tile_B_i[0:n_baselines-1]
      vis_weight_use=0>Reform(*vis_weight_ptr_use[pol_i],n_freq,n_baselines,n_time)<1
      IF Keyword_Set(preserve_visibilities) THEN vis_model=Reform(*vis_model_ptr[pol_i],n_freq,n_baselines,n_time) $
        ELSE vis_model=Reform(Temporary(*vis_model_ptr[pol_i]),n_freq,n_baselines,n_time)
      vis_model=Total(Temporary(vis_model)*vis_weight_use,3)
      vis_measured=Reform(*vis_ptr[pol_i],n_freq,n_baselines,n_time)
      vis_avg=Total(Temporary(vis_measured)*vis_weight_use,3)
      weight=Total(Temporary(vis_weight_use),3)
      
      kx_arr=cal.uu[0:n_baselines-1]/kbinsize ;ignore slight variation with time
      ky_arr=cal.vv[0:n_baselines-1]/kbinsize
      kr_arr=Sqrt(kx_arr^2.+ky_arr^2.)
      dist_arr=(freq_arr#Temporary(kr_arr))*kbinsize
      xcen=freq_arr#Abs(kx_arr)
      ycen=freq_arr#Abs(ky_arr)
      
      IF Keyword_Set(calibration_weights) THEN BEGIN
        flag_dist_cut=where((dist_arr LT min_baseline) OR (Temporary(xcen) GT dimension/2.) OR (Temporary(ycen) GT elements/2.),n_dist_cut)
        IF min_cal_baseline GT min_baseline THEN taper_min=((Sqrt(2.)*min_cal_baseline-dist_arr)/min_cal_baseline)>0. ELSE taper_min=0. 
        IF max_cal_baseline LT max_baseline THEN taper_max=((dist_arr-max_cal_baseline)/min_cal_baseline)>0. ELSE taper_max=0.
        baseline_weights=(1.-(taper_min+taper_max)^2.)>0.
      ENDIF ELSE BEGIN
        flag_dist_cut=where((dist_arr LT min_cal_baseline) OR (dist_arr GT max_cal_baseline) OR (Temporary(xcen) GT dimension/2.) OR (Temporary(ycen) GT elements/2.),n_dist_cut)
      ENDELSE
    ENDIF ELSE BEGIN
      vis_weight_use=0>*vis_weight_ptr_use[pol_i]<1
      IF Keyword_Set(preserve_visibilities) THEN vis_model=*vis_model_ptr[pol_i] $
        ELSE vis_model=Temporary(*vis_model_ptr[pol_i])
      vis_model=Temporary(vis_model)*vis_weight_use
      vis_avg=*vis_ptr[pol_i]*vis_weight_use
      weight=Temporary(vis_weight_use)
      
      kx_arr=cal.uu/kbinsize
      ky_arr=cal.vv/kbinsize
      kr_arr=Sqrt(kx_arr^2.+ky_arr^2.)
      dist_arr=(freq_arr#Temporary(kr_arr))*kbinsize
      xcen=freq_arr#Abs(Temporary(kx_arr))
      ycen=freq_arr#Abs(Temporary(ky_arr))
      IF Keyword_Set(calibration_weights) THEN BEGIN
        flag_dist_cut=where((Temporary(dist_arr) LT min_baseline) OR (Temporary(xcen) GT dimension/2.) OR (Temporary(ycen) GT elements/2.),n_dist_cut)
        IF min_cal_baseline GT min_baseline THEN taper_min=((Sqrt(2.)*min_cal_baseline-dist_arr)/min_cal_baseline)>0. ELSE taper_min=0. 
        IF max_cal_baseline LT max_baseline THEN taper_max=((dist_arr-max_cal_baseline)/min_cal_baseline)>0. ELSE taper_max=0.
        baseline_weights=(1.-(taper_min+taper_max)^2.)>0.
      ENDIF ELSE flag_dist_cut=where((dist_arr LT min_cal_baseline) OR (dist_arr GT max_cal_baseline) OR (Temporary(xcen) GT dimension/2.) OR (Temporary(ycen) GT elements/2.),n_dist_cut)
    ENDELSE
    kx_arr=(ky_arr=(dist_arr=0))
    ;***
    
    ;Remove the weights from data and model after the optional time averaging above. FLAG FOR LOOP INCLUSION
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
      IF hist_tile_A[tile_use[tile_i]] GT 0 THEN tile_A_i_use[riA[riA[tile_use[tile_i]]:riA[tile_use[tile_i]+1]-1]]=tile_i  ;Calculate tile contributions for each
      IF hist_tile_B[tile_use[tile_i]] GT 0 THEN tile_B_i_use[riB[riB[tile_use[tile_i]]:riB[tile_use[tile_i]+1]-1]]=tile_i  ; non-flagged baseline
    ENDFOR
    
    ref_tile_use=Min(where(reference_tile EQ tile_use))
    IF ref_tile_use EQ -1 THEN BEGIN
      ref_tile_use=0L
      cal.ref_antenna=tile_use[ref_tile_use]
      cal.ref_antenna_name=(*obs.baseline_info).tile_names[cal.ref_antenna]
    ENDIF
    
    ;Set NAN data to 0 in both the data and the model 
    nan_i=where(Finite(vis_avg,/nan),n_nan)
    IF n_nan GT 0 THEN vis_model[nan_i]=(vis_avg[nan_i]=0)
    
    conv_test=fltarr(n_freq_use,max_cal_iter)
    n_converged = 0L
;    if not keyword_set(calib_freq_func) then begin
      FOR fii=0L,n_freq_use-1 DO BEGIN
        fi=freq_use[fii]
        gain_curr=Reform(gain_arr[fi,tile_use])
        ;Reuse same gain solution between successive frequency channels IF input gains are default values
        ;        IF fii EQ 0 THEN gain_curr=Reform(gain_arr[fi,tile_use])
        ;        IF Stddev(gain_arr[fi,tile_use]) GT 0 THEN gain_curr=Reform(gain_arr[fi,tile_use])
        vis_data2=Reform(vis_avg[fi,baseline_use]) & vis_data2=[vis_data2,Conj(vis_data2)]        ;Set up data and model arrays of the original and conjugated versions. This
        vis_model2=Reform(vis_model[fi,baseline_use]) & vis_model2=[vis_model2,Conj(vis_model2)]  ;provides twice as many equations into the linear least-squares solver.
        weight2=Reform(weight[fi,baseline_use]) & weight2=[weight2,weight2]
        IF Keyword_Set(calibration_weights) THEN BEGIN baseline_wts2=Reform(baseline_weights[fi,baseline_use]) & baseline_wts2=[baseline_wts2,baseline_wts2] & ENDIF 
        
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
        
        gain_new=Complexarr(n_tile_use)
        FOR i=0L,(max_cal_iter-1)>1 DO BEGIN
            vis_use=vis_data2
            
            vis_model_matrix=vis_model2*Conj(gain_curr[B_ind])
            IF Keyword_Set(calibration_weights) THEN BEGIN
                FOR tile_i=0L,n_tile_use-1 DO IF n_arr[tile_i] GE min_cal_solutions THEN BEGIN
                    xmat=vis_model_matrix[*A_ind_arr[tile_i]]
                    xmat_dag=conj(xmat)*baseline_wts2
                    gain_new[tile_i]=1./(matrix_multiply(xmat_dag,xmat,/btranspose))*matrix_multiply(xmat_dag,vis_use[*A_ind_arr[tile_i]],/btrans)
                ENDIF
            ENDIF ELSE BEGIN
                FOR tile_i=0L,n_tile_use-1 DO IF n_arr[tile_i] GE min_cal_solutions THEN $
                    gain_new[tile_i]=LA_Least_Squares(vis_model_matrix[*A_ind_arr[tile_i]],vis_use[*A_ind_arr[tile_i]],method=2, double=double_precision)
            ENDELSE
            
            gain_old=gain_curr
            IF Total(Abs(gain_new)) EQ 0 THEN BEGIN
                gain_curr=gain_new
                BREAK
            ENDIF
            IF phase_fit_iter-i GT 0 THEN gain_new*=Abs(gain_old)*weight_invert(Abs(gain_new)) ;fit only phase at first
;          IF (2.*phase_fit_iter-i GT 0) AND (phase_fit_iter-i LE 0) THEN $
;            gain_new*=Mean(Abs(gain_new[where(gain_new)]))*weight_invert(Abs(gain_new)) ;then fit only average amplitude
            gain_curr=(gain_new+gain_old)/2.
            dgain=Abs(gain_curr)*weight_invert(Abs(gain_old))
            diverge_i=where(dgain LT Abs(gain_old)/2.,n_diverge)
            IF n_diverge GT 0 THEN gain_curr[diverge_i]=(gain_new[diverge_i]+gain_old[diverge_i]*2.)/3.
            IF nan_test(gain_curr) GT 0 THEN gain_curr[where(Finite(gain_curr,/nan))]=gain_old[where(Finite(gain_curr,/nan))]
            if ~keyword_set(no_ref_tile) then begin
              gain_curr*=Conj(gain_curr[ref_tile_use])/Abs(gain_curr[ref_tile_use])
            endif
            conv_test[fii,i]=Max(Abs(gain_curr-gain_old)*weight_invert(Abs(gain_old)))
            IF i GT phase_fit_iter THEN IF conv_test[fii,i] LE conv_thresh THEN BEGIN
                n_converged += 1
                BREAK
            ENDIF
        ENDFOR
        IF i EQ max_cal_iter THEN BEGIN
            print,String(format='("Calibration reached max iterations before converging for pol_i: ", A, " freq_i: ",A)', Strn(pol_i), Strn(fi)) 
        ENDIF
        convergence[fi,tile_use]=Abs(gain_curr-gain_old)*weight_invert(Abs(gain_old))
        Ptr_free,A_ind_arr
        gain_arr[fi,tile_use]=gain_curr
      ENDFOR
    nan_i=where(Finite(gain_arr,/nan),n_nan)
    IF n_nan GT 0 THEN BEGIN
      ;any gains with NANs -> all tiles for that freq will have NANs
      freq_nan_i=nan_i mod n_freq
      freq_nan_i=freq_nan_i[Uniq(freq_nan_i,Sort(freq_nan_i))]
      (*vis_weight_ptr_use[pol_i])[freq_nan_i,*]=0
      weight[freq_nan_i,*]=0
      gain_arr[nan_i]=0.
      
    ENDIF
    *cal_return.gain[pol_i]=gain_arr
    cal_return.convergence[pol_i]=Ptr_new(convergence)
    cal_return.n_converged[pol_i] = n_converged
  ENDFOR
  
  vis_count_i=where(weight,n_vis_cal)
  cal_return.n_vis_cal=n_vis_cal
  
  RETURN,cal_return
END
