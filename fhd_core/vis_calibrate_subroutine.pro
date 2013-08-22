FUNCTION vis_calibrate_subroutine,vis_ptr,vis_model_ptr,flag_ptr,obs,params,cal,n_cal_iter=n_cal_iter,$
    min_cal_baseline=min_cal_baseline,max_cal_baseline=max_cal_baseline,reference_tile=reference_tile,$
    preserve_visibilities=preserve_visibilities,_Extra=extra
    

IF N_Elements(n_cal_iter) EQ 0 THEN n_cal_iter=10L
IF N_Elements(reference_tile) EQ 0 THEN reference_tile=1L
IF N_Elements(min_cal_baseline) EQ 0 THEN min_cal_baseline=obs.min_baseline
IF N_Elements(max_cal_baseline) EQ 0 THEN max_cal_baseline=obs.max_baseline
IF N_Elements(cal) EQ 0 THEN cal=vis_struct_init_cal(obs,params)

n_pol=cal.n_pol
n_freq=cal.n_freq
n_tile=cal.n_tile
n_time=cal.n_time

IF Keyword_Set(preserve_visibilities) THEN BEGIN
    flag_ptr_use=Ptrarr(n_pol,/allocate)
    FOR pol_i=0,n_pol-1 DO *flag_ptr_use[pol_i]=*flag_ptr[pol_i]
ENDIF ELSE flag_ptr_use=flag_ptr
tile_A_i=cal.tile_A-1
tile_B_i=cal.tile_B-1
freq_arr=cal.freq
bin_offset=cal.bin_offset
n_baselines=bin_offset[1]

kbinsize=obs.kpix
kx_arr=cal.uu[0:n_baselines-1]/kbinsize ;ignore slight variation with time
ky_arr=cal.vv[0:n_baselines-1]/kbinsize
kr_arr=Sqrt(kx_arr^2.+ky_arr^2.)
dist_arr=(freq_arr#kr_arr)*kbinsize
flag_dist_cut=where((dist_arr LT min_cal_baseline) OR (dist_arr GT max_cal_baseline),n_dist_cut)

;baselines_use=where((kr_arr GE min_cal_baseline) AND (kr_arr LE max_cal_basline),n_baseline_use)

cal_return=cal
FOR pol_i=0,n_pol-1 DO cal_return.gain[pol_i]=Ptr_new(*cal.gain[pol_i])

tile_A_i=tile_A_i[0:n_baselines-1]
tile_B_i=tile_B_i[0:n_baselines-1]
FOR pol_i=0,n_pol-1 DO BEGIN
    gain_arr=*cal.gain[pol_i]
    
    ;average over time
    ;the visibilities have dimension nfreq x (n_baselines x n_time), 
    ; which can be reformed to nfreq x n_baselines x n_time 
    vis_measured=Reform(*vis_ptr[pol_i],n_freq,n_baselines,n_time)
    vis_model=Reform(*vis_model_ptr[pol_i],n_freq,n_baselines,n_time)
    flag_use=0>Reform(*flag_ptr_use[pol_i],n_freq,n_baselines,n_time)<1
    
    vis_avg=Total(vis_measured*flag_use,3)
    vis_model=Total(vis_model*flag_use,3)
    weight=Total(flag_use,3)
    IF n_dist_cut GT 0 THEN weight[flag_dist_cut]=0.
    vis_avg*=weight_invert(weight)
    vis_model*=weight_invert(weight)
    
    i_use=where(weight GT 0,n_use)
    freq_weight=Total(weight,2)
    baseline_weight=Total(weight,1)
    freq_use=where(freq_weight,n_freq_use)
    baseline_use=where(baseline_weight,n_baseline_use)
    hist_tile_A=histogram(tile_A_i[baseline_use],min=0,/bin,max=n_tile-1,reverse_ind=riA)
    hist_tile_B=histogram(tile_B_i[baseline_use],min=0,/bin,max=n_tile-1,reverse_ind=riB)
    tile_use=where(hist_tile_A+hist_tile_B,n_tile_use)
    
    tile_A_i_use=Lonarr(n_baseline_use)
    tile_B_i_use=Lonarr(n_baseline_use)
    FOR tile_i=0L,n_tile_use-1 DO BEGIN
        IF hist_tile_A[tile_use[tile_i]] GT 0 THEN tile_A_i_use[riA[riA[tile_use[tile_i]]:riA[tile_use[tile_i]+1]-1]]=tile_i
        IF hist_tile_B[tile_use[tile_i]] GT 0 THEN tile_B_i_use[riB[riB[tile_use[tile_i]]:riB[tile_use[tile_i]+1]-1]]=tile_i
    ENDFOR
;    gain_matrix_inds=tile_A_i_use+Lindgen(n_baseline_use)*n_tile_use
    
    ref_tile_use=Min(where(reference_tile EQ tile_use))
    IF ref_tile_use EQ -1 THEN ref_tile_use=0L
    nan_i=where(Finite(vis_avg,/nan),n_nan)
    IF n_nan GT 0 THEN vis_avg[nan_i]=0

    FOR fii=0L,n_freq_use-1 DO BEGIN
        fi=freq_use[fii]
        gain_curr=Reform(gain_arr[fi,tile_use])
        vis_data2=[Reform(vis_avg[fi,baseline_use]),Conj(Reform(vis_avg[fi,baseline_use]))]
        vis_model2=[Reform(vis_model[fi,baseline_use]),Conj(Reform(vis_model[fi,baseline_use]))]
        
        
        vis_model_matrix=Complexarr(n_tile_use,n_baseline_use*2.)
        A_ind=[tile_A_i_use,tile_B_i_use]
        B_ind=[tile_B_i_use,tile_A_i_use]
        
        model_matrix_inds=A_ind+Lindgen(n_baseline_use*2.)*n_tile_use
        
        phase_fit_iter=Floor(n_cal_iter/4.)
        FOR i=0L,(n_cal_iter-1)>1 DO BEGIN
            vis_model_matrix[model_matrix_inds]=vis_model2*Conj(gain_curr[B_ind])
;            vis_model_matrix[model_matrix_inds]=vis_model2*gain_curr[B_ind]
            vis_use=Conj(vis_data2)
;            vis_use=Conj(vis_data2)
            
;            real_matrix=Real_part(vis_model_matrix)
;            im_matrix=Imaginary(vis_model_matrix)
;            vis_real=Real_part(vis_use)
;            vis_im=Imaginary(vis_use)
;            gain_real=LA_Least_Squares(real_matrix,vis_real,/double,method=2)
;            gain_im=LA_Least_Squares(im_matrix,vis_im,/double,method=2)
;            gain_new=Complex(gain_real,gain_im)
;            
;            vis_use=Reform(vis_data_matrix##(1./Conj(gain_curr)))
            gain_new=LA_Least_Squares(vis_model_matrix,vis_use,/double,method=2)
            
            gain_new*=Conj(gain_new[ref_tile_use])/Abs(gain_new[ref_tile_use])
            IF phase_fit_iter-i GT 0 THEN gain_new*=weight_invert(Abs(gain_new)) ;fit only phase at first
            gain_curr=(gain_new+gain_curr)/2.
        ENDFOR
;        gain_arr[fi,tile_use]=gain_curr
        gain_arr[fi,tile_use]=Conj(gain_curr)
;        gain_arr[fi,tile_use]=1./Conj(gain_curr)
;        gain_arr[fi,tile_use]=1./(gain_curr)
    ENDFOR
    
;    FOR fii=0L,n_freq_use-1 DO BEGIN
;        fi=freq_use[fii]
;        gain_curr=Reform(gain_arr[fi,tile_use])
;        
;        
;        vis_data_matrix=Complexarr(n_tile_use,n_tile_use)
;        vis_data_matrix[tile_A_i_use,tile_B_i_use]=Reform(vis_avg[fi,baseline_use])
;        vis_data_matrix[tile_B_i_use,tile_A_i_use]=Conj(Reform(vis_avg[fi,baseline_use]))
;        
;        vis_model_matrix=Complexarr(n_tile_use,n_tile_use)
;        vis_model_matrix[tile_A_i_use,tile_B_i_use]=Reform(vis_model[fi,baseline_use])
;        vis_model_matrix[tile_B_i_use,tile_A_i_use]=Conj(Reform(vis_model[fi,baseline_use]))
;        
;        FOR i=0L,(n_cal_iter-1)>1 DO BEGIN
;            vis_use=Reform(vis_data_matrix##Conj(gain_curr))
;;            vis_use=Reform(vis_data_matrix##(1./Conj(gain_curr)))
;            gain_new=LA_Least_Squares(vis_model_matrix,vis_use,/double,method=2)
;            gain_new=weight_invert(gain_new)
;            
;            gain_new*=Conj(gain_new[ref_tile_use])/Abs(gain_new[ref_tile_use])
;            gain_curr=(gain_new+gain_curr)/2.
;        ENDFOR
;        gain_arr[fi,tile_use]=gain_curr
;    ENDFOR
        
;    FOR fii=0L,n_freq_use-1 DO BEGIN
;        fi=freq_use[fii]
;        gain_curr=Reform(gain_arr[fi,tile_use])
;        vis_matrix=Complexarr(n_tile,n_tile)
;        vis_matrix[tile_A_i[baseline_use],tile_B_i[baseline_use]]=vis_avg[fi,baseline_use]
;;        vis_matrix+=Conj(transpose(vis_matrix))
;        vis_matrix+=diag_matrix(replicate(1.,n_tile))
;;        vis_matrix+=Conj(transpose(vis_matrix))
;;        vis_matrix/=2.
;        
;        FOR i=0L,(n_cal_iter-1)>1 DO BEGIN
;            vis_matrix_use=extract_subarray(vis_matrix,tile_use,tile_use)
;            gain_new=LA_Least_Squares(vis_matrix_use,Conj(gain_curr),/double)
;;            gain_new=1./gain_new
;            gain_new*=Conj(gain_new[ref_tile_use])/Abs(gain_new[ref_tile_use])
;            gain_curr=(gain_new+gain_curr)/2.
;        ENDFOR 
;        gain_arr[fi,tile_use]=gain_curr
;    ENDFOR
    
    ;need some error checking in case bad tile_use or freq_use
    gain_freq_test=Median(Abs(gain_arr[*,tile_use]),dimension=2)
    gain_tile_test=Median(Abs(gain_arr[freq_use,*]),dimension=1)
    
;    conv_iter=5
    sigma_threshold=5.
    tile_mask=fltarr(n_tile) & tile_mask[tile_use]=1
    freq_mask=fltarr(n_freq) & freq_mask[freq_use]=1
;    FOR iter=0,conv_iter-1 DO BEGIN
;        tile_sigma=Stddev(gain_tile_test[tile_use],/nan,/double)
;        freq_sigma=Stddev(gain_freq_test[freq_use],/nan,/double)

        gain_arr_sub=extract_subarray(Abs(gain_arr),freq_use,tile_use)
        gain_vals=gain_arr_sub[sort(gain_arr_sub)]
        n_vals=N_Elements(gain_vals)
        sigma_use=stddev(gain_vals[n_vals/4.:(3.*n_vals/4.)],/nan,/double)
        tile_use=where((Abs(gain_tile_test-Median(gain_tile_test)) LE sigma_threshold*sigma_use) AND tile_mask,$
            n_tile_use,complement=tile_cut,ncomplement=n_tile_cut)
        IF n_tile_cut GT 0 THEN tile_mask[tile_cut]=0
        freq_use=where((Abs(gain_freq_test-Median(gain_freq_test)) LE sigma_threshold*sigma_use) AND freq_mask,$
            n_freq_use,complement=freq_cut,ncomplement=n_freq_cut)
        IF n_freq_cut GT 0 THEN freq_mask[freq_cut]=0
;    ENDFOR
    
    IF n_tile_cut GT 0 THEN BEGIN
        gain_arr[*,tile_cut]=1.
        tile_cut_full=tile_cut#Replicate(1.,n_time)+Replicate(1.,n_tile_cut)#bin_offset
        FOR pol_i2=0,n_pol-1 DO (*flag_ptr_use[pol_i2])[*,tile_cut_full]=0
    ENDIF
    IF n_freq_cut GT 0 THEN BEGIN
        gain_arr[freq_cut,*]=1.
        FOR pol_i2=0,n_pol-1 DO (*flag_ptr_use[pol_i2])[freq_cut,*]=0
    ENDIF
    
    nan_i=where(Finite(gain_arr,/nan),n_nan)
    IF n_nan GT 0 THEN BEGIN
        ;any gains with NANs -> all tiles for that freq will have NANs
        freq_nan_i=nan_i mod n_freq
        freq_nan_i=freq_nan_i[Uniq(freq_nan_i,Sort(freq_nan_i))]
        FOR pol_i2=0,n_pol-1 DO (*flag_ptr_use[pol_i2])[freq_nan_i,*]=0
        gain_arr[nan_i]=1.
    ENDIF
    *cal_return.gain[pol_i]=gain_arr
ENDFOR

RETURN,cal_return
END