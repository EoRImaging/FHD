FUNCTION vis_calibrate_subroutine,cal,vis_ptr,vis_model_ptr,flag_ptr,n_cal_iter=n_cal_iter

IF N_Elements(n_cal_iter) EQ 0 THEN n_cal_iter=3L

n_pol=cal.n_pol
n_freq=cal.n_freq
n_tile=cal.n_tile
n_time=cal.n_time

cal_return=cal
FOR pol_i=0,n_pol-1 DO cal_return.gain[pol_i]=Ptr_new(*cal.gain[pol_i])

tile_A_i=cal.tile_A-1
tile_B_i=cal.tile_B-1
freq_arr=cal.freq
bin_offset=cal.bin_offset
n_baselines=bin_offset[1]
tile_A_i=tile_A_i[0:bin_offset[1]-1]
tile_B_i=tile_B_i[0:bin_offset[1]-1]
FOR pol_i=0,n_pol-1 DO BEGIN
    gain_arr=*cal.gain[pol_i]
    vis_use=*vis_ptr[pol_i]
    vis_use/=*vis_model_ptr[pol_i]
    flag_use=0>*flag_ptr[pol_i]<1
;    i_nan=where(Finite(result,/nan),n_nan)
;    IF n_nan GT 0 THEN vis_use[i_nan]=0.
    
    ;average over time
    ;the visibilities have dimension nfreq x (n_baselines x n_time), 
    ; which can be reformed to nfreq x n_baselines x n_time 
    vis_use=Total(Reform(vis_use,n_freq,n_baselines,n_time),3,/NAN)
    weight=Total(Reform(flag_use,n_freq,n_baselines,n_time),3,/NAN)
    i_use=where(weight GT 0,n_use)
    freq_weight=Total(weight,2)
    baseline_weight=Total(weight,1)
    freq_use=where(freq_weight,n_freq_use)
    baseline_use=where(baseline_weight,n_baseline_use)
    tile_use=where(histogram(tile_A_i[baseline_use],min=0,/bin,max=n_tile-1) $
        +histogram(tile_B_i[baseline_use],min=0,/bin,max=n_tile-1),n_tile_use)
;    tile_use=Uniq(tile_A_i[baseline_use])
;    n_tile_use=N_Elements(tile_use)
    
    vis_use*=weight_invert(weight)
    FOR fii=0L,n_freq_use-1 DO BEGIN
        fi=freq_use[fii]
        gain_curr=Reform(gain_arr[fi,*])
;        vis_matrix=Complexarr(n_tile,n_baseline_use)
;        gain_curr=Reform(gain_arr[fi,tile_A_i[baseline_use]])*Reform(gain_arr[fi,tile_B_i[baseline_use]])
;        vis_matrix[tile_A_i[baseline_use],baseline_use]+=vis_use[fi,baseline_use]
;        vis_matrix[tile_B_i[baseline_use],baseline_use]+=vis_use[fi,baseline_use]
        vis_matrix=Complexarr(n_tile,n_tile)
        vis_matrix[tile_A_i[baseline_use],tile_B_i[baseline_use]]=vis_use[fi,baseline_use]
        vis_matrix+=Conj(transpose(vis_matrix))
        vis_matrix/=2.
        
        FOR i=0L,(n_cal_iter-1)>1 DO BEGIN
            vis_matrix_use=extract_subarray(vis_matrix,tile_use,tile_use)
            gain_new=LA_Least_Squares(vis_matrix_use,Conj(gain_curr[tile_use]),method=2)
;            gain_new=1./Conj(gain_new)
            gain_new*=Conj(gain_new[0])/Abs(gain_new[0])
;            gain_new2=gain_new[tile_A_i[baseline_use]]*gain_new[tile_B_i[baseline_use]]
            gain_curr[tile_use]=(gain_new+gain_curr[tile_use])/2.
        ENDFOR 
        gain_arr[fi,*]=gain_curr
    ENDFOR
    
    *cal_return.gain[pol_i]=gain_arr
ENDFOR

RETURN,cal_return
END