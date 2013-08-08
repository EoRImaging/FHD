FUNCTION vis_calibration_apply,vis_ptr,cal,preserve_original=preserve_original,pol_i=pol_i

n_pol=cal.n_pol
n_freq=cal.n_freq
n_tile=cal.n_tile
n_time=cal.n_time
bin_offset=cal.bin_offset
tile_A_i=cal.tile_A-1 ;tile numbering starts at 1
tile_B_i=cal.tile_B-1 ;tile numbering starts at 1
n_baselines=Long(N_Elements(tile_A_i))

gain_ptr=cal.gain ;Ptrarr(n_pol) to n_freq x n_tile complex arrays

inds_A=Rebin(Lindgen(n_freq),n_freq,n_baselines,/sample)+Rebin(transpose(tile_A_i)*n_freq,n_freq,n_baselines)
inds_B=Rebin(Lindgen(n_freq),n_freq,n_baselines,/sample)+Rebin(transpose(tile_B_i)*n_freq,n_freq,n_baselines)

IF N_Elements(pol_i) EQ 0 THEN BEGIN
    IF Keyword_Set(preserve_original) THEN BEGIN
        vis_cal_ptr=Ptrarr(n_pol,/allocate)
        FOR pol_i=0,n_pol-1 DO *vis_cal_ptr[pol_i]=*vis_ptr[pol_i]
    ENDIF ELSE vis_cal_ptr=vis_ptr
    
    FOR pol_i=0,n_pol-1 DO BEGIN
        gain_arr=*gain_ptr[pol_i]
;        vis_gain=gain_arr[inds_A]*(gain_arr[inds_B])
        vis_gain=gain_arr[inds_A]*Conj(gain_arr[inds_B])
        *vis_cal_ptr[pol_i]/=Temporary(vis_gain)
;        *vis_cal_ptr[pol_i]*=Temporary(vis_gain)
    ENDFOR
    RETURN,vis_cal_ptr
ENDIF ELSE BEGIN
    ;return a complex array, not a pointer, and don't overwrite if selecting one pol
    vis_cal=*vis_ptr[pol_i] 
    gain_arr=*gain_ptr[pol_i]
    vis_gain=gain_arr[inds_A]*gain_arr[inds_B]
    vis_cal/=Temporary(vis_gain)
    RETURN,vis_cal
ENDELSE

END