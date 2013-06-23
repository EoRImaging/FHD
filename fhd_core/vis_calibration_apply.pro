PRO vis_calibration_apply,cal,vis_ptr

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
FOR pol_i=0,n_pol-1 DO BEGIN
    gain_arr=*gain_ptr[pol_i]
    vis_gain=gain_arr[inds_A]*gain_arr[inds_B]
    *vis_ptr[pol_i]/=Temporary(vis_gain)
ENDFOR
END