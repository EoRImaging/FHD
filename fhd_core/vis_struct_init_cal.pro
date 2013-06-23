FUNCTION vis_struct_init_cal,obs,gain_arr_ptr
tile_A=(*obs.baseline_info).tile_A
tile_B=(*obs.baseline_info).tile_B
freq=(*obs.baseline_info).freq
bin_offset=(*obs.baseline_info).bin_offset
tile_names=(*obs.baseline_info).tile_names
n_pol=obs.n_pol
n_freq=obs.n_freq
n_tile=obs.n_tile
n_time=N_Elements(bin_offset)
IF N_Elements(gain_arr_ptr) EQ 0 THEN BEGIN
    gain_arr=Complexarr(n_freq,n_tile)+1 
    gain_arr_ptr=Ptrarr(n_pol,/allocate)
    FOR pol_i=0,n_pol-1 DO *gain_arr_ptr[pol_i]=gain_arr
ENDIF

cal_struct={n_pol:n_pol,n_freq:n_freq,n_tile:n_tile,n_time:n_time,$
    tile_A:tile_A,tile_B:tile_B,tile_names:tile_names,bin_offset:bin_offset,freq:freq,gain:gain_arr_ptr}
RETURN,cal_struct
END