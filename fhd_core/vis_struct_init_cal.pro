FUNCTION vis_struct_init_cal,obs,params,gain_arr_ptr=gain_arr_ptr,n_pol=n_pol,n_freq=n_freq,n_tile=n_tile,n_time=n_time,source_list=source_list,$
    tile_A=tile_A,tile_B=tile_B,freq=freq,bin_offset=bin_offset,tile_names=tile_names,u_loc=u_loc,v_loc=v_loc,n_cal_src=n_cal_src,galaxy_cal=galaxy_cal
IF N_Elements(tile_A) EQ 0 THEN tile_A=(*obs.baseline_info).tile_A
IF N_Elements(tile_B) EQ 0 THEN tile_B=(*obs.baseline_info).tile_B
IF N_Elements(freq) EQ 0 THEN freq=(*obs.baseline_info).freq
IF N_Elements(bin_offset) EQ 0 THEN bin_offset=(*obs.baseline_info).bin_offset
IF N_Elements(tile_names) EQ 0 THEN tile_names=(*obs.baseline_info).tile_names
IF N_Elements(u_loc) EQ 0 THEN u_loc=params.uu
IF N_Elements(v_loc) EQ 0 THEN v_loc=params.vv

IF N_Elements(n_pol) EQ 0 THEN n_pol=obs.n_pol
IF N_Elements(n_freq) EQ 0 THEN n_freq=obs.n_freq
IF N_Elements(n_tile) EQ 0 THEN n_tile=obs.n_tile
IF N_Elements(n_time) EQ 0 THEN n_time=N_Elements(bin_offset)
IF N_Elements(n_cal_src) EQ 0 THEN n_cal_src=-1
IF N_Elements(source_list) EQ 0 THEN source_comp_init,source_list,n_sources=n_cal_src
IF N_Elements(galaxy_cal) EQ 0 THEN galaxy_cal=0
IF N_Elements(gain_arr_ptr) EQ 0 THEN BEGIN
    gain_arr=Complexarr(n_freq,n_tile)+1 
    gain_arr_ptr=Ptrarr(n_pol,/allocate)
    FOR pol_i=0,n_pol-1 DO *gain_arr_ptr[pol_i]=gain_arr
ENDIF

cal_struct={n_pol:n_pol,n_freq:n_freq,n_tile:n_tile,n_time:n_time,uu:u_loc,vv:v_loc,source_list:source_list,$
    tile_A:tile_A,tile_B:tile_B,tile_names:tile_names,bin_offset:bin_offset,freq:freq,gain:gain_arr_ptr,n_cal_src:n_cal_src,galaxy_cal:galaxy_cal}
RETURN,cal_struct
END