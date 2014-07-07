FUNCTION moff_test,vis_arr,obs

print,"MOFF test: all visibilities recalculated from cross visibilities with reference tile"
ref_i=1 ;tile numbers start from 1; the reference tile will be Tile A below

tile_A=(*obs.baseline_info).tile_A
tile_B=(*obs.baseline_info).tile_B

n_pol=obs.n_pol
n_freq=obs.n_freq
n_baselines=obs.nbaselines
n_tiles=obs.n_tile
n_times=obs.n_time
bin_start=(*obs.baseline_info).bin_offset

FOR ti=0L,n_times-1 DO BEGIN
    IF ti LT n_times-1 THEN bin_end=bin_start[ti+1]-1 ELSE bin_end=N_Elements(tile_A)-1
    tile_A_use=tile_A[bin_start[ti]:bin_end]
    tile_B_use=tile_B[bin_start[ti]:bin_end]
    i_use=where(tile_A_use EQ ref_i,n_use)    
    
    FOR pol_i=0,n_pol-1 DO BEGIN
        B_i=tile_B_use[i_use]
        ref_auto_i=where(B_i EQ ref_i,n_auto) ;will break a lot of things if n_auto=0...
        
        vis_data=(*vis_arr[pol_i])[*,i_use+bin_start[ti]]
        IF n_auto GT 1 THEN auto_vals=Sqrt(Total(Abs(vis_data[*,ref_auto_i]),2)/n_auto) $
            ELSE auto_vals=Reform(Sqrt(Abs(vis_data[*,ref_auto_i])))
        
        Electric_field_arr=Complexarr(n_freq,n_tiles)
        FOR tile_i=0L,n_tiles-1 DO BEGIN
            tile_i_use=where(tile_A_use EQ tile_i+1,n_use1)
            CASE n_use1 OF
                0:
                1:Electric_field_arr[*,tile_i]=Reform(vis_data[*,tile_i_use])*weight_invert(auto_vals)
                ELSE:Electric_field_arr[*,tile_i]=Total(vis_data[*,tile_i_use],2)*weight_invert(auto_vals)/n_use1
            ENDCASE
           
        ENDFOR
        vis_data_calc=Electric_field_arr[*,(tile_A_use-1)]*Conj(Electric_field_arr[*,(tile_B_use-1)])
        (*vis_arr[pol_i])[*,bin_start[ti]:bin_end]=vis_data_calc
    ENDFOR

ENDFOR

RETURN,vis_arr
END