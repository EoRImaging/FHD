PRO vis_flag_tiles, obs, vis_weight_ptr, tile_flag_list=tile_flag_list
;Modifies *vis_weight_ptr in place to flag all visibilities associated with any of the specified tiles.
n_pol = obs.n_pol
IF size(tile_flag_list,/type) EQ 7 THEN BEGIN
    tile_names=(*obs.baseline_info).tile_names
    tile_flag_list_use=[-1L];value of 0 will be ignored. Use to initialize the array 
    FOR flag_i=0L,N_Elements(tile_flag_list)-1 DO BEGIN
        tile_i=(where(Strcompress(tile_names,/remove_all) EQ strcompress(tile_flag_list[flag_i],/remove_all),tile_match_flag))[0]
        IF tile_match_flag GT 0 THEN tile_flag_list_use=[tile_flag_list_use,tile_i+1]
    ENDFOR
ENDIF ELSE message, "tile_flag_list is expected to be a string array of tile names"
tile_A=(*obs.baseline_info).tile_A
tile_B=(*obs.baseline_info).tile_B
hist_A=histogram(tile_A,min=1,/bin,reverse=ra)
hist_B=histogram(tile_B,min=1,/bin,reverse=rb)
hist_C=histogram(tile_flag_list_use,min=1,/bin,reverse=rc)
hist_AB=hist_A+hist_B
n_ab=N_Elements(hist_AB)
n_c=N_Elements(hist_C)
n_bin=n_c<n_ab
tile_cut_i=where((hist_AB[0:n_bin-1] GT 0) AND (hist_C[0:n_bin-1] GT 0),n_cut)
IF n_cut GT 0 THEN BEGIN
    FOR ci=0,n_cut-1 DO BEGIN
        ti=tile_cut_i[ci]
        na=ra[ra[ti+1]-1]-ra[ra[ti]]
        IF na GT 0 THEN FOR pol_i=0,n_pol-1 DO (*vis_weight_ptr[pol_i])[*,ra[ra[ti]:ra[ti+1]-1]]=0
        nb=rb[rb[ti+1]-1]-rb[rb[ti]]
        IF nb GT 0 THEN FOR pol_i=0,n_pol-1 DO (*vis_weight_ptr[pol_i])[*,rb[rb[ti]:rb[ti+1]-1]]=0
    ENDFOR
ENDIF
END
