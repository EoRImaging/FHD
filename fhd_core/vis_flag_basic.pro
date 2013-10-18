FUNCTION vis_flag_basic,flag_ptr,obs,params,instrument=instrument,mask_mirror_indices=mask_mirror_indices,$
    freq_start=freq_start,freq_end=freq_end,tile_flag_list=tile_flag_list,_Extra=extra

IF tag_exist(obs,'instrument') THEN instrument=obs.instrument
IF N_Elements(instrument) EQ 0 THEN instrument='mwa' ELSE instrument=StrLowCase(instrument)

n_pol=obs.n_pol
n_freq=obs.n_freq
n_tile=obs.n_tile
freq_arr=(*obs.baseline_info).freq

IF Keyword_Set(mask_mirror_indices) AND Keyword_Set(params) THEN BEGIN
    conj_i=where(params.uu GT 0,n_conj)
    IF n_conj GT 0 THEN BEGIN
        FOR pol_i=0,n_pol-1 DO (*flag_ptr[pol_i])[*,conj_i]=0
    ENDIF
ENDIF

IF Keyword_Set(freq_start) THEN BEGIN
        frequency_array_MHz=freq_arr/1E6
        freq_start_cut=where(frequency_array_MHz LT freq_start,nf_cut_start)
        IF nf_cut_start GT 0 THEN FOR pol_i=0,n_pol-1 DO (*flag_ptr[pol_i])[freq_start_cut,*]=0
    ENDIF ELSE nf_cut_start=0
    IF Keyword_Set(freq_end) THEN BEGIN
        frequency_array_MHz=freq_arr/1E6
        freq_end_cut=where(frequency_array_MHz GT freq_end,nf_cut_end)
        IF nf_cut_end GT 0 THEN FOR pol_i=0,n_pol-1 DO (*flag_ptr[pol_i])[freq_end_cut,*]=0
    ENDIF ELSE nf_cut_end=0

    IF Keyword_Set(tile_flag_list) THEN BEGIN
        tile_A=(*obs.baseline_info).tile_A
        tile_B=(*obs.baseline_info).tile_B
        hist_A=histogram(tile_A,min=1,/bin,reverse=ra)
        hist_B=histogram(tile_B,min=1,/bin,reverse=rb)
        hist_C=histogram(tile_flag_list,min=1,/bin,reverse=rc)
        hist_AB=hist_A+hist_B
        n_ab=N_Elements(hist_AB)
        n_c=N_Elements(hist_C)
        n_bin=n_c<n_ab
        tile_cut_i=where((hist_AB[0:n_bin-1] GT 0) AND (hist_C[0:n_bin-1] GT 0),n_cut)
        IF n_cut GT 0 THEN BEGIN
            FOR ci=0,n_cut-1 DO BEGIN
                ti=tile_cut_i[ci]
                na=ra[ra[ti+1]-1]-ra[ra[ti]]
                IF na GT 0 THEN FOR pol_i=0,n_pol-1 DO (*flag_ptr[pol_i])[*,ra[ra[ti]:ra[ti+1]-1]]=0
                nb=rb[rb[ti+1]-1]-rb[rb[ti]]
                IF nb GT 0 THEN FOR pol_i=0,n_pol-1 DO (*flag_ptr[pol_i])[*,rb[rb[ti]:rb[ti+1]-1]]=0
            ENDFOR
        ENDIF
    ENDIF

CASE instrument OF
    'mwa32t':BEGIN
        coarse_channel_width=32
        channel_edge_flag_width=4
        fine_channel_i=lindgen(n_freq) mod coarse_channel_width
        channel_edge_flag=where(fine_channel_i<((coarse_channel_width-1)-fine_channel_i) LT channel_edge_flag_width)
        channel_center_flag=where(fine_channel_i EQ 15)
        FOR pol_i=0,n_pol-1 DO BEGIN
            (*flag_ptr[pol_i])[channel_edge_flag,*]=0
            (*flag_ptr[pol_i])[channel_center_flag,*]=0
        ENDFOR
        freq_use=(*obs.baseline_info).freq_use
        freq_use[channel_edge_flag]=0
        freq_use[channel_center_flag]=0
        (*obs.baseline_info).freq_use=freq_use
    END
    'mwa':BEGIN
        freq_avg=Round(768./n_freq)
        channel_edge_flag_width=Ceil(2./freq_avg)
        coarse_channel_width=Round(32./freq_avg)
        fine_channel_i=lindgen(n_freq) mod coarse_channel_width
        channel_edge_flag=where(fine_channel_i<((coarse_channel_width-1)-fine_channel_i) LT channel_edge_flag_width)
        FOR pol_i=0,n_pol-1 DO (*flag_ptr[pol_i])[channel_edge_flag,*]=0
        freq_use=(*obs.baseline_info).freq_use
        freq_use[channel_edge_flag]=0
        (*obs.baseline_info).freq_use=freq_use
    END
    ELSE:
END

tile_A_i=(*obs.baseline_info).tile_A-1
tile_B_i=(*obs.baseline_info).tile_B-1
freq_use=Replicate(1,n_freq)
tile_use=Replicate(1,n_tile)
FOR pol_i=0,n_pol-1 DO BEGIN
    baseline_flag=Max(*flag_ptr[pol_i],dimension=1)>0
    freq_flag=Max(*flag_ptr[pol_i],dimension=2)>0
    
    fi_use=where(freq_flag GT 0)
    bi_use=where(baseline_flag GT 0)
    
    freq_use1=intarr(n_freq) 
    freq_use1[fi_use]=1.
    freq_use*=freq_use1
    
    tile_use1=intarr(n_tile)
    tile_use1[tile_A_i[bi_use]]=1
    tile_use1[tile_B_i[bi_use]]=1
    tile_use*=tile_use1
    
ENDFOR
freq_use=0>freq_use<1
tile_use=0>tile_use<1

tile_use_new=tile_use AND (*obs.baseline_info).tile_use
freq_use_new=freq_use AND (*obs.baseline_info).freq_use
(*obs.baseline_info).tile_use=tile_use_new
(*obs.baseline_info).freq_use=freq_use_new

vis_count_i=where(*flag_ptr[0],n_vis_in)
obs.n_vis_in=n_vis_in
RETURN,flag_ptr
END