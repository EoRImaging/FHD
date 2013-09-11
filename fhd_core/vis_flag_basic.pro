FUNCTION vis_flag_basic,flag_ptr,obs,params,instrument=instrument,tile_flag=tile_flag,freq_flag=freq_flag,$
    mask_mirror_indices=mask_mirror_indices,_Extra=extra
;dimensions of flag_arr should be (polarization,freq,baselineXtime)
IF N_Elements(instrument) EQ 0 THEN instrument='mwa' ELSE instrument=StrLowCase(instrument)

IF N_Elements(obs) GT 0 THEN BEGIN
    tile_names=(*obs.baseline_info).tile_names
ENDIF
n_pol=obs.n_pol
n_freq=obs.n_freq

IF Keyword_Set(mask_mirror_indices) AND Keyword_Set(params) THEN BEGIN
    conj_i=where(params.uu GT 0,n_conj)
    IF n_conj GT 0 THEN BEGIN
        FOR pol_i=0,n_pol-1 DO *flag_ptr[*,conj_i]=0
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
    END
    'mwa':BEGIN
        freq_avg=Round(768./n_freq)
        channel_edge_flag_width=2./freq_avg
        coarse_channel_width=32./freq_avg
        fine_channel_i=lindgen(n_freq) mod coarse_channel_width
        channel_edge_flag=where(fine_channel_i<((coarse_channel_width-1)-fine_channel_i) LT channel_edge_flag_width)
        FOR pol_i=0,n_pol-1 DO (*flag_ptr[pol_i])[channel_edge_flag,*]=0
    END
    ELSE:
END
RETURN,flag_ptr
END