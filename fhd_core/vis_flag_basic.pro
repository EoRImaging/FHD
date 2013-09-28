FUNCTION vis_flag_basic,flag_ptr,obs,params,instrument=instrument,mask_mirror_indices=mask_mirror_indices,_Extra=extra

;dimensions of flag_arr should be (polarization,freq,baselineXtime)
IF N_Elements(instrument) EQ 0 THEN instrument='mwa' ELSE instrument=StrLowCase(instrument)

n_pol=obs.n_pol
n_freq=obs.n_freq

IF Keyword_Set(mask_mirror_indices) AND Keyword_Set(params) THEN BEGIN
    conj_i=where(params.uu GT 0,n_conj)
    IF n_conj GT 0 THEN BEGIN
        FOR pol_i=0,n_pol-1 DO (*flag_ptr[pol_i])[*,conj_i]=0
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
        channel_edge_flag_width=Ceil(4./freq_avg)
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

FOR pol_i=0,n_pol-1 DO BEGIN
    freq_flag=Min(*flag_ptr[pol_i],dimension=1)>0
    baseline_flag=Min(*flag_ptr[pol_i],dimension=2)>0
ENDFOR
RETURN,flag_ptr
END