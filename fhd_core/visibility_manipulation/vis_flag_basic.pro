FUNCTION vis_flag_basic,vis_weight_ptr,obs,params,instrument=instrument,mask_mirror_indices=mask_mirror_indices,$
    freq_start=freq_start,freq_end=freq_end,tile_flag_list=tile_flag_list,no_frequency_flagging=no_frequency_flagging,$
    unflag_all=unflag_all,vis_ptr=vis_ptr,channel_edge_flag_width=channel_edge_flag_width,coplanar_baseline_threshold=coplanar_baseline_threshold,_Extra=extra

IF tag_exist(obs,'instrument') THEN instrument=obs.instrument
IF N_Elements(instrument) EQ 0 THEN instrument='mwa' ELSE instrument=StrLowCase(instrument)

n_pol=obs.n_pol
n_freq=obs.n_freq
n_tile=obs.n_tile
freq_arr=(*obs.baseline_info).freq

IF Keyword_Set(mask_mirror_indices) AND Keyword_Set(params) THEN BEGIN
    conj_i=where(params.uu GT 0,n_conj)
    IF n_conj GT 0 THEN BEGIN
        FOR pol_i=0,n_pol-1 DO (*vis_weight_ptr[pol_i])[*,conj_i]=0
    ENDIF
ENDIF

IF Keyword_Set(freq_start) THEN BEGIN
    frequency_array_MHz=freq_arr/1E6
    freq_start_cut=where(frequency_array_MHz LT freq_start,nf_cut_start)
    IF nf_cut_start GT 0 THEN FOR pol_i=0,n_pol-1 DO (*vis_weight_ptr[pol_i])[freq_start_cut,*]=0
ENDIF ELSE nf_cut_start=0
IF Keyword_Set(freq_end) THEN BEGIN
    frequency_array_MHz=freq_arr/1E6
    freq_end_cut=where(frequency_array_MHz GT freq_end,nf_cut_end)
    IF nf_cut_end GT 0 THEN FOR pol_i=0,n_pol-1 DO (*vis_weight_ptr[pol_i])[freq_end_cut,*]=0
ENDIF ELSE nf_cut_end=0

IF Keyword_Set(tile_flag_list) THEN BEGIN
    vis_flag_tiles, obs, vis_weight_ptr, tile_flag_list=tile_flag_list
ENDIF
IF Keyword_Set(no_frequency_flagging) OR Keyword_Set(unflag_all) THEN do_nothing=1 ELSE BEGIN
  CASE instrument OF
      'mwa32t':BEGIN
          coarse_channel_width=32
          IF N_Elements(channel_edge_flag_width) EQ 0 THEN channel_edge_flag_width=4
          fine_channel_i=lindgen(n_freq) mod coarse_channel_width
          channel_edge_flag=where(fine_channel_i<((coarse_channel_width-1)-fine_channel_i) LT channel_edge_flag_width)
          channel_center_flag=where(fine_channel_i EQ 15)
          FOR pol_i=0,n_pol-1 DO BEGIN
              (*vis_weight_ptr[pol_i])[channel_edge_flag,*]=0
              (*vis_weight_ptr[pol_i])[channel_center_flag,*]=0
          ENDFOR
      END
      'mwa':BEGIN
          freq_avg=Round(768./n_freq)
          IF N_Elements(channel_edge_flag_width) EQ 0 THEN channel_edge_flag_width=Ceil(2./freq_avg)
          coarse_channel_width=Round(32./freq_avg)
          fine_channel_i=lindgen(n_freq) mod coarse_channel_width
          channel_edge_flag=where(fine_channel_i<((coarse_channel_width-1)-fine_channel_i) LT channel_edge_flag_width, n_flagged)
          IF n_flagged GT 0 THEN FOR pol_i=0,n_pol-1 DO (*vis_weight_ptr[pol_i])[channel_edge_flag,*]=0
      END
      ELSE:
  END
ENDELSE

tile_A_i=(*obs.baseline_info).tile_A-1
tile_B_i=(*obs.baseline_info).tile_B-1
n_baselines = N_Elements(tile_A_i)
freq_use=Replicate(1,n_freq)
tile_use=Replicate(1,n_tile)
FOR pol_i=0,n_pol-1 DO BEGIN
    baseline_flag=Max(*vis_weight_ptr[pol_i],dimension=1)>0
    freq_flag=Max(*vis_weight_ptr[pol_i],dimension=2)>0
    
    fi_use=where(freq_flag GT 0,nf_use)
    bi_use=where(baseline_flag GT 0,nb_use)
    
    freq_use1=intarr(n_freq) 
    IF nf_use GT 0 THEN freq_use1[fi_use]=1.
    freq_use*=freq_use1
    
    tile_use1=intarr(n_tile)
    IF nb_use GT 0 THEN tile_use1[tile_A_i[bi_use]]=1
    IF nb_use GT 0 THEN tile_use1[tile_B_i[bi_use]]=1
    ;tile_use*=tile_use1
    
ENDFOR

IF Keyword_Set(coplanar_baseline_threshold) AND Keyword_Set(params) THEN BEGIN
    ww=params.ww
    ww-=Median(ww)
    ww=Abs(ww)#freq_arr
    
    w_flag_i=where(ww GT coplanar_baseline_threshold,n_w_flag)
    IF n_w_flag GT 0 THEN BEGIN
        FOR pol_i=0,n_pol-1 DO (*vis_weight_ptr[pol_i])[w_flag_i]=0
    ENDIF
    ww=0
ENDIF

IF Keyword_Set(no_frequency_flagging) THEN BEGIN
    ;if pre-processing has flagged frequencies, need to unflag them if the data are nonzero (but DON'T unflag tiles that should be flagged)
    freq_cut_i=where(freq_use EQ 0,nf_cut)
    IF nf_cut GT 0 THEN FOR pol_i=0,n_pol-1 DO BEGIN
        freq_flag=0>Max(*vis_weight_ptr[pol_i],dimension=2)<1
        freq_unflag_i=where(freq_flag EQ 0,n_unflag)
        IF n_unflag GT 0 THEN BEGIN
            baseline_flag=Max(*vis_weight_ptr[pol_i],dimension=1)>0
            bi_use=where(baseline_flag GT 0,nb_use)
            FOR fi=0L,n_unflag-1 DO BEGIN
                IF nb_use EQ 0 THEN CONTINUE 
                data_test=Abs((*vis_ptr[pol_i])[freq_unflag_i[fi],bi_use])
                unflag_i=where(data_test GT 0,n_unflag1)
                IF n_unflag1 GT 0 THEN (*vis_weight_ptr[pol_i])[freq_unflag_i[fi],bi_use[unflag_i]]=1
            ENDFOR  
        ENDIF      
    ENDFOR
    freq_use=Replicate(1,n_freq) 
ENDIF ELSE freq_use=0>freq_use<1
tile_use=0>tile_use<1

;Time-based flagging
time_use=(*obs.baseline_info).time_use
n_time = obs.n_time
IF Min(time_use) LE 0 THEN BEGIN
    n_baselines=N_Elements((*obs.baseline_info).tile_A)
    bin_offset=(*obs.baseline_info).bin_offset
    bin_offset=[bin_offset,n_baselines]
    time_bin=Lonarr(n_baselines)
    FOR ti=0L,n_time-1 DO BEGIN
        IF time_use[ti] LE 0 THEN BEGIN
            FOR pol_i=0, n_pol-1 DO BEGIN
                (*vis_weight_ptr[pol_i])[*, bin_offset[ti]:bin_offset[ti+1]-1] = 0
            ENDFOR
        ENDIF
    ENDFOR
ENDIF

IF Keyword_Set(unflag_all) THEN BEGIN
    tile_use[*]=1
    freq_use[*]=1
    FOR pol_i=0,n_pol-1 DO (*vis_weight_ptr[pol_i])[*]=1>(*vis_weight_ptr[pol_i])
ENDIF

tile_use_new=tile_use AND (*obs.baseline_info).tile_use
freq_use_new=freq_use AND (*obs.baseline_info).freq_use
;(*obs.baseline_info).tile_use=tile_use_new
(*obs.baseline_info).freq_use=freq_use_new

IF Tag_exist(obs,'n_time_flag') THEN obs.n_time_flag=Total(1L-(*obs.baseline_info).time_use)
IF Tag_exist(obs,'n_tile_flag') THEN obs.n_tile_flag=Total(1L-(*obs.baseline_info).tile_use)
IF Tag_exist(obs,'n_freq_flag') THEN obs.n_freq_flag=Total(1L-(*obs.baseline_info).freq_use)

vis_count_i=where(*vis_weight_ptr[0],n_vis_in)
obs.n_vis_in=n_vis_in
RETURN,vis_weight_ptr
END
