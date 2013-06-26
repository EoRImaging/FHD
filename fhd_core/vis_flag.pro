;+
; :Description:
;    Describe the procedure.
;
; :Params:
;    data_array - [# of polarizations, # of frequencies, (# of baselines) * (# of time integrations)] complex array of visibilty data  
;    
;    flag_arr - Same dimensions as data_array. Values LE 0 are considered flagged as bad data!
;    
;    obs - structure containing details of the observation
;    
;    params - structure containing u and v coordinates of the baselines, in meters/c. 
;
; :Keywords:
;    cut_baselines
;    flag_nsigma
;
; :Author: isullivan 2012
;-
PRO vis_flag,vis_arr,flag_ptr,obs,params,flag_nsigma=flag_nsigma,_Extra=extra

min_baseline=obs.min_baseline
max_baseline=obs.max_baseline
IF N_Elements(flag_nsigma) EQ 0 THEN flag_nsigma=3.

n_pol=obs.n_pol
n_freq=obs.n_freq

data_abs=Abs(*vis_arr[0])
IF n_pol GT 1 THEN data_abs=SQRT(data_abs^2.+Abs(*vis_arr[1])^2.)

tile_A=(*obs.baseline_info).tile_A
tile_B=(*obs.baseline_info).tile_B
IF Tag_exist(obs,'freq') THEN freq=obs.freq ELSE freq=(*obs.baseline_info).freq
n_tiles=obs.n_tile
;now taken care of in a more appropriate place
;flag_center=1
;flag_edge=3
;coarse_channel_id=Floor(indgen(n_freq)/24.)
;coarse_channel_pos=indgen(n_freq) mod 32
;flag_arr[*,where(coarse_channel_pos EQ 16),*]=0
;flag_arr[*,where((coarse_channel_pos LT flag_edge) OR (coarse_channel_pos GT 32-flag_edge-1)),*]=0

;IF Keyword_Set(cut_baselines) THEN BEGIN
uv_dist=Sqrt(params.uu^2.+params.vv^2.)*median(freq)
cut_baselines_i=where((uv_dist LT min_baseline) OR (uv_dist GT max_baseline),n_baselines_cut)
IF N_baselines_cut GT 0 THEN FOR pol_i=0,n_pol-1 DO (*flag_ptr[pol_i])[*,cut_baselines_i]=0
;ENDIF

tile_fom=fltarr(n_tiles)
FOR tile_i=0L,n_tiles-1 DO BEGIN
    tile_Ai=where((tile_A-1) EQ tile_i,nA,/L64)
    tile_Bi=where((tile_B-1) EQ tile_i,nB,/L64)
    IF nB GT 0 THEN IF nA GT 0 THEN tile_ABi=[tile_Ai,tile_Bi] ELSE tile_ABi=tile_Bi ELSE IF nA GT 0 THEN tile_ABi=tile_Ai 
    data_subset=data_abs[*,tile_ABi]
    FOR pol_i=0,n_pol-1 DO BEGIN
        i_use=where(((*flag_ptr[pol_i])[*,tile_ABi] GT 0) AND (data_subset GT 0),n_use)
        IF n_use GT 10 THEN tile_fom[tile_i]+=Stddev(data_subset[i_use]);/Median(data_subset[i_use])
    ENDFOR
ENDFOR

freq_fom=fltarr(n_freq)
FOR freq_i=0,n_freq-1 DO BEGIN
    data_subset=Reform(data_abs[freq_i,*])
    FOR pol_i=0,n_pol-1 DO BEGIN
        i_use=where(((*flag_ptr[pol_i])[freq_i,*] GT 0) AND (data_subset GT 0),n_use)
        IF n_use GT 10 THEN freq_fom[freq_i]+=Stddev(data_subset[i_use]);/Median(data_subset[i_use])
    ENDFOR
ENDFOR

tile_freq_fom=fltarr(n_tiles,n_freq)
FOR tile_i=0,n_tiles-1 DO BEGIN
    tile_Ai=where((tile_A-1) EQ tile_i,nA)
    tile_Bi=where((tile_B-1) EQ tile_i,nB)
    IF nB GT 0 THEN IF nA GT 0 THEN tile_ABi=[tile_Ai,tile_Bi] ELSE tile_ABi=tile_Bi ELSE IF nA GT 0 THEN tile_ABi=tile_Ai 
    FOR freq_i=0,n_freq-1 DO BEGIN
        data_subset=Reform(data_abs[freq_i,tile_ABi])
        FOR pol_i=0,n_pol-1 DO BEGIN
            i_use=where(((*flag_ptr[pol_i])[freq_i,tile_ABi] GT 0) AND (data_subset GT 0),n_use)
            IF n_use GT 10 THEN tile_freq_fom[tile_i,freq_i]+=Stddev(data_subset[i_use])
        ENDFOR
    ENDFOR
ENDFOR
freq_nonzero_i=where(freq_fom,complement=freq_zero_i,ncomp=nf_zero)
tile_mean=Median(tile_fom[where(tile_fom)])
tile_dev=Stddev(tile_fom[where(tile_fom)])    
freq_mean1=Median(freq_fom[freq_nonzero_i],n_freq/20.)
freq_mean=fltarr(n_freq)
freq_mean[freq_nonzero_i]=freq_mean1
freq_dev=Stddev((freq_fom[freq_nonzero_i]-freq_mean[freq_nonzero_i]))

tile_cut0=where((Abs(tile_mean-tile_fom) GT 2.*flag_nsigma*tile_dev) OR (tile_fom EQ 0),n_tile_cut0,complement=tile_i_use0)
freq_cut0=where((Abs(freq_mean-freq_fom) GT 2.*flag_nsigma*freq_dev) OR (freq_fom EQ 0),n_freq_cut0,complement=freq_i_use0)
tile_mean2=Median(tile_fom[tile_i_use0])
tile_dev2=Stddev(tile_fom[tile_i_use0])    
freq_mean2=Median(freq_fom,n_freq/20.)
freq_dev2=Stddev((freq_fom-freq_mean2)[freq_i_use0])
tile_cut=where((Abs(tile_mean2-tile_fom) GT flag_nsigma*tile_dev2) OR (tile_fom EQ 0),n_tile_cut,complement=tile_i_use)
freq_cut=where((Abs(freq_mean2-freq_fom) GT flag_nsigma*freq_dev2) OR (freq_fom EQ 0.),n_freq_cut,complement=freq_i_use)
        
IF n_tile_cut GT 0 THEN BEGIN
    print,'Tiles cut:',tile_cut+1
    FOR bad_i=0,n_tile_cut-1 DO BEGIN
        FOR pol_i=0,n_pol-1 DO BEGIN
            (*flag_ptr[pol_i])[*,where(tile_A EQ (tile_cut[bad_i]+1))]=0
            (*flag_ptr[pol_i])[*,where(tile_B EQ (tile_cut[bad_i]+1))]=0
        ENDFOR
    ENDFOR
ENDIF
IF n_freq_cut GT 0 THEN FOR pol_i=0,n_pol-1 DO (*flag_ptr[pol_i])[freq_cut,*]=0
obs.n_vis=N_Elements(where(*flag_ptr[0] GT 0))
END