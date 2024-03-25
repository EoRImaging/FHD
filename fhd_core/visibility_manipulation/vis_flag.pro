;+
; :Description:
;    Describe the procedure.
;
; :Params:
;    data_array - [# of polarizations, # of frequencies, (# of baselines) * (# of time integrations)] complex array of visibilty data  
;    
;    vis_weights - Same dimensions as data_array. Values LE 0 are considered flagged as bad data!
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
PRO vis_flag,vis_arr,vis_weight_ptr,obs,psf,params,flag_nsigma=flag_nsigma,flag_sparse_uv_coverage=flag_sparse_uv_coverage,_Extra=extra

min_baseline=obs.min_baseline
max_baseline=obs.max_baseline
IF N_Elements(flag_nsigma) EQ 0 THEN flag_nsigma=3.

n_pol=obs.n_pol
n_freq=obs.n_freq

data_abs=Abs(*vis_arr[0])
IF n_pol GT 1 THEN data_abs=SQRT(data_abs^2.+Abs(*vis_arr[1])^2.)
b_info=*obs.baseline_info

tile_A=b_info.tile_A
tile_B=b_info.tile_B
freq=b_info.freq
n_tiles_use=Max(tile_A)>Max(tile_B)
tile_names=b_info.tile_names
n_baselines=N_Elements(tile_A)

uv_dist=Sqrt(params.uu^2.+params.vv^2.)*median(freq)
cut_baselines_i=where((uv_dist LT min_baseline) OR (uv_dist GT max_baseline),n_baselines_cut)
IF N_baselines_cut GT 0 THEN FOR pol_i=0,n_pol-1 DO (*vis_weight_ptr[pol_i])[*,cut_baselines_i]=0

tile_fom=fltarr(n_tiles_use)
FOR tile_i=0L,n_tiles_use-1 DO BEGIN
    tile_Ai=where((tile_A-1) EQ tile_i,nA,/L64)
    tile_Bi=where((tile_B-1) EQ tile_i,nB,/L64)
    IF nA+nB EQ 0 THEN CONTINUE
    IF nB GT 0 THEN IF nA GT 0 THEN tile_ABi=[tile_Ai,tile_Bi] ELSE tile_ABi=tile_Bi ELSE IF nA GT 0 THEN tile_ABi=tile_Ai 
    data_subset=data_abs[*,tile_ABi]
    FOR pol_i=0,n_pol<2-1 DO BEGIN
        i_use=where(((*vis_weight_ptr[pol_i])[*,tile_ABi] GT 0) AND (data_subset GT 0),n_use)
        IF n_use GT 10 THEN tile_fom[tile_i]+=Stddev(data_subset[i_use]);/Median(data_subset[i_use])
    ENDFOR
ENDFOR

freq_fom=fltarr(n_freq)
FOR freq_i=0,n_freq-1 DO BEGIN
    data_subset=Reform(data_abs[freq_i,*])
    FOR pol_i=0,n_pol<2-1 DO BEGIN
        i_use=where(((*vis_weight_ptr[pol_i])[freq_i,*] GT 0) AND (data_subset GT 0),n_use)
        IF n_use GT 10 THEN freq_fom[freq_i]+=Stddev(data_subset[i_use]);/Median(data_subset[i_use])
    ENDFOR
ENDFOR

;tile_freq_fom=fltarr(n_tiles_use,n_freq)
;FOR tile_i=0,n_tiles_use-1 DO BEGIN
;    tile_Ai=where((tile_A-1) EQ tile_i,nA)
;    tile_Bi=where((tile_B-1) EQ tile_i,nB)
;    IF nB GT 0 THEN IF nA GT 0 THEN tile_ABi=[tile_Ai,tile_Bi] ELSE tile_ABi=tile_Bi ELSE IF nA GT 0 THEN tile_ABi=tile_Ai 
;    FOR freq_i=0,n_freq-1 DO BEGIN
;        data_subset=Reform(data_abs[freq_i,tile_ABi])
;        FOR pol_i=0,n_pol<2-1 DO BEGIN
;            i_use=where(((*vis_weight_ptr[pol_i])[freq_i,tile_ABi] GT 0) AND (data_subset GT 0),n_use)
;            IF n_use GT 10 THEN tile_freq_fom[tile_i,freq_i]+=Stddev(data_subset[i_use])
;        ENDFOR
;    ENDFOR
;ENDFOR
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
    print,'Tiles cut:',tile_names[tile_cut]
    FOR bad_i=0,n_tile_cut-1 DO BEGIN
        FOR pol_i=0,n_pol-1 DO BEGIN
            cut_A_i=where(tile_A EQ (tile_cut[bad_i]+1),n_cut_A)
            cut_B_i=where(tile_B EQ (tile_cut[bad_i]+1),n_cut_B)
            IF n_cut_A GT 0 THEN (*vis_weight_ptr[pol_i])[*,cut_A_i]=0
            IF n_cut_B GT 0 THEN (*vis_weight_ptr[pol_i])[*,cut_B_i]=0
        ENDFOR
    ENDFOR
    tile_use1=b_info.tile_use
    tile_use1[tile_cut]=0
    b_info.tile_use=tile_use1
ENDIF
IF n_freq_cut GT 0 THEN BEGIN
    FOR pol_i=0,n_pol-1 DO (*vis_weight_ptr[pol_i])[freq_cut,*]=0
    freq_use1=b_info.freq_use
    freq_use1[freq_cut]=0
    b_info.freq_use=freq_use1
ENDIF

IF Tag_exist(b_info,'time_use') THEN time_use=b_info.time_use ELSE time_use=Fltarr(N_Elements(tile_A))+1
nt=N_Elements(time_use)
bin_offset=b_info.bin_offset
bin_offset=[bin_offset,n_baselines]
time_bin=Lonarr(n_baselines)
FOR ti=0L,nt-1 DO time_bin[bin_offset[ti]:bin_offset[ti+1]-1]=ti

time_fom=Fltarr(nt)
FOR ti=0,nt-1 DO BEGIN
    data_subset=data_abs[*,bin_offset[ti]:bin_offset[ti+1]-1]
    FOR pol_i=0,n_pol<2-1 DO BEGIN
        i_use=where((*vis_weight_ptr[pol_i])[*,bin_offset[ti]:bin_offset[ti+1]-1] GT 0,n_use)
        IF n_use GT 10 THEN time_fom[ti]+=Stddev(data_subset[i_use])
    ENDFOR
ENDFOR
time_mean=Median(time_fom[where(time_fom)])
time_dev=Stddev(time_fom[where(time_fom)])    
time_cut0=where((Abs(time_mean-time_fom) GT 2.*flag_nsigma*time_dev) OR (time_fom EQ 0),n_time_cut0,complement=time_i_use0)
time_mean2=Median(time_fom[time_i_use0])
time_dev2=Stddev(time_fom[time_i_use0])    
time_cut=where((Abs(time_mean2-time_fom) GT flag_nsigma*time_dev2) OR (time_fom EQ 0),n_time_cut,complement=time_i_use)
FOR ti=0L,n_time_cut-1 DO BEGIN
    ti_cut=where(time_bin EQ time_cut[ti],n_ti_cut)
    IF n_ti_cut GT 0 THEN FOR pol_i=0,n_pol-1 DO (*vis_weight_ptr[pol_i])[*,ti_cut]=0
ENDFOR

IF Keyword_Set(flag_sparse_uv_coverage) THEN sparse_uv_flag,obs,psf,params,vis_weight_ptr,flag_sparse_uv_coverage=flag_sparse_uv_coverage

print,'n_vis from vis_flag: ',Strn(N_Elements(where(*vis_weight_ptr[0] GT 0)))
obs.n_vis=N_Elements(where(*vis_weight_ptr[0] GT 0))
END