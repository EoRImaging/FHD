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
PRO vis_flag,data_array,flag_arr,obs,params,flag_nsigma=flag_nsigma,_Extra=extra

min_baseline=obs.min_baseline
max_baseline=obs.max_baseline
IF N_Elements(flag_nsigma) EQ 0 THEN flag_nsigma=3.

nbt=(size(data_array,/dimension))[3]
nf=(size(data_array,/dimension))[2]
np=(size(data_array,/dimension))[1]
data_abs=Sqrt(Reform(data_array[0,*,*,*],1,np,nf,nbt)^2.+Reform(data_array[1,*,*,*],1,np,nf,nbt)^2.)
;IF (size(data_array,/dimension))[1] EQ 1 THEN data_abs=Reform(data_abs,1,(size(data_abs,/dimension))[0],(size(data_abs,/dimension))[1])

n_frequencies=obs.n_freq
n_pol=(size(data_array,/dimension))[1]
tile_A=(*obs.baseline_info).tile_A
tile_B=(*obs.baseline_info).tile_B
freq=obs.freq
n_tiles=n_elements(uniq(tile_A[sort(tile_A)]))
flag_center=1
flag_edge=3
coarse_channel_id=Floor(indgen(n_frequencies)/24.)
coarse_channel_pos=indgen(n_frequencies) mod 32
flag_arr[*,where(coarse_channel_pos EQ 16),*]=0
flag_arr[*,where((coarse_channel_pos LT flag_edge) OR (coarse_channel_pos GT 32-flag_edge-1)),*]=0

;IF Keyword_Set(cut_baselines) THEN BEGIN
uv_dist=Sqrt(params.uu^2.+params.vv^2.)*median(freq)
cut_baselines_i=where((uv_dist LT min_baseline) OR (uv_dist GT max_baseline),n_baselines_cut)
IF N_baselines_cut GT 0 THEN flag_arr[*,*,cut_baselines_i]=0
;ENDIF

tile_fom=fltarr(n_tiles)
FOR tile_i=0,n_tiles-1 DO BEGIN
    tile_ABi=[where((tile_A-1) EQ tile_i,nA),where((tile_B-1) EQ tile_i,nB)];2442
    data_subset=Reform(data_abs[*,*,tile_ABi],np,nf,nA+nB)
    FOR pol_i=0,n_pol-1 DO BEGIN
        i_use=where((flag_arr[pol_i,*,tile_ABi] GT 0) AND (data_subset[pol_i,*,tile_ABi] GT 0),n_use)
        IF n_use GT 10 THEN tile_fom[tile_i]+=Stddev((data_subset[pol_i,*,tile_ABi])[i_use]);/Median(data_subset[i_use])
    ENDFOR
ENDFOR

freq_fom=fltarr(n_frequencies)
FOR freq_i=0,n_frequencies-1 DO BEGIN
    data_subset=Reform(data_abs[*,freq_i,*],np,nbt)
    FOR pol_i=0,n_pol-1 DO BEGIN
        i_use=where((flag_arr[pol_i,freq_i,*] GT 0) AND (data_subset[pol_i,*] GT 0),n_use)
        IF n_use GT 10 THEN freq_fom[freq_i]+=Stddev(data_subset[pol_i,i_use]);/Median(data_subset[i_use])
    ENDFOR
ENDFOR

tile_freq_fom=fltarr(n_tiles,n_frequencies)
FOR tile_i=0,n_tiles-1 DO BEGIN
    tile_ABi=[where((tile_A-1) EQ tile_i,nA),where((tile_B-1) EQ tile_i,nB)]
    FOR freq_i=0,n_frequencies-1 DO BEGIN
        data_subset=Reform(data_abs[*,freq_i,tile_ABi],np,nA+nB)
        FOR pol_i=0,n_pol-1 DO BEGIN
            i_use=where((flag_arr[pol_i,freq_i,tile_ABi] GT 0) AND (data_subset[pol_i,tile_ABi] GT 0),n_use)
            IF n_use GT 10 THEN tile_freq_fom[tile_i,freq_i]+=Stddev(data_subset[pol_i,i_use])
        ENDFOR
    ENDFOR
ENDFOR
freq_nonzero_i=where(freq_fom)
tile_mean=Median(tile_fom[where(tile_fom)])
tile_dev=Stddev(tile_fom[where(tile_fom)])    
freq_mean=Median(freq_fom[freq_nonzero_i],n_frequencies/20.)
freq_dev=Stddev((freq_fom[freq_nonzero_i]-freq_mean))

tile_cut0=where((Abs(tile_mean-tile_fom) GT 2.*flag_nsigma*tile_dev) OR (tile_fom EQ 0),n_tile_cut0,complement=tile_i_use0)
freq_cut0=freq_nonzero_i[where((Abs(freq_mean-freq_fom) GT 2.*flag_nsigma*freq_dev) OR (freq_fom EQ 0),n_freq_cut0,complement=freq_i_use0)]
freq_i_use0=freq_nonzero_i[freq_i_use0]
tile_mean2=Median(tile_fom[tile_i_use0])
tile_dev2=Stddev(tile_fom[tile_i_use0])    
freq_mean2=Median(freq_fom,n_frequencies/20.)
freq_dev2=Stddev((freq_fom-freq_mean2)[freq_i_use0])
tile_cut=where((Abs(tile_mean2-tile_fom) GT flag_nsigma*tile_dev2) OR (tile_fom EQ 0),n_tile_cut,complement=tile_i_use)
freq_cut=where((Abs(freq_mean2-freq_fom) GT flag_nsigma*flag_nsigma*freq_dev2) OR (freq_fom EQ 0.),n_freq_cut,complement=freq_i_use)
        
IF n_tile_cut GT 0 THEN BEGIN
    print,'Tiles cut:',tile_cut+1
    FOR bad_i=0,n_tile_cut-1 DO BEGIN
        flag_arr[*,*,where(tile_A EQ (tile_cut[bad_i]+1))]=0
        flag_arr[*,*,where(tile_B EQ (tile_cut[bad_i]+1))]=0
    ENDFOR
ENDIF
IF n_freq_cut GT 0 THEN flag_arr[*,freq_cut,*]=0
obs.n_vis=N_Elements(where(flag_arr[0,*,*] GT 0))
END