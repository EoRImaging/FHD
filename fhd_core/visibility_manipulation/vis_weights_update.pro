PRO vis_weights_update,vis_weight_ptr,obs,psf,params,xmin=xmin,xcen=xcen,ymin=ymin,no_frequency_flagging=no_frequency_flagging
t0_0=Systime(1)
heap_gc

;extract information from the structures
n_pol=obs.n_pol
n_tile=obs.n_tile
n_freq=obs.n_freq
dimension=Long(obs.dimension)
elements=Long(obs.elements)
kbinsize=obs.kpix
kx_span=kbinsize*Float(dimension) ;Units are # of wavelengths
ky_span=kx_span
min_baseline=obs.min_baseline
max_baseline=obs.max_baseline
b_info=*obs.baseline_info

freq_bin_i=b_info.fbin_i
fi_use=where(b_info.freq_use)
freq_bin_i=freq_bin_i;[fi_use]

frequency_array=b_info.freq
frequency_array=frequency_array;[fi_use]

psf_dim=psf.dim
psf_resolution=psf.resolution

kx_arr=params.uu/kbinsize
ky_arr=params.vv/kbinsize
n_frequencies=N_Elements(frequency_array)
n_baselines=N_Elements(kx_arr)

dist_test=Sqrt((kx_arr)^2.+(ky_arr)^2.)*kbinsize
dist_test=frequency_array#dist_test
flag_dist_i=where((dist_test LT min_baseline) OR (dist_test GT max_baseline),n_dist_flag)
dist_test=0

conj_i=where(ky_arr GT 0,n_conj)
IF n_conj GT 0 THEN BEGIN
    kx_arr[conj_i]=-kx_arr[conj_i]
    ky_arr[conj_i]=-ky_arr[conj_i]
ENDIF

xcen=frequency_array#kx_arr
xmin=Long(Floor(Temporary(xcen))+dimension/2-(psf_dim/2-1))
ycen=frequency_array#ky_arr
ymin=Long(Floor(Temporary(ycen))+elements/2-(psf_dim/2-1))

range_test_x_i=where((xmin LE 0) OR ((xmin+psf_dim-1) GE dimension-1),n_test_x)
IF n_test_x GT 0 THEN xmin[range_test_x_i]=(ymin[range_test_x_i]=-1)
range_test_x_i=0

range_test_y_i=where((ymin LE 0) OR ((ymin+psf_dim-1) GE elements-1),n_test_y)
IF n_test_y GT 0 THEN xmin[range_test_y_i]=(ymin[range_test_y_i]=-1)
range_test_y_i=0

IF n_dist_flag GT 0 THEN BEGIN
    xmin[flag_dist_i]=-1
    ymin[flag_dist_i]=-1
    flag_dist_i=0
ENDIF

IF Keyword_Set(no_frequency_flagging) THEN (*obs.baseline_info).freq_use=Replicate(1,n_freq) ELSE BEGIN
    freq_cut_i=where(b_info.freq_use EQ 0,n_freq_cut)
    IF n_freq_cut GT 0 THEN FOR pol_i=0,n_pol-1 DO (*vis_weight_ptr[pol_i])[freq_cut_i,*]=0
ENDELSE
tile_cut_i=where(b_info.tile_use EQ 0,n_tile_cut)
IF n_tile_cut GT 0 THEN BEGIN
    bi_cut=array_match(b_info.tile_A,b_info.tile_B,value_match=(tile_cut_i+1),n_match=n_bi_cut)
    IF n_bi_cut GT 0 THEN FOR pol_i=0,n_pol-1 DO (*vis_weight_ptr[pol_i])[*,bi_cut]=0
ENDIF

time_use=b_info.time_use
nt=N_Elements(time_use)
time_cut_i=where(time_use EQ 0,n_time_cut)
bin_offset=b_info.bin_offset
bin_offset=[bin_offset,n_baselines]
time_bin=Lonarr(n_baselines)
FOR ti=0L,nt-1 DO time_bin[bin_offset[ti]:bin_offset[ti+1]-1]=ti
FOR ti=0L,n_time_cut-1 DO BEGIN
    ti_cut=where(time_bin EQ time_cut_i[ti],n_ti_cut)
    IF n_ti_cut GT 0 THEN FOR pol_i=0,n_pol-1 DO (*vis_weight_ptr[pol_i])[*,ti_cut]=0
ENDFOR

flag_i=where(*vis_weight_ptr[0] LE 0,n_flag,ncomplement=n_unflag)
flag_i_new=where(xmin LT 0,n_flag_new)
IF n_flag_new GT 0 THEN FOR pol_i=0,n_pol-1 DO (*vis_weight_ptr[pol_i])[flag_i_new]=0
IF n_flag GT 0 THEN BEGIN
    xmin[flag_i]=-1
    ymin[flag_i]=-1
ENDIF

IF max(xmin)<max(ymin) LT 0 THEN BEGIN
    obs.n_vis=0
    RETURN
ENDIF
;match all visibilities that map from and to exactly the same pixels
bin_n=histogram(Temporary(xmin)+Temporary(ymin)*dimension,binsize=1,min=0) ;should miss any (xmin,ymin)=(-1,-1) from weights
bin_i=where(bin_n,n_bin_use);+bin_min
obs.n_vis=Total(bin_n)

obs.n_time_flag=Total(1L-(*obs.baseline_info).time_use)
obs.n_tile_flag=Total(1L-(*obs.baseline_info).tile_use)
obs.n_freq_flag=Total(1L-(*obs.baseline_info).freq_use)
END
