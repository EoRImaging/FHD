PRO vis_flag_update,flag_ptr,obs,psf,params,xmin=xmin,xcen=xcen,ymin=ymin
t0_0=Systime(1)
heap_gc

;extract information from the structures
n_pol=obs.n_pol
n_tile=obs.n_tile
n_freq=obs.n_freq
dimension=Float(obs.dimension)
elements=Float(obs.elements)
kbinsize=obs.kpix
kx_span=kbinsize*dimension ;Units are # of wavelengths
ky_span=kx_span
min_baseline=obs.min_baseline
max_baseline=obs.max_baseline
b_info=*obs.baseline_info
freq_cut_i=where(b_info.freq_use,n_freq_cut)

freq_bin_i=b_info.fbin_i
fi_use=where(b_info.freq_use)
freq_bin_i=freq_bin_i;[fi_use]

frequency_array=b_info.freq
frequency_array=frequency_array;[fi_use]

psf_base=psf.base
psf_dim=Sqrt((Size(*psf_base[0],/dimension))[0])
psf_resolution=(Size(psf_base,/dimension))[2]

kx_arr=params.uu/kbinsize
ky_arr=params.vv/kbinsize
n_frequencies=N_Elements(frequency_array)

xcen=frequency_array#kx_arr
ycen=frequency_array#ky_arr

conj_i=where(ky_arr GT 0,n_conj)
IF n_conj GT 0 THEN BEGIN
    xcen[*,conj_i]=-xcen[*,conj_i]
    ycen[*,conj_i]=-ycen[*,conj_i]
ENDIF

xmin=Long(Floor(xcen)+dimension/2.-(psf_dim/2.-1))
ymin=Long(Floor(ycen)+elements/2.-(psf_dim/2.-1))

range_test_x_i=where((xmin LE 0) OR ((xmin+psf_dim-1) GE dimension-1),n_test_x)
IF n_test_x GT 0 THEN xmin[range_test_x_i]=(ymin[range_test_x_i]=-1)
range_test_x_i=0

range_test_y_i=where((ymin LE 0) OR ((ymin+psf_dim-1) GE elements-1),n_test_y)
IF n_test_y GT 0 THEN xmin[range_test_y_i]=(ymin[range_test_y_i]=-1)
range_test_y_i=0

dist_test=Sqrt((xcen)^2.+(ycen)^2.)*kbinsize
flag_dist_i=where((dist_test LT min_baseline) OR (dist_test GT max_baseline),n_dist_flag)
xcen=(ycen=(dist_test=0))
IF n_dist_flag GT 0 THEN BEGIN
    xmin[flag_dist_i]=-1
    ymin[flag_dist_i]=-1
    flag_dist_i=0
ENDIF

IF Keyword_Set(flag_ptr) THEN BEGIN
    n_flag_dim=size(*flag_ptr[0],/n_dimension)
    flag_i=where(*flag_ptr[0] LE 0,n_flag,ncomplement=n_unflag)
    flag_i_new=where(xmin LT 0,n_flag_new)
    IF n_flag_new GT 0 THEN FOR pol_i=0,n_pol-1 DO (*flag_ptr[pol_i])[flag_i_new]=-1
    IF n_flag GT 0 THEN BEGIN
        xmin[flag_i]=-1
        ymin[flag_i]=-1
    ENDIF
ENDIF

IF max(xmin)<max(ymin) LT 0 THEN BEGIN
    obs.n_vis=0
    RETURN
ENDIF
;match all visibilities that map from and to exactly the same pixels
bin_n=histogram(xmin+ymin*dimension,binsize=1,min=0) ;should miss any (xmin,ymin)=(-1,-1) from flags
bin_i=where(bin_n,n_bin_use);+bin_min
obs.n_vis=Total(bin_n)

END