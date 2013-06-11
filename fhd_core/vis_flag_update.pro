PRO vis_flag_update,flag_arr,obs,psf,params,file_path_fhd,fi_use=fi_use,_Extra=extra
t0_0=Systime(1)
heap_gc

;extract information from the structures
dimension=Float(obs.dimension)
elements=Float(obs.elements)
kbinsize=obs.kpix
kx_span=kbinsize*dimension ;Units are # of wavelengths
ky_span=kx_span
min_baseline=obs.min_baseline
max_baseline=obs.max_baseline

freq_bin_i=obs.fbin_i
fi_use=where((*obs.baseline_info).freq_use)
freq_bin_i=freq_bin_i[fi_use]

frequency_array=(obs.freq)[fi_use]

psf_base=psf.base
psf_dim=Sqrt((Size(*psf_base[0],/dimension))[0])
psf_resolution=(Size(psf_base,/dimension))[2]

kx_arr=params.uu/kbinsize
ky_arr=params.vv/kbinsize
n_frequencies=N_Elements(frequency_array)
;psf_dim2=2*psf_dim

xcen=frequency_array#kx_arr
ycen=frequency_array#ky_arr
x_offset=Round((Ceil(xcen)-xcen)*psf_resolution) mod psf_resolution    
y_offset=Round((Ceil(ycen)-ycen)*psf_resolution) mod psf_resolution
xmin=Floor(Round(xcen+x_offset/psf_resolution+dimension/2.)-psf_dim/2.) 
ymin=Floor(Round(ycen+y_offset/psf_resolution+elements/2.)-psf_dim/2.) 
xmax=xmin+psf_dim-1
ymax=ymin+psf_dim-1

range_test_x_i=where((xmin LE 0) OR (xmax GE dimension-1),n_test_x)
range_test_y_i=where((ymin LE 0) OR (ymax GE elements-1),n_test_y)
xmax=(ymax=0)
IF n_test_x GT 0 THEN xmin[range_test_x_i]=(ymin[range_test_x_i]=-1)
IF n_test_y GT 0 THEN xmin[range_test_y_i]=(ymin[range_test_y_i]=-1)

;dist_test=Sqrt((xcen-dimension/2.)^2.+(ycen-elements/2.)^2.)
dist_test=Sqrt((xcen)^2.+(ycen)^2.)*kbinsize
flag_dist_i=where((dist_test LT min_baseline) OR (dist_test GT max_baseline),n_dist_flag)
IF n_dist_flag GT 0 THEN BEGIN
    xmin[flag_dist_i]=-1
    ymin[flag_dist_i]=-1
ENDIF

IF Keyword_Set(flag_arr) THEN BEGIN
    n_flag_dim=size(flag_arr,/n_dimension)
    IF n_flag_dim EQ 2 THEN flag_i=where(flag_arr LE 0,n_flag,ncomplement=n_unflag) $
        ELSE flag_i=where(flag_arr[0,*,*] LE 0,n_flag,ncomplement=n_unflag)
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