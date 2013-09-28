PRO vis_flag_update,flag_ptr,obs,psf,params
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
freq_bin_i=freq_bin_i[fi_use]

;tile_cut_i=where(b_info.tile_use,n_tile_cut)
;FOR pol_i=0,n_pol-1 DO BEGIN
;    flag_arr=Temporary(*flag_ptr[pol_i])
;    
;    IF n_tile_cut GT 0 THEN BEGIN
;        tile_cut_i=(b_info.tile_names)[tile_cut_i]
;        tile_A=b_info.tile_A
;        tile_B=b_info.tile_B
;        
;        hist_A=histogram(tile_A,/bin,min=1,max=n_tile,reverse_ind=ria)
;        hist_B=histogram(tile_B,/bin,min=1,max=n_tile,reverse_ind=rib)
;        hist_AB=hist_A+hist_B
;        
;        hist_cut=histogram(tile_cut_i+1,min=1,max=n_tile)
;        
;        ti_cut=where(hist_AB*hist_cut,n_cut)
;        FOR ci=0,n_cut-1 DO BEGIN
;            IF hist_A[ti_cut[ci]] GT 0 THEN flag_arr[*,ria[ria[ti_cut[ci]]:ria[ti_cut[ci]+1]-1]]=0
;            IF hist_B[ti_cut[ci]] GT 0 THEN flag_arr[*,rib[rib[ti_cut[ci]]:rib[ti_cut[ci]+1]-1]]=0
;        ENDFOR
;        ria=0
;        rib=0
;    ENDIF
;        
;    IF n_freq_cut GT 0 THEN flag_arr[freq_cut_i,*]=0
;
;    *flag_ptr[pol_i]=Temporary(flag_arr)
;ENDFOR

frequency_array=b_info.freq
frequency_array=frequency_array[fi_use]

psf_base=psf.base
psf_dim=Sqrt((Size(*psf_base[0],/dimension))[0])
psf_resolution=(Size(psf_base,/dimension))[2]

kx_arr=params.uu/kbinsize
ky_arr=params.vv/kbinsize
n_frequencies=N_Elements(frequency_array)

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

dist_test=Sqrt((xcen)^2.+(ycen)^2.)*kbinsize
flag_dist_i=where((dist_test LT min_baseline) OR (dist_test GT max_baseline),n_dist_flag)
IF n_dist_flag GT 0 THEN BEGIN
    xmin[flag_dist_i]=-1
    ymin[flag_dist_i]=-1
ENDIF

IF Keyword_Set(flag_ptr) THEN BEGIN
    n_flag_dim=size(*flag_ptr[0],/n_dimension)
    flag_i=where(*flag_ptr[0] LE 0,n_flag,ncomplement=n_unflag)
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