PRO sparse_uv_flag,obs,psf,params,flag_arr,flag_sparse_uv_coverage=flag_sparse_uv_coverage
t_sparse=Systime(1)
n_pol=obs.n_pol
n_freq=obs.n_freq
dimension=obs.dimension
elements=obs.elements
psf_dim=psf.dim
psf_resolution=psf.resolution
freq=(*obs.baseline_info).freq
xcen=freq#(params.uu/obs.kpix)
ycen=freq#(params.vv/obs.kpix)
x_offset=Floor((xcen-Floor(xcen))*psf_resolution) mod psf_resolution    
y_offset=Floor((ycen-Floor(ycen))*psf_resolution) mod psf_resolution 
xmin=Long(Floor(Temporary(xcen))+dimension/2.-(psf_dim/2.-1))
ymin=Long(Floor(Temporary(ycen))+elements/2.-(psf_dim/2.-1))
flag_base=Fix(0>*flag_arr[0]<1)
IF n_pol GE 2 THEN flag_base*=Fix(0>*flag_arr[1]<1)
flag_i=where(flag_base LE 0,n_flag)
IF n_flag GT 0 THEN xmin[flag_i]=-1
IF n_flag GT 0 THEN ymin[flag_i]=-1
bin_n=histogram(xmin+ymin*dimension,binsize=1,reverse_indices=ri,min=0)
bin_i=where(bin_n,n_bin_use)
count=Lonarr(dimension,elements)
FOR bi=0L,n_bin_use-1 DO BEGIN
    inds=ri[ri[bin_i[bi]]:ri[bin_i[bi]+1]-1]
    ind0=inds[0]
    
    xmin_use=xmin[ind0] ;should all be the same, but don't want an array
    ymin_use=ymin[ind0]
    count[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=bin_n[bin_i[bi]]
ENDFOR
cut_i=where((count GT 0) AND (count LE flag_sparse_uv_coverage),n_cut)
n_cut_total=0L
cx_arr=cut_i mod dimension
cy_arr=Floor(cut_i/dimension)
FOR c_i=0L,n_cut-1 DO BEGIN
    cx=cx_arr[c_i]
    cy=cy_arr[c_i]
    bi_cut=where(((cx-xmin) LE psf_dim) AND ((cy-ymin) LE psf_dim) AND (flag_base GT 0),n_add_cut)
    IF n_add_cut GT 0 THEN BEGIN
        n_cut_total+=n_add_cut
        flag_base[bi_cut]=0
    ENDIF
ENDFOR
IF n_cut_total THEN FOR pol_i=0,n_pol-1 DO *flag_arr[pol_i]=flag_base
t_sparse=Systime(1)-t_sparse
print,'Flagging: '+Strn(n_cut_total)+' visibilities flagged because of sparse uv coverage (threshold='$
    +Strn(flag_sparse_uv_coverage)+' baselines)'
print,t_sparse
END