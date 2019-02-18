FUNCTION visibility_count,obs,psf,params,vis_weight_ptr=vis_weight_ptr,file_path_fhd=file_path_fhd,$
    no_conjugate=no_conjugate,fill_model_vis=fill_model_vis,_Extra=extra

IF N_Elements(obs) EQ 0 THEN fhd_save_io,status_str,obs,var='obs',/restore,file_path_fhd=file_path_fhd,_Extra=extra
IF N_Elements(psf) EQ 0 THEN fhd_save_io,status_str,psf,var='psf',/restore,file_path_fhd=file_path_fhd,_Extra=extra
IF N_Elements(params) EQ 0 THEN fhd_save_io,status_str,params,var='params',/restore,file_path_fhd=file_path_fhd,_Extra=extra
IF Min(ptr_valid(vis_weight_ptr)) EQ 0 THEN fhd_save_io,status_str,vis_weight_ptr,var='vis_weights',/restore,file_path_fhd=file_path_fhd,_Extra=extra

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
IF Keyword_Set(fill_model_vis) THEN fi_use=lindgen(n_freq)
freq_bin_i=freq_bin_i[fi_use]

frequency_array=b_info.freq
frequency_array=frequency_array[fi_use]

tile_use=where(b_info.tile_use)+1
IF Keyword_Set(fill_model_vis) THEN tile_use=lindgen(n_tile)+1
bi_use=array_match(b_info.tile_A,b_info.tile_B,value_match=tile_use)
n_b_use=N_Elements(bi_use)
n_f_use=N_Elements(fi_use)

psf_dim=psf.dim
psf_resolution=psf.resolution

kx_arr=params.uu[bi_use]/kbinsize
ky_arr=params.vv[bi_use]/kbinsize

dist_test=Sqrt((kx_arr)^2.+(ky_arr)^2.)*kbinsize
dist_test=frequency_array#dist_test
flag_dist_i=where((dist_test LT min_baseline) OR (dist_test GT max_baseline),n_dist_flag)
dist_test=0

xcen=frequency_array#kx_arr
ycen=frequency_array#ky_arr

conj_i=where(ky_arr GT 0,n_conj)
IF n_conj GT 0 THEN BEGIN
    xcen[*,conj_i]=-xcen[*,conj_i]
    ycen[*,conj_i]=-ycen[*,conj_i]
ENDIF

xmin=Long(Floor(Temporary(xcen))+dimension/2-(psf_dim/2-1))
ymin=Long(Floor(Temporary(ycen))+elements/2-(psf_dim/2-1))

IF n_dist_flag GT 0 THEN BEGIN
    xmin[flag_dist_i]=-1
    ymin[flag_dist_i]=-1
    flag_dist_i=0
ENDIF

range_test_x_i=where((xmin LE 0) OR ((xmin+psf_dim-1) GE dimension-1),n_test_x)
IF n_test_x GT 0 THEN xmin[range_test_x_i]=(ymin[range_test_x_i]=-1)
range_test_x_i=0
range_test_y_i=where((ymin LE 0) OR ((ymin+psf_dim-1) GE elements-1),n_test_y)
IF n_test_y GT 0 THEN xmin[range_test_y_i]=(ymin[range_test_y_i]=-1)
range_test_y_i=0

IF Keyword_Set(vis_weight_ptr) THEN BEGIN
    flag_i=where(*vis_weight_ptr[0] LE 0,n_flag,ncomplement=n_unflag)
    IF Keyword_Set(fill_model_vis) THEN n_flag=0L
    IF n_flag GT 0 THEN BEGIN
        xmin[flag_i]=-1
        ymin[flag_i]=-1
    ENDIF
    IF ~Arg_present(vis_weight_ptr) THEN undefine_fhd,vis_weight_ptr
ENDIF
IF ~Arg_present(obs) THEN undefine_fhd,obs
IF ~Arg_present(params) THEN undefine_fhd,params
IF ~Arg_present(psf) THEN undefine_fhd,psf

;match all visibilities that map from and to exactly the same pixels
bin_n=histogram(xmin+ymin*dimension,binsize=1,reverse_indices=ri,min=0) ;should miss any (xmin,ymin)=(-1,-1) from weights
bin_i=where(bin_n,n_bin_use)

weights=fltarr(dimension,elements)
FOR bi=0L,n_bin_use-1 DO BEGIN
    inds=ri[ri[bin_i[bi]]:ri[bin_i[bi]+1]-1]
    ind0=inds[0]
    
    xmin_use=xmin[ind0] ;should all be the same, but don't want an array
    ymin_use=ymin[ind0]
    weights[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=bin_n[bin_i[bi]]
ENDFOR
    
IF ~Keyword_Set(no_conjugate) THEN BEGIN
    weights_mirror=Shift(Reverse(reverse(weights,1),2),1,1)
    weights=(weights+weights_mirror)/2.
ENDIF

fhd_save_io,status_str,weights,var='vis_count',file_path_fhd=file_path_fhd,_Extra=extra
RETURN,weights
END
