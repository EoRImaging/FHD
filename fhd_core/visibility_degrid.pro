FUNCTION visibility_degrid,image_uv,flag_ptr,obs,psf,params,$
    timing=timing,polarization=polarization,silent=silent,$
    complex=complex,double=double,fill_model_vis=fill_model_vis,$
    vis_input_ptr=vis_input_ptr,spectral_model_uv_arr=spectral_model_uv_arr,_Extra=extra
t0=Systime(1)
heap_gc

pol_names=obs.pol_names
complex=psf.complex_flag
n_spectral=obs.degrid_spectral_terms

;extract information from the structures
dimension=Float(obs.dimension)
elements=Float(obs.elements)
kbinsize=obs.kpix
kx_span=kbinsize*dimension ;Units are # of wavelengths
ky_span=kx_span
min_baseline=obs.min_baseline
max_baseline=obs.max_baseline

freq_bin_i=(*obs.baseline_info).fbin_i
nfreq_bin=psf.n_freq
bin_offset=(*obs.baseline_info).bin_offset
frequency_array=(*obs.baseline_info).freq
freq_delta=(frequency_array-obs.freq_center)/obs.freq_center

psf_dim=psf.dim
psf_resolution=psf.resolution

flag_switch=Ptr_valid(flag_ptr)
kx_arr=params.uu/kbinsize
ky_arr=params.vv/kbinsize
;baseline_i=params.baseline_arr
nbaselines=obs.nbaselines
n_samples=obs.n_time
n_freq_use=N_Elements(frequency_array)
n_freq=Long(obs.n_freq)
n_freq_bin=N_Elements(freq_bin_i)
psf_dim2=2*psf_dim
group_arr=reform(psf.id[polarization,freq_bin_i,*])
beam_arr=*psf.beam_ptr

vis_dimension=Float(nbaselines*n_samples)
IF Keyword_Set(double) THEN visibility_array=DComplexarr(n_freq,vis_dimension) $
    ELSE visibility_array=Complexarr(n_freq,vis_dimension) 

dist_test=Sqrt((kx_arr)^2.+(ky_arr)^2.)*kbinsize
dist_test=frequency_array#dist_test
flag_dist_i=where((dist_test LT min_baseline) OR (dist_test GT max_baseline),n_dist_flag)
dist_test=0

conj_i=where(ky_arr GT 0,n_conj)
conj_flag=intarr(N_Elements(ky_arr)) 
IF n_conj GT 0 THEN BEGIN
    conj_flag[conj_i]=1
    kx_arr[conj_i]=-kx_arr[conj_i]
    ky_arr[conj_i]=-ky_arr[conj_i]
ENDIF

xcen=frequency_array#kx_arr
ycen=frequency_array#ky_arr
 
x_offset=Floor((xcen-Floor(xcen))*psf_resolution) mod psf_resolution    
y_offset=Floor((ycen-Floor(ycen))*psf_resolution) mod psf_resolution 
xmin=Long(Floor(Temporary(xcen))+dimension/2.-(psf_dim/2.-1))
ymin=Long(Floor(Temporary(ycen))+elements/2.-(psf_dim/2.-1))

range_test_x_i=where((xmin LE 0) OR ((xmin+psf_dim-1) GE dimension-1),n_test_x)
range_test_y_i=where((ymin LE 0) OR ((ymin+psf_dim-1) GE elements-1),n_test_y)

IF n_test_x GT 0 THEN xmin[range_test_x_i]=(ymin[range_test_x_i]=-1)
IF n_test_y GT 0 THEN xmin[range_test_y_i]=(ymin[range_test_y_i]=-1)

IF n_dist_flag GT 0 THEN BEGIN
    xmin[flag_dist_i]=-1
    ymin[flag_dist_i]=-1
ENDIF

IF flag_switch THEN BEGIN
    flag_i=where(*flag_ptr LE 0,n_flag)
    IF Keyword_Set(fill_model_vis) THEN n_flag=0L
    IF n_flag GT 0 THEN BEGIN
        xmin[flag_i]=-1
        ymin[flag_i]=-1
    ENDIF
    flag_i=0
ENDIF

;match all visibilities that map from and to exactly the same pixels
bin_n=histogram(xmin+ymin*dimension,binsize=1,reverse_indices=ri,min=0) ;should miss any (xmin,ymin)=(-1,-1) from flags
bin_i=where(bin_n,n_bin_use);+bin_min

ind_ref=indgen(max(bin_n))

CASE 1 OF
    Keyword_Set(complex) AND Keyword_Set(double): init_arr=Dcomplexarr(psf_dim2,psf_dim2)
    Keyword_Set(double): init_arr=Dblarr(psf_dim2,psf_dim2)
    Keyword_Set(complex): init_arr=Complexarr(psf_dim2,psf_dim2)
    ELSE: init_arr=Fltarr(psf_dim2,psf_dim2)
ENDCASE
arr_type=Size(init_arr,/type)

time_check_interval=Ceil(n_bin_use/10.)
t1=0
t2=0
t3=0
t4=0
t5=0
image_uv_use=image_uv
psf_dim3=psf_dim*psf_dim

;pdim=size(psf_base,/dimension)
;psf_base_dag=Ptrarr(pdim,/allocate)
;FOR pdim_i=0L,Product(pdim)-1 DO *psf_base_dag[pdim_i]=Conj(*psf_base[pdim_i])
prefactor=deriv_coefficients(n_spectral,/divide_factorial)
FOR bi=0L,n_bin_use-1 DO BEGIN
    t1_0=Systime(1)
    inds=ri[ri[bin_i[bi]]:ri[bin_i[bi]+1]-1]
    ind0=inds[0]
    
    x_off=x_offset[inds]
    y_off=y_offset[inds]
        
    xmin_use=xmin[ind0] ;should all be the same, but don't want an array
    ymin_use=ymin[ind0] ;should all be the same, but don't want an array

    freq_i=(inds mod n_freq_use)
    fbin=freq_bin_i[freq_i]
     
    vis_n=bin_n[bin_i[bi]]
    baseline_inds=Floor(inds/n_freq_use) mod nbaselines
    group_id=group_arr[inds]
    group_max=Max(group_id)+1
    
;    psf_conj_flag=intarr(vis_n)
;    IF n_conj GT 0 THEN BEGIN
;        bi_vals=Floor(inds/n_freq_use)
;        psf_conj_flag=conj_flag[bi_vals]
;    ENDIF 
    
    xyf_i=(x_off+y_off*psf_resolution+fbin*psf_resolution^2.)*group_max+group_id
    xyf_si=Sort(xyf_i)
    xyf_i=xyf_i[xyf_si]
    xyf_ui=Uniq(xyf_i)
    n_xyf_bin=N_Elements(xyf_ui)
    
    IF (vis_n GT 1.1*n_xyf_bin) AND (not Keyword_Set(n_spectral)) THEN BEGIN ;there might be a better selection criteria to determine which is most efficient
        ind_remap_flag=1
        inds=inds[xyf_si]
        freq_i=freq_i[xyf_si]
        
        inds_use=xyf_si[xyf_ui]
        x_off=x_off[inds_use] 
        y_off=y_off[inds_use]
        fbin=fbin[inds_use]
        baseline_inds=baseline_inds[inds_use]
;        psf_conj_flag=psf_conj_flag[inds_use]
        
        IF n_xyf_bin EQ 1 THEN ind_remap=intarr(vis_n) ELSE BEGIN
            hist_inds_u=histogram(xyf_ui,/binsize,min=0,reverse_ind=ri_xyf)
            ind_remap=ind_ref[ri_xyf[0:n_elements(hist_inds_u)-1]-ri_xyf[0]]
        ENDELSE
        
        vis_n=n_xyf_bin
    ENDIF ELSE $
        ind_remap_flag=0
    
    box_matrix=Make_array(psf_dim3,vis_n,type=arr_type) 
    
    box_arr=Reform(image_uv_use[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1],psf_dim3)
    t3_0=Systime(1)
    t2+=t3_0-t1_0
;    FOR ii=0L,vis_n-1 DO box_matrix[psf_dim3*ii]=*psf_base[polarization,fbin[ii],x_off[ii],y_off[ii]] ;more efficient array subscript notation
    FOR ii=0L,vis_n-1 DO box_matrix[psf_dim3*ii]=*(*beam_arr[polarization,fbin[ii],baseline_inds[ii]])[x_off[ii],y_off[ii]] ;more efficient array subscript notation
;    FOR ii=0L,vis_n-1 DO box_matrix[psf_dim3*ii]=psf_conj_flag[ii] ? $
;        *psf_base_dag[polarization,fbin[ii],x_off[ii],y_off[ii]]:*psf_base[polarization,fbin[ii],x_off[ii],y_off[ii]]
    
    t4_0=Systime(1)
    t3+=t4_0-t3_0
    IF Keyword_Set(n_spectral) THEN BEGIN
        vis_box=matrix_multiply(box_matrix,Temporary(box_arr),/atranspose)
        freq_term_arr=Rebin(transpose(freq_delta[freq_i]),psf_dim3,vis_n,/sample)
        FOR s_i=0,n_spectral-1 DO BEGIN
            box_arr=prefactor[s_i]*Reform((*spectral_model_uv_arr[s_i])[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1],psf_dim3)
            box_matrix*=freq_term_arr
            vis_box+=matrix_multiply(box_matrix,Temporary(box_arr),/atranspose)
        ENDFOR
    ENDIF ELSE vis_box=matrix_multiply(Temporary(box_matrix),Temporary(box_arr),/atranspose) ;box_matrix#box_arr
    t5_0=Systime(1)
    t4+=t5_0-t4_0
    IF ind_remap_flag THEN vis_box=vis_box[ind_remap]
    visibility_array[inds]=vis_box
    
    t5_1=Systime(1)
    t5+=t5_1-t5_0
    t1+=t5_1-t1_0 
ENDFOR

x_offset=(y_offset=0)
xmin=(ymin=0)
bin_n=0
IF n_conj GT 0 THEN BEGIN
    visibility_array[*,conj_i]=Conj(visibility_array[*,conj_i])
ENDIF

IF Ptr_valid(vis_input_ptr) THEN IF N_Elements(*vis_input_ptr) EQ N_Elements(visibility_array) THEN BEGIN
    vis_return=vis_input_ptr
    *vis_return+=Temporary(visibility_array)
ENDIF
IF ~Ptr_valid(vis_return) THEN vis_return=Ptr_new(visibility_array,/no_copy)

timing=Systime(1)-t0
IF not Keyword_Set(silent) THEN print,timing,t1,t2,t3,t4,t5
RETURN,vis_return
END