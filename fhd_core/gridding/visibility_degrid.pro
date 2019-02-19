FUNCTION visibility_degrid,image_uv,vis_weight_ptr,obs,psf,params,$
    timing=timing,polarization=polarization,silent=silent,$
    complex=complex,fill_model_visibilities=fill_model_visibilities,$
    vis_input_ptr=vis_input_ptr,spectral_model_uv_arr=spectral_model_uv_arr,$
    beam_mask_threshold=beam_mask_threshold,beam_per_baseline=beam_per_baseline,$
    uv_grid_phase_only=uv_grid_phase_only,_Extra=extra
t0=Systime(1)
heap_gc

pol_names=obs.pol_names
complex=psf.complex_flag
n_spectral=obs.degrid_spectral_terms
double_precision=0
IF Tag_Exist(obs, 'double_precision') THEN double_precision=obs.double_precision
IF Tag_exist(psf,'interpolate_kernel') THEN interp_flag=psf.interpolate_kernel ELSE interp_flag=0

;extract information from the structures
dimension=Long(obs.dimension)
elements=Long(obs.elements)
kbinsize=obs.kpix
kx_span=kbinsize*Float(dimension) ;Units are # of wavelengths
ky_span=kx_span
min_baseline=obs.min_baseline
max_baseline=obs.max_baseline

freq_bin_i=(*obs.baseline_info).fbin_i
nfreq_bin=psf.n_freq
bin_offset=(*obs.baseline_info).bin_offset
frequency_array=(*obs.baseline_info).freq
freq_delta=(frequency_array-obs.freq_center)/obs.freq_center

psf_dim=psf.dim
psf_resolution=Long(psf.resolution)

vis_weight_switch=Ptr_valid(vis_weight_ptr)
uu=params.uu
vv=params.vv
ww=params.ww
kx_arr=uu/kbinsize
ky_arr=vv/kbinsize
nbaselines=obs.nbaselines
n_samples=obs.n_time
n_freq_use=N_Elements(frequency_array)
n_freq=Long(obs.n_freq)
n_freq_bin=N_Elements(freq_bin_i)
psf_dim2=2*psf_dim
group_arr=reform(psf.id[polarization,freq_bin_i,*])
beam_arr=*psf.beam_ptr


if keyword_set(beam_per_baseline) then begin
    uv_grid_phase_only=1 ;w-terms have not been tested, thus they've been turned off for now
    psf_image_dim=(*psf.image_info).psf_image_dim
    psf_intermediate_res=(Ceil(Sqrt(psf_resolution)/2)*2.)<psf_resolution
    image_bot=-(psf_dim/2)*psf_intermediate_res+psf_image_dim/2
    image_top=(psf_dim*psf_resolution-1)-(psf_dim/2)*psf_intermediate_res+psf_image_dim/2

    n_tracked = l_m_n(obs, psf, l_mode=l_mode, m_mode=m_mode)
    if keyword_set(uv_grid_phase_only) then n_tracked[*]=0

    beam_int=FLTARR(n_freq)
    beam2_int=FLTARR(n_freq)
    n_grp_use=FLTARR(n_freq)
    primary_beam_area=ptr_new(Fltarr(n_freq))
    primary_beam_sq_area=ptr_new(Fltarr(n_freq))
endif

vis_dimension=nbaselines*n_samples
IF Keyword_Set(double_precision) THEN visibility_array=DComplexarr(n_freq,vis_dimension) $
    ELSE visibility_array=Complexarr(n_freq,vis_dimension) 

dist_test=Sqrt((kx_arr)^2.+(ky_arr)^2.)*kbinsize
dist_test=Float(frequency_array#dist_test)
flag_dist_i=where((dist_test LT min_baseline) OR (dist_test GT max_baseline),n_dist_flag)
dist_test=0

conj_i=where(ky_arr GT 0,n_conj)
conj_flag=intarr(N_Elements(ky_arr)) 
IF n_conj GT 0 THEN BEGIN
    conj_flag[conj_i]=1
    kx_arr[conj_i]=-kx_arr[conj_i]
    ky_arr[conj_i]=-ky_arr[conj_i]
    uu[conj_i]=-uu[conj_i]
    vv[conj_i]=-vv[conj_i]
    ww[conj_i]=-ww[conj_i]
ENDIF

xcen=Float(frequency_array#kx_arr)
ycen=Float(frequency_array#ky_arr)

x = (FINDGEN(dimension) - dimension/2.)*obs.kpix
y = (FINDGEN(dimension) - dimension/2.)*obs.kpix

x_offset=Fix(Floor((xcen-Floor(xcen))*psf_resolution) mod psf_resolution, type=12) ; type=12 is unsigned int
y_offset=Fix(Floor((ycen-Floor(ycen))*psf_resolution) mod psf_resolution, type=12) ; type=12 is unsigned int
dx_arr = (xcen-Floor(xcen))*psf_resolution - Floor((xcen-Floor(xcen))*psf_resolution)
dy_arr = (ycen-Floor(ycen))*psf_resolution - Floor((ycen-Floor(ycen))*psf_resolution)
dx0dy0_arr = (1-dx_arr)*(1-dy_arr)
dx0dy1_arr = (1-dx_arr)*dy_arr
dx1dy0_arr = dx_arr*(1-dy_arr)
dx1dy1_arr = Temporary(dx_arr) * Temporary(dy_arr)
xmin=Long(Floor(Temporary(xcen))+dimension/2-(psf_dim/2-1))
ymin=Long(Floor(Temporary(ycen))+elements/2-(psf_dim/2-1))

range_test_x_i=where((xmin LE 0) OR ((xmin+psf_dim-1) GE dimension-1),n_test_x)
range_test_y_i=where((ymin LE 0) OR ((ymin+psf_dim-1) GE elements-1),n_test_y)

IF n_test_x GT 0 THEN xmin[range_test_x_i]=(ymin[range_test_x_i]=-1)
IF n_test_y GT 0 THEN xmin[range_test_y_i]=(ymin[range_test_y_i]=-1)

;IF n_dist_flag GT 0 THEN BEGIN
;    xmin[flag_dist_i]=-1
;    ymin[flag_dist_i]=-1
;ENDIF

IF vis_weight_switch THEN BEGIN
    flag_i=where(*vis_weight_ptr LE 0,n_flag)
    IF Keyword_Set(fill_model_visibilities) THEN n_flag=0L
    IF n_flag GT 0 THEN BEGIN
        xmin[flag_i]=-1
        ymin[flag_i]=-1
    ENDIF
    flag_i=0
ENDIF

;match all visibilities that map from and to exactly the same pixels
bin_n=Long(histogram(xmin+ymin*dimension,binsize=1,reverse_indices=ri,min=0)) ;should miss any (xmin,ymin)=(-1,-1) from weights
bin_i=Long(where(bin_n,n_bin_use));+bin_min

ind_ref=Lindgen(max(bin_n))

CASE 1 OF
    Keyword_Set(complex) AND Keyword_Set(double_precision): init_arr=Dcomplexarr(psf_dim2,psf_dim2)
    Keyword_Set(double_precision): init_arr=Dblarr(psf_dim2,psf_dim2)
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
IF Keyword_Set(n_spectral) THEN BEGIN
    prefactor=Ptrarr(n_spectral)
    FOR s_i=0,n_spectral-1 DO prefactor[s_i]=Ptr_new(deriv_coefficients(s_i+1,/divide_factorial))
    box_arr_ptr=Ptrarr(n_spectral)
ENDIF

FOR bi=0L,n_bin_use-1 DO BEGIN
    t1_0=Systime(1)
    inds=ri[ri[bin_i[bi]]:ri[bin_i[bi]+1]-1]
    ind0=inds[0]
    
    x_off=x_offset[inds]
    y_off=y_offset[inds]
    dx1dy1 = dx1dy1_arr[inds]
    dx1dy0 = dx1dy0_arr[inds]
    dx0dy1 = dx0dy1_arr[inds]
    dx0dy0 = dx0dy0_arr[inds]
        
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
    
    IF interp_flag THEN n_xyf_bin=vis_n ELSE BEGIN
        xyf_i=(x_off+y_off*psf_resolution+fbin*psf_resolution^2.)*group_max+group_id
        xyf_si=Sort(xyf_i)
        xyf_i=xyf_i[xyf_si]
        xyf_ui=Uniq(xyf_i)
        n_xyf_bin=N_Elements(xyf_ui)
    ENDELSE
    
    IF (vis_n GT Ceil(1.1*n_xyf_bin)) AND ~keyword_set(beam_per_baseline) THEN BEGIN ;there might be a better selection criteria to determine which is most efficient
        ind_remap_flag=1
        
        inds=inds[xyf_si]
        inds_use=[xyf_si[xyf_ui]]
        
        freq_i=freq_i[inds_use]
        x_off=x_off[inds_use] 
        y_off=y_off[inds_use]
        fbin=fbin[inds_use]
        baseline_inds=baseline_inds[inds_use]
;        psf_conj_flag=psf_conj_flag[inds_use]
        
        IF n_xyf_bin EQ 1 THEN ind_remap=intarr(vis_n) ELSE BEGIN
            hist_inds_u=histogram(xyf_ui,/binsize,min=0,reverse_ind=ri_xyf)
            ind_remap=ind_ref[ri_xyf[0:n_elements(hist_inds_u)-1]-ri_xyf[0]]
        ENDELSE
        
        vis_n=Long64(n_xyf_bin)
    ENDIF ELSE BEGIN
        ind_remap_flag=0
        bt_index = inds / n_freq_use
    ENDELSE
    
    box_matrix=Make_array(psf_dim3,vis_n,type=arr_type) 
    
    box_arr=Reform(image_uv_use[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1],psf_dim3)
    t3_0=Systime(1)
    t2+=t3_0-t1_0

    ;Make the beams on the fly with corrective phases given the baseline location
    if keyword_set(beam_per_baseline) then begin
        box_matrix = grid_beam_per_baseline(psf, uu, vv, ww, l_mode, m_mode, n_tracked, frequency_array, x, y,$
          xmin_use, ymin_use, freq_i, bt_index, polarization, fbin, image_bot, image_top, psf_dim3,$ 
          box_matrix, vis_n, beam_int=beam_int, beam2_int=beam2_int, n_grp_use=n_grp_use,degrid_flag=1,_Extra=extra)
    endif else begin
        IF interp_flag THEN $
          FOR ii=0L,vis_n-1 DO box_matrix[psf_dim3*ii]=$
            interpolate_kernel(*beam_arr[polarization,fbin[ii],baseline_inds[ii]],x_offset=x_off[ii], $
                y_offset=y_off[ii],dx0dy0=dx0dy0[ii], dx1dy0=dx1dy0[ii], dx0dy1=dx0dy1[ii], dx1dy1=dx1dy1[ii]) $
        ELSE FOR ii=0L,vis_n-1 DO box_matrix[psf_dim3*ii]=*(*beam_arr[polarization,fbin[ii],baseline_inds[ii]])[x_off[ii],y_off[ii]] ;more efficient array subscript notation
    endelse

    t4_0=Systime(1)
    t3+=t4_0-t3_0
    IF Keyword_Set(n_spectral) THEN BEGIN
        vis_box=matrix_multiply(box_matrix,Temporary(box_arr),/atranspose)
        freq_term_arr=Rebin(transpose(freq_delta[freq_i]),psf_dim3,vis_n,/sample)
        FOR s_i=0,n_spectral-1 DO BEGIN
            ;s_i loop is over terms of the Taylor expansion, starting from the lowest-order term
            prefactor_use=*prefactor[s_i]
            box_matrix*=freq_term_arr
            box_arr_ptr[s_i]=Ptr_new(Reform((*spectral_model_uv_arr[s_i])[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1],psf_dim3))
            
            FOR s_i_i=0,s_i DO BEGIN
                ;s_i_i loop is over powers of the model x alpha^n, n=s_i_i+1
                degree=n_spectral
                box_arr=prefactor_use[s_i_i]*(*box_arr_ptr[s_i_i])
                vis_box+=matrix_multiply(box_matrix,Temporary(box_arr),/atranspose)
            ENDFOR
        ENDFOR
        ptr_free,box_arr_ptr
    ENDIF ELSE vis_box=matrix_multiply(Temporary(box_matrix),Temporary(box_arr),/atranspose) ;box_matrix#box_arr
    t5_0=Systime(1)
    t4+=t5_0-t4_0
    IF ind_remap_flag THEN vis_box=vis_box[ind_remap]
    visibility_array[inds]=vis_box
    
    t5_1=Systime(1)
    t5+=t5_1-t5_0
    t1+=t5_1-t1_0 
ENDFOR

if keyword_set(beam_per_baseline) then begin
    beam2_int*=weight_invert(n_grp_use)/kbinsize^2. ;factor of kbinsize^2 is FFT units normalization
    beam_int*=weight_invert(n_grp_use)/kbinsize^2.
    (*primary_beam_sq_area)=Float(beam2_int)
    (*primary_beam_area)=Float(beam_int)
    obs.primary_beam_area[polarization]=primary_beam_area
    obs.primary_beam_sq_area[polarization]=primary_beam_sq_area
endif

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
