;+
; :Description:
;    Grids visibilities to the uv plane using a high-resolution beam model. Written using the efficient notation of holo_mapfn_generate.pro
;
; :Params:
;    visibility_array - single polarization visibility data
;    
;    flag_arr - 
;    
;    obs - structure containing details of the observation
;    
;    params - structure containing u and v coordinates of the baselines, in meters/c.
;    
;    psf - structure containing the high-resolution gridded beam model.
;
; :Keywords:
;    weights - if set, contains the weights generated from gridding visibilities with value 1
;    
;    timing
;    
;    polarization - polarization index used to pick the correct beam
;
; :Author: Ian Sullivan
;-
FUNCTION visibility_grid,visibility_array,flag_arr,obs,psf,params,file_path_fhd,weights=weights,variance=variance,$
    timing=timing,polarization=polarization,mapfn_recalculate=mapfn_recalculate,silent=silent,$
    GPU_enable=GPU_enable,complex=complex,double=double,time_arr=time_arr,fi_use=fi_use,preserve_visibilities=preserve_visibilities,$
    visibility_list=visibility_list,image_list=image_list,n_vis=n_vis,no_conjugate=no_conjugate,_Extra=extra
t0_0=Systime(1)
heap_gc
IF N_Elements(complex) EQ 0 THEN complex=1

pol_names=['xx','yy','xy','yx']

;extract information from the structures
dimension=Float(obs.dimension)
elements=Float(obs.elements)
kbinsize=obs.kpix
kx_span=kbinsize*dimension ;Units are # of wavelengths
ky_span=kx_span
min_baseline=obs.min_baseline
max_baseline=obs.max_baseline

freq_bin_i=obs.fbin_i
IF N_Elements(fi_use) EQ 0 THEN fi_use=where((*obs.baseline_info).freq_use)
freq_bin_i=freq_bin_i[fi_use]
IF Keyword_Set(preserve_visibilities) THEN vis_arr_use=visibility_array[fi_use,*] ELSE vis_arr_use=Temporary(visibility_array[fi_use,*])

;n_freq_bin=N_Elements(freq_bin_i)
;
;nfreq_bin=Max(freq_bin_i)+1
;bin_offset=(*obs.baseline_info).bin_offset
frequency_array=(obs.freq)[fi_use]

psf_base=psf.base
psf_dim=Sqrt((Size(*psf_base[0],/dimension))[0])
psf_resolution=(Size(psf_base,/dimension))[2]

flag_switch=Keyword_Set(flag_arr)
kx_arr=params.uu/kbinsize
ky_arr=params.vv/kbinsize

;baseline_i=params.baseline_arr
;nbaselines=bin_offset[1]
;n_samples=N_Elements(bin_offset)
n_frequencies=N_Elements(frequency_array)
psf_dim2=2*psf_dim

;vis_dimension=Float(nbaselines*n_samples)

image_uv=Complexarr(dimension,elements)
weights=Complexarr(dimension,elements)
variance=Fltarr(dimension,elements)

;IF Keyword_Set(visibility_list) THEN BEGIN
;    n_additional=N_Elements(visibility_list)
;    image_list=Ptrarr(size(visibility_list,/dimension))
;    FOR vi=0L,n_additional-1 DO image_list[vi]=Ptr_new(Complexarr(dimension,elements))
;ENDIF ELSE n_additional=0

IF Keyword_Set(mapfn_recalculate) THEN BEGIN
    map_flag=1
    map_fn=Ptrarr(dimension,elements)
ENDIF ELSE map_flag=0

xcen=frequency_array#kx_arr
ycen=frequency_array#ky_arr

;conj_i=where(ycen+xcen GT 0,n_conj) 
conj_i=where(ky_arr LT 0,n_conj)
IF n_conj GT 0 THEN BEGIN
    xcen[*,conj_i]=-xcen[*,conj_i]
    ycen[*,conj_i]=-ycen[*,conj_i]
    vis_arr_use[*,conj_i]=Conj(vis_arr_use[*,conj_i])
ENDIF

x_offset=Round((Ceil(xcen)-xcen)*psf_resolution) mod psf_resolution    
y_offset=Round((Ceil(ycen)-ycen)*psf_resolution) mod psf_resolution
xmin=Floor(Round(xcen+x_offset/psf_resolution+dimension/2.)-psf_dim/2.)+1 
ymin=Floor(Round(ycen+y_offset/psf_resolution+elements/2.)-psf_dim/2.)+1 
xmax=xmin+psf_dim-1
ymax=ymin+psf_dim-1

range_test_x_i=where((xmin LT 0) OR (xmax GE dimension),n_test_x)
range_test_y_i=where((ymin LT 0) OR (ymax GE elements),n_test_y)
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
;    IF Keyword_Set(preserve_visibilities) THEN flag_arr_use=flag_arr[fi_use,*] ELSE flag_arr_use=Temporary(flag_arr[fi_use,*])
    
    flag_i=where((flag_arr[fi_use,*]) LE 0,n_flag,ncomplement=n_unflag)
    IF n_unflag EQ 0 THEN BEGIN
        timing=Systime(1)-t0_0
        image_uv=Complexarr(dimension,elements)
        RETURN,image_uv
    ENDIF
    IF n_flag GT 0 THEN BEGIN
        xmin[flag_i]=-1
        ymin[flag_i]=-1
    ENDIF
    
    IF n_conj GT 0 THEN BEGIN
        xmin[*,conj_i]=-1
        ymin[*,conj_i]=-1
    ENDIF
ENDIF

;match all visibilities that map from and to exactly the same pixels
bin_n=histogram(xmin+ymin*dimension,binsize=1,reverse_indices=ri,min=0) ;should miss any (xmin,ymin)=(-1,-1) from flags
bin_i=where(bin_n,n_bin_use);+bin_min
;obs.n_vis=Total(bin_n)

;vis_density=Float(Total(bin_n))/(dimension*elements)
n_vis=Float(Total(bin_n))
;vis_density=n_vis*(obs.degpix*!DtoR)^2.

index_arr=Lindgen(dimension,elements)
n_psf_dim=N_Elements(psf_base)
CASE 1 OF
    Keyword_Set(complex) AND Keyword_Set(double): BEGIN
        init_arr=Dcomplexarr(psf_dim2,psf_dim2)
        FOR i=0.,n_psf_dim-1 DO *psf_base[i]=Dcomplex(*psf_base[i])
    END
    Keyword_Set(double): BEGIN
        init_arr=Dblarr(psf_dim2,psf_dim2)
        FOR i=0.,n_psf_dim-1 DO *psf_base[i]=Double(Abs(*psf_base[i]))
    END
    Keyword_Set(complex): BEGIN
        init_arr=Complexarr(psf_dim2,psf_dim2)
        FOR i=0.,n_psf_dim-1 DO *psf_base[i]=Complex(*psf_base[i])
    END
    ELSE: BEGIN
        init_arr=Fltarr(psf_dim2,psf_dim2)
        FOR i=0.,n_psf_dim-1 DO *psf_base[i]=Float(abs(*psf_base[i]))
    ENDELSE
ENDCASE
arr_type=Size(init_arr,/type)

;initialize ONLY those elements of the map_fn array that will receive data
IF map_flag THEN BEGIN
    FOR bi=0L,n_bin_use-1 DO BEGIN
        xmin1=xmin[ri[ri[bin_i[bi]]]]
        ymin1=ymin[ri[ri[bin_i[bi]]]]
        ptr_test=Ptr_valid(map_fn[xmin1:xmin1+psf_dim-1,ymin1:ymin1+psf_dim-1])
        inds_init=where(ptr_test EQ 0,nzero)
        IF nzero EQ 0 THEN CONTINUE
        inds_init=(index_arr[xmin1:xmin1+psf_dim-1,ymin1:ymin1+psf_dim-1])[inds_init]
        FOR ii=0,nzero-1 DO map_fn[inds_init[ii]]=Ptr_new(init_arr)
    ENDFOR
ENDIF

;;pre-compute indices:
;ind_map_base=lindgen(psf_dim^2.,psf_dim^2.)
;ind_map=Ptrarr(psf_dim,psf_dim)
;FOR i=0,psf_dim-1 DO FOR j=0,psf_dim-1 DO BEGIN
;    ij=i+j*psf_dim
;    ind_map[i,j]=Ptr_new(Reform(ind_map_base[*,ij],psf_dim,psf_dim))
;ENDFOR

t0=Systime(1)-t0_0
time_check_interval=Ceil(n_bin_use/10.)
t1=0
t2=0
t3=0
t4=0
t5=0
t6=0
t6a=0
t6b=0
FOR bi=0L,n_bin_use-1 DO BEGIN
    t1_0=Systime(1)
;    ;MUST use double precision!
;    box_arr=Make_array(psf_dim*psf_dim,psf_dim*psf_dim,type=arr_type)
    inds=ri[ri[bin_i[bi]]:ri[bin_i[bi]+1]-1]
    
    x_off1=x_offset[inds]
    y_off1=y_offset[inds]
    vis_box=vis_arr_use[inds]
        
    xmin_use=Min(xmin[inds]) ;should all be the same, but don't want an array
    ymin_use=Min(ymin[inds]) ;should all be the same, but don't want an array

;    bt_i=Floor(inds/n_frequencies)
;    base_i=baseline_i[bt_i]
    freq_i=(inds mod n_frequencies)
    fbin=freq_bin_i[freq_i]
    
    ;match all visibilities that use exactly the same beam model
;    bl_fr_i=fbin+base_i*nfreq_bin
;    bl_fr_n=histogram(bl_fr_i,binsize=1,reverse_indices=rbfi,omin=bl_fr_min)
;    bl_fr_ii=where(bl_fr_n,n_bl_fr_ii)
    
    vis_n=bin_n[bin_i[bi]]
    box_matrix=Make_array(vis_n,psf_dim*psf_dim,type=arr_type)
    
    t3_0=Systime(1)
    t2+=t3_0-t1_0
    FOR ii=0L,vis_n-1 DO box_matrix[ii,*]=*psf_base[polarization,fbin[ii],x_off1[ii],y_off1[ii]]
    box_matrix_dag=Conj(box_matrix)

    t4_0=Systime(1)
    t3+=t4_0-t3_0
    box_arr=vis_box#box_matrix_dag/n_vis
    t5_0=Systime(1)
    t4+=t5_0-t4_0
    
    image_uv[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=box_arr
;    FOR addv_i=0L,n_additional-1 DO (*image_list[addv_i])[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]$
;        +=(*visibility_list[addv_i])[inds]#box_matrix_dag/n_vis
    
;    IF Arg_present(weights) THEN weights[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=$
;        Replicate(1./n_vis,vis_n)#Abs(box_matrix)
    IF Arg_present(weights) THEN weights[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=$
        Replicate(1./n_vis,vis_n)#box_matrix_dag
    IF Arg_present(variance) THEN variance[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=$
        Replicate(1./n_vis,vis_n)#(Abs(box_matrix)^2.)
    
    t6_0=Systime(1)
    t5+=t6_0-t5_0
    IF map_flag THEN BEGIN
        t6a_0=Systime(1)
        box_arr_map=matrix_multiply(box_matrix,box_matrix_dag,/atranspose)/n_vis
        ;alternate indexing approach. Does not appear to be any faster.
;        FOR i=0,psf_dim-1 DO FOR j=0,psf_dim-1 DO $
;            (*map_fn[xmin_use+i,ymin_use+j])[psf_dim-i:2*psf_dim-i-1,psf_dim-j:2*psf_dim-j-1]+=box_arr_map[*ind_map[i,j]]
        t6b_0=Systime(1)
        t6a+=t6b_0-t6a_0
        FOR i=0,psf_dim-1 DO FOR j=0,psf_dim-1 DO BEGIN
            ij=i+j*psf_dim
            (*map_fn[xmin_use+i,ymin_use+j])[psf_dim-i:2*psf_dim-i-1,psf_dim-j:2*psf_dim-j-1]+=Reform(box_arr_map[*,ij],psf_dim,psf_dim)
        ENDFOR
        t6b+=Systime(1)-t6b_0
    ENDIF
    t6_1=Systime(1)
    t6+=t6_1-t6_0
    t1+=t6_1-t1_0 
ENDFOR

t7_0=Systime(1)
IF map_flag THEN BEGIN
    map_fn=holo_mapfn_convert(map_fn,psf_dim=psf_dim,dimension=dimension)
    save,map_fn,filename=file_path_fhd+'_mapfn_'+pol_names[polarization]+'.sav'
ENDIF
t7=Systime(1)-t7_0

IF ~Keyword_Set(no_conjugate) THEN BEGIN
    image_uv_conj=Shift(Reverse(reverse(Conj(image_uv),1),2),1,1)
    image_uv=(image_uv+image_uv_conj)/2.
    ;FOR addv_i=0L,n_additional-1 DO *image_list[addv_i]=(*image_list[addv_i]+Shift(Reverse(reverse(Conj(*image_list[addv_i]),1),2),1,1))/2.
    
    IF Arg_present(weights) THEN BEGIN
        weights_conj=Shift(Reverse(reverse(Conj(weights),1),2),1,1)
        weights=(weights+weights_conj)/2.
    ENDIF
    IF Arg_present(variance) THEN BEGIN
        variance_mirror=Shift(Reverse(reverse(variance,1),2),1,1)
        variance=(variance+variance_mirror)/2.
    ENDIF
ENDIF
;normalization=dimension*elements
;image_uv*=normalization ;account for FFT convention

IF ~Keyword_Set(silent) THEN print,t0,t1,t2,t3,t4,t5,t6,t7
IF ~Keyword_Set(silent) THEN print,t6a,t6b
time_arr=[t0,t1,t2,t3,t4,t5,t6,t7]
timing=Systime(1)-t0_0
RETURN,image_uv
END