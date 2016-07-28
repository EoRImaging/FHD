;+
; :Description:
;    Grids visibilities to the uv plane using a high-resolution beam model. Written using the efficient notation of holo_mapfn_generate.pro
;
; :Params:
;    visibility_array - single polarization visibility data
;    
;    vis_weights - 
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
FUNCTION visibility_grid_GPU,visibility_array,vis_weights,obs,psf,params,weights=weights,$
    timing=timing,polarization=polarization,mapfn_recalculate=mapfn_recalculate,silent=silent,GPU_enable=GPU_enable    
t0=Systime(1)
heap_gc

pol_names=['xx','yy','xy','yx']

;extract information from the structures
dimension=obs.dimension
elements=obs.elements
kbinsize=obs.kpix
kx_span=kbinsize*dimension ;Units are # of wavelengths
ky_span=kx_span

freq_bin_i=obs.fbin_i
nfreq_bin=Max(freq_bin_i)+1
bin_offset=obs.bin_offset
frequency_array=obs.freq

psf_base=psf.base
psf_dim=(Size(*psf_base[0],/dimension))[0]
psf_resolution=(Size(psf_base,/dimension))[2]

vis_weight_switch=Keyword_Set(vis_weights)
kx_arr=params.uu/kbinsize
ky_arr=params.vv/kbinsize
baseline_i=params.baseline_arr
nbaselines=bin_offset[1]
n_samples=N_Elements(bin_offset)
n_frequencies=N_Elements(frequency_array)
n_freq_bin=N_Elements(freq_bin_i)
psf_dim2=2*psf_dim

vis_dimension=Float(nbaselines*n_samples)

image_uv=Complexarr(dimension,elements)
weights=fltarr(dimension,elements)

IF Keyword_Set(mapfn_recalculate) THEN BEGIN
    map_flag=1
    map_fn=Ptrarr(dimension,elements,/allocate)
    FOR i=0,dimension-1 DO FOR j=0,elements-1 DO *map_fn[i,j]=fltarr(psf_dim2,psf_dim2)
ENDIF ELSE map_flag=0

xcen=frequency_array#kx_arr
ycen=frequency_array#ky_arr
x_offset=Round((Ceil(xcen)-xcen)*psf_resolution) mod psf_resolution    
y_offset=Round((Ceil(ycen)-ycen)*psf_resolution) mod psf_resolution
xmin=Floor(Round(xcen+x_offset/psf_resolution+dimension/2.)-psf_dim/2.) 
ymin=Floor(Round(ycen+y_offset/psf_resolution+elements/2.)-psf_dim/2.) 

IF Keyword_Set(vis_weights) THEN BEGIN
    flag_i=where(vis_weights LE 0,n_flag)
    IF n_flag GT 0 THEN BEGIN
        xmin[flag_i]=-1
        ymin[flag_i]=-1
    ENDIF
ENDIF

;match all visibilities that map from and to exactly the same pixels
bin_n=histogram(xmin+ymin*dimension,binsize=1,reverse_indices=ri,min=0) ;should miss any (xmin,ymin)=(-1,-1) from weights
bin_i=where(bin_n,n_bin_use);+bin_min

time_check_interval=Ceil(n_bin_use/10.)
t1=0
t2=0
t3=0
t4=0
t5=0
t6=0
IF not Keyword_Set(silent) THEN Print,"Gridding "+pol_names[polarization]+" polarization time elapsed: estimated time remaining"
FOR bi=0L,n_bin_use-1 DO BEGIN
    IF Keyword_Set(time_check_interval) AND (bi mod time_check_interval EQ 0) AND (bi GT 0) AND (not Keyword_Set(silent)) THEN BEGIN
        bi_n0=Total(bin_n[bin_i[0:bi-1]])
        bi_n1=Total(bin_n[bin_i[bi:*]])
        
        print,Strcompress(String(format='("Baseline ",A," : ",I," : ",I)',Strn(bi),t1,(t1/bi_n0)*bi_n1))
        breakpoint=0
    ENDIF 
    t1_0=Systime(1)
    ;MUST use double precision!
    box_arr=fltarr(psf_dim*psf_dim,psf_dim*psf_dim)
    inds=ri[ri[bin_i[bi]]:ri[bin_i[bi]+1]-1]
    
    x_off1=x_offset[inds]
    y_off1=y_offset[inds]
    vis_box=visibility_array[inds]
        
    xmin_use=Min(xmin[inds]) ;should all be the same, but don't want an array
    ymin_use=Min(ymin[inds]) ;should all be the same, but don't want an array

    bt_i=Floor(inds/n_frequencies)
    base_i=baseline_i[bt_i]
    freq_i=(inds mod n_frequencies)
    fbin=freq_bin_i[freq_i]
    
    ;match all visibilities that use exactly the same beam model
;    bl_fr_i=fbin+base_i*nfreq_bin
;    bl_fr_n=histogram(bl_fr_i,binsize=1,reverse_indices=rbfi,omin=bl_fr_min)
;    bl_fr_ii=where(bl_fr_n,n_bl_fr_ii)
    
    vis_n=bin_n[bin_i[bi]]
    box_matrix=fltarr(vis_n,psf_dim*psf_dim)
    
    t3_0=Systime(1)
    t2+=t3_0-t1_0
    FOR ii=0L,vis_n-1 DO BEGIN
        psf_use=*psf_base[polarization,fbin[ii],x_off1[ii],y_off1[ii]]
        box_matrix[ii,*]=Reform(psf_use,psf_dim*psf_dim,/overwrite)        
    ENDFOR

    t4_0=Systime(1)
    t3+=t4_0-t3_0

    box_arr=vis_box#box_matrix
    IF Arg_present(weights) THEN box_arr_W=Replicate(1,bin_n[bin_i[bi]])#box_matrix
    t5_0=Systime(1)
    t4+=t5_0-t4_0
    
    image_uv[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=box_arr
    IF Arg_present(weights) THEN weights[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=box_arr_W
    
    t6_0=Systime(1)
    t5+=t6_0-t5_0
    IF map_flag THEN BEGIN
;        box_arr_map=matrix_multiply(box_matrix,box_matrix,/atranspose)
        gpu_box_matrix=gpuPutArr(box_matrix)
        box_arr_map=gpuGetArr(gpumatrix_multiply(gpu_box_matrix,gpu_box_matrix,/atranspose))
        FOR i=0,psf_dim-1 DO FOR j=0,psf_dim-1 DO BEGIN
            ij=i+j*psf_dim
            (*map_fn[xmin_use+i,ymin_use+j])[psf_dim-i:2*psf_dim-i-1,psf_dim-j:2*psf_dim-j-1]+=Reform(box_arr_map[*,ij],psf_dim,psf_dim)
        ENDFOR
        
        gpufree,gpu_box_matrix
    ENDIF
    t6_1=Systime(1)
    t6+=t6_1-t6_0
    t1+=t6_1-t1_0 
ENDFOR

IF map_flag THEN BEGIN
    map_fn=holo_mapfn_convert(map_fn,psf_dim=psf_dim,dimension=dimension)
    vis_path_default,data_directory,filename,file_path,obs=obs
    save,map_fn,filename=file_path+'_mapfn_'+pol_names[polarization]+'.sav'
ENDIF

image_uv_conj=Shift(Reverse(reverse(Conj(image_uv),1),2),1,1)
image_uv+=image_uv_conj
IF Arg_present(weights) THEN weights+=Shift(Reverse(reverse(weights,1),2),1,1)
normalization=dimension*elements
image_uv*=normalization ;account for FFT convention
;print,t1
IF not Keyword_Set(silent) THEN print,t1,t2,t3,t4,t5,t6
timing=Systime(1)-t0
RETURN,image_uv
END