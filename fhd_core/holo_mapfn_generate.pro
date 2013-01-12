;+
; :Description:
;    Generates the Holographic Mapping function for a set of visibilities. The HMF is saved to disk.
;    
;    The HMF is stored in the row-indexed sparse matrix storage format from Numerical Recipes in C
;    
;    Apply the HMF to a uv-plane image with holo_mapfn_apply, which uses a modified version of sprsax
;
; :Params:
;    obs - structure containing details of the observation
;    
;    params - structure containing u and v coordinates of the baselines, in meters/c.
;    
;    psf - structure containing the high-resolution gridded beam model.
;
; :Keywords:
;    map_fn - if set, returns the HMF. 
;    
;    restore_last - set to load a previously generated HMF rather than recalculating it
;    
;    timing
;    
;    test
;    
;    new
;    
;    polarization
;    
;    flag_arr - [# of polarizations, # of frequencies, (# of baselines) * (# of time integrations)] array. Values LE 0 are considered flagged as bad data!
;    
;    file_identifier - default: polarization+'00_00'
;
; :Author: isullivan May 6, 2012
;-
PRO holo_mapfn_generate,obs,params,psf,map_fn=map_fn,restore_last=restore_last,timing=timing,test=test,new=new,$
    polarization=polarization,flag_arr=flag_arr,silent=silent
t0=Systime(1)
heap_gc
compile_opt idl2,strictarrsubs  

pol_names=['xx','yy','xy','yx']
vis_path_default,data_directory,filename,file_path,obs=obs
file_name_base='_mapfn_'+pol_names[polarization]
IF file_test(file_path+file_name_base+'.sav') EQ 0 THEN restore_last=0
IF Keyword_Set(restore_last) THEN BEGIN
    restore,file_path+file_name_base+'.sav'; ELSE restore,obs.file_path+file_name_base+'_old.sav'
    timing=Systime(1)-t0
    RETURN
ENDIF

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

flag_switch=Keyword_Set(flag_arr)
kx_arr=params.uu/kbinsize
ky_arr=params.vv/kbinsize
baseline_i=params.baseline_arr
nbaselines=bin_offset[1]
n_samples=N_Elements(bin_offset)
n_frequencies=N_Elements(frequency_array)
n_freq_bin=N_Elements(freq_bin_i)
psf_dim2=2*psf_dim

vis_dimension=Float(nbaselines*n_samples)

map_fn=Ptrarr(dimension,elements,/allocate)
FOR i=0,dimension-1 DO FOR j=0,elements-1 DO *map_fn[i,j]=dblarr(psf_dim2,psf_dim2)

xcen=frequency_array#kx_arr
ycen=frequency_array#ky_arr
x_offset=Round((Ceil(xcen)-xcen)*psf_resolution) mod psf_resolution    
y_offset=Round((Ceil(ycen)-ycen)*psf_resolution) mod psf_resolution
xmin=Floor(Round(xcen+x_offset/psf_resolution+dimension/2.)-psf_dim/2.) 
ymin=Floor(Round(ycen+y_offset/psf_resolution+elements/2.)-psf_dim/2.) 

IF Keyword_Set(flag_arr) THEN BEGIN
    flag_i=where(flag_arr LE 0,n_flag)
    IF n_flag GT 0 THEN BEGIN
        xmin[flag_i]=-1
        ymin[flag_i]=-1
    ENDIF
ENDIF

;match all visibilities that map from and to exactly the same pixels
bin_n=histogram(xmin+ymin*dimension,binsize=1,reverse_indices=ri,min=0) ;should miss any (xmin,ymin)=(-1,-1) from flags
bin_i=where(bin_n,n_bin_use);+bin_min

time_check_interval=Ceil(n_bin_use/20.)
t1=0
t2=0
t3=0
t4=0
t5=0
IF not Keyword_Set(silent) THEN Print,pol_names[polarization]+" time elapsed: estimated time remaining"
FOR bi=0L,n_bin_use-1 DO BEGIN
    IF Keyword_Set(time_check_interval) AND (bi mod time_check_interval EQ 0) AND (bi GT 0) AND (not Keyword_Set(silent)) THEN BEGIN
        bi_n0=Total(bin_n[bin_i[0:bi-1]])
        bi_n1=Total(bin_n[bin_i[bi:*]])
        
        print,Strcompress(String(format='("Baseline ",A," : ",I," : ",I)',Strn(Fix(bi)),t1,(t1/bi_n0)*bi_n1))
        breakpoint=0
    ENDIF 
    t1_0=Systime(1)
    ;MUST use double precision!
    box_arr=dblarr(psf_dim*psf_dim,psf_dim*psf_dim)
    inds=ri[ri[bin_i[bi]]:ri[bin_i[bi]+1]-1]
    
    x_off1=x_offset[inds]
    y_off1=y_offset[inds]
        
    xmin_use=Min(xmin[inds]) ;should all be the same, but don't want an array
    ymin_use=Min(ymin[inds]) ;should all be the same, but don't want an array

    bt_i=Floor(inds/n_frequencies)
    base_i=baseline_i[bt_i]
    freq_i=(inds mod n_frequencies)
    fbin=freq_bin_i[freq_i]
    
    ;match all visibilities that use exactly the same beam model
    bl_fr_i=fbin+base_i*nfreq_bin
    bl_fr_n=histogram(bl_fr_i,binsize=1,reverse_indices=rbfi,omin=bl_fr_min)
    bl_fr_ii=where(bl_fr_n,n_bl_fr_ii)
    
    off_arr=Ptrarr(2,n_bl_fr_ii,/allocate)
    box_nvis=Lonarr(n_bl_fr_ii) ;total number of UNIQUE visibilities contributing to the box
    FOR ii=0L,n_bl_fr_ii-1L DO BEGIN
        bl_fr_i_i=rbfi[rbfi[bl_fr_ii[ii]]:rbfi[bl_fr_ii[ii]+1]-1]
        fbin_use=fbin[bl_fr_i_i[0]]
        x_off2=x_off1[bl_fr_i_i]
        y_off2=y_off1[bl_fr_i_i]
        
        ;Finally, match all visibilities that share the same resolution element. There will not be many, but every bit of efficiency helps
        off_i=x_off2+y_off2*psf_resolution
        off_n=histogram(off_i,binsize=1.,reverse_indices=roffi,omin=off_min)
        off_ii=where(off_n,n_off_ii)
        box_nvis[ii]=n_off_ii
        
        off_arr2=Lonarr(2,n_off_ii)
        off_n_arr=off_n[off_ii]
        off_iii_arr=roffi[roffi[off_ii]]
        *off_arr[0,ii]=off_n_arr
        *off_arr[1,ii]=off_iii_arr 
        
    ENDFOR
    t3_0=Systime(1)
    t2+=t3_0-t1_0
    box_matrix_A=dblarr(psf_dim*psf_dim,Total(box_nvis))
    box_matrix_B=dblarr(Total(box_nvis),psf_dim*psf_dim)
    
    ind_start=[0,Total(box_nvis,/cumulative)]
    FOR ii=0L,n_bl_fr_ii-1L DO BEGIN
        off_n_arr=*off_arr[0,ii]
        off_iii_arr=*off_arr[1,ii]
        n_off_ii=box_nvis[ii]
        
        x_off_use=x_off2[off_iii_arr]
        y_off_use=y_off2[off_iii_arr]   
        FOR iii=0,n_off_ii-1 DO BEGIN   
            psf_use=*psf_base[polarization,fbin_use,x_off_use[iii],y_off_use[iii]]
            psf_use_A=Reform(psf_use,psf_dim*psf_dim,/overwrite)
            psf_use_B=off_n_arr[iii]*psf_use_A
            box_matrix_A[*,iii+ind_start[ii]]=psf_use_A
            box_matrix_B[iii+ind_start[ii],*]=psf_use_B
        ENDFOR
    ENDFOR
    t4_0=Systime(1)
    t3+=t4_0-t3_0
    box_arr=box_matrix_A#box_matrix_B
    t5_0=Systime(1)
    t4+=t5_0-t4_0

    FOR i=0,psf_dim-1 DO FOR j=0,psf_dim-1 DO BEGIN
        ij=i+j*psf_dim
        (*map_fn[xmin_use+i,ymin_use+j])[psf_dim-i:2*psf_dim-i-1,psf_dim-j:2*psf_dim-j-1]+=Reform(box_arr[*,ij],psf_dim,psf_dim)
    ENDFOR   
    t5_1=Systime(1)
    t5+=t5_1-t5_0
    t1+=t5_1-t1_0 
ENDFOR

map_fn=holo_mapfn_convert(map_fn,psf_dim=psf_dim,dimension=dimension)
save,map_fn,filename=file_path+file_name_base+'.sav'
IF not Keyword_Set(silent) THEN print,t1,t2,t3,t4,t5
timing=Systime(1)-t0

END