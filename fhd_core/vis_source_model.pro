FUNCTION vis_source_model,source_list,obs,psf,params,flag_arr,model_uv_arr=model_uv_arr,file_path=file_path,timing=timing

t0=Systime(1)

flags_filepath=file_path+'_flags.sav'
;vis_filepath=file_path+'_vis.sav'
params_filepath=file_path+'_params.sav'
psf_filepath=file_path+'_beams.sav'
obs_filepath=file_path+'_obs.sav'

SWITCH N_Params() OF
    1:restore,obs_filepath
    2:restore,psf_filepath
    3:restore,params_filepath
    4:restore,flags_filepath
    ELSE:
ENDSWITCH

heap_gc

IF Keyword_Set(flag_arr) THEN flag_switch=1 ELSE flag_switch=0

pol_names=['xx','yy','xy','yx']

;extract information from the structures
n_pol=obs.n_pol
dimension=obs.dimension
elements=obs.elements
kbinsize=obs.kpix
kx_span=kbinsize*dimension ;Units are # of wavelengths
ky_span=kx_span
icomp=Complex(0,1)

xvals=meshgrid(dimension,elements,1)-dimension/2
yvals=meshgrid(dimension,elements,2)-elements/2

freq_bin_i=obs.fbin_i
nfreq_bin=Max(freq_bin_i)+1
bin_offset=(*obs.baseline_info).bin_offset
frequency_array=obs.freq

kx_arr=params.uu/kbinsize
ky_arr=params.vv/kbinsize
baseline_i=params.baseline_arr
nbaselines=bin_offset[1]
n_samples=N_Elements(bin_offset)
n_freq=N_Elements(frequency_array)
n_freq_bin=N_Elements(freq_bin_i)

vis_dimension=Float(nbaselines*n_samples)
n_sources=N_Elements(source_list)

;xcen=frequency_array#kx_arr
;ycen=frequency_array#ky_arr

IF N_Elements(model_uv_arr) EQ 0 THEN BEGIN
    t_model0=Systime(1)
    beam_corr_src=fltarr(n_pol,n_sources)
    FOR pol_i=0,n_pol-1 DO BEGIN
        beam_single=beam_image(psf,pol_i=pol_i,dimension=obs.dimension)
        beam_corr_single=weight_invert(beam_single)
        beam_corr_src[pol_i,*]=beam_corr_single[source_list.x,source_list.y]
    ENDFOR
    
    model_uv_arr=Ptrarr(n_pol,/allocate)
    FOR pol_i=0,n_pol-1 DO *model_uv_arr[pol_i]=Complexarr(dimension,elements)
    
    FOR si=0.,n_sources-1 DO BEGIN
        source_uv=Exp(icomp*(2.*!Pi/dimension)*((source_list[si].x-dimension/2.)*xvals+(source_list[si].y-elements/2.)*yvals))
        FOR pol_i=0,n_pol-1 DO *model_uv_arr[pol_i]+=source_list[si].flux.(pol_i)*beam_corr_src[pol_i,si]*source_uv
        
    ENDFOR
    t_model=Systime(1)-t_model0
    print,"DFT timing: ",strn(t_model)
ENDIF

vis_arr=Ptrarr(n_pol,/allocate)
n_sources=(size(source_list,/dimension)) ;??

psf_base=psf.base
psf_residuals_n=psf.res_n
psf_residuals_i=psf.res_i
psf_residuals_val=psf.res_val
psf_dim=(Size(*psf_base[0],/dimension))[0]
psf_resolution=(Size(psf_base,/dimension))[2]

FOR pol_i=0,n_pol-1 DO BEGIN
;    vis_single=Complexarr(n_freq,vis_dimension)
;    FOR bi=0L,nbaselines-1 DO BEGIN
;        baseline_i=bi mod nbaselines
;        IF Keyword_Set(flag_switch) THEN IF Total((*flag_arr[pol_i])[*,bi+bin_offset]) EQ 0 THEN CONTINUE
;        IF kx_arr[bi] EQ 0 THEN CONTINUE ;skip the auto-correlations
;        FOR fi=0L,n_freq-1 DO BEGIN
;            freq_i=freq_bin_i[fi]
;            IF Keyword_Set(flag_switch) THEN IF Total((*flag_arr[pol_i])[fi,bi+bin_offset]) EQ 0 THEN CONTINUE
;            freq=frequency_array[fi]
;            
;            FOR ti=0L,n_samples-1 DO BEGIN
;                bi_use=bi+bin_offset[ti]
;                IF Keyword_Set(flag_switch) THEN IF (*flag_arr[pol_i])[fi,bi_use] EQ 0 THEN CONTINUE
;                vis_single[fi,bi_use]=visibility_psf(psf_base, psf_residuals_n, psf_residuals_i, psf_residuals_val,$
;                    freq_i=freq_i,baseline_i=baseline_i,xcenter=kx_arr[bi_use]*freq,$
;                    ycenter=ky_arr[bi_use]*freq,image_sample=*model_uv_arr[pol_i],dimension=dimension,$
;                    polarization=pol_i,psf_dim=psf_dim,psf_resolution=psf_resolution)
;            ENDFOR
;        ENDFOR
;    ENDFOR
    *vis_arr[pol_i]=visibility_degrid(*model_uv_arr[pol_i],*flag_arr[pol_i],obs,psf,params,$
        timing=t_degrid0,polarization=pol_i,silent=silent,complex=complex,double=double,_Extra=extra)
    print,"Degridding timing: ",strn(t_degrid0)
ENDFOR

timing=Systime(1)-t0

RETURN,vis_arr
END