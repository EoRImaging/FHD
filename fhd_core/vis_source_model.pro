FUNCTION vis_source_model,source_list,obs,psf,params,flag_arr,model_uv_arr=model_uv_arr,file_path=file_path,$
    timing=timing,silent=silent,uv_mask=uv_mask

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
degpix=obs.degpix
kbinsize=obs.kpix
kx_span=kbinsize*dimension ;Units are # of wavelengths
ky_span=kx_span
icomp=Complex(0,1)

xvals=meshgrid(dimension,elements,1)-dimension/2
yvals=meshgrid(dimension,elements,2)-elements/2
IF ~Keyword_Set(uv_mask) THEN uv_mask=Fltarr(dimension,elements)+1
uv_i_use=where(uv_mask)
xvals=xvals[uv_i_use]
yvals=yvals[uv_i_use]

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
    model_uv_arr=Ptrarr(n_pol,/allocate)
    FOR pol_i=0,n_pol-1 DO *model_uv_arr[pol_i]=Complexarr(dimension,elements)
    model_uv_stks=Ptrarr(4,/allocate)
    flux_I=source_list.flux.I
    flux_Q=source_list.flux.Q
    flux_U=source_list.flux.U
    flux_V=source_list.flux.V
    x_vec=source_list.x
    y_vec=source_list.y
    *model_uv_stks[0]=source_dft(x_vec,y_vec,xvals,yvals,dimension=dimension,elements=elements,degpix=degpix,flux=flux_I)
    IF Total(flux_Q) EQ 0 THEN *model_uv_stks[1]=0. $
        ELSE *model_uv_stks[1]=source_dft(x_vec,y_vec,xvals,yvals,dimension=dimension,elements=elements,degpix=degpix,flux=flux_Q) 
    IF Total(flux_U) EQ 0 THEN *model_uv_stks[2]=0. $
        ELSE *model_uv_stks[2]=source_dft(x_vec,y_vec,xvals,yvals,dimension=dimension,elements=elements,degpix=degpix,flux=flux_U)
    IF Total(flux_V) EQ 0 THEN *model_uv_stks[3]=0. $
        ELSE *model_uv_stks[3]=source_dft(x_vec,y_vec,xvals,yvals,dimension=dimension,elements=elements,degpix=degpix,flux=flux_V)
    SWITCH n_pol OF
        4:(*model_uv_arr[3])[uv_i_use]+=(*model_uv_stks[2]-*model_uv_stks[3])/2.
        3:(*model_uv_arr[2])[uv_i_use]+=(*model_uv_stks[2]+*model_uv_stks[3])/2.
        2:(*model_uv_arr[1])[uv_i_use]+=(*model_uv_stks[0]-*model_uv_stks[1])/2.
        1:(*model_uv_arr[0])[uv_i_use]+=(*model_uv_stks[0]+*model_uv_stks[1])/2.
    ENDSWITCH
    t_model=Systime(1)-t_model0
    IF ~Keyword_Set(silent) THEN print,"DFT timing: ",strn(t_model)
ENDIF

vis_arr=Ptrarr(n_pol)

psf_base=psf.base
psf_residuals_n=psf.res_n
psf_residuals_i=psf.res_i
psf_residuals_val=psf.res_val
psf_dim=Sqrt((Size(*psf_base[0],/dimension))[0])
psf_resolution=(Size(psf_base,/dimension))[2]

FOR pol_i=0,n_pol-1 DO BEGIN
    vis_arr[pol_i]=visibility_degrid(*model_uv_arr[pol_i],flag_arr[pol_i],obs,psf,params,/silent,$
        timing=t_degrid0,polarization=pol_i,complex=complex,double=double)
    IF ~Keyword_Set(silent) THEN print,"Degridding timing: ",strn(t_degrid0)
ENDFOR

timing=Systime(1)-t0

RETURN,vis_arr
END