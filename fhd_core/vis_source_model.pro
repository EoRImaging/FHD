FUNCTION vis_source_model,source_list,obs,psf,params,flag_ptr,cal,model_uv_arr=model_uv_arr,file_path_fhd=file_path_fhd,$
    timing=timing,silent=silent,uv_mask=uv_mask,galaxy_calibrate=galaxy_calibrate,error=error,$
    fill_model_vis=fill_model_vis,_Extra=extra

t0=Systime(1)
IF N_Elements(error) EQ 0 THEN error=0
IF N_Elements(file_path_fhd) EQ 0 THEN file_path_fhd=''
flags_filepath=file_path_fhd+'_flags.sav'
;vis_filepath=file_path_fhd+'_vis.sav'
params_filepath=file_path_fhd+'_params.sav'
psf_filepath=file_path_fhd+'_beams.sav'
obs_filepath=file_path_fhd+'_obs.sav'
IF N_Elements(silent) EQ 0 THEN silent=1

IF N_Elements(obs) EQ 0 THEN obs=getvar_savefile(obs_filepath,'obs')
IF N_Elements(psf) EQ 0 THEN psf=getvar_savefile(psf_filepath,'psf')
IF N_Elements(params) EQ 0 THEN params=getvar_savefile(params_filepath,'params')
IF N_Elements(flag_ptr) EQ 0 THEN flag_ptr=getvar_savefile(flags_filepath,'flag_arr')

heap_gc

;IF Keyword_Set(flag_ptr) THEN flag_switch=1 ELSE flag_switch=0

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


;only the LOWER half of the u-v plane is used for gridding/degridding. 
; Visibilities that would land in the upper half use the complex conjugate of their mirror in the lower half 
IF ~Keyword_Set(uv_mask) THEN BEGIN
    uv_mask=Fltarr(dimension,elements)
    vis_count=visibility_count(obs,psf,params,flag_ptr=flag_ptr,no_conjugate=1,fill_model_vis=fill_model_vis)
    mask_i_use=where(vis_count)
    uv_mask[mask_i_use]=1
ENDIF ELSE uv_mask[*,elements/2+psf.dim:*]=0. 

IF Tag_exist(obs,'fbin_i') THEN freq_bin_i=obs.fbin_i ELSE freq_bin_i=(*obs.baseline_info).fbin_i
nfreq_bin=Max(freq_bin_i)+1
bin_offset=(*obs.baseline_info).bin_offset
IF Tag_exist(obs,'freq') THEN frequency_array=obs.freq ELSE frequency_array=(*obs.baseline_info).freq

kx_arr=params.uu/kbinsize
ky_arr=params.vv/kbinsize
baseline_i=params.baseline_arr
nbaselines=bin_offset[1]
n_samples=N_Elements(bin_offset)
n_freq=N_Elements(frequency_array)
n_freq_bin=N_Elements(freq_bin_i)

vis_dimension=Float(nbaselines*n_samples)
n_sources=N_Elements(source_list)
IF N_Elements(cal) NE 0 THEN BEGIN
    cal.n_cal_src=n_sources
    cal.galaxy_cal=Keyword_Set(galaxy_calibrate)
ENDIF

;xcen=frequency_array#kx_arr
;ycen=frequency_array#ky_arr

IF N_Elements(model_uv_arr) EQ 0 THEN BEGIN
    model_uv_arr=source_dft_model(obs,source_list,t_model=t_model,sigma_threshold=2.,uv_mask=uv_mask)
    IF ~Keyword_Set(silent) THEN print,"DFT timing: "+strn(t_model)+" (",strn(n_sources)+" sources)"
ENDIF

IF Keyword_Set(galaxy_calibrate) THEN BEGIN
    freq_use=where((*obs.baseline_info).freq_use,nf_use)
    IF Tag_exist(obs,'fbin_i') THEN f_bin=obs.fbin_i ELSE f_bin=(*obs.baseline_info).fbin_i
    fb_use=Uniq(f_bin[freq_use])
    nbin=N_Elements(fb_use)
    IF Tag_exist(obs,'freq') THEN freq_arr=obs.freq ELSE freq_arr=(*obs.baseline_info).freq
    IF Tag_exist(obs,'alpha') THEN alpha=obs.alpha ELSE alpha=0.
    freq_norm=freq_arr^(-alpha)
    ;freq_norm/=Sqrt(Mean(freq_norm^2.))
    freq_norm/=Mean(freq_norm) 
    freq_arr=freq_arr[freq_use[fb_use]]/1E6
    fb_hist=histogram(f_bin[freq_use],min=0,bin=1)
    nf_arr=fb_hist[f_bin[freq_use[fb_use]]]
    
    dimension=obs.dimension
    elements=obs.elements
    astr=obs.astr
    degpix=obs.degpix
    beam_width=(!RaDeg/(obs.MAX_BASELINE/obs.KPIX)/obs.degpix);*(2.*Sqrt(2.*Alog(2.)))
    beam_area=2.*!Pi*beam_width^2. ;area under a 2D gaussian with sigma_x=sigma_y=beam_width
    xy2ad,meshgrid(dimension,elements,1),meshgrid(dimension,elements,2),astr,ra_arr,dec_arr
    
    model_arr=globalskymodel_read(freq_arr,ra_arr=ra_arr,dec_arr=dec_arr,/haslam_filtered,_Extra=extra)
    
    IF N_Elements(model_arr) GT 1 THEN BEGIN
        model=fltarr(dimension,elements)
        FOR fi=0L,nbin-1 DO model+=*model_arr[fi]*nf_arr[fi]*freq_norm[fi]
        model/=Total(nf_arr)
    ENDIF ELSE model=*model_arr[0]
;    model*=weight_invert(pixel_area)
    Ptr_free,model_arr
    
;    model*=(degpix*!DtoR)^2. ;flux unit conversion
    edge_match,model
    valid_i=where(Finite(ra_arr),n_valid)
    Jdate=obs.Jd0
    Eq2Hor,ra_arr[valid_i],dec_arr[valid_i],Jdate,alt_arr1,az_arr1,lat=obs.lat,lon=obs.lon,alt=obs.alt,precess=1
    alt_arr=fltarr(dimension,elements) & alt_arr[valid_i]=alt_arr1
    horizon_proj=Sin(alt_arr*!DtoR)
    antialias_filter=Sqrt(Hanning(dimension,elements))
    model_use=model
    model_use*=horizon_proj
    model_use*=antialias_filter
    model_use*=(dimension*degpix*!DtoR)^2.*beam_area ;flux unit conversion
;    model_uv=dirty_image_generate(fft_shift(model),degpix=degpix,/no_real,/antialias)*uv_mask
    model_uv=fft_shift(FFT(fft_shift(model_use),/inverse))
    model_uv/=dimension ;FFT normalization
    
;    model_uv=fft_shift(FFT(model*horizon_proj,/inverse)/(dimension*degpix*!DtoR)^2.)
    FOR pol_i=0,n_pol-1 DO *model_uv_arr[pol_i]+=model_uv
ENDIF

vis_arr=Ptrarr(n_pol)

psf_base=psf.base
psf_residuals_n=psf.res_n
psf_residuals_i=psf.res_i
psf_residuals_val=psf.res_val
psf_dim=Sqrt((Size(*psf_base[0],/dimension))[0])
psf_resolution=(Size(psf_base,/dimension))[2]

valid_test=fltarr(n_pol)
FOR pol_i=0,n_pol-1 DO valid_test[pol_i]=Total(Abs(*model_uv_arr[pol_i]))
IF min(valid_test) EQ 0 THEN BEGIN
    error=1
    print,"ERROR: Invalid calibration model."
    timing=Systime(1)-t0
    RETURN,vis_arr
ENDIF

t_degrid=Fltarr(n_pol)
FOR pol_i=0,n_pol-1 DO BEGIN
    vis_arr[pol_i]=visibility_degrid(*model_uv_arr[pol_i],flag_ptr[pol_i],obs,psf,params,silent=silent,$
        timing=t_degrid0,polarization=pol_i,fill_model_vis=fill_model_vis)
    t_degrid[pol_i]=t_degrid0
ENDFOR
IF ~Keyword_Set(silent) THEN print,"Degridding timing: ",strn(t_degrid)

timing=Systime(1)-t0

RETURN,vis_arr
END