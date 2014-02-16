FUNCTION fhd_galaxy_model,obs,file_path_fhd=file_path_fhd,restore=restore,$
    galaxy_model_img=galaxy_model_img,galaxy_model_uv=galaxy_model_uv,uv_return=uv_return,_Extra=extra

IF Keyword_Set(file_path_fhd) THEN file_path_galmodel=file_path_fhd+'_GalaxyModel.sav' ELSE file_path_galmodel=''
IF Keyword_Set(restore) AND file_test(file_path_galmodel) THEN BEGIN
    restore,file_path_galmodel
    IF Keyword_Set(uv_return) THEN RETURN,galaxy_model_uv ELSE RETURN,galaxy_model_img 
ENDIF

dimension=obs.dimension
elements=obs.elements
astr=obs.astr
degpix=obs.degpix
n_pol=obs.n_pol
xy2ad,meshgrid(dimension,elements,1),meshgrid(dimension,elements,2),astr,ra_arr,dec_arr

;i_use=where(Finite(ra_arr))
;glactc,ra_arr[i_use],dec_Arr[i_use],2000.,gl_vals,gb_vals,1,/degree
;gal_lat_weights=fltarr(dimension,elements) & gal_lat_weights[i_use]=(1./(Abs(gb_vals)>5))

freq_use=where((*obs.baseline_info).freq_use,nf_use)
IF Tag_exist(obs,'fbin_i') THEN f_bin=obs.fbin_i ELSE f_bin=(*obs.baseline_info).fbin_i
fb_use=Uniq(f_bin[freq_use])
nbin=N_Elements(fb_use)
IF Tag_exist(obs,'freq') THEN freq_arr=obs.freq ELSE freq_arr=(*obs.baseline_info).freq
IF Tag_exist(obs,'alpha') THEN alpha=obs.alpha ELSE alpha=0.
freq_norm=freq_arr^(-alpha)
;freq_norm/=Sqrt(Mean(freq_norm^2.))
freq_norm/=Mean(freq_norm) 

freq_norm=freq_norm[freq_use[fb_use]]

freq_arr=freq_arr[freq_use[fb_use]]/1E6
fb_hist=histogram(f_bin[freq_use],min=0,bin=1)
nf_arr=fb_hist[f_bin[freq_use[fb_use]]]

;IF ~Keyword_Set(galaxy_component_fit) THEN BEGIN
    model_arr=globalskymodel_read(freq_arr,ra_arr=ra_arr,dec_arr=dec_arr,/haslam_filtered,_Extra=extra)
    
    IF N_Elements(model_arr) GT 1 THEN BEGIN
        model=fltarr(dimension,elements)
        FOR fi=0L,nbin-1 DO model+=*model_arr[fi]*nf_arr[fi]*freq_norm[fi]
        model/=Total(nf_arr)
    ENDIF ELSE model=*model_arr[0]
    Ptr_free,model_arr
    
    edge_match,model
    valid_i=where(Finite(ra_arr),n_valid)
    Jdate=obs.Jd0
    
    beam_width=(!RaDeg/(obs.MAX_BASELINE/obs.KPIX)/obs.degpix);*(2.*Sqrt(2.*Alog(2.)))
    beam_area=2.*!Pi*beam_width^2. ;area under a 2D gaussian with sigma_x=sigma_y=beam_width
    Eq2Hor,ra_arr[valid_i],dec_arr[valid_i],Jdate,alt_arr1,az_arr1,lat=obs.lat,lon=obs.lon,alt=obs.alt,precess=1
    alt_arr=fltarr(dimension,elements) & alt_arr[valid_i]=alt_arr1
    horizon_proj=Sin(alt_arr*!DtoR)
    antialias_filter=Sqrt(Hanning(dimension,elements))
    model_use=model/2. ;convert Stokes I to "True sky" instrumental pol
    model_use*=horizon_proj
    model_use*=antialias_filter
    model_use*=(dimension*degpix*!DtoR)^2.*beam_area ;flux unit conversion
    model_uv=fft_shift(FFT(fft_shift(model_use),/inverse))
    model_uv/=dimension ;FFT normalization   
    galaxy_model_img=model
    
    galaxy_model_uv=Ptrarr(n_pol,/allocate)
    FOR pol_i=0,n_pol-1 DO *galaxy_model_uv[pol_i]=model_uv
;ENDIF ELSE BEGIN
;    comp_arr=globalskymodel_read(freq_arr,ra_arr=ra_arr,dec_arr=dec_arr,/components)
;    n_comp=N_Elements(comp_arr)
;    
;    model_uv=Ptrarr(n_comp)
;    FOR ci=0,n_comp-1 DO model_uv[ci]=Ptr_new(fft_shift(FFT(fft_shift(*comp_arr[ci]),/inverse)))
;    model_uv_holo=Ptrarr(n_comp,n_pol)
;    model_img_holo=Ptrarr(n_comp,n_pol)
;    dirty_img=Ptrarr(n_pol)
;    scale_arr=fltarr(n_comp,n_pol)
;    
;    FOR pol_i=0,n_pol-1 DO BEGIN
;        dirty_img[pol_i]=Ptr_new(dirty_image_generate(*image_uv_arr[pol_i],degpix=degpix))
;        beam_i=Region_grow(*beam_base[pol_i],Round(obs.obsx)+Round(obs.obsy)*dimension,threshold=[0.05,Max(*beam_base[pol_i])])
;        beam_vals=(*beam_base[pol_i])[beam_i]
;        image_vals=(*dirty_img[pol_i])[beam_i]
;        FOR ci=0,n_comp-1 DO BEGIN
;            model_uv_holo[ci,pol_i]=Ptr_new(holo_mapfn_apply(model_uv[ci],map_fn_arr[pol_i]))
;            model_img_holo[ci,pol_i]=Ptr_new(dirty_image_generate(*model_uv_holo[ci,pol_i],degpix=degpix))
;            
;            model_vals=(*model_img_holo[ci,pol_i])[beam_i]
;            scale_arr[ci,pol_i]=(linfit(model_vals,image_vals,measure_error=1./beam_vals))[1]
;        ENDFOR
;    ENDFOR
;    scale_arr2=Total(scale_arr,2)/n_pol
;    
;    model*=scale
;    
;    FOR pol_i=0,n_pol-1 DO BEGIN
;        *model_uv[pol_i]*=scale;/dimension*elements
;        *model_uv_holo[pol_i]*=scale
;        *model_img_holo[pol_i]*=scale
;    ENDFOR
;    galaxy_model_img=model
;    galaxy_model_uv=model_uv
;ENDELSE  
;IF map_fn_free_flag THEN Ptr_free,map_fn_arr
  
IF Keyword_Set(file_path_galmodel) THEN $
    save,galaxy_model_img,galaxy_model_uv,filename=file_path_galmodel,/compress
IF Keyword_Set(uv_return) THEN RETURN,galaxy_model_uv ELSE RETURN,galaxy_model_img
END