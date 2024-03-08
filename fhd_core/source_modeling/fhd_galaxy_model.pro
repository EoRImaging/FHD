FUNCTION fhd_galaxy_model,obs,jones,file_path_fhd=file_path_fhd,restore=restore,antialias=antialias,$
    gal_model_img=gal_model_img,gal_model_uv=gal_model_uv,uv_return=uv_return,$
    spectral_model_uv_arr=spectral_model_uv_arr,_Extra=extra

IF Keyword_Set(file_path_fhd) THEN file_path_galmodel=file_path_fhd+'_GalaxyModel.sav' ELSE file_path_galmodel=''
IF Keyword_Set(restore) AND file_test(file_path_galmodel) THEN BEGIN
    restore,file_path_galmodel
    IF Keyword_Set(uv_return) THEN RETURN,gal_model_uv ELSE RETURN,gal_model_img 
ENDIF

dimension=obs.dimension
elements=obs.elements
astr=obs.astr
degpix=obs.degpix
n_pol=obs.n_pol
apply_astrometry, obs, x_arr=meshgrid(dimension,elements,1), y_arr=meshgrid(dimension,elements,2), ra_arr=ra_arr, dec_arr=dec_arr, /xy2ad, /refraction
IF N_Elements(jones) EQ 0 THEN jones=fhd_struct_init_jones(obs,file_path_fhd=file_path_fhd,/restore)

;i_use=where(Finite(ra_arr))
;glactc,ra_arr[i_use],dec_Arr[i_use],2000.,gl_vals,gb_vals,1,/degree
;gal_lat_weights=fltarr(dimension,elements) & gal_lat_weights[i_use]=(1./(Abs(gb_vals)>5))

freq_use=where((*obs.baseline_info).freq_use,nf_use)
f_bin=(*obs.baseline_info).fbin_i
fb_use=Uniq(f_bin[freq_use])
nbin=N_Elements(fb_use)
freq_arr=(*obs.baseline_info).freq
alpha=obs.alpha
freq_norm=freq_arr^(-alpha)
;freq_norm/=Sqrt(Mean(freq_norm^2.))
freq_norm/=Mean(freq_norm) 
d_freq=Median(Float(deriv(freq_arr)))

freq_norm=freq_norm[freq_use[fb_use]]

freq_arr_use=freq_arr[freq_use[fb_use]]/1E6
fb_hist=histogram(f_bin[freq_use],min=0,bin=1)
nf_arr=fb_hist[f_bin[freq_use[fb_use]]]

model_arr=globalskymodel_read(obs,freq_arr_use,/haslam_filtered,_Extra=extra) ;maps are read in Kelvin, and converted to Jy/pixel

IF N_Elements(model_arr) GT 1 THEN BEGIN
    model=fltarr(dimension,elements)
    FOR fi=0L,nbin-1 DO model+=*model_arr[fi]*nf_arr[fi]*freq_norm[fi]
    model/=Total(nf_arr)
ENDIF ELSE model=*model_arr[0]
Ptr_free,model_arr
;unit conversion now handled earlier in globalskymodel_read.pro
;c_light=299792458.
;kb=1.38065E3 ;actually 1.38065*10^-23 / 10^-26 (10^-26 from definition of Jy) 
;conv_to_Jy_per_sr=2.*kb*(obs.freq_center/c_light)^2.
;conv_to_Jy_per_pixel=conv_to_Jy_per_sr*4.*!Pi/
;model*=conv_to_Jy_per_sr

edge_match,model
valid_i=where(Finite(ra_arr),n_valid)
Jdate=obs.Jd0
pixel_area_factor=pixel_area(obs,/relative)
model*=weight_invert(pixel_area_factor)
;antialias_filter=Sqrt(Hanning(dimension,elements))
;antialias_filter/=Mean(antialias_filter[valid_i])
model_use=model
;IF Keyword_Set(antialias) THEN model_use*=antialias_filter
gal_model_img=model_use

gal_model_stks=Ptrarr(4,/allocate)
*gal_model_stks[0]=gal_model_img
FOR pol_i=1,3 DO *gal_model_stks[pol_i]=Fltarr(dimension,elements)
gal_model_instr=Stokes_cnv(gal_model_stks,jones,/inverse,_Extra=extra)

gal_model_uv=Ptrarr(n_pol,/allocate)
FOR pol_i=0,n_pol-1 DO BEGIN
    model_uv=fft_shift(FFT(fft_shift(*gal_model_instr[pol_i]),/inverse))
    *gal_model_uv[pol_i]=model_uv
ENDFOR
  
IF Keyword_Set(file_path_galmodel) THEN $
    save,gal_model_img,gal_model_uv,filename=file_path_galmodel,/compress
IF Keyword_Set(uv_return) THEN RETURN,gal_model_uv ELSE RETURN,gal_model_img
END