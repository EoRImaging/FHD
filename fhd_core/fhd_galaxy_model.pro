FUNCTION fhd_galaxy_model,obs,jones,file_path_fhd=file_path_fhd,restore=restore,antialias=antialias,$
    gal_model_img=gal_model_img,gal_model_uv=gal_model_uv,uv_return=uv_return,_Extra=extra

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
xy2ad,meshgrid(dimension,elements,1),meshgrid(dimension,elements,2),astr,ra_arr,dec_arr
IF N_Elements(jones) EQ 0 THEN jones=fhd_struct_init_jones(obs,file_path_fhd=file_path_fhd,/restore)

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

model_arr=globalskymodel_read(freq_arr,ra_arr=ra_arr,dec_arr=dec_arr,/haslam_filtered,_Extra=extra) ;returns temperature in Kelvin

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
print,'beam area used in galaxy model: ',beam_area
Eq2Hor,ra_arr[valid_i],dec_arr[valid_i],Jdate,alt_arr1,az_arr1,lat=obs.lat,lon=obs.lon,alt=obs.alt,precess=1
alt_arr=fltarr(dimension,elements) & alt_arr[valid_i]=alt_arr1
horizon_proj=Sin(alt_arr*!DtoR)
antialias_filter=Sqrt(Hanning(dimension,elements))
model_use=model
model_use*=horizon_proj
IF Keyword_Set(antialias) THEN model_use*=antialias_filter
model_use*=(dimension*degpix*!DtoR)^2.*beam_area ;flux unit conversion
gal_model_img=model_use

gal_model_stks=Ptrarr(4,/allocate)
*gal_model_stks[0]=gal_model_img
FOR pol_i=1,3 DO *gal_model_stks[pol_i]=Fltarr(dimension,elements)
gal_model_instr=Stokes_cnv(gal_model_stks,jones,/inverse,_Extra=extra)

gal_model_uv=Ptrarr(n_pol,/allocate)
FOR pol_i=0,n_pol-1 DO BEGIN
    model_uv=fft_shift(FFT(fft_shift(*gal_model_instr[pol_i]),/inverse))
    model_uv/=dimension ;FFT normalization   
    *gal_model_uv[pol_i]=model_uv
ENDFOR
  
IF Keyword_Set(file_path_galmodel) THEN $
    save,gal_model_img,gal_model_uv,filename=file_path_galmodel,/compress
IF Keyword_Set(uv_return) THEN RETURN,gal_model_uv ELSE RETURN,gal_model_img
END