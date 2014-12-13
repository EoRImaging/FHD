FUNCTION fhd_diffuse_model,obs,jones,model_filepath=model_filepath,uv_return=uv_return,_Extra=extra

dimension=obs.dimension
elements=obs.elements
astr=obs.astr
degpix=obs.degpix
n_pol=obs.n_pol
xy2ad,meshgrid(dimension,elements,1),meshgrid(dimension,elements,2),astr,ra_arr,dec_arr
radec_i=where(Finite(ra_arr))

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

IF file_test(model_filepath) EQ 0 THEN RETURN,Ptrarr(n_pol)
var_dummy=getvar_savefile(model_filepath,names=var_names)
;save file must have one variable called 'hpx_inds', one called 'nside', and at least one other variable. If there are multiple other variables, it must be called 'model_arr'
hpx_inds=getvar_savefile(model_filepath,'hpx_inds')
nside=getvar_savefile(model_filepath,'nside')
var_name_inds=where((StrLowCase(var_names) NE 'hpx_inds') AND (StrLowCase(var_names) NE 'nside'))
var_names=var_names[var_name_inds]
var_name_use=var_names[(where(StrLowCase(var_names) EQ 'model_arr',n_match))[0]>0] ;will pick 'model_arr' if present, or the first variable that is not 'hpx_inds' or 'nside'
model_hpx_arr=getvar_savefile(model_filepath,var_name_use)

model_stokes_arr=healpix_interpolate(model_hpx_arr,obs,nside=nside,hpx_inds=hpx_inds)
IF size(model_stokes_arr,/type) EQ 10 THEN BEGIN
    np_hpx=Total(Ptr_valid(model_hpx_arr))
    IF np_hpx LT n_pol THEN BEGIN
        model_tmp=Pointer_copy(model_stokes_arr)
        Ptr_free,model_stokes_arr
        model_stokes_arr=Ptrarr(n_pol,/allocate)
        FOR pol_i=0,np_hpx-1 DO *model_stokes_arr[pol_i]=Temporary(*model_tmp[pol_i])
        Ptr_free,model_tmp
        FOR pol_i=np_hpx,n_pol-1 DO *model_stokes_arr[pol_i]=Fltarr(dimension,elements)
    ENDIF 
ENDIF ELSE BEGIN
    model_tmp=model_stokes_arr
    model_stokes_arr=Ptrarr(n_pol,/allocate)
    *model_stokes_arr[0]=model_tmp
    FOR pol_i=1,n_pol-1 DO *model_stokes_arr[pol_i]=Fltarr(dimension,elements)
ENDELSE

model_arr=Stokes_cnv(model_stokes_arr,jones,/inverse,_Extra=extra)
Ptr_free,model_stokes_arr
IF Keyword_Set(uv_return) THEN BEGIN
    model_uv_arr=Ptrarr(n_pol,/allocate)
    FOR pol_i=0,n_pol-1 DO BEGIN
        model_uv=2.*!Pi*fft_shift(FFT(fft_shift(*model_arr[pol_i]),/inverse))
        *model_uv_arr[pol_i]=model_uv
    ENDFOR
    Ptr_free,model_arr
    RETURN,model_uv_arr 
ENDIF ELSE RETURN,model_arr
END