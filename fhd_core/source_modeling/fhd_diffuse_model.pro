FUNCTION fhd_diffuse_model,obs,jones,skymodel,psf,model_filepath=model_filepath,uv_return=uv_return,$
    spectral_model_arr=spectral_model_arr,diffuse_units_kelvin=diffuse_units_kelvin, $
    flatten_spectrum=flatten_spectrum,no_polarized_diffuse=no_polarized_diffuse,$
    diffuse_fft_window=diffuse_fft_window,_Extra=extra

dimension=obs.dimension
elements=obs.elements
n_pol=obs.n_pol
n_spectral=obs.degrid_spectral_terms
IF Keyword_Set(skymodel) THEN diffuse_spectral_index=skymodel.diffuse_spectral_index ELSE diffuse_spectral_index=0
IF Keyword_Set(flatten_spectrum) THEN alpha_corr=obs.alpha ELSE alpha_corr=0.

IF file_test(model_filepath) EQ 0 THEN RETURN,Ptrarr(n_pol)

; Get variables from the savefile. Must have one variable called 'hpx_inds', one called 'nside', 
;  and one data variable. If there are multiple other variables, data variable must be 'model_arr'
var_dummy = getvar_savefile(model_filepath, names = var_names)
hpx_inds = getvar_savefile(model_filepath, 'hpx_inds')
nside = getvar_savefile(model_filepath, 'nside')
var_name_inds = where((StrLowCase(var_names) NE 'hpx_inds') AND (StrLowCase(var_names) NE 'nside'))
var_names = var_names[var_name_inds]
; Will pick 'model_arr' if present, or the first variable that is not 'hpx_inds' or 'nside'
var_name_use = var_names[(where(StrLowCase(var_names) EQ 'model_arr',n_match))[0]>0]
model_hpx_arr = getvar_savefile(model_filepath, var_name_use)

; Get spectral information.
model_spectra_i = where(StrLowCase(var_names) EQ 'model_spectral_arr',n_match)
IF n_match GE 1 THEN diffuse_spectral_index = getvar_savefile(model_filepath, var_names[model_spectra_i])
IF n_spectral LE 0 THEN undefine_fhd, diffuse_spectral_index

; Get coordinate system if present. Assume celestial if not defined otherwise. 
coord_use = where(StrLowCase(var_names) EQ 'coord_sys',m_match)
IF m_match EQ 0 THEN coord_use = 'celestial' ELSE coord_use = getvar_savefile(model_filepath, 'coord_sys')

; Get units if present. Assume Jy/str if not defined otherwise.
units_use = where(StrLowCase(var_names) EQ 'units',m_match)
IF m_match EQ 0 THEN units_use = 'Jy/str' ELSE units_use=getvar_savefile(model_filepath, 'units')
if STRMATCH(units_use, 'kelvin', /FOLD_CASE) then diffuse_units_kelvin=1
IF N_Elements(diffuse_units_kelvin) EQ 0 THEN diffuse_units_kelvin = 0
IF Keyword_Set(diffuse_units_kelvin) THEN from_jy_per_sr = 0 ELSE from_jy_per_sr = 1

IF size(diffuse_spectral_index,/type) EQ 10 THEN BEGIN ;check if pointer type
    ;need to write this!
    print,"A spectral index is defined in the saved diffuse model, but this is not yet supported!"
ENDIF ;case of specifying a single scalar to be applied to the entire diffuse model is treated AFTER building the model in instrumental polarization

model_stokes_arr=healpix_interpolate(model_hpx_arr,obs,nside=nside,hpx_inds=hpx_inds,$
  from_jy_per_sr=from_jy_per_sr,from_kelvin=diffuse_units_kelvin,coord_sys=coord_use)

IF size(model_stokes_arr,/type) EQ 10 THEN BEGIN
    np_hpx=Total(Ptr_valid(model_hpx_arr))
    IF Keyword_Set(no_polarized_diffuse) THEN np_hpx=1
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

IF (size(diffuse_spectral_index,/type) GE 1) AND (size(diffuse_spectral_index,/type) LE 5) THEN BEGIN 
    ;if a scalar, assume a single spectral index will be used for the diffuse model
    IF Finite(diffuse_spectral_index) THEN BEGIN
        spectral_model_arr=Ptrarr(n_pol,n_spectral)
        diffuse_spectral_index-=alpha_corr
        FOR pol_i=0,n_pol-1 DO BEGIN
            FOR s_i=0,n_spectral-1 DO BEGIN
                IF Keyword_Set(uv_return) THEN $
                    spectral_model_arr[pol_i,s_i]=Ptr_new(fft_shift(FFT(fft_shift(*model_arr[pol_i]*diffuse_spectral_index^(s_i+1.)),/inverse))) $
                        ELSE spectral_model_arr[pol_i,s_i]=Ptr_new(*model_arr[pol_i]*diffuse_spectral_index^(s_i+1.))
            ENDFOR
        ENDFOR
    ENDIF
ENDIF

IF Keyword_Set(uv_return) THEN BEGIN

    ; Option to apply fft filters to the image before/after the FFT to reduce aliasing
    IF keyword_set(diffuse_fft_window) THEN BEGIN

        ; Default to the Tukey window pre-FFT and Blackman-Harris window post-FFT
        IF size(diffuse_fft_window,/type) NE 7 THEN BEGIN
           diffuse_fft_window = ['Tukey','Blackman-Harris']
        ENDIF ELSE BEGIN
           IF N_elements(diffuse_fft_window) NE 2 THEN BEGIN
               diffuse_fft_window = [diffuse_fft_window, diffuse_fft_window]
           ENDIF
        ENDELSE

        ; Calculate pre-FFT window
        IF diffuse_fft_window[0] EQ 'Tukey' THEN BEGIN
            ; Calculate size of Tukey window to begin taper at the primary lobe null.
            ; This allows for the maximum amount of diffuse emission (important if calibrating)
            ;  while taking advantage of the natural taper of the instrument.  
            pbr = primary_beam_radius(obs,psf,beam_threshold=.01)
            frac_size = (pbr * 2 / obs.degpix) / dimension
        ENDIF ELSE frac_size = 1
        window_1d_dim = spectral_window(dimension, type = diffuse_fft_window[0], $
          fractional_size = frac_size, periodic=1)
        window_1d_ele = spectral_window(elements, type = diffuse_fft_window[0], $
          fractional_size = frac_size, periodic=1)
        window_2d_pre = window_1d_dim # transpose(window_1d_ele)

       ; Calculate post-FFT window
       IF diffuse_fft_window[1] EQ 'Tukey' THEN BEGIN
           ; Estimate that most contributions past 50 wavelengths are artifacts (Byrne et al 2022)
           frac_size = ((50. / obs.kpix) * 2) / dimension
       ENDIF ELSE frac_size=1
       window_1d_dim = spectral_window(dimension, type = diffuse_fft_window[1], $
         fractional_size = frac_size, periodic=1)
       window_1d_ele = spectral_window(elements, type = diffuse_fft_window[1], $
         fractional_size = frac_size, periodic=1)
       window_2d_post = window_1d_dim # transpose(window_1d_ele)

    ENDIF

    model_uv_arr=Ptrarr(n_pol,/allocate)
    FOR pol_i=0,n_pol-1 DO BEGIN
        ; Apply filters before/after FFT if specified, otherwise calculate the unwindowed FFT  
        IF keyword_set(diffuse_fft_window) THEN BEGIN
            model_uv = fft_shift(FFT(fft_shift(*model_arr[pol_i]*window_2d_pre),/inverse))
            model_uv = abs(model_uv)*window_2d_post*exp(Complex(0,1)*atan(model_uv,/phase))
        ENDIF ELSE model_uv = fft_shift(FFT(fft_shift(*model_arr[pol_i]),/inverse))
        *model_uv_arr[pol_i]=model_uv
    ENDFOR

    Ptr_free,model_arr
    RETURN,model_uv_arr
ENDIF ELSE RETURN,model_arr

END

