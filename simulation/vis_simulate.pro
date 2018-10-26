FUNCTION vis_simulate,obs,status_str,psf,params,jones,skymodel,file_path_fhd=file_path_fhd,vis_weights=vis_weights,$
    recalculate_all=recalculate_all,$
    include_eor=include_eor, flat_sigma = flat_sigma, no_distrib = no_distrib, delta_power = delta_power, $
    bubble_fname=bubble_fname, select_radius=select_radius, ltaper=ltaper,orthomap_var=orthomap_var, shellreplace=shellreplace, $
    delta_uv_loc = delta_uv_loc, eor_real_sky = eor_real_sky, $
    include_noise = include_noise, noise_sigma_freq = noise_sigma_freq, $
    include_catalog_sources = include_catalog_sources, source_array=source_array, catalog_file_path=catalog_file_path, $
    model_uvf_cube=model_uvf_cube, model_image_cube=model_image_cube,eor_uvf_cube_file=eor_uvf_cube_file, diffuse_model=diffuse_model, _Extra=extra
    
  n_freq=obs.n_freq
  n_pol=obs.n_pol
  dimension=obs.dimension
  elements=obs.elements
  degpix=obs.degpix

  IF ~Keyword_Set(file_path_fhd) THEN BEGIN no_save=1 & file_path_fhd='' & recalculate_all=1 & ENDIF
  input_model_filepath = file_path_fhd + '_input_model.sav'
  coarse_input_model_filepath = file_path_fhd + '_input_model_coarse.sav'
  init_beam_filepath = file_path_fhd + '_initial_beam2_image.sav'
  
  IF N_Elements(status_str) GT 0 THEN IF Min(status_str.vis_ptr[0:n_pol-1]) EQ 0 THEN recalculate_all=1
  IF N_Elements(recalculate_all) EQ 0 THEN recalculate_all=1
 
 
  ;Construct model visibilities. Start by building a model u-v-f cube
  if keyword_set(include_catalog_sources) then begin
    catalog_source_array=generate_source_cal_list(obs,psf,catalog_path=catalog_file_path,_Extra=extra)
    if n_elements(source_array) gt 0 then source_array = [source_array, catalog_source_array] else source_array = catalog_source_array
  endif    
  n_sources=N_Elements(source_array)
  print, 'n_sources: '+string(n_sources)
  skymodel=fhd_struct_init_skymodel(obs,source_list=source_array,catalog_path=catalog_file_path,diffuse_model=diffuse_model,return_cal=0,_Extra=extra)
  
  if keyword_set(recalculate_all) then begin
    fhd_save_io,status_str,file_path_fhd=file_path_fhd,/reset,no_save=no_save
    if n_sources gt 0 then begin
      source_model_uv_arr=source_dft_model(obs,jones,source_array,t_model=t_model,sigma_threshold=2.,uv_mask=uv_mask)
      IF ~Keyword_Set(silent) THEN print,"DFT timing: "+strn(t_model)+" (",strn(n_sources)+" sources)"
    endif
    
    beam2_xx_image = fltarr(dimension, elements, n_freq)
    beam2_yy_image = fltarr(dimension, elements, n_freq)
    beam_arr=beam_image_cube(obs,psf, n_freq_bin = n_freq,/square)
    for freq_i=0,n_freq-1 do begin
      beam2_xx_image[*,*, freq_i] = Temporary(*beam_arr[0,freq_i])
    ;  beam2_yy_image[*,*, freq_i] = Temporary(*beam_arr[1,freq_i])
      IF n_pol gt 1 THEN BEGIN
	 beam2_yy_image[*,*, freq_i] = Temporary(*beam_arr[1,freq_i])
      ENDIF ELSE BEGIN 
	beam2_yy_image[*,*,freq_i] = beam2_xx_image[*,*,freq_i]
      ENDELSE

    endfor
    test = strpos(init_beam_filepath, '_000_')
    if test NE -1 THEN IF ~Keyword_Set(no_save) THEN save, file=init_beam_filepath, beam2_xx_image, beam2_yy_image, obs
    undefine_fhd, beam2_xx_image, beam2_yy_image, beam_arr

    if n_elements(model_image_cube) gt 0 or n_elements(model_uvf_cube) gt 0 or keyword_set(include_eor) then begin
      model_uvf_arr=Ptrarr(n_pol,/allocate)
      for pol_i=0,n_pol-1 do *model_uvf_arr[pol_i]=Complexarr(dimension,elements, n_freq)
      
      if n_elements(model_uvf_cube) eq 0 and n_elements(model_image_cube) gt 0 then begin
        ;; convert from Jy/str to Jy/pixel
        model_image_use = model_image_cube/(degpix*!DtoR)^2. ;; Jy/pixel
        model_uvf_cube = Complexarr(dimension,elements, n_freq)
        for i=0, n_freq-1 do model_uvf_cube[*,*,i] = fft_shift(FFT(fft_shift(model_image_use[*,*,1]),/inverse)) * (degpix*!DtoR)^2.
        undefine, model_image_use
      endif

      if keyword_set(include_eor) then begin
        freq_arr = (*obs.baseline_info).freq
        delta_uv=obs.kpix
        uv_arr = (findgen(dimension)-dimension/2)*delta_uv
        
        if n_elements(eor_uvf_cube_file) gt 0 then begin
          ;; use passed in eor_uvf_cube_file but check that it matches the required size
        
          if file_test(eor_uvf_cube_file) then begin
            eor_gen = 0
            
            void = getvar_savefile(eor_uvf_cube_file, names = varnames)
            wh_uv_arr = where(strlowcase(varnames) eq 'uv_arr', count_uv_arr)
            if count_uv_arr eq 1 then begin
              eor_uv_arr = getvar_savefile(eor_uvf_cube_file, 'uv_arr')
              if max(abs(uv_arr - eor_uv_arr)/uv_arr) gt 1e-6 or n_elements(eor_uv_arr) ne dimension then begin
                print, 'uv_arr in ' + eor_uvf_cube_file + ' does not match simulation uv_arr. Generating new eor_uvf_cube. '
                eor_gen = 1
              endif
            endif
            
            wh_freq_arr = where(strlowcase(varnames) eq 'freq_arr', count_freq_arr)
            if count_freq_arr eq 1 then begin
              eor_freq_arr = getvar_savefile(eor_uvf_cube_file, 'freq_arr')
              if max(abs(freq_arr - eor_freq_arr)/freq_arr) gt 1e-6 or n_elements(freq_arr) ne n_freq then begin
                print, 'freq_arr in ' + eor_uvf_cube_file + ' does not match simulation freq_arr. Generating new eor_uvf_cube. '
                eor_gen = 1
              endif
            endif
            
            if eor_gen eq 0 then begin
              ;; check that dimensions are correct
              wh_eor = where(strlowcase(varnames) eq 'eor_uvf_cube', count_eor)
              if count_eor ne 1 then wh_eor = where(stregex(strlowcase(varnames), 'uvf', /boolean), count_eor)
              if count_eor eq 1 then begin
                eor_size = getvar_savefile(eor_uvf_cube_file, varnames[wh_eor], /return_size)
                eor_dims = eor_size[1:eor_size[0]]
                
                if eor_dims[0] ne dimension or eor_dims[1] ne dimension or eor_dims[2] ne n_freq then begin
                  print, 'dimensions of cube in ' + eor_uvf_cube_file + ' does not match simulation dimensions. Generating new eor_uvf_cube. '
                  eor_gen = 1
                endif else begin
                  print, 'restoring cube from ' + eor_uvf_cube_file
                  time0 = systime(1)
                  eor_uvf_cube = getvar_savefile(eor_uvf_cube_file, varnames[wh_eor[0]])*2. ;; factor of 2 because of division by 2 before saving
                  time1 = systime(1)
                  print, 'time for eor restore (min): ' + number_formatter((time1-time0)/60.)
                  if n_elements(model_uvf_cube) gt 0 then model_uvf_cube = model_uvf_cube + temporary(eor_uvf_cube) $
                  else model_uvf_cube = temporary(eor_uvf_cube)
                endelse
                
              endif else begin
                print, 'eor cube could not be identified in ' + eor_uvf_cube_file + '. Generating new eor_uvf_cube. '
                eor_gen = 1
              endelse
            endif
            
          endif else begin
            print, eor_uvf_cube_file + ' does not exist. Generating new eor_uvf_cube'
            eor_gen = 1
          endelse
        endif else eor_gen = 1
        
        if eor_gen ne 0 then begin
          print, 'Generating model EoR cube'
          uv_locs = findgen(101)*4.-200.
  ;        eor_uvf = eor_sim(uv_locs, uv_locs, freq_arr, flat_sigma = flat_sigma, no_distrib = no_distrib, $
  ;          delta_power = delta_power, delta_uv_loc = delta_uv_loc, real_sky = eor_real_sky);, /lidz)
  ;        IF ~Keyword_Set(no_save) THEN save,filename=coarse_input_model_filepath, eor_uvf, uv_locs, $
  ;          freq_arr, /compress
            
          time0 = systime(1)
          eor_uvf_cube = eor_sim(uv_arr, uv_arr, freq_arr, flat_sigma = flat_sigma, no_distrib = no_distrib, $
            delta_power = delta_power, delta_uv_loc = delta_uv_loc, real_sky = eor_real_sky);, /lidz)
          time1 = systime(1)
          print, 'time for eor modelling (min): ' + number_formatter((time1-time0)/60.)
          if n_elements(model_uvf_cube) gt 0 then model_uvf_cube = model_uvf_cube + temporary(eor_uvf_cube) $
          else model_uvf_cube = temporary(eor_uvf_cube)
        endif ;; end if eor_gen gt 0
      endif ;; end if keyword_set(include_eor)
      
      ;; model cube assumed to be Stokes I
      switch n_pol of
        4:(*model_uvf_arr[3])[*]=0.
        3:(*model_uvf_arr[2])[*]=0.
        2:(*model_uvf_arr[1])[*]=model_uvf_cube/2.
        1:(*model_uvf_arr[0])[*]=model_uvf_cube/2.
      endswitch
      
      undefine, model_uvf_cube
      
    endif ;; end if n_elements(model_image_cube) gt 0 or n_elements(model_uvf_cube) gt 0 or keyword_set(include_eor)
    
    ;; If bubble file is included, add to the model_uvf_arr
    IF Keyword_Set(bubble_fname) THEN BEGIN 
       bubble_uvf = eor_bubble_sim(obs, jones, select_radius=select_radius, bubble_fname=bubble_fname,ltaper=ltaper,file_path_fhd=file_path_fhd,orthomap_var=orthomap_var, shellreplace=shellreplace)
       IF Min(Ptr_valid(bubble_uvf)) GT 0 THEN BEGIN
         IF n_elements(model_uvf_arr) GT 0 then begin
           FOR pol_i=0,n_pol-1 DO *model_uvf_arr[pol_i] += *bubble_uvf[pol_i];*uv_mask_use
         endif else model_uvf_arr = Pointer_copy(bubble_uvf)
       ENDIF
    ENDIF 

    ;; If diffuse model is included, calculate its contribution and add to the source_model_uv_arr
    IF Keyword_Set(diffuse_model) THEN BEGIN
      IF file_test(diffuse_model) EQ 0 THEN diffuse_model=(file_search(diffuse_model+'*'))[0]
         print,"Reading diffuse model file: "+diffuse_model
      diffuse_model_uv=fhd_diffuse_model(obs,jones,skymodel,spectral_model_arr=diffuse_spectral_model_uv,/uv_return,/diffuse_units_kelvin,model_filepath=diffuse_model,_Extra=extra)
      IF Max(Ptr_valid(diffuse_model_uv)) EQ 0 THEN print,"Error reading or building diffuse model. Null pointer returned!"
    ENDIF

    IF Min(Ptr_valid(diffuse_model_uv)) GT 0 THEN BEGIN
      IF n_elements(source_model_uv_arr) GT 0 then begin
        FOR pol_i=0,n_pol-1 DO *source_model_uv_arr[pol_i]+=*diffuse_model_uv[pol_i];*uv_mask_use
      endif else source_model_uv_arr = Pointer_copy(diffuse_model_uv)
    ENDIF
    
    ;; If galaxy model is set, calculate its contribution and add to the source_model_uv_arr
    If keyword_set(skymodel.galaxy_model) then begin
      gal_model_uv=fhd_galaxy_model(obs,jones,spectral_model_uv_arr=gal_spectral_model_uv,antialias=1,/uv_return,_Extra=extra)
      IF Min(Ptr_valid(gal_model_uv)) GT 0 THEN BEGIN
        IF n_elements(source_model_uv_arr) GT 0 then begin
          FOR pol_i=0,n_pol-1 DO *source_model_uv_arr[pol_i]+=*gal_model_uv[pol_i]
	ENDIF ELSE source_model_uv_arr = Pointer_copy(gal_model_uv)
      ENDIF
      ;IF Min(Ptr_valid(gal_spectral_model_uv)) GT 0 THEN FOR pol_i=0,n_pol-1 DO FOR s_i=0,n_spectral-1 DO $
      ;  *spectral_model_uv_arr[pol_i,s_i]+=*gal_spectral_model_uv[pol_i,s_i];*uv_mask_use
      undefine_fhd,gal_model_uv;,gal_spectral_model_uv
    endif

    if n_elements(source_model_uv_arr) gt 0 then begin
      if n_elements(model_uvf_arr) gt 0 then begin
        ;; if there is also a uvf cube, add the uv from the sources to the cube at each freq.
	IF size(*source_model_uv_arr[0],/type) eq 6 then begin     ;; If array is complex
	        FOR pol_i=0,n_pol-1 DO BEGIN
			*model_uvf_arr[pol_i]+=rebin_complex(*source_model_uv_arr[pol_i],dimension,elements,n_freq,/sample)
			;j = Complex(0,1)
			;*model_uvf_arr[pol_i]+=Rebin(Real_part(*source_model_uv_arr[pol_i]),dimension,elements,n_freq,/sample)$
			;				+j*(Rebin(imaginary(*source_model_uv_arr[pol_i]),dimension,elements,n_freq,/sample))
		ENDFOR
	ENDIF ELSE BEGIN
	        FOR pol_i=0,n_pol-1 DO *model_uvf_arr[pol_i]+=Rebin(*source_model_uv_arr[pol_i],dimension,elements,n_freq,/sample)
	ENDELSE
      endif else model_uvf_arr = Pointer_copy(source_model_uv_arr) ;; otherwise just use the uv from the sources
      undefine_fhd, source_model_uv_arr
    endif
    
    if n_elements(model_uvf_arr) eq 0 and not keyword_set(include_noise) then begin
      print, 'No input model (image cube, model_uvf or sources) and include_noise not set'
      error=1
      RETURN,Ptrarr(n_pol)
    endif
    
    if n_elements(model_uvf_arr) gt 0 then begin
      model_uvf = *model_uvf_arr[0]
      IF ~Keyword_Set(no_save) and keyword_set(include_eor) then if eor_gen eq 1 THEN $
        save,filename=input_model_filepath, model_uvf, uv_arr, freq_arr, /compress
      undefine, model_uvf
      
      vis_dimension=N_Elements(params.uu)
      
      vis_model_arr = Ptrarr(n_pol,/allocate)
      for pol_i=0,n_pol-1 do *vis_model_arr[pol_i]=Complexarr(n_freq,vis_dimension)
      
      time0=systime(1)
      dim_uv_arr = size(*model_uvf_arr[0], /dimension)
      if n_elements(dim_uv_arr) gt 3 or n_elements(dim_uv_arr) lt 2 then $
        message, 'model_uvf_arr must point to 2 or 3 dimensional arrays'
      
      if n_elements(dim_uv_arr) eq 2 then begin
        ;; 2 dimensional -- same for all frequencies
      
        ;; vis_weights is passed in from array_simulator
        ;Call visibility_degrid directly, instead of calling the wrapper, since we are adding the uv models earlier
        FOR pol_i=0,n_pol-1 DO BEGIN
            vis_model_arr[pol_i]=visibility_degrid(*model_uvf_arr[pol_i],vis_weights[pol_i],obs,psf,params,silent=silent,$
                polarization=pol_i,_Extra=extra)
        ENDFOR
;        vis_model_arr = vis_source_model(skymodel,obs,status_str,psf,params,vis_weights,model_uv_arr=model_uvf_arr,$
;          timing=model_timing,silent=silent,error=error,_Extra=extra)
          
      endif else begin
        ;; 3 dimensional -- loop over frequencies
        for fi=0, n_freq-1 do begin
          if max([(*vis_weights[0])[fi,*], (*vis_weights[1])[fi,*]]) lt 1 then continue
          
          this_vis_weight_ptr = Ptrarr(n_pol,/allocate)
          this_model_uv = Ptrarr(n_pol,/allocate)
          for pol_i=0,n_pol-1 do begin
            *this_vis_weight_ptr[pol_i]=intarr(n_freq, vis_dimension)
            (*this_vis_weight_ptr[pol_i])[fi,*] = (*vis_weights[pol_i])[fi,*]
            
            *this_model_uv[pol_i] = (*model_uvf_arr[pol_i])[*,*,fi]
          endfor
          
          if max(abs(*this_model_uv[0])) eq 0 and max(abs(*this_model_uv[1])) eq 0 then continue
           ;this_model_ptr=vis_source_model(skymodel,obs,status_str,psf,params,this_vis_weight_ptr,model_uv_arr=this_model_uv,$
           ;timing=model_timing,silent=silent,error=error,_Extra=extra)
           model_timing=0.0
           ;vis_source_model is inexplicably taking a long time. Since the uvf cube was already FFTed, just run visibility_degrid.
           FOR pol_i=0,n_pol-1 DO BEGIN
              (*vis_model_arr[pol_i])[fi,*] = (*(visibility_degrid(*this_model_uv[pol_i],this_vis_weight_ptr[pol_i],obs,psf,params,timing=timing,silent=silent,$
                  polarization=pol_i,_Extra=extra)))[fi,*]
               model_timing += timing
          ENDFOR
          print, 'model loop num, timing(s):'+ number_formatter(fi) + ' , ' + number_formatter(model_timing)
          
          undefine_fhd, this_vis_weight_ptr, this_model_ptr, this_model_uv
        endfor
      endelse
      undefine_fhd, model_uvf_arr
      time1=systime(1)
      print, 'model visibility timing(s):'+ number_formatter(time1-time0)
    endif ;; end if n_elements(model_uvf_arr) gt 0
    
    if keyword_set(include_noise) then begin
      if n_elements(noise_sigma_freq) eq 0 then noise_sigma_freq = 28. ;; something close to the average noise in a visibility
      if n_elements(noise_sigma_freq) eq 1 then noise_sigma_freq = fltarr(n_freq) + noise_sigma_freq
      
      vis_noise_ptr=Ptrarr(n_pol,/allocate)
      
      if n_elements(vis_model_arr) gt 0 then begin
        vis_dims = size(*(vis_model_arr[0]), /dimension)
        if vis_dims[0] ne n_freq then message, 'number of frequencies in visibility array does not match n_freq'
        vis_dimension = vis_dims[1]
      endif else begin
        nbaselines=obs.nbaselines
        n_samples=obs.n_time
        vis_dimension=nbaselines*n_samples
      endelse
      noise_sigma_arr = rebin(noise_sigma_freq, n_freq, vis_dimension)
      
      for pol_i=0,n_pol-1 do *(vis_noise_ptr[pol_i]) = randomn(seed, [n_freq, vis_dimension]) * noise_sigma_arr + $
        complex(0,1) * randomn(seed, [n_freq, vis_dimension]) * noise_sigma_arr
        
      undefine, noise_sigma_arr
      
      if n_elements(vis_model_arr) gt 0 then begin
        for pol_i=0,n_pol-1 do *(vis_model_arr[pol_i]) += *(vis_noise_ptr[pol_i])
        undefine_fhd, vis_noise_ptr
      endif else vis_model_arr = vis_noise_ptr
      
    endif
    
    fhd_save_io,status_str,vis_weights,var='vis_weights',/compress,file_path_fhd=file_path_fhd,no_save=no_save,_Extra=extra
  ENDIF ELSE BEGIN ;; end if recalculate_all
    vis_model_arr=Ptrarr(n_pol)
    FOR pol_i=0,n_pol-1 DO BEGIN
      fhd_save_io,status_str,vis_model_ptr,var='vis_ptr',/restore,file_path_fhd=file_path_fhd,obs=obs_out,pol_i=pol_i,_Extra=extra
      vis_model_arr[pol_i]=vis_model_ptr
    ENDFOR
    fhd_save_io,status_str,skymodel,var='skymodel',/restore,file_path_fhd=file_path_fhd,_Extra=extra
  ENDELSE
  RETURN,vis_model_arr
END
