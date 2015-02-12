FUNCTION vis_simulate,obs,status_str,psf,params,jones,file_path_fhd=file_path_fhd,flag_arr=flag_arr,$
    recalculate_all=recalculate_all,$
    eor_sim=eor_sim, flat_sigma = flat_sigma, no_distrib = no_distrib, delta_power = delta_power, delta_uv_loc = delta_uv_loc, $
    include_catalog_sources = include_catalog_sources, source_list=source_list, catalog_file_path=catalog_file_path, $
    model_uvf_cube=model_uvf_cube, model_image_cube=model_image_cube,_Extra=extra

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
if keyword_set(recalculate_all) then begin
    if keyword_set(include_catalog_sources) then begin
      catalog_source_list=generate_source_cal_list(obs,psf,catalog_path=catalog_file_path,_Extra=extra)
      if n_elements(source_list) gt 0 then source_list = [source_list, catalog_source_list] else source_list = catalog_source_list
    endif
    
    n_sources=N_Elements(source_list)
    if n_sources gt 0 then begin
      source_model_uv_arr=source_dft_model(obs,jones,source_list,t_model=t_model,sigma_threshold=2.,uv_mask=uv_mask)
      IF ~Keyword_Set(silent) THEN print,"DFT timing: "+strn(t_model)+" (",strn(n_sources)+" sources)"
    endif
    
    beam2_xx_image = fltarr(dimension, elements, n_freq)
    beam2_yy_image = fltarr(dimension, elements, n_freq)
    beam_arr=beam_image_cube(obs,psf, n_freq_bin = n_freq,/square)
    for freq_i=0,n_freq-1 do begin
      beam2_xx_image[*,*, freq_i] = Temporary(*beam_arr[0,freq_i])
      beam2_yy_image[*,*, freq_i] = Temporary(*beam_arr[1,freq_i])
    endfor
    IF ~Keyword_Set(no_save) THEN save, file=init_beam_filepath, beam2_xx_image, beam2_yy_image, obs
    undefine_fhd, beam2_xx_image, beam2_yy_image,beam_arr
    
    if n_elements(model_image_cube) gt 0 or n_elements(model_uvf_cube) gt 0 or keyword_set(eor_sim) then begin
      model_uvf_arr=Ptrarr(n_pol,/allocate)
      for pol_i=0,n_pol-1 do *model_uvf_arr[pol_i]=Complexarr(dimension,elements, n_freq)
      
      if n_elements(model_uvf_cube) eq 0 and n_elements(model_image_cube) gt 0 then begin
        ;; convert from Jy/str to Jy/pixel
        model_image_use = model_image_cube/(degpix*!DtoR)^2. ;; Jy/pixel
        model_uvf_cube = Complexarr(dimension,elements, n_freq)
        for i=0, n_freq-1 do model_uvf_cube[*,*,i] = fft_shift(FFT(fft_shift(model_image_use[*,*,1]),/inverse)) * (degpix*!DtoR)^2.
        undefine, model_image_use
      endif
      
      if keyword_set(eor_sim) then begin
        print, 'Generating model EoR cube'
        freq_arr = (*obs.baseline_info).freq
        delta_uv=obs.kpix
        uv_arr = (findgen(dimension)-dimension/2)*delta_uv
        
        uv_locs = findgen(101)*4.-200.
        eor_uvf = eor_sim(uv_locs, uv_locs, freq_arr, flat_sigma = flat_sigma, no_distrib = no_distrib, delta_power = delta_power, delta_uv_loc = delta_uv_loc)
        IF ~Keyword_Set(no_save) THEN save,filename=coarse_input_model_filepath, eor_uvf, uv_locs, freq_arr, /compress
        
        time0 = systime(1)        
        eor_uvf_cube = eor_sim(uv_arr, uv_arr, freq_arr, flat_sigma = flat_sigma, no_distrib = no_distrib, delta_power = delta_power, delta_uv_loc = delta_uv_loc)       
        time1 = systime(1)
        print, 'time for eor modelling: ' + number_formatter(time1-time0)
        if n_elements(model_uvf_cube) gt 0 then model_uvf_cube = model_uvf_cube + temporary(eor_uvf_cube) $
        else model_uvf_cube = temporary(eor_uvf_cube)
      endif
      
      ;; model cube assumed to be Stokes I
      switch n_pol of
        4:(*model_uvf_arr[3])[*]=0.
        3:(*model_uvf_arr[2])[*]=0.
        2:(*model_uvf_arr[1])[*]=model_uvf_cube/2.
        1:(*model_uvf_arr[0])[*]=model_uvf_cube/2.
      endswitch
      
      undefine, model_uvf_cube
      
    endif
    
    if n_elements(source_model_uv_arr) gt 0 then begin
      if n_elements(model_uvf_arr) gt 0 then begin
        FOR pol_i=0,n_pol-1 DO *model_uv_arr[pol_i]+=*source_model_uv_arr[pol_i]
      endif else model_uvf_arr = Pointer_copy(source_model_uv_arr)
      undefine_fhd, source_model_uv_arr
    endif
    
    if n_elements(model_uvf_arr) eq 0 then begin
      print, 'No input model (image cube, model_uvf or sources)'
      error=1
      RETURN,Ptrarr(n_pol)
    endif
    
    model_uvf = *model_uvf_arr[0]
    IF ~Keyword_Set(no_save) THEN save,filename=input_model_filepath, model_uvf, uv_arr, freq_arr, /compress
    undefine, model_uvf
    
    vis_dimension=N_Elements(params)
    
    vis_model_arr = Ptrarr(n_pol,/allocate)
    for pol_i=0,n_pol-1 do *vis_model_arr[pol_i]=Complexarr(n_freq,vis_dimension)
    
    time0=systime(1)
    for fi=0, n_freq-1 do begin
      if max([(*flag_arr[0])[fi,*], (*flag_arr[1])[fi,*]]) lt 1 then continue
      
      this_flag_ptr = Ptrarr(n_pol,/allocate)
      this_model_uv = Ptrarr(n_pol,/allocate)
      for pol_i=0,n_pol-1 do begin
        *this_flag_ptr[pol_i]=intarr(n_freq, vis_dimension)
        (*this_flag_ptr[pol_i])[fi,*] = (*flag_arr[pol_i])[fi,*]
        
        *this_model_uv[pol_i] = (*model_uvf_arr[pol_i])[*,*,fi]
      endfor
      
      if max(abs(*this_model_uv[0])) eq 0 and max(abs(*this_model_uv[1])) eq 0 then continue
      
      this_model_ptr=vis_source_model(0,obs,status_str,psf,params,this_flag_ptr,model_uv_arr=this_model_uv,$
        timing=model_timing,silent=silent,error=error,_Extra=extra)
      print, 'model loop num, timing(s):'+ number_formatter(fi) + ' , ' + number_formatter(model_timing)
      
      for pol_i=0,n_pol-1 do (*vis_model_arr[pol_i])[fi,*] = (*this_model_ptr[pol_i])[fi,*]
      
      undefine_fhd, this_flag_ptr, this_model_ptr, this_model_uv
    endfor
    undefine_fhd, model_uvf_arr
    time1=systime(0)
    print, 'model visibility timing(s):'+ number_formatter(time1-time0)
    
    fhd_save_io,status_str,flag_arr,var='flag_arr',/compress,file_path_fhd=file_path_fhd,no_save=no_save,_Extra=extra
ENDIF ELSE BEGIN
    vis_model_arr=Ptrarr(n_pol)
    FOR pol_i=0,n_pol-1 DO BEGIN
        fhd_save_io,status_str,vis_model_ptr,var='vis_ptr',/restore,file_path_fhd=file_path_fhd,obs=obs_out,pol_i=pol_i,_Extra=extra
        vis_model_arr[pol_i]=vis_model_ptr
    ENDFOR 
ENDELSE
RETURN,vis_model_ptr
END