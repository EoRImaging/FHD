FUNCTION beam_area_fit,obs,weights_arr,antialias=antialias,image_filter_fn=image_filter_fn,pad_uv_image=pad_uv_image,$
    file_path_fhd=file_path_fhd,filter_arr=filter_arr,renorm_factor=renorm_factor,width_fit=width_fit,_Extra=extra
    
n_pol=obs.n_pol
n_pol_use=n_pol<2
degpix=obs.degpix
dimension=obs.dimension
elements=obs.elements
approx_beam_fwhm=beam_width_calculate(obs,/fwhm)
box_dim=approx_beam_fwhm*2.+1
beam_fit_box=fltarr(box_dim,box_dim)
FOR pol_i=0,n_pol_use-1 DO BEGIN
    synth_beam=dirty_image_generate(*weights_arr[pol_i],degpix=degpix,antialias=antialias,image_filter_fn=image_filter_fn,pad_uv_image=pad_uv_image,$
        file_path_fhd=file_path_fhd,filter=filter_arr[pol_i],_Extra=extra)*renorm_factor
    beam_fit_box=synth_beam[dimension/2.-box_dim/2:dimension/2.+approx_beam_fwhm*5,elements/2.-approx_beam_fwhm*5:elements/2.+approx_beam_fwhm*5]
    gauss_beam=gauss2dfit(beam_fit_box,fit_params,/tilt)
    width_x=fit_params[2] ;gaussian width 
    width_y=fit_params[3] ;gaussian width
    theta=fit_params[6] ;angle in radians
    
    debug_point=1
ENDFOR

beam_area=!Pi*Product(width_arr,2)
IF Tag_exist(obs,'synth_beam') THEN BEGIN
    obs.synth_beam.width
ENDIF
RETURN,beam_area
END