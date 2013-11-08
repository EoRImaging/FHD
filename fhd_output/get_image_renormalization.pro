function get_image_renormalization,weights_arr=weights_arr,beam_base_out=beam_base_out,beam_correction_out=beam_correction_out,$
      image_filter_fn=image_filter_fn,pad_uv_image=pad_uv_image,degpix=degpix,_Extra=extra
  ; Use the weights to renormalize the image to units of Jy/beam
  
  n_pol=size(weights_arr,/dimension)
  n_pol=n_pol[0]

  weight_images = Ptrarr(n_pol,/allocate)
  for pol_i=0,n_pol-1 do begin
    *weight_images[pol_i]=dirty_image_generate(*weights_arr[pol_i],degpix=degpix,weights=*weights_arr[pol_i],$
        image_filter_fn=image_filter_fn,pad_uv_image=pad_uv_image,_Extra=extra)*(*beam_correction_out[pol_i])
  endfor
  
  weight_stokes=stokes_cnv(weight_images,beam=beam_base_out) ;NOTE one factor of the beam already corrected for
  dims=size(*weight_stokes[0],/dimension)
  stokes_I_peak = (*weight_stokes[0])[dims[0]/2,dims[1]/2]
  
  ; Now what we should measure in stokes I
  ;model_stokes_I_peak = 1/(*beam_base_out[0])[dims[0]/2,dims[1]/2] + 1/(*beam_base_out[1])[dims[0]/2,dims[1]/2]
  ; I'm still not sure about this, but it seems to work. I think the other factors are already accounted for elsewhere.
  model_stokes_I_peak = 2
  
  renorm_factor = model_stokes_I_peak / stokes_I_peak
  
return,renorm_factor
end