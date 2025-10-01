function get_image_renormalization,obs,weights_arr=weights_arr,beam_base=beam_base,degpix=degpix,psf=psf,params=params,$
      image_filter_fn=image_filter_fn,pad_uv_image=pad_uv_image,filter_arr=filter_arr,$
      antialias=antialias,baseline_threshold=baseline_threshold,_Extra=extra
  ; Use the weights to renormalize the image to units of Jy/sr
  ; Note: include keyword baseline_threshold even though it is not used, to strip it from _Extra if present
  
n_pol=obs.n_pol
IF image_filter_fn EQ 'filter_uv_optimal' THEN RETURN, make_array(n_pol, value=1.)
dimension=obs.dimension
elements=obs.elements
IF N_Elements(filter_arr) EQ 0 THEN filter_arr=intarr(n_pol)

renorm_factor=fltarr(n_pol)
FOR pol_i=0,n_pol-1 DO BEGIN    
    renorm_factor[pol_i]=1./(dirty_image_generate(*weights_arr[pol_i],degpix=degpix,obs=obs,psf=psf,params=params,$
        weights=*weights_arr[pol_i],pad_uv_image=pad_uv_image,image_filter_fn=image_filter_fn,$
        filter=filter_arr[pol_i],antialias=antialias,beam_ptr=beam_base[pol_i],_Extra=extra))[dimension/2.,elements/2.]
    renorm_factor[pol_i]*=((*beam_base[pol_i])[obs.obsx,obs.obsy])^2.
    renorm_factor[pol_i]/=(degpix*!DtoR)^2. ; Convert from Jy/pixel to Jy/sr
ENDFOR

RETURN,renorm_factor

end