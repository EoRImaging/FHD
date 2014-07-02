function get_image_renormalization,obs,weights_arr=weights_arr,beam_base=beam_base,degpix=degpix,psf=psf,params=params,$
      image_filter_fn=image_filter_fn,pad_uv_image=pad_uv_image,filter_arr=filter_arr,antialias=antialias,_Extra=extra
  ; Use the weights to renormalize the image to units of Jy/beam
  
n_pol=obs.n_pol
dimension=obs.dimension
elements=obs.elements
IF N_Elements(filter_arr) EQ 0 THEN filter_arr=intarr(n_pol)

normalization_arr=fltarr(n_pol)
FOR pol_i=0,n_pol-1 DO BEGIN    
    normalization_arr[pol_i]=1./(dirty_image_generate(*weights_arr[pol_i],degpix=degpix,obs=obs,psf=psf,params=params,$
        weights=*weights_arr[pol_i],pad_uv_image=pad_uv_image,image_filter_fn=image_filter_fn,$
        filter=filter_arr[pol_i],antialias=antialias,_Extra=extra))[dimension/2.,elements/2.]
    normalization_arr[pol_i]*=((*beam_base[pol_i])[obs.obsx,obs.obsy])^2.
ENDFOR
renorm_factor=mean(normalization_arr[0:n_pol-1])
RETURN,renorm_factor

end