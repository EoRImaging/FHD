FUNCTION filter_uv_hanning,image_uv,obs=obs,psf=psf,params=params,name=name,$
    weights=weights,filter=filter,return_name_only=return_name_only,_Extra=extra
name='hanning'
IF Keyword_Set(return_name_only) THEN RETURN,image_uv
IF N_Elements(weights) NE N_Elements(image_uv) THEN RETURN,image_uv
dimension=(size(image_uv,/dimension))[0]
elements=(size(image_uv,/dimension))[1]

filter_use=fltarr(dimension,elements)
radial_smooth=dimension/40.
val0=0.

xv=meshgrid(dimension,elements,1)-dimension/2
yv=meshgrid(dimension,elements,2)-elements/2
radial_map=Sqrt(xv^2.+yv^2.)

ft_kernel = exp(-(radial_map/radial_smooth)^2.)

filter_use = fft_shift(fft(fft_shift(weights)))
filter_use = filter_use * ft_kernel
filter_use = Abs(fft_shift(fft(fft_shift(filter_use),1)))

thresh=Max(filter_use)/1e4 ; 10e-10
filter_use=weight_invert(filter_use)<(1./thresh)

IF Max(filter_use) EQ 0 THEN RETURN,image_uv 

filter_use *= hanning(dimension,elements)

wts_i=where(weights,n_wts)
IF n_wts GT 0 THEN filter_use=filter_use*mean(weights[wts_i])/Mean(weights[wts_i]*filter_use[wts_i]) $
    ELSE filter_use=filter_use*mean(weights)/Mean(weights*filter_use)

IF Ptr_valid(filter) THEN *filter=filter_use

;filter/=Mean(filter) ;preserve mean value
image_uv_filtered=image_uv*filter_use
RETURN,image_uv_filtered
END
