FUNCTION filter_uv_radial,image_uv,name=name,weights=weights,filter=filter,_Extra=extra
name='radial'
IF N_Elements(filter) EQ N_Elements(image_uv) THEN RETURN,image_uv*filter
IF N_Elements(weights) NE N_Elements(image_uv) THEN RETURN,image_uv
dimension=(size(image_uv,/dimension))[0]
elements=(size(image_uv,/dimension))[1]

radial_map=fft_shift(dist(dimension,elements))
rad_hist=histogram(radial_map,min=1,/binsize,reverse_ind=ri)
rad_bin=where(rad_hist,n_bin)

filter=fltarr(dimension,elements)
radial_smooth=5.
val0=0.
FOR i=0L,n_bin-1 DO BEGIN
    bin_i_start=(rad_bin[i]-radial_smooth)>0
    bin_i_end=(rad_bin[i]+radial_smooth)<(n_bin-1)
    inds=ri[ri[bin_i_start]:ri[bin_i_end+1]-1]
    weights0=Abs(weights[inds])
    ind_i_use=where(weights0 GT 0,n_use)
    IF n_use EQ 0 THEN CONTINUE
    IF val0 EQ 0 THEN val0=1./Median(weights0[ind_i_use])
    val1=val0<(1./Median(weights0[ind_i_use]))
    
    filter[inds]=Sqrt(val1*val0)
    val0=val1
ENDFOR

IF Max(filter) EQ 0 THEN RETURN,image_uv ELSE filter/=Mean(filter)

;filter/=Mean(filter) ;preserve mean value
image_uv_filtered=image_uv*filter
RETURN,image_uv_filtered
END