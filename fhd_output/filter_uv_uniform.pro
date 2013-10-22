FUNCTION filter_uv_uniform,image_uv,name=name,weights=weights,filter=filter,_Extra=extra
name='uniform'
;IF N_Elements(filter) EQ N_Elements(image_uv) THEN RETURN,image_uv*filter
IF N_Elements(weights) NE N_Elements(image_uv) THEN RETURN,image_uv
dimension=(size(image_uv,/dimension))[0]
elements=(size(image_uv,/dimension))[1]

filter_use=fltarr(dimension,elements)
radial_smooth=5.
val0=0.

xv=meshgrid(dimension,elements,1)-dimension/2
yv=meshgrid(dimension,elements,2)-elements/2

radial_map=Sqrt(xv^2.+yv^2.)
rad_hist=histogram(radial_map,min=1,/binsize,reverse_ind=ri)
rad_bin=where(rad_hist,n_bin)
rad_vals=fltarr(n_bin)
FOR i=1L,n_bin-1 DO BEGIN
    bin_i_start=(rad_bin[i]-radial_smooth)>0
    bin_i_end=(rad_bin[i]+radial_smooth)<(n_bin-1)
    inds=ri[ri[bin_i_start]:ri[bin_i_end+1]-1]
    weights0=Abs(weights[inds])
    ind_i_use=where(weights0 GT 0,n_use)
    IF n_use LT 3 THEN CONTINUE
    rad_vals[i]=Sqrt(Mean(weights0[ind_i_use]^2.));/(2.*!Pi*i)
;    IF val0 EQ 0 THEN val0=1./Median(weights0[ind_i_use])
;    val1=1./Median(weights0[ind_i_use])
;    
;    filter_use[inds]=(val1+val0)/2.
;    val0=val1
ENDFOR

rad_i_use=where(rad_vals)
thresh=Max(rad_vals)/100.
rad_vals_use=(1./rad_vals[rad_i_use])<(1./thresh)
;rad_vals_use=1./rad_vals[rad_i_use]

filter_use=Reform(interpol(rad_vals_use,rad_i_use,reform(radial_map,dimension*Float(elements))),dimension,elements)

;degree=7.
;fit=poly_fit(rad_i_use,rad_vals[rad_i_use],degree,measure_errors=1./Sqrt(rad_vals[rad_i_use]))
;FOR di=0.,degree DO filter_use+=fit[di]*radial_map^di

IF Max(filter_use) EQ 0 THEN RETURN,image_uv 

wts_i=where(weights,n_wts)
IF n_wts GT 0 THEN filter_use/=Mean(filter_use[wts_i]) ELSE filter_use/=Mean(filter_use)

IF Ptr_valid(filter) THEN *filter=filter_use

;filter/=Mean(filter) ;preserve mean value
image_uv_filtered=image_uv*filter_use
RETURN,image_uv_filtered
END