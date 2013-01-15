FUNCTION healpix_cnv_apply,image,hpx_cnv,timing=timing
t0=Systime(1)

dimension=(size(image,/dimension))[0]
elements=(size(image,/dimension))[1]

image_vector=reform(image,Float(dimension)*elements)
hpx_map=Fltarr(N_Elements(hpx_cnv.inds))
SPRSAX2,hpx_cnv,image_vector,hpx_map,transpose=1,mask=0

timing=Systime(1)-t0
RETURN,hpx_map
END