FUNCTION healpix_cnv_apply,image,hpx_cnv,timing=timing,double_precision=double_precision
t0=Systime(1)
IF N_Elements(double_precision) EQ 0 THEN double_precision=0

dimension=(size(image,/dimension))[0]
elements=(size(image,/dimension))[1]

image_vector=reform(image,Float(dimension)*elements)
IF size(hpx_cnv,/type) EQ 10 THEN hpx_map_size=N_Elements((*hpx_cnv).inds) $
    ELSE hpx_map_size=N_Elements(hpx_cnv.inds)
IF Keyword_Set(double_precision) THEN hpx_map=Dblarr(hpx_map_size) ELSE hpx_map=Fltarr(hpx_map_size)
SPRSAX2,hpx_cnv,image_vector,hpx_map,transpose=1,mask=0,double=double_precision

timing=Systime(1)-t0
RETURN,hpx_map
END
