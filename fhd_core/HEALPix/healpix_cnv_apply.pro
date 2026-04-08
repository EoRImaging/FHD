FUNCTION healpix_cnv_apply,image,hpx_cnv,timing=timing
t0=Systime(1)

dimension=(size(image,/dimension))[0]
elements=(size(image,/dimension))[1]

IF size(hpx_cnv,/type) EQ 10 THEN hpx_cnv_inds=(*hpx_cnv).inds ELSE hpx_cnv_inds=hpx_cnv.inds

image_vector=reform(image,Float(dimension)*elements)
case size(image_vector,/type) of
  4: hpx_map=fltarr(N_Elements(hpx_cnv_inds))
  5: hpx_map=dblarr(N_Elements(hpx_cnv_inds))
  6: hpx_map=complex(fltarr(N_Elements(hpx_cnv_inds)))
  9: hpx_map=dcomplex(dblarr(N_Elements(hpx_cnv_inds)))
  else: hpx_map=fltarr(N_Elements(hpx_cnv_inds))
endcase

SPRSAX2,hpx_cnv,image_vector,hpx_map,transpose=1,mask=0

timing=Systime(1)-t0
RETURN,hpx_map
END