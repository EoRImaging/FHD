FUNCTION source_array_regrid,component_array,obs,hpx_cnv=hpx_cnv,source_peak=source_peak,_Extra=extra

ncomp=N_Elements(component_array)
dimension=obs.dimension
elements=obs.elements

flux_ref = Total(component_array.flux.I)
source_image=source_image_generate(component_array,obs,resolution=16.,pol=4,/divide_pixel_area)
weight_arr=component_array
FOR pol_i=0,7 DO weight_arr.flux.(pol_i)=0.
weight_arr.flux.I=1.
weight_image=source_image_generate(weight_arr,obs,resolution=16.,pol=4,/divide_pixel_area)

IF Keyword_Set(hpx_cnv) THEN BEGIN
    source_image_hpx=healpix_cnv_apply(source_image,hpx_cnv)
    weight_hpx=healpix_cnv_apply(weight_image,hpx_cnv)
    candidate_i=where(weight_hpx GE 1,n_candidates)
    source_vals=source_image_hpx[candidate_i]
    IF size(hpx_cnv,/type) EQ 10 THEN BEGIN 
        hpx_inds=(*hpx_cnv).inds 
        nside=(*hpx_cnv).nside
    ENDIF ELSE BEGIN
        hpx_inds=hpx_cnv.inds ;allow to be pointer type
        nside=hpx_cnv.nside
    ENDELSE
        
    hpx_inds=hpx_inds[candidate_i]
    pix2vec_ring,nside,hpx_inds,pix_coords
    vec2ang,pix_coords,pix_dec,pix_ra,/astro
    ad2xy,pix_ra,pix_dec,obs.astr,xvals,yvals
ENDIF ELSE BEGIN
    x_arr=meshgrid(dimension,elements,1)
    y_arr=meshgrid(dimension,elements,2)
    pix_i=where(source_image GT 0,n_pix_use)
    candidate_i=where(weight_image[pix_i] GE 1,n_candidates)
    source_vals=source_image[pix_i]
    xvals=x_arr[pix_i]
    yvals=y_arr[pix_i]
ENDELSE
flux_new = Total(source_vals)
flux_norm = flux_ref/flux_new
IF Keyword_Set(source_peak) THEN BEGIN
    xvals=[source_peak.x,xvals]
    yvals=[source_peak.y,yvals]
    peak_flux=interpolate(source_image,source_peak.x,source_peak.y,cubic=-0.5)
    source_vals=[peak_flux,source_vals]
    n_candidates+=1
ENDIF

n_use=N_Elements(xvals)

candidate_matrix=Fltarr(n_use,n_candidates)
FOR c_i=0,


RETURN,component_array_out
END