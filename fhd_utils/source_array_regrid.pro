FUNCTION source_array_regrid,component_array,obs,hpx_cnv=hpx_cnv,source_peak=source_peak,_Extra=extra

ncomp=N_Elements(component_array)
dimension=obs.dimension
elements=obs.elements

source_image=source_image_generate(component_array,obs,resolution=16.,pol=4,/divide_pixel_area)

IF Keyword_Set(hpx_cnv) THEN BEGIN
    source_image_hpx=healpix_cnv_apply(source_image,hpx_cnv)
    hpx_i=where(source_image_hpx,n_pix_use)
    source_vals=source_image_hpx[hpx_i]
    IF size(hpx_cnv,/type) EQ 10 THEN BEGIN 
        hpx_inds=(*hpx_cnv).inds 
        nside=(*hpx_cnv).nside
    ENDIF ELSE BEGIN
        hpx_inds=hpx_cnv.inds ;allow to be pointer type
        nside=hpx_cnv.nside
    ENDELSE
        
    hpx_inds=hpx_inds[hpx_i]
    pix2vec_ring,nside,hpx_inds,pix_coords
    vec2ang,pix_coords,pix_dec,pix_ra,/astro
    ad2xy,pix_ra,pix_dec,obs.astr,xvals,yvals
ENDIF ELSE BEGIN
    x_arr=meshgrid(dimension,elements,1)
    y_arr=meshgrid(dimension,elements,2)
    pix_i=where(source_image GT 0,n_pix_use)
    source_vals=source_image[pix_i]
    xvals=x_arr[pix_i]
    yvals=y_arr[pix_i]
ENDELSE
IF Keyword_Set(source_peak) THEN BEGIN
    xvals=[source_peak.x,xvals]
    yvals=[source_peak.y,yvals]
    peak_flux=interpolate(source_image,source_peak.x,source_peak.y,cubic=-0.5)
    source_vals=[peak_flux,source_vals]
ENDIF

n_use=N_Elements(xvals)


RETURN,component_array_out
END