PRO healpix_view,hpx_vals,hpx_inds,nside=nside,hpx_cnv=hpx_cnv,color_table=color_table,lon=lon,lat=lat

IF not Keyword_Set(color_table) THEN color_table=0.1

IF Keyword_Set(hpx_cnv) THEN BEGIN
    nside=hpx_cnv.nside
    hpx_inds=hpx_cnv.inds
;    IF size(hpx_vals,/n_dimension) EQ 2 THEN hpx_vals_use=healpix_cnv_apply(hpx_vals,hpx_cnv)
ENDIF

IF (N_Elements(lon) EQ 0) OR (N_Elements(lat) EQ 0) THEN BEGIN
    pix2vec_ring,nside,hpx_inds,pix_coords
    vec2ang,pix_coords,pix_dec,pix_ra,/astro
    IF N_Elements(lat) EQ 0 THEN lat_use=Median(pix_dec) ELSE lat_use=lat
    IF N_Elements(lon) EQ 0 THEN lon_use=ATan(Median(Tan(pix_ra*!DtoR)))*!Radeg ELSE lon_use=lon
ENDIF ELSE BEGIN lon_use=lon & lat_use=lat & ENDELSE

file_path_img=rootdir('mwa')+'Healpix_tmp'
write_fits_cut4,file_path_img+'.fits',hpx_inds,hpx_vals,/ring,Coords='C',nside=nside
healpix_image,file_path_img,moll=1,cart=0,gnom=0,orth=1,ps_write=0,png_write=1,silent=1,$
        lon=lon_use,lat=lat_use,min=min(hpx_vals),max=max(hpx_vals),/half,color_table=color_table
END