PRO healpix_view,hpx_vals,hpx_inds,nside=nside,hpx_cnv=hpx_cnv,color_table=color_table,lon=lon,lat=lat, filepath = filepath

IF not Keyword_Set(color_table) THEN color_table=0.1

IF Keyword_Set(hpx_cnv) THEN BEGIN
    nside=hpx_cnv.nside
    hpx_inds=hpx_cnv.inds
;    IF size(hpx_vals,/n_dimension) EQ 2 THEN hpx_vals_use=healpix_cnv_apply(hpx_vals,hpx_cnv)
ENDIF
IF N_Elements(nside) EQ 0 THEN nside=npix2nside(N_Elements(hpx_vals))
IF N_Elements(hpx_inds) EQ 0 THEN hpx_inds=L64indgen(N_Elements(hpx_vals))

IF (N_Elements(lon) EQ 0) OR (N_Elements(lat) EQ 0) THEN BEGIN
    pix2vec_ring,nside,hpx_inds,pix_coords
    vec2ang,pix_coords,pix_dec,pix_ra,/astro
    IF N_Elements(lat) EQ 0 THEN lat_use=Median(pix_dec) ELSE lat_use=lat
    IF N_Elements(lon) EQ 0 THEN lon_use=ATan(Median(Tan(pix_ra*!DtoR)))*!Radeg ELSE lon_use=lon
ENDIF ELSE BEGIN lon_use=lon & lat_use=lat & ENDELSE

if n_elements(filepath) ne 0 then begin
   if file_test(filepath, /directory) then begin
      ;; this is a directory, add default filename
      file_path_img = file_dirname(filepath, /mark_directory) + file_basename(filepath, /mark_directory) + $
                      path_sep + 'Healpix_tmp.fits'
   endif else begin
      ;; this is a filename, if no directory add default path, ensure extension is .fits
      froot = file_dirname(filepath, /mark_directory)
      if froot eq '.' then froot = rootdir('mwa')
      
      fbase = file_basename(filepath)
      UPNAME=StrUpCase(fbase)
      pfits=strpos(UPNAME,'.FIT')
      IF pfits EQ -1 THEN file_path_img = froot + fbase + '.fits' ELSE file_path_img = froot + fbase
   endelse
endif else file_path_img=rootdir('mwa')+'Healpix_tmp.fits'

write_fits_cut4,file_path_img,hpx_inds,hpx_vals,/ring,Coords='C',nside=nside
healpix_image,file_path_img,moll=1,cart=0,gnom=0,orth=1,ps_write=0,png_write=1,silent=1,$
        lon=lon_use,lat=lat_use,min=min(hpx_vals),max=max(hpx_vals),/half,color_table=color_table
END
