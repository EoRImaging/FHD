PRO write_healpix_fits,file_path_img,hpx_vals,hpx_inds,weights=weights,nside=nside,full_sky=full_sky

IF N_Elements(full_sky) EQ 0 THEN full_sky=0
npix=N_Elements(hpx_vals)
IF N_Elements(weights) EQ 0 THEN hpx_weights=fltarr(npix)+1 ELSE hpx_weights=weights
npix_full=nside2npix(nside)
ring2nest, nside, hpx_inds, hpx_inds2

IF npix EQ npix_full THEN full_sky=1
IF Keyword_Set(full_sky) THEN BEGIN
    IF npix NE npix_full THEN BEGIN
        hpx_vals2=fltarr(npix_full)-1.6375e30 & hpx_vals2[hpx_inds2]=hpx_vals
        IF Keyword_Set(weights) THEN BEGIN 
            hpx_weights2=fltarr(npix_full) & hpx_weights2[hpx_inds2]=hpx_weights 
            hpx_struct={HDR:' ',signal:Temporary(hpx_vals2),n_obs:Temporary(hpx_weights2)}
        ENDIF ELSE hpx_struct={HDR:' ',signal:Temporary(hpx_vals2)}
    ENDIF ELSE BEGIN
        IF Keyword_Set(weights) THEN hpx_struct={HDR:' ',signal:hpx_vals,n_obs:hpx_weights} $
            ELSE hpx_struct={HDR:' ',signal:hpx_vals}
    ENDELSE
    write_fits_sb,file_path_img+'.fits',0,hpx_struct,/nest,coord='C',partial=1-full_sky,nside=nside
ENDIF ELSE BEGIN
    IF Keyword_Set(weights) THEN write_fits_cut4,file_path_img+'.fits',hpx_inds2,hpx_vals,hpx_weights,nside=nside,/nested,coord='C'
    write_fits_cut4,file_path_img+'.fits',hpx_inds2,hpx_vals,nside=nside,/nested,coord='C'
ENDELSE

END