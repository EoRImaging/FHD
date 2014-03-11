PRO write_healpix_fits,file_path_img,hpx_vals,hpx_inds,weights=weights,nside=nside

IF N_Elements(weights) EQ 0 THEN hpx_weights=fltarr(N_Elements(hpx_vals))+1 ELSE hpx_weights=weights
npix=nside2npix(nside)
hpx_vals2=fltarr(npix)-1.6375e30 & hpx_vals2[hpx_inds]=hpx_vals
IF Keyword_Set(weights) THEN BEGIN
    hpx_weights2=fltarr(npix) & hpx_weights2[hpx_inds]=hpx_weights
    hpx_struct={HDR:' ',signal:Temporary(hpx_vals2),n_obs:Temporary(hpx_weights2)}
ENDIF ELSE hpx_struct={HDR:' ',signal:Temporary(hpx_vals2)}
write_fits_sb,file_path_img+'.fits',0,hpx_struct,/ring,coord='C'

END