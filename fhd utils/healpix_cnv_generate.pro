FUNCTION healpix_cnv_generate,obs,nside=nside,mask=mask,radius=radius,restore_last=restore_last

vis_path_default,data_directory,filename,file_path,obs=obs
IF Keyword_Set(restore_last) AND (file_test(file_path+'_hpxcnv'+'.sav') EQ 0) THEN BEGIN 
    print,file_path+'_hpxcnv'+'.sav' +' Not found. Recalculating.' 
    restore_last=0
ENDIF
IF Keyword_Set(restore_last) THEN BEGIN
    print,'Saved beam model restored'
    restore,file_path+'_hpxcnv'+'.sav'
    RETURN,hpx_cnv
ENDIF

astr=obs.astr
dimension=obs.dimension
elements=obs.elements
IF N_Elements(radius) EQ 0 THEN radius=obs.degpix*(dimension>elements)/4.
;all angles in DEGREES
;uses RING index scheme
IF not Keyword_Set(nside) THEN BEGIN
    pix_sky=4.*!Pi*!RaDeg^2./Product(astr.cdelt)
    Nside=2.^(Ceil(ALOG(Sqrt(pix_sky/12.))/ALOG(2))) ;=1024. for 0.1119 degrees/pixel
ENDIF
npix=nside2npix(nside)


ang2vec,obs.obsdec,obs.obsra,cen_coords,/astro

Query_disc,nside,cen_coords,radius,hpx_inds0,ninds,/deg

pix2vec_ring,nside,hpx_inds0,pix_coords
vec2ang,pix_coords,pix_dec,pix_ra,/astro
ad2xy,pix_ra,pix_dec,astr,xv_hpx,yv_hpx

;NOTE: slightly more restrictive boundary here ('LT' and 'GT' instead of 'LE' and 'GE') 
pix_i_use=where((xv_hpx GT 0) AND (xv_hpx LT dimension-1) AND (yv_hpx GT 0) AND (yv_hpx LT elements-1),n_hpx_use)
xv_hpx=xv_hpx[pix_i_use]
yv_hpx=yv_hpx[pix_i_use]
IF Keyword_Set(mask) THEN BEGIN
    hpx_mask00=mask[Floor(xv_hpx),Floor(yv_hpx)]
    hpx_mask01=mask[Floor(xv_hpx),Ceil(yv_hpx)]
    hpx_mask10=mask[Ceil(xv_hpx),Floor(yv_hpx)]
    hpx_mask11=mask[Ceil(xv_hpx),Ceil(yv_hpx)]
    hpx_mask=Temporary(hpx_mask00)*Temporary(hpx_mask01)*Temporary(hpx_mask10)*Temporary(hpx_mask11)
    pix_i_use2=where(hpx_mask,n_hpx_use)
    xv_hpx=xv_hpx[pix_i_use2]
    yv_hpx=yv_hpx[pix_i_use2]
    pix_i_use=pix_i_use[pix_i_use2]
ENDIF
hpx_inds=hpx_inds0[pix_i_use]



hpx_cnv={nside:nside}

save,hpx_cnv,filename=file_path+'_hpxcnv'+'.sav'
RETURN,hpx_cnv
END