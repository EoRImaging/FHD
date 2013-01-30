FUNCTION mrc_catalog_read,astr,names=names,file_path=file_path
;MRC catalog is 99% complete to 1 Jy. Catalog is at 408MHz
;filename='MRC full radio catalog.fits'
;data_dir='DATA'
;file_path=filepath('MRC full radio catalog.fits',root=rootdir('mwa'),subdir='DATA')
catalog=mrdfits(file_path,1,header,/silent)
flux=catalog.flux
ra=catalog.ra
dec=catalog.dec
names=catalog.name
flux_error=catalog.flux_error
;The Crab nebula appears to be missing from the catalog, so add it in by hand:
flux=[flux/1000.,1526./2.8985507]
ra=[ra,ten(5,34,31.94)*15.]
dec=[dec,ten(22,0,52.2)]
names=[names,'Crab']
flux_error=[flux_error/1000.,-1]
ns=N_Elements(flux)
source_comp_init,mrc_cat,n_sources=ns

IF Keyword_Set(astr) THEN BEGIN
    ra0=astr.crval[0]
    dec0=astr.crval[1]
    angs=angle_difference(dec0,ra0,dec,ra,/degree)
    i_use=where(Abs(angs) LE 45,n_use)
    IF n_use GT 0 THEN BEGIN
        ad2xy,ra[i_use],dec[i_use],astr,x_arr,y_arr
        mrc_cat[i_use].x=x_arr
        mrc_cat[i_use].y=y_arr
    ENDIF
ENDIF 

mrc_cat.ra=ra
mrc_cat.dec=dec
mrc_cat.flux.I=flux
mrc_cat.flux.xx=flux/2.
mrc_cat.flux.yy=flux/2.
RETURN,mrc_cat

END