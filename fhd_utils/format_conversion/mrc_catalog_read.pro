FUNCTION mrc_catalog_read,obs,names=names,file_path=file_path,frequency=frequency
;MRC catalog is 99% complete to 1 Jy. Catalog is at 408MHz
;filename='MRC full radio catalog.fits'
;data_dir='DATA'
;file_path=filepath('MRC full radio catalog.fits',root=rootdir('mwa'),subdir='DATA')
;catalog=mrdfits(file_path,1,header,/silent,columns=[1,2,3,4])
sav_test=0>Strpos(StrLowCase(file_path),'.sav')<1
IF sav_test THEN BEGIN
  mrc_cat=getvar_savefile(file_path,'catalog') 
  ra=mrc_cat.ra
  dec=mrc_cat.dec
ENDIF ELSE BEGIN
  Fitsfast,catalog,/read,file_path=file_path
  flux=Reform(catalog[3,*])
  ra=Reform(catalog[1,*])
  dec=Reform(catalog[2,*])
  flux_error=Reform(catalog[4,*])
  ;flux=catalog.flux
  ;ra=catalog.ra
  ;dec=catalog.dec
  ;;names=catalog.name
  ;flux_error=catalog.flux_error
  catalog_freq=408. ;MHz
  IF N_Elements(frequency) EQ 0 THEN frequency=catalog_freq
  spectral_index=-0.8
  flux_scale=(frequency/catalog_freq)^spectral_index
  ;The Crab nebula appears to be missing from the catalog, so add it in by hand:
  flux=[flux/1000.,1526.]*flux_scale
  ra=[ra,ten(5,34,31.94)*15.]
  dec=[dec,ten(22,0,52.2)]
  ;names=[names,'Crab']
  flux_error=[flux_error/1000.,-1]
  ns=N_Elements(flux)
  mrc_cat=source_comp_init(n_sources=ns,freq=catalog_freq,alpha=spectral_index)
  
  mrc_cat.ra=ra
  mrc_cat.dec=dec
  mrc_cat.flux.I=flux
  mrc_cat.flux.xx=flux/2.
  mrc_cat.flux.yy=flux/2.

ENDELSE

IF Keyword_Set(obs) THEN BEGIN
    ra0=obs.obsra
    dec0=obs.obsdec
    angs=angle_difference(dec0,ra0,dec,ra,/degree)
    i_use=where(Abs(angs) LE 60.,n_use)
    IF n_use GT 0 THEN BEGIN
        apply_astrometry, obs, ra_arr=ra[i_use], dec_arr=dec[i_use], x_arr=x_arr, y_arr=y_arr, /ad2xy, /refraction
        mrc_cat[i_use].x=x_arr
        mrc_cat[i_use].y=y_arr
    ENDIF
ENDIF 

RETURN,mrc_cat

END