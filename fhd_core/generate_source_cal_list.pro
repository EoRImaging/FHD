FUNCTION generate_source_cal_list,obs,psf,catalog_path=catalog_path
catalog=getvar_savefile(catalog_path,'catalog')

astr=obs.astr
dimension=obs.dimension
elements=obs.elements
degpix=obs.degpix

FoV=!RaDeg/obs.kpix
freq_arr=psf.freq
freq_use=Mean(freq_arr)
spectral_index=-0.8

ra0=astr.crval[0]
dec0=astr.crval[1]
angs=angle_difference(dec0,ra0,catalog.dec,catalog.ra,/degree)
i_use=where(Abs(angs) LE FoV/2.,n_use)

source_comp_init,source_list,n_sources=n_use
IF n_use GT 0 THEN BEGIN
    catalog=catalog[i_use]
    source_list.ra=catalog.ra
    source_list.dec=catalog.dec
    ad2xy,source_list.ra,source_list.dec,astr,x_arr,y_arr
    source_list.x=x_arr
    source_list.y=y_arr
    source_list.flux.I=catalog.flux*(freq_use/catalog.freq)^spectral_index
    source_list.alpha=spectral_index
    source_list.StoN=(catalog.flux/(catalog.flux_error>1.))>100.
    
    src_use=where((x_arr GE 0) AND (x_arr LE dimension-1) AND (y_arr GE 0) AND (y_arr LE elements-1),n_src_use)
    IF n_src_use GT 0 THEN source_list=source_list[src_use]
ENDIF

RETURN,source_list
END