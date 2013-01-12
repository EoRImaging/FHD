PRO source_comp_init,source_comp,n_sources=n_sources,xvals=xvals,yvals=yvals,$
    ra=ra,dec=dec,flux=flux,id=id,StoN=StoN,alpha=alpha,extend=extend,overwrite=overwrite

IF N_Elements(n_sources) EQ 0 THEN $
    n_sources=Max([N_Elements(xvals),N_Elements(yvals),N_Elements(ra),N_Elements(dec),1.])

flux_struct={flux,xx:0.,yy:0.,xy:0.,yx:0.,I:0.,Q:0.,U:0.,V:0.}
;flux order is 0-3: xx, yy, xy, yx in apparent brightness; 4-7: I, Q, U, V in sky brightness
struct_base={source_component,id:-1,x:0.,y:0.,ra:0.,dec:0.,ston:0.,alpha:0.,extend:0L,flux:flux_struct}
source_comp_new=Replicate(struct_base,n_sources)

IF Keyword_Set(xvals) THEN source_comp_new.x=xvals
IF Keyword_Set(yvals) THEN source_comp_new.y=yvals
IF Keyword_Set(ra) THEN source_comp_new.ra=ra
IF Keyword_Set(dec) THEN source_comp_new.dec=dec
IF Keyword_Set(flux) THEN source_comp_new.flux=flux 
IF Keyword_Set(StoN) THEN source_comp_new.ston=StoN ;signal to noise
IF Keyword_Set(id) THEN source_comp_new.id=id ;unique source id
IF Keyword_Set(alpha) THEN source_comp_new.alpha=alpha ;spectral index
IF Keyword_Set(extend) THEN source_comp_new.extend=extend ;extended source flag

IF N_Elements(source_comp) GT 0 AND not Keyword_Set(overwrite) THEN source_comp=[source_comp,source_comp_new] ELSE source_comp=source_comp_new
END  