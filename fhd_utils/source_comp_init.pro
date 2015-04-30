FUNCTION source_comp_init,source_comp,n_sources=n_sources,xvals=xvals,yvals=yvals,frequency=frequency,$
    ra=ra,dec=dec,flux=flux,id=id,StoN=StoN,alpha=alpha,extend=extend,gain_factor=gain_factor,overwrite=overwrite

IF N_Elements(n_sources) EQ 0 THEN $
    n_sources=Max([N_Elements(xvals),N_Elements(yvals),N_Elements(ra),N_Elements(dec),1.])

flux_struct={flux,xx:0.,yy:0.,xy:0.,yx:0.,I:0.,Q:0.,U:0.,V:0.}
;flux order is 0-3: xx, yy, xy, yx in apparent brightness; 4-7: I, Q, U, V in sky brightness
;flag type codes are 0: no flag, 1: low confidence 2: sidelobe contamination
struct_base={id:-1L,x:0.,y:0.,ra:0.,dec:0.,ston:0.,freq:100.,alpha:0.,gain:1.,flag:0,extend:Ptr_new(),flux:flux_struct}
source_comp_new=Replicate(struct_base,n_sources>1)

IF Keyword_Set(xvals) THEN source_comp_new.x=xvals
IF Keyword_Set(yvals) THEN source_comp_new.y=yvals
IF Keyword_Set(ra) THEN source_comp_new.ra=ra
IF Keyword_Set(dec) THEN source_comp_new.dec=dec
IF Keyword_Set(flux) THEN source_comp_new.flux.I=flux 
IF Keyword_Set(StoN) THEN source_comp_new.ston=StoN ;signal to noise
IF Keyword_Set(id) THEN source_comp_new.id=id ;unique source id
IF Keyword_Set(alpha) THEN source_comp_new.alpha=alpha ;spectral index
IF Keyword_Set(extend) THEN source_comp_new.extend=extend ;extended source component list (not an extended source if Ptr_valid(source_comp[si].extend)=0)
IF Keyword_Set(frequency) THEN IF frequency GT 1E5 THEN source_comp_new.freq=frequency/1E6 ELSE source_comp_new.freq=frequency ;frequency in MHz
IF Keyword_Set(gain_factor) THEN source_comp_new.gain=gain_factor

IF N_Elements(source_comp) GT 0 AND ~Keyword_Set(overwrite) THEN source_comp=[source_comp,source_comp_new] ELSE source_comp=source_comp_new
RETURN,source_comp
END  