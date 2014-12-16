FUNCTION holographic_source_model,obs,jones,map_fn,source_array,xvals=xvals,yvals=yvals,flux=flux,$
    ra=ra,dec=dec,pol_i=pol_i,_Extra=extra

dimension=obs.dimension
elements=obs.elements
IF N_Elements(pol_i) EQ 0 THEN pol_i=0

IF N_Elements(source_array) EQ 0 THEN BEGIN
    IF N_Elements(xvals) EQ 0 THEN ad2xy,ra,dec,obs.astr,xvals,yvals
    source_array=source_comp_init(xvals=xvals,yvals=yvals,flux=flux,frequency=obs.freq_center)
ENDIF

source_dft_multi,obs,jones,source_array,model_uv_full,_Extra=extra

model_uv_holo=holo_mapfn_apply(*model_uv_full[pol_i],map_fn,_Extra=extra,/indexed)
RETURN,model_uv_holo
END