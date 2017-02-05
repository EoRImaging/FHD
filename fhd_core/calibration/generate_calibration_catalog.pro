PRO generate_calibration_catalog,source_list=source_list,file_path=file_path,spectral_index=spectral_index,$
    no_polarization=no_polarization,no_extend=no_extend,flux_threshold=flux_threshold,$
    combined_obs=combined_obs,frequency=frequency,ston_threshold=ston_threshold

IF StrLowCase(Strmid(file_path,2,3,/reverse)) NE 'sav' THEN file_path_use=file_path+'.sav' $
    ELSE file_path_use=file_path

IF size(source_list,/type) EQ 7 THEN catalog=getvar_savefile(source_list,'source_array') ELSE catalog=source_list

IF Keyword_Set(combined_obs) THEN BEGIN
   ; using combined obs sourcelist, must reformat
   IF ~Keyword_set(frequency) THEN frequency=0
   combined_sourcelist=catalog
   n_sources=n_elements(combined_sourcelist)
   catalog=source_comp_init(n_sources=n_sources,ra=combined_sourcelist.ra,dec=combined_sourcelist.dec,$
                            id=combined_sourcelist.id,frequency=frequency)
   FOR i=0,n_sources-1 DO BEGIN
      weights=((*combined_sourcelist[i].sources).ston / (*combined_sourcelist[i].sources).flux)^2.
      catalog[i].flux.i = total(weights * (*combined_sourcelist[i].sources).flux) / total(weights)
      catalog[i].alpha = total(weights * (*combined_sourcelist[i].sources).alpha) / total(weights)
      catalog[i].ston = catalog[i].flux.i * sqrt(total(weights))
   ENDFOR
      
ENDIF

IF Keyword_Set(flux_threshold) THEN BEGIN
    src_i_use=where(catalog.flux.I GT flux_threshold,n_use)
    catalog=catalog[src_i_use]
ENDIF
IF Keyword_Set(ston_threshold) THEN BEGIN ;signal to noise threshold
    src_i_use=where(catalog.ston GT ston_threshold,n_use)
    catalog=catalog[src_i_use]
ENDIF

n_src=N_Elements(catalog)

FOR i=0,3 DO catalog.flux.(i)=0.
IF Keyword_Set(no_polarization) THEN FOR i=5,7 DO catalog.flux.(i)=0.
IF Keyword_Set(no_extend) THEN catalog.extend=Ptrarr(n_src)

SAVE,catalog,filename=file_path_use,/compress
END
