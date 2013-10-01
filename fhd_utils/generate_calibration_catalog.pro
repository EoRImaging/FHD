PRO generate_calibration_catalog,source_list,file_path=file_path,spectral_index=spectral_index,$
    no_polarization=no_polarization,no_extend=no_extend,flux_threshold=flux_threshold

IF StrLowCase(Strmid(file_path,2,3,/reverse)) NE 'sav' THEN file_path_use=file_path+'.sav' $
    ELSE file_path_use=file_path

IF size(source_list,/type) EQ 7 THEN catalog=getvar_savefile(source_list,'source_array') ELSE catalog=source_list

IF Keyword_Set(flux_threshold) THEN BEGIN
    src_i_use=where(catalog.flux.I GT flux_threshold,n_use)
    catalog=catalog[src_i_use]
ENDIF
n_src=N_Elements(catalog)

FOR i=0,3 DO catalog.flux.(i)=0.
IF Keyword_Set(no_polarization) THEN FOR i=5,7 DO catalog.flux.(i)=0.
IF Keyword_Set(no_extend) THEN catalog.extend=Ptrarr(n_src)

SAVE,catalog,filename=file_path_use,/compress
END