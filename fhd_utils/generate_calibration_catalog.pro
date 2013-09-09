PRO generate_calibration_catalog,source_list,file_path=file_path,spectral_index=spectral_index,$
    no_polarization=no_polarization,no_extend=no_extend

IF StrLowCase(Strmid(file_path,2,3,/reverse)) NE 'sav' THEN file_path_use=file_path+'.sav' $
    ELSE file_path_use=file_path
    
catalog=source_list
n_src=N_Elements(catalog)

FOR i=0,3 DO catalog.flux.(i)=0.
IF Keyword_Set(no_polarization) THEN FOR i=5,7 DO catalog.flux.(i)=0.
IF Keyword_Set(no_extend) THEN catalog.extend=Ptrarr(n_src)

SAVE,catalog,filename=file_path_use,/compress
END