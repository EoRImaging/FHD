PRO generate_calibration_catalog,obs,source_list,file_path=file_path,catalog_freq=catalog_freq


IF N_Elements(catalog_freq) EQ 0 THEN catalog_freq=obs.freq_center


END