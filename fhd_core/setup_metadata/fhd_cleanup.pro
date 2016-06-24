PRO fhd_cleanup,file_path_fhd

pol_names=['xx','yy','xy','yx','I','Q','U','V']
product_list = ['hdr','hpx_cnv','params','psf','cal','jones','flag_arr','fhd','skymodel','antenna']
product_pol_list = ['map_fn']
FOR i=0,N_Elements(product_list)-1 DO BEGIN
    fhd_save_io,var=product_list[i],file_path_fhd=file_path_fhd,path_use=path_use,/no_save
    IF N_Elements(file_path_list) EQ 0 THEN file_path_list=[path_use] ELSE file_path_list=[file_path_list,path_use]
ENDFOR

FOR i=0,N_Elements(product_pol_use)-1 DO BEGIN 
    FOR pol_i=0,3 DO BEGIN 
        fhd_save_io,var=product_pol_list[i],file_path_fhd=file_path_fhd,path_use=path_use,pol_i=pol_,/no_save
        file_path_list=[file_path_list,path_use]
    ENDFOR
ENDFOR

n_paths=N_Elements(file_path_list)
FOR i=0,n_paths-1 DO file_delete,file_path_list[i],/allow_nonexistent
END