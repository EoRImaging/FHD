PRO vis_export,obs,vis_ptr,flag_ptr,file_path_fhd=file_path_fhd,pol_i=pol_i,compress=compress
IF N_Elements(compress) EQ 0 THEN compress=1
pol_names=['xx','yy','xy','yx']

IF N_Elements(pol_i) NE 1 THEN pol_i=indgen(obs.n_pol)
n_pol=N_Elements(pol_i)

FOR i=0,n_pol-1 DO BEGIN
    vis_filepath=file_path_fhd+'_vis_'+pol_names[pol_i[i]]+'.sav'
    IF Ptr_valid(flag_ptr[pol_i[i]]) THEN flag_i=where(*flag_ptr[pol_i[i]] LE 0,n_flag) ELSE n_flag=0
    IF n_flag GT 0 THEN (*vis_ptr[pol_i[i]])[flag_i]=0
    save,obs,vis_ptr[pol_i[i]],compress=compress
ENDFOR
END