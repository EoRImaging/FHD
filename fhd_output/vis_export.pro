PRO vis_export,obs,status_str,vis_ptr_arr,vis_weight_ptr,file_path_fhd=file_path_fhd,pol_i=pol_i,compress=compress,model=model
IF N_Elements(compress) EQ 0 THEN compress=1
pol_names=obs.pol_names

res_name='Residual'
IF obs.residual EQ 0 THEN res_name='Dirty'
IF Keyword_Set(model) THEN BEGIN
    res_name='Model'
    var_name='vis_model_ptr'
ENDIF ELSE var_name='vis_ptr'

IF min(Ptr_valid(vis_ptr_arr)) EQ 0 THEN BEGIN
    print,res_name+" visibilities NULL! Not exported!"
    RETURN
ENDIF
print,"Exporting "+res_name+" visibilities"

IF N_Elements(pol_i) NE 1 THEN pol_i=indgen(obs.n_pol)
n_pol=N_Elements(pol_i)
IF N_Elements(vis_weight_ptr) LT n_pol THEN vis_weight_ptr_use=Ptrarr(n_pol) ELSE vis_weight_ptr_use=vis_weight_ptr

FOR i=0,n_pol-1 DO BEGIN
    vis_ptr=vis_ptr_arr[pol_i[i]]
    ;IF Ptr_valid(vis_weight_ptr_use[pol_i[i]]) THEN flag_i=where(*vis_weight_ptr_use[pol_i[i]] LE 0,n_flag) ELSE n_flag=0
    ;IF n_flag GT 0 THEN (*vis_ptr)[flag_i]=0
    fhd_save_io,status_str,vis_ptr,var=var_name,file_path_fhd=file_path_fhd,obs=obs,compress=compress,pol_i=pol_i[i]
ENDFOR
END
