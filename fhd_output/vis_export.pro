PRO vis_export,obs,vis_ptr_arr,flag_ptr,file_path_fhd=file_path_fhd,pol_i=pol_i,compress=compress,model=model
IF N_Elements(compress) EQ 0 THEN compress=1
pol_names=obs.pol_names

res_name='Residual'
IF tag_exist(obs,'residual') THEN IF obs.residual EQ 0 THEN res_name='Dirty'
IF Keyword_Set(model) THEN BEGIN
    res_name='Model'
    path_insert='model_'
ENDIF ELSE path_insert=''

IF min(Ptr_valid(vis_ptr_arr)) EQ 0 THEN BEGIN
    print,res_name+" visibilities NULL! Not exported!"
    RETURN
ENDIF
print,"Exporting "+res_name+" visibilities"

IF N_Elements(pol_i) NE 1 THEN pol_i=indgen(obs.n_pol)
n_pol=N_Elements(pol_i)
IF N_Elements(flag_ptr) LT n_pol THEN flag_ptr_use=Ptrarr(n_pol) ELSE flag_ptr_use=flag_ptr

FOR i=0,n_pol-1 DO BEGIN
    vis_ptr=vis_ptr_arr[pol_i[i]]
    vis_filepath=file_path_fhd+'_vis_'+path_insert+pol_names[pol_i[i]]+'.sav'
    IF Ptr_valid(flag_ptr_use[pol_i[i]]) THEN flag_i=where(*flag_ptr_use[pol_i[i]] LE 0,n_flag) ELSE n_flag=0
    IF n_flag GT 0 THEN (*vis_ptr)[flag_i]=0
    save,obs,vis_ptr,filename=vis_filepath,compress=compress
ENDFOR
END