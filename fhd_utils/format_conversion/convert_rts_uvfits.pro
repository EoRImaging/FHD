PRO convert_rts_uvfits,data_directory,obsname=obsname,start_fi=start_fi,end_fi=end_fi,n_pol=n_pol,$
    vis_time_average=vis_time_average,vis_freq_average=vis_freq_average,rts_output_directory=rts_output_directory
;wrapper to convert RTS uvfits output to FHD visibility save file input

IF N_Elements(n_pol) EQ 0 THEN n_pol=4
IF N_Elements(rts_output_directory) EQ 0 THEN rts_output_directory=data_directory
IF file_test(rts_output_directory,/directory) EQ 0 THEN file_mkdir,rts_output_directory
obsname_list=file_basename(file_search(data_directory,'*',/test_directory,count=n_obs))
IF n_obs EQ 0 THEN RETURN
n_uvfits_arr=lonarr(n_obs)
FOR obs_i=0,n_obs-1 DO BEGIN
    uvfits_test=file_search(filepath('*.uvfits',root=data_directory,sub=obsname_list[obs_i]),count=n_uvfits)
    n_uvfits_arr[obs_i]=n_uvfits
ENDFOR
obs_use=where(n_uvfits_arr GT 0,n_obs)
IF n_obs EQ 0 THEN RETURN
obsname_list=obsname_list[obs_use]

IF N_Elements(start_fi) EQ 0 THEN start_fi=0
IF N_Elements(end_fi) EQ 0 THEN end_fi=n_obs-1
IF Keyword_Set(obsname) THEN obsname_list=obsname ELSE obsname_list=obsname_list[start_fi:end_fi]

n_obs=N_Elements(obsname_list)
FOR obs_i=0,n_obs-1 DO BEGIN
    print,"Processing "+obsname_list[obs_i]+" in "+data_directory
    t0=Systime(1)
    uvfits_list=file_search(filepath('*.uvfits',root=data_directory,sub=obsname_list[obs_i]),count=n_uvfits)
    IF n_uvfits EQ 0 THEN CONTINUE
    
    vis_arr2=Ptrarr(n_uvfits,n_pol,/allocate)
    vis_weights2=Ptrarr(n_uvfits,n_pol,/allocate)
    FOR fi=0,n_uvfits-1 DO BEGIN
        data_struct=mrdfits(uvfits_list[fi],0,data_header0,/silent)
        hdr=vis_header_extract(data_header0, params = data_struct.params)  
        IF N_Elements(n_pol) EQ 0 THEN n_pol=hdr.n_pol ELSE n_pol=n_pol<hdr.n_pol
        IF fi EQ 0 THEN params=vis_param_extract(data_struct.params,hdr)
        IF n_pol LT hdr.n_pol THEN data_array=Temporary(data_struct.array[*,0:n_pol-1,*]) ELSE data_array=Temporary(data_struct.array) 
        data_struct=0. ;free memory
        IF fi EQ 0 THEN hdr_arr=Replicate(hdr,n_uvfits) ELSE hdr_arr[fi]=hdr  
;        IF fi EQ 0 THEN params_arr=Replicate(params,n_uvfits) ELSE params_arr[fi]=params  
        
        pol_dim=hdr.pol_dim
        freq_dim=hdr.freq_dim
        real_index=hdr.real_index
        imaginary_index=hdr.imaginary_index
        weights_index=hdr.weights_index
        FOR pol_i=0,n_pol-1 DO BEGIN
            *vis_arr2[fi,pol_i]=Complex(reform(data_array[real_index,pol_i,*,*]),Reform(data_array[imaginary_index,pol_i,*,*]))
            *vis_weights2[fi,pol_i]=reform(data_array[weights_index,pol_i,*,*])
        ENDFOR
        ;free memory
        data_array=0 
        vis_weights0=0
    ENDFOR
    order_i=Sort(hdr_arr.freq_ref)
    
    hdr_arr=hdr_arr[order_i]
;    params_arr=params_arr[order_i]
    vis_arr2=vis_arr2[order_i,*]
    vis_weights2=vis_weights2[order_i,*]
    hdr=hdr_arr[0]
    n_freq=Total(hdr_arr.n_freq)
    hdr.n_freq=n_freq
    n_bt=N_Elements(params.time)
;    params=params_arr[0]
    
    vis_arr=Ptrarr(n_pol,/allocate)
    vis_weights=Ptrarr(n_pol,/allocate)
    
    freq_end=Total(hdr_arr.n_freq,/cumulative)-1
    freq_start=[0,freq_end[0:n_uvfits-2]+1]
    
    FOR pol_i=0,n_pol-1 DO BEGIN
        *vis_arr[pol_i]=Complexarr(n_freq,n_bt)
        *vis_weights[pol_i]=Fltarr(n_freq,n_bt)
        FOR fi=0,n_uvfits-1 DO BEGIN
            (*vis_arr[pol_i])[freq_start[fi]:freq_end[fi],*]=Temporary(*vis_arr2[fi,pol_i])
            (*vis_weights[pol_i])[freq_start[fi]:freq_end[fi],*]=Temporary(*vis_weights2[fi,pol_i])
        ENDFOR
    ENDFOR
    IF Keyword_Set(vis_time_average) OR Keyword_Set(vis_freq_average) THEN BEGIN
        IF Keyword_Set(vis_time_average) THEN print,"Averaging visibilities in time by a factor of: "+Strtrim(Strn(vis_time_average),2)
        IF Keyword_Set(vis_freq_average) THEN print,"Averaging visibilities in frequency by a factor of: "+Strtrim(Strn(vis_freq_average),2)
        vis_average,vis_arr,vis_weights,params,hdr,vis_time_average=vis_time_average,vis_freq_average=vis_freq_average,timing=t_averaging
        IF ~Keyword_Set(silent) THEN print,"Visibility averaging time: "+Strtrim(String(t_averaging),2)
    ENDIF
    file_path_vis_sav=filepath(obsname_list[obs_i]+".uvfits.sav",root=rts_output_directory)
    SAVE,vis_arr,vis_weights,hdr,params,/compress,filename=file_path_vis_sav
    print,"RTS conversion timing: ",Strn(Systime(1)-t0)
        debug_point=1
ENDFOR

END