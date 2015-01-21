PRO uvfits_read,hdr,params,vis_arr,flag_arr,file_path_vis=file_path_vis,n_pol=n_pol,silent=silent,restore_vis_savefile=restore_vis_savefile,$
    reorder_visibilities=reorder_visibilities,vis_time_average=vis_time_average,vis_freq_average=vis_freq_average
;set n_pol=0 to not return any data, only header and parameters

IF Strpos(file_path_vis,'.sav') EQ -1 THEN file_path_vis_sav=file_path_vis+".sav" ELSE file_path_vis_sav=file_path_vis
IF Keyword_Set(restore_vis_savefile) THEN BEGIN
    IF file_test(file_path_vis_sav) EQ 0 THEN BEGIN
        error=1
        RETURN
    ENDIF
    t_readfits=Systime(1)
    RESTORE,file_path_vis_sav
    t_readfits=Systime(1)-t_readfits
    print,"Time restoring visibility save file: "+Strn(t_readfits)
ENDIF ELSE BEGIN
    IF file_test(file_path_vis) EQ 0 THEN BEGIN
        print,"File: "+file_path_vis+" not found! Returning"
        error=1
        RETURN
    ENDIF
    
    t_readfits=Systime(1)
    data_struct=mrdfits(file_path_vis,0,data_header0,/silent)
    hdr=vis_header_extract(data_header0, params = data_struct.params)    
    IF N_Elements(n_pol) EQ 0 THEN n_pol=hdr.n_pol ELSE n_pol=n_pol<hdr.n_pol
    IF N_Params() LT 3 THEN n_pol_use=0 ELSE n_pol_use=n_pol ;check if visibility data will be returned. If not, set this so that the data will be skipped as much as possible
    
    params=vis_param_extract(data_struct.params,hdr)
    t_readfits=Systime(1)-t_readfits
    print,"Time reading UVFITS files and extracting header: "+Strn(t_readfits)
    IF n_pol_use GT 0 THEN IF n_pol LT hdr.n_pol THEN data_array=Temporary(data_struct.array[*,0:n_pol-1,*]) ELSE data_array=Temporary(data_struct.array) 
    data_struct=0. ;free memory
    
    pol_dim=hdr.pol_dim
    freq_dim=hdr.freq_dim
    real_index=hdr.real_index
    imaginary_index=hdr.imaginary_index
    flag_index=hdr.flag_index
    IF n_pol_use GT 0 THEN BEGIN
        vis_arr=Ptrarr(n_pol,/allocate)
        flag_arr=Ptrarr(n_pol,/allocate)
    ENDIF
    n_freq0=hdr.n_freq
    nbaselines0=hdr.nbaselines
    FOR pol_i=0,n_pol_use-1 DO BEGIN
        *vis_arr[pol_i]=Complex(reform(data_array[real_index,pol_i,*,*],n_freq0,nbaselines0),Reform(data_array[imaginary_index,pol_i,*,*],n_freq0,nbaselines0))
        *flag_arr[pol_i]=reform(data_array[flag_index,pol_i,*,*],n_freq0,nbaselines0)
    ENDFOR
    ;free memory
    data_array=0 
    flag_arr0=0
    
    IF Keyword_Set(reorder_visibilities) THEN vis_reorder,hdr,params,vis_arr,flag_arr
    
    ;Optionally average data in time and/or frequency if the visibilities are too large to store in memory as-is, or just to save time later
    IF Keyword_Set(vis_time_average) OR Keyword_Set(vis_freq_average) THEN BEGIN
        IF Keyword_Set(vis_time_average) THEN print,"Averaging visibilities in time by a factor of: "+Strtrim(Strn(vis_time_average),2)
        IF Keyword_Set(vis_freq_average) THEN print,"Averaging visibilities in frequency by a factor of: "+Strtrim(Strn(vis_freq_average),2)
        vis_average,vis_arr,flag_arr,params,hdr,vis_time_average=vis_time_average,vis_freq_average=vis_freq_average,timing=t_averaging
        IF ~Keyword_Set(silent) THEN print,"Visibility averaging time: "+Strtrim(String(t_averaging),2)
    ENDIF
    
ENDELSE    
END