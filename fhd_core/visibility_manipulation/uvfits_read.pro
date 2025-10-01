PRO uvfits_read,hdr,params,layout,vis_arr,vis_weights,file_path_vis=file_path_vis,n_pol=n_pol,silent=silent,$
    restore_vis_savefile=restore_vis_savefile,reorder_visibilities=reorder_visibilities,$
    vis_time_average=vis_time_average,vis_freq_average=vis_freq_average,error=error,$
    uvfits_spectral_dimension=uvfits_spectral_dimension,_Extra=extra
;set n_pol=0 to not return any data, only header and parameters

IF Strpos(file_path_vis,'.sav') EQ -1 THEN file_path_vis_sav=file_path_vis+".sav" ELSE file_path_vis_sav=file_path_vis
IF Keyword_Set(restore_vis_savefile) THEN BEGIN
    IF file_test(file_path_vis_sav) EQ 0 THEN BEGIN
        error=1
        print,"File: "+file_path_vis_sav+" not found! Returning"
        RETURN
    ENDIF
    t_readfits=Systime(1)
    RESTORE,file_path_vis_sav
    IF Keyword_Set(flag_arr) THEN vis_weights=flag_arr
    t_readfits=Systime(1)-t_readfits
    print,"Time restoring visibility save file: "+Strn(t_readfits)
ENDIF ELSE BEGIN
    IF file_test(file_path_vis) EQ 0 THEN BEGIN
        print,"File: "+file_path_vis+" not found! Returning"
        error=1
        RETURN
    ENDIF
    
    t_readfits=Systime(1)
    lun = fxposit(file_path_vis, 0,/readonly)
    data_struct=mrdfits(lun,0,data_header0,/silent)
    hdr=vis_header_extract(data_header0, params = data_struct.params,error=error,_Extra=extra)    
    IF Keyword_Set(error) THEN RETURN
    IF N_Elements(n_pol) EQ 0 THEN n_pol=hdr.n_pol ELSE n_pol=n_pol<hdr.n_pol
    
    params=vis_param_extract(data_struct.params,hdr,_Extra=extra)
    t_readfits=Systime(1)-t_readfits
    print,"Time reading UVFITS files and extracting header: "+Strn(t_readfits)
    
    data_array=Temporary(data_struct.array) 
    data_struct=0. ;free memory
    
    pol_dim=hdr.pol_dim
    freq_dim=hdr.freq_dim
    real_index=hdr.real_index
    imaginary_index=hdr.imaginary_index
    weights_index=hdr.weights_index
    n_freq0=hdr.n_freq
    nbaselines0=hdr.nbaselines
    uvfits_dim = size(data_array,/n_dimension)
    
    IF uvfits_dim GT 4 THEN BEGIN
        uvfits_dim_arr = size(data_array,/dimension)
        IF max(uvfits_dim_arr[3:uvfits_dim-2]) EQ 1 THEN spec_i = 0 $
        ELSE BEGIN
            IF N_Elements(uvfits_spectral_dimension) EQ 0 THEN BEGIN
                print,"Extra dimensions with size greater than 1 found in the uvfits file!"
                print,"To use such files you must set the keyword uvfits_spectral_dimension"
                error=1
                RETURN
            ENDIF
            spec_i=uvfits_spectral_dimension
        ENDELSE 
        
        CASE uvfits_dim OF
            7:data_array = Temporary(reform(data_array[*,*,*,spec_i,spec_i,spec_i,*],3,hdr.n_pol,n_freq0,nbaselines0))
            6:data_array = Temporary(reform(data_array[*,*,*,spec_i,spec_i,*],3,hdr.n_pol,n_freq0,nbaselines0))
            5:data_array = Temporary(reform(data_array[*,*,*,spec_i,*],3,hdr.n_pol,n_freq0,nbaselines0))
            ELSE: BEGIN
                print,"Extra dimensions found in the uvfits file!"
                print,"Total dimensions: " + Strn(uvfits_dim)
                print,"Dimensions over 7 are not supported"
                error=1
                RETURN
            END
        ENDCASE
    ENDIF
    
    IF n_pol GT 0 THEN BEGIN
        vis_arr=Ptrarr(n_pol,/allocate)
        vis_weights=Ptrarr(n_pol,/allocate)
    ENDIF
    FOR pol_i=0,n_pol-1 DO BEGIN
        *vis_arr[pol_i]=Complex(reform(data_array[real_index,pol_i,*,*],n_freq0,nbaselines0),Reform(data_array[imaginary_index,pol_i,*,*],n_freq0,nbaselines0))
        *vis_weights[pol_i]=reform(data_array[weights_index,pol_i,*,*],n_freq0,nbaselines0)
    ENDFOR
    ;free memory
    data_array=0 
    vis_weights0=0

    ;; now read extensions one by one until we find the antenna table
    t_read_ant=Systime(1)
    ext_name = ''
    while ext_name ne 'AIPS AN' do begin
        ext_data = mrdfits(lun, 0, ext_header)
        ext_name=strtrim(sxpar(ext_header, 'extname'))
    endwhile
    layout = fhd_struct_init_layout(ext_header, ext_data,_Extra=extra)
    t_read_ant=Systime(1)-t_read_ant
    print,"Time finding and reading antenna table in UVFITS file and extracting header: "+Strn(t_read_ant) 

    ;; use the antenna table to update hdr.n_tile, which was set to a default value of 128 in fhd_struct_init_hdr
    hdr.n_tile=N_Elements(layout.antenna_names)

    IF Keyword_Set(reorder_visibilities) THEN vis_reorder,hdr,params,vis_arr,vis_weights
    
    ;Optionally average data in time and/or frequency if the visibilities are too large to store in memory as-is, or just to save time later
    IF Keyword_Set(vis_time_average) OR Keyword_Set(vis_freq_average) THEN BEGIN
        IF Keyword_Set(vis_time_average) THEN print,"Averaging visibilities in time by a factor of: "+Strtrim(Strn(vis_time_average),2)
        IF Keyword_Set(vis_freq_average) THEN print,"Averaging visibilities in frequency by a factor of: "+Strtrim(Strn(vis_freq_average),2)
        vis_average,vis_arr,vis_weights,params,hdr,vis_time_average=vis_time_average,vis_freq_average=vis_freq_average,timing=t_averaging
        IF ~Keyword_Set(silent) THEN print,"Visibility averaging time: "+Strtrim(String(t_averaging),2)
    ENDIF
    

        
ENDELSE    
END