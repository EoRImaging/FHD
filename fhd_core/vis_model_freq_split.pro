FUNCTION vis_model_freq_split,source_list,obs,psf,params,flag_arr,model_uv_arr=model_uv_arr,vis_data_arr=vis_data_arr,$
    weights_arr=weights_arr,n_avg=n_avg,timing=timing,no_data=no_data,fft=fft,$
    fhd_file_path=fhd_file_path,vis_file_path=vis_file_path,_Extra=extra
;no need to specify data_directory or filename if obs exists
;vis_path_default,data_directory,filename,file_path,obs=obs
ext='.UVFITS'
t0=Systime(1)

flags_filepath=fhd_file_path+'_flags.sav'
;vis_filepath=file_path+'_vis.sav'
params_filepath=fhd_file_path+'_params.sav'
psf_filepath=fhd_file_path+'_beams.sav'
obs_filepath=fhd_file_path+'_obs.sav'

SWITCH N_Params() OF
    1:restore,obs_filepath ;obs
    2:restore,psf_filepath ;psf
    3:restore,params_filepath ;params
    4:restore,flags_filepath ;flag_arr0
    ELSE:
ENDSWITCH


n_freq=obs.n_freq
n_pol=obs.n_pol
dimension=obs.dimension
real_index=0
imaginary_index=1

IF Keyword_Set(source_list) + Keyword_Set(model_uv_arr) EQ 0 THEN model_flag=0. ELSE model_flag=1.
IF model_flag EQ 0 THEN no_data=0
IF N_Elements(no_data) EQ 0 THEN no_data=0
IF N_Elements(vis_data_arr) EQ 0 AND ~Keyword_Set(no_data) THEN BEGIN
    data_flag=1
    data_struct=mrdfits(vis_file_path,0,/silent)
    data_array=data_struct.array
    data_struct=0. ;free memory
    vis_data_arr=Ptrarr(n_pol,/allocate)
    FOR pol_i=0,n_pol-1 DO BEGIN
        *vis_data_arr[pol_i]=Complex(reform(data_array[real_index,pol_i,*,*]),Reform(data_array[imaginary_index,pol_i,*,*]))
    ENDFOR
ENDIF ELSE data_flag=1-no_data

IF N_Elements(flag_arr) EQ 0 THEN BEGIN
    flag_arr=Ptrarr(n_pol,/allocate)
    FOR pol_i=0,n_pol-1 DO BEGIN
        *flag_arr[pol_i]=reform(flag_arr0[pol_i,*,*])
    ENDFOR
ENDIF

IF N_Elements(n_avg) EQ 0 THEN BEGIN
    freq_bin_i=obs.fbin_i
    n_avg=Round(n_freq/Max(freq_bin_i+1))
    
ENDIF ELSE BEGIN
    freq_bin_i=Floor(lindgen(n_freq)/n_avg)
ENDELSE

nf=Max(freq_bin_i)+1L
IF model_flag THEN BEGIN
   vis_model_arr=vis_source_model(source_list,obs,psf,params,flag_arr,model_uv_arr=model_uv_arr,$
    file_path=fhd_file_path,timing=t_model,silent=silent)
   IF ~Keyword_Set(silent) THEN print,"Vis modeling and degridding: ", strn(t_model)
ENDIF
residual_arr=Ptrarr(n_pol,nf,/allocate)
weights_arr=Ptrarr(n_pol,nf,/allocate)

t_grid=0
FOR pol_i=0,n_pol-1 DO BEGIN
    CASE 1 OF 
        (data_flag AND model_flag):vis_use=*vis_data_arr[pol_i]-*vis_model_arr[pol_i]/n_avg
        data_flag:vis_use=*vis_data_arr[pol_i]
        model_flag:vis_use=*vis_model_arr[pol_i]/n_avg
    ENDCASE
    
    FOR fi=0L,nf-1 DO BEGIN
        flags_use=*flag_arr[pol_i]
        freq_cut=where(freq_bin_i NE fi,n_cut)
        IF n_cut GT 0 THEN flags_use[freq_cut,*]=0
        dirty_UV=visibility_grid(vis_use,flags_use,obs,psf,params,timing=t_grid0,$
            polarization=pol_i,weights=weights_holo,silent=1,mapfn_recalculate=0,_Extra=extra)
        IF Keyword_Set(fft) THEN BEGIN
            *residual_arr[pol_i,fi]=dirty_image_generate(dirty_uv,_Extra=extra)
            *weights_arr[pol_i,fi]=dirty_image_generate(weights_holo,_Extra=extra)
        ENDIF ELSE BEGIN
            *residual_arr[pol_i,fi]=dirty_uv
            *weights_arr[pol_i,fi]=weights_holo
        ENDELSE
        t_grid+=t_grid0
    ENDFOR  
    vis_use=0 ;free memory  
ENDFOR
IF ~Keyword_Set(silent) THEN print,"Gridding timing: ",strn(t_grid)
timing=Systime(1)-t0
RETURN,residual_arr
END