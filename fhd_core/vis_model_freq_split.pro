FUNCTION vis_model_freq_split,source_list,obs,psf,params,flag_arr,model_uv_arr=model_uv_arr,vis_data_arr=vis_data_arr,$
    weights_arr=weights_arr,variance_arr=variance_arr,n_avg=n_avg,timing=timing,no_data=no_data,fft=fft,uv_mask=uv_mask,$
    fhd_file_path=fhd_file_path,vis_file_path=vis_file_path,even_only=even_only,odd_only=odd_only,x_range=x_range,y_range=y_range,_Extra=extra
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
    0:
    1:restore,obs_filepath ;obs
    2:restore,psf_filepath ;psf
    3:restore,params_filepath ;params
    4:restore,flags_filepath ;flag_arr0
    ELSE:
ENDSWITCH


n_freq=obs.n_freq
n_pol=obs.n_pol
dimension=obs.dimension
degpix=obs.degpix
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

IF Keyword_Set(even_only) OR Keyword_Set(odd_only) THEN BEGIN
    bin_start=(*obs.baseline_info).bin_offset
    nt=N_Elements(bin_start)
    nb=(size(*flag_arr[0],/dimension))[1]
    bin_end=fltarr(nt)
    bin_end[0:nt-2]=bin_start[1:nt-1]-1
    bin_end[nt-1]=nb-1
    bin_i=lonarr(nb)-1
    nt2=Floor(nt/2)
    FOR t_i=0,2*nt2-1 DO bin_i[bin_start[t_i]:bin_end[t_i]]=t_i
    bi_n=findgen(nb)
    even_bi_use=where(bin_i mod 2 EQ 0)
    odd_bi_use=where(bin_i mod 2 EQ 1)
    flag_arr1=fltarr(size(*flag_arr[0],/dimension))
    FOR pol_i=0,n_pol-1 DO BEGIN
        IF Keyword_Set(even_only) THEN flag_arr1[*,even_bi_use]=flag_arr0[pol_i,*,even_bi_use]
        IF Keyword_Set(odd_only) THEN flag_arr1[*,odd_bi_use]=flag_arr0[pol_i,*,odd_bi_use]
        *flag_arr[pol_i]*=flag_arr1
    ENDFOR
ENDIF 
flag_test=Total(*flag_arr[0]>0,1)
bi_use=where(flag_test)

IF N_Elements(n_avg) EQ 0 THEN BEGIN
    freq_bin_i2=obs.fbin_i
    n_avg=Round(n_freq/Max(freq_bin_i2+1))
ENDIF ELSE BEGIN
    freq_bin_i2=Floor(lindgen(n_freq)/n_avg)
ENDELSE

nf=Max(freq_bin_i2)+1L
IF model_flag THEN BEGIN
   vis_model_arr=vis_source_model(source_list,obs,psf,params,flag_arr,model_uv_arr=model_uv_arr,$
        file_path=fhd_file_path,timing=t_model,silent=silent,uv_mask=uv_mask)
   IF ~Keyword_Set(silent) THEN print,"Vis modeling and degridding: ", strn(t_model)
ENDIF
residual_arr=Ptrarr(n_pol,nf,/allocate)
weights_arr=Ptrarr(n_pol,nf,/allocate)
variance_arr=Ptrarr(n_pol,nf,/allocate)

t_grid=0
tarr=fltarr(8)
FOR pol_i=0,n_pol-1 DO BEGIN
    CASE 1 OF 
        (data_flag AND model_flag):BEGIN
            vis_ptr=vis_data_arr[pol_i]
            model_ptr=vis_model_arr[pol_i];/n_avg
        END
        data_flag:BEGIN
            vis_ptr=vis_data_arr[pol_i]
            model_ptr=Ptr_new() ;null pointer
        END
        model_flag:BEGIN
            vis_ptr=vis_model_arr[pol_i];/n_avg
            model_ptr=Ptr_new() ;null pointer
        END
    ENDCASE
    freq_use=(*obs.baseline_info).freq_use
    n_vis_use=0.
    IF Keyword_Set(fft) THEN init_arr=Fltarr(dimension,dimension) ELSE init_arr=Complexarr(dimension,dimension)
    IF N_Elements(x_range)<N_Elements(y_range) GT 0 THEN init_arr=extract_subarray(init_arr,x_range,y_range)
    FOR fi=0L,nf-1 DO BEGIN
        fi_use=where((freq_bin_i2 EQ fi) AND (freq_use GT 0),nf_use)
;        flags_use1=(*flag_arr[pol_i])[fi_use,*]
;        vis_use1=vis_use[fi_use,*]
        IF Ptr_valid(model_ptr) THEN model_return=1
        variance_holo=1 ;initialize
        weights_holo=1 ;initialize
        IF nf_use EQ 0 THEN n_vis=0 ELSE $
            dirty_UV=visibility_grid(vis_ptr,flag_arr[pol_i],obs,psf,params,timing=t_grid0,fi_use=fi_use,bi_use=bi_use,$
                polarization=pol_i,weights=weights_holo,variance=variance_holo,silent=1,mapfn_recalculate=0,time_arr=tarr0,$
                model_ptr=model_ptr,n_vis=n_vis,/preserve_visibilities,model_return=model_return)
        IF n_vis EQ 0 THEN BEGIN
            *residual_arr[pol_i,fi]=init_arr
            *weights_arr[pol_i,fi]=init_arr
            *variance_arr[pol_i,fi]=init_arr
            CONTINUE
        ENDIF
        n_vis_use+=n_vis
        IF Keyword_Set(fft) THEN BEGIN
            IF N_Elements(x_range)<N_Elements(y_range) GT 0 THEN BEGIN
                *residual_arr[pol_i,fi]=extract_subarray(dirty_image_generate(dirty_uv,degpix=degpix)*n_vis,x_range,y_range)
                *weights_arr[pol_i,fi]=extract_subarray(dirty_image_generate(weights_holo,degpix=degpix)*n_vis,x_range,y_range)
                *variance_arr[pol_i,fi]=extract_subarray(dirty_image_generate(variance_holo,degpix=degpix)*n_vis,x_range,y_range)
            ENDIF ELSE BEGIN 
                *residual_arr[pol_i,fi]=dirty_image_generate(dirty_uv,degpix=degpix)*n_vis
                *weights_arr[pol_i,fi]=dirty_image_generate(weights_holo,degpix=degpix)*n_vis
                *variance_arr[pol_i,fi]=dirty_image_generate(variance_holo,degpix=degpix)*n_vis
            ENDELSE
        ENDIF ELSE BEGIN
            *residual_arr[pol_i,fi]=dirty_uv*n_vis
            *weights_arr[pol_i,fi]=weights_holo*n_vis
            *variance_arr[pol_i,fi]=variance_holo*n_vis
        ENDELSE
        IF Keyword_Set(tarr0) THEN tarr+=tarr0
        IF Keyword_Set(t_grid0) THEN t_grid+=t_grid0
    ENDFOR  
;    vis_use=0 ;free memory  
ENDFOR
obs.n_vis=n_vis_use
    
IF ~Keyword_Set(silent) THEN print,"Gridding timing: ",strn(t_grid)
IF ~Keyword_Set(silent) THEN print,tarr
timing=Systime(1)-t0
RETURN,residual_arr
END