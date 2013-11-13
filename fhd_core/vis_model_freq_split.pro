FUNCTION vis_model_freq_split,obs,psf,params,flag_arr,model_uv_arr=model_uv_arr,vis_data_arr=vis_data_arr,vis_model_arr=vis_model_arr,$
    weights_arr=weights_arr,variance_arr=variance_arr,model_arr=model_arr,n_avg=n_avg,timing=timing,fft=fft,source_list=source_list,$
    file_path_fhd=file_path_fhd,even_only=even_only,odd_only=odd_only,$
    vis_n_arr=vis_n_arr,x_range=x_range,y_range=y_range,preserve_visibilities=preserve_visibilities,_Extra=extra
ext='.UVFITS'
t0=Systime(1)

pol_names=['xx','yy','xy','yx']
flags_filepath=file_path_fhd+'_flags.sav'
params_filepath=file_path_fhd+'_params.sav'
psf_filepath=file_path_fhd+'_beams.sav'
obs_filepath=file_path_fhd+'_obs.sav'
vis_filepath=file_path_fhd+'_vis_'

SWITCH N_Params() OF
    0:obs=getvar_savefile(obs_filepath,'obs')
    1:psf=getvar_savefile(psf_filepath,'psf')
    2:params=getvar_savefile(params_filepath,'params')
    3:flag_arr=getvar_savefile(flags_filepath,'flag_arr')
    ELSE:
ENDSWITCH

n_freq=obs.n_freq
n_pol=obs.n_pol
dimension=obs.dimension
degpix=obs.degpix

IF Min(Ptr_valid(vis_data_arr)) EQ 0 THEN BEGIN
    vis_data_arr=Ptrarr(n_pol)
    FOR pol_i=0,n_pol-1 DO vis_data_arr[pol_i]=getvar_savefile(vis_filepath+pol_names[pol_i]+'.sav','vis_ptr')
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
        IF Keyword_Set(even_only) THEN flag_arr1[*,even_bi_use]=(*flag_arr[pol_i])[*,even_bi_use]
        IF Keyword_Set(odd_only) THEN flag_arr1[*,odd_bi_use]=(*flag_arr[pol_i])[*,odd_bi_use]
        *flag_arr[pol_i]*=flag_arr1
    ENDFOR
ENDIF 
IF n_pol GT 1 THEN flag_test=Total(*flag_arr[1]>*flag_arr[0]>0,1) ELSE flag_test=Total(*flag_arr[0]>0,1)
bi_use=where(flag_test)

IF N_Elements(n_avg) EQ 0 THEN BEGIN
    freq_bin_i2=(*obs.baseline_info).fbin_i
    n_avg=Round(n_freq/Max(freq_bin_i2+1))
ENDIF ELSE BEGIN
    freq_bin_i2=Floor(lindgen(n_freq)/n_avg)
ENDELSE

nf=Max(freq_bin_i2)+1L
IF Keyword_Set(source_list) OR Keyword_Set(model_uv_arr) THEN model_flag=1
IF Min(Ptr_valid(vis_model_arr)) EQ 0 THEN BEGIN
    IF model_flag THEN BEGIN
       vis_model_arr=vis_source_model(source_list,obs,psf,params,flag_arr,model_uv_arr=model_uv_arr,$
            file_path_fhd=file_path_fhd,timing=t_model,silent=silent,_Extra=extra)
       IF ~Keyword_Set(silent) THEN print,"Vis modeling and degridding: ", strn(t_model)
    ENDIF ELSE vis_model_arr=Ptrarr(n_pol)
ENDIF
dirty_arr=Ptrarr(n_pol,nf,/allocate)
weights_arr=Ptrarr(n_pol,nf,/allocate)
variance_arr=Ptrarr(n_pol,nf,/allocate)
model_arr=Ptrarr(n_pol,nf,/allocate)
vis_n_arr=Fltarr(n_pol,nf)

t_grid=0
FOR pol_i=0,n_pol-1 DO BEGIN
    vis_ptr=vis_data_arr[pol_i]
    model_ptr=vis_model_arr[pol_i]
    freq_use=(*obs.baseline_info).freq_use
    n_vis_use=0.
    IF Keyword_Set(fft) THEN init_arr=Fltarr(dimension,dimension) ELSE init_arr=Complexarr(dimension,dimension)
    IF N_Elements(x_range)<N_Elements(y_range) GT 0 THEN init_arr=extract_subarray(init_arr,x_range,y_range)
    FOR fi=0L,nf-1 DO BEGIN
        fi_use=where((freq_bin_i2 EQ fi) AND (freq_use GT 0),nf_use)
        IF Ptr_valid(model_ptr) THEN model_return=1
        variance_holo=1 ;initialize
        weights_holo=1 ;initialize
        IF nf_use EQ 0 THEN n_vis=0 ELSE IF Keyword_Set(inds_patch) THEN $
            dirty_UV=visibility_patch_grid(vis_ptr,flag_arr[pol_i],obs,psf,params,timing=t_grid0,fi_use=fi_use,bi_use=bi_use,$
                polarization=pol_i,weights=weights_holo,variance=variance_holo,silent=1,mapfn_recalculate=0,$
                model_ptr=model_ptr,n_vis=n_vis,/preserve_visibilities,model_return=model_return,inds_patch=inds_patch,$
                obs_patch=obs_patch,psf_patch=psf_patch,rephase_vis_flag=rephase_vis_flag,_Extra=extra) $
        ELSE $
            dirty_UV=visibility_grid(vis_ptr,flag_arr[pol_i],obs,psf,params,timing=t_grid0,fi_use=fi_use,bi_use=bi_use,$
                polarization=pol_i,weights=weights_holo,variance=variance_holo,silent=1,mapfn_recalculate=0,$
                model_ptr=model_ptr,n_vis=n_vis,/preserve_visibilities,model_return=model_return)
        IF n_vis EQ 0 THEN BEGIN
            *dirty_arr[pol_i,fi]=init_arr
            *weights_arr[pol_i,fi]=init_arr
            *variance_arr[pol_i,fi]=init_arr
            IF N_Elements(model_return) GT 1 THEN *model_arr[pol_i,fi]=init_arr
            CONTINUE
        ENDIF
        n_vis_use+=n_vis
        vis_n_arr[pol_i,fi]=n_vis
        IF Keyword_Set(fft) THEN BEGIN
            IF N_Elements(x_range)<N_Elements(y_range) GT 0 THEN BEGIN
                *dirty_arr[pol_i,fi]=extract_subarray(dirty_image_generate(dirty_uv,degpix=degpix)*n_vis,x_range,y_range)
                *weights_arr[pol_i,fi]=extract_subarray(dirty_image_generate(weights_holo,degpix=degpix)*n_vis,x_range,y_range)
                *variance_arr[pol_i,fi]=extract_subarray(dirty_image_generate(variance_holo,degpix=degpix)*n_vis,x_range,y_range)
                IF N_Elements(model_return) GT 1 THEN *model_arr[pol_i,fi]=extract_subarray(dirty_image_generate(model_return,degpix=degpix)*n_vis,x_range,y_range)
            ENDIF ELSE BEGIN 
                *dirty_arr[pol_i,fi]=dirty_image_generate(dirty_uv,degpix=degpix)*n_vis
                *weights_arr[pol_i,fi]=dirty_image_generate(weights_holo,degpix=degpix)*n_vis
                *variance_arr[pol_i,fi]=dirty_image_generate(variance_holo,degpix=degpix)*n_vis
                IF N_Elements(model_return) GT 1 THEN *model_arr[pol_i,fi]=dirty_image_generate(model_return,degpix=degpix)*n_vis
            ENDELSE
        ENDIF ELSE BEGIN
            *dirty_arr[pol_i,fi]=dirty_uv*n_vis
            *weights_arr[pol_i,fi]=weights_holo*n_vis
            *variance_arr[pol_i,fi]=variance_holo*n_vis
            IF N_Elements(model_return) GT 1 THEN *model_arr[pol_i,fi]=model_return*n_vis
        ENDELSE
        IF Keyword_Set(t_grid0) THEN t_grid+=t_grid0
    ENDFOR  
    IF ~Keyword_Set(preserve_visibilities) THEN ptr_free,vis_ptr,model_ptr
ENDFOR
obs.n_vis=n_vis_use
    
IF ~Keyword_Set(silent) THEN print,"Gridding timing: ",strn(t_grid)
timing=Systime(1)-t0
RETURN,dirty_arr
END