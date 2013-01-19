PRO vis_split_export_multi,hpx_inds,filename_list=filename_list,version=version,$
    data_directory=data_directory,n_avg=n_avg
heap_gc

IF N_Elements(version) EQ 0 THEN version=0 ELSE version=Fix(version)
version_name='v'+strn(version)
version_dirname='fhd_'+version_name

n_obs=N_Elements(filename_list)
dir=filepath('',root=data_directory,subdir=['Combined_obs',version_dirname])
output_path=rootdir('mwa')+dir+'multi_freq_residuals.sav'
IF file_test(rootdir('mwa')+dir) EQ 0 THEN file_mkdir,rootdir('mwa')+dir   

obs_arr=Ptrarr(n_obs,/allocate)
hpx_cnv=Ptrarr(n_obs,/allocate)

FOR obs_i=0,n_obs-1 DO BEGIN
    vis_path_default,data_directory,filename_list[obs_i],file_path,version=version
    restore,file_path+'_obs.sav'
    *obs_arr[obs_i]=obs
    *hpx_cnv[obs_i]=healpix_cnv_generate(obs,nside=nside,restore_last=1)
    IF obs_i EQ 0 THEN nside_check=nside ELSE IF nside NE nside_check THEN $
        message,String(format='("Mismatched HEALPix NSIDE for ",A)',filename_list[obs_i]) 
ENDFOR
hpx_ind_map=healpix_combine_inds(hpx_cnv,hpx_inds=hpx_inds)

n_hpx=N_Elements(hpx_inds)
residual_hpx_arr=Ptrarr(n_pol,n_freq,/allocate)
model_hpx_arr=Ptrarr(n_pol,n_freq,/allocate)
dirty_hpx_arr=Ptrarr(n_pol,n_freq,/allocate)
weights_hpx_arr=Ptrarr(n_pol,n_freq,/allocate)
FOR pol_i=0,n_pol-1 DO BEGIN
    FOR freq_i=0,n_freq-1 DO BEGIN
        *residual_hpx_arr[pol_i,freq_i]=fltarr(n_hpx)
        *model_hpx_arr[pol_i,freq_i]=fltarr(n_hpx)
        *dirty_hpx_arr[pol_i,freq_i]=fltarr(n_hpx)
        *weights_hpx_arr[pol_i,freq_i]=fltarr(n_hpx)
    ENDFOR
ENDFOR    
FOR obs_i=0,n_obs-1 DO BEGIN 
    vis_path_default,data_directory,filename_list[obs_i],file_path,version=version
    restore,file_path+'_beams.sav'
    restore,file_path+'_obs.sav'
    ; *_fhd.sav contains:
    ;residual_array,dirty_array,image_uv_arr,source_array,comp_arr,model_uv_full,model_uv_holo,normalization,weights_arr,$
    ;    beam_base,beam_correction,ra_arr,dec_arr,astr
    restore,file_path+'_fhd.sav'
    
    IF N_Elements(sources_list) + N_Elements(model_uv_arr) EQ 0 THEN BEGIN ;consistency with old .SAV files
        source_list=source_array
    ENDIF
    obs=*obs_arr[obs_i]
    dimension=obs.dimension
    elements=obs.elements    
    
    model_arr1=vis_model_freq_split(source_list,obs,psf,model_uv_arr=model_uv_arr,$
        weights_arr=weights_arr0,n_avg=n_avg,filename=filename,data_directory=data_directory,timing=t_split,/no_data,/fft)
    
    dirty_arr1=vis_model_freq_split(0,obs,psf,model_uv_arr=0,$
        n_avg=n_avg,filename=filename,data_directory=data_directory,timing=t_split1,/fft)    
    
    n_pol=(size(model_arr1,/dimension))[0]
    n_freq=(size(model_arr1,/dimension))[1]
    weights_in=Ptrarr(n_pol,n_freq,/allocate)
    FOR i=0,N_Elements(weights_in)-1 DO *weights_in[i]=complexarr(obs.dimension,obs.elements)+1    
    weights_arr1=vis_model_freq_split(0,obs,psf,model_uv_arr=weights_in,$
        n_avg=n_avg,filename=filename,data_directory=data_directory,timing=t_split2,/no_data,/fft)
        
    FOR pol_i=0,n_pol-1 DO FOR freq_i=0,n_freq-1 DO BEGIN
        (*residual_hpx_arr[pol_i,freq_i])[*hpx_ind_map[obs_i]]+=healpix_cnv_apply(*dirty_arr1[pol_i,freq_i]-*model_arr1[pol_i,freq_i],*hpx_cnv[obs_i])
        (*dirty_hpx_arr[pol_i,freq_i])[*hpx_ind_map[obs_i]]+=healpix_cnv_apply(*dirty_arr1[pol_i,freq_i],*hpx_cnv[obs_i])
        (*model_hpx_arr[pol_i,freq_i])[*hpx_ind_map[obs_i]]+=healpix_cnv_apply(*model_arr1[pol_i,freq_i],*hpx_cnv[obs_i])
        (*weights_hpx_arr[pol_i,freq_i])[*hpx_ind_map[obs_i]]+=healpix_cnv_apply(*weights_arr1[pol_i,freq_i],*hpx_cnv[obs_i])
    ENDFOR
ENDFOR

dirty_xx_cube=fltarr(n_hpx,n_freq)
dirty_yy_cube=fltarr(n_hpx,n_freq)
FOR fi=0L,n_freq-1 DO BEGIN
    dirty_xx_cube[*,fi]=*dirty_hpx_arr[0,fi]
    dirty_yy_cube[*,fi]=*dirty_hpx_arr[1,fi]
ENDFOR
Ptr_free,dirty_hpx_arr

res_xx_cube=fltarr(n_hpx,n_freq)
res_yy_cube=fltarr(n_hpx,n_freq)
FOR fi=0L,n_freq-1 DO BEGIN
    res_xx_cube[*,fi]=*residual_hpx_arr[0,fi]
    res_yy_cube[*,fi]=*residual_hpx_arr[1,fi]
ENDFOR
Ptr_free,residual_hpx_arr

model_xx_cube=fltarr(n_hpx,n_freq)
model_yy_cube=fltarr(n_hpx,n_freq)
FOR fi=0L,n_freq-1 DO BEGIN
    model_xx_cube[*,fi]=*model_hpx_arr[0,fi]
    model_yy_cube[*,fi]=*model_hpx_arr[1,fi]
ENDFOR
Ptr_free,model_hpx_arr

weights_xx_cube=fltarr(n_hpx,n_freq)
weights_yy_cube=fltarr(n_hpx,n_freq)
FOR fi=0L,n_freq-1 DO BEGIN
    weights_xx_cube[*,fi]=*weights_hpx_arr[0,fi]
    weights_yy_cube[*,fi]=*weights_hpx_arr[1,fi]
ENDFOR
Ptr_free,weights_hpx_arr

save,filename=output_path,dirty_xx_cube,res_xx_cube,model_xx_cube,weights_xx_cube,$
    dirty_yy_cube,res_yy_cube,model_yy_cube,weights_yy_cube,obs_arr,nside,hpx_inds
END