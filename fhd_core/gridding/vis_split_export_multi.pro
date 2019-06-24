PRO vis_split_export_multi,hpx_inds,n_avg=n_avg,output_path=output_path,ps_dimension=ps_dimension,ps_fov=ps_fov,ps_degpix=ps_degpix,$
    ps_kbinsize=ps_kbinsize,ps_kspan=ps_kspan,ps_beam_threshold=ps_beam_threshold,ps_model_cube=ps_model_cube,$
    fhd_file_list=fhd_file_list,even_only=even_only,odd_only=odd_only,rephase_weights=rephase_weights,_Extra=extra
heap_gc

n_obs=N_Elements(fhd_file_list)
IF N_Elements(rephase_weights) EQ 0 THEN rephase_weights=1
base_filename_range=file_basename([fhd_file_list[0],fhd_file_list[n_obs-1]])
obs_range=strjoin(base_filename_range,'-',/single)
cube_filepath=output_path+'_'+obs_range
IF Keyword_Set(even_only) THEN cube_filepath+='_even'
IF Keyword_Set(odd_only) THEN cube_filepath+='_odd'
cube_filepath+='_cube.sav'
dir=file_dirname(output_path)
IF file_test(dir) EQ 0 THEN file_mkdir,dir   

FOR obs_i=0,n_obs-1 DO BEGIN
    file_path=fhd_file_list[obs_i]
    restore,file_path+'_obs.sav'    
    IF obs_i EQ 0 THEN obs_arr=Replicate(obs,n_obs) ELSE obs_arr[obs_i]=obs
ENDFOR

; We don't use the cross polarizations currently.
; To use them in the future, the healpix cubes will need to be complex for XY and YX
; See crosspol_reformat.pro for details on the way XY and YX are stored
n_pol=Min(obs_arr.n_pol) < 2
n_freq=Min(obs_arr.n_freq)
IF N_Elements(n_avg) EQ 0 THEN n_avg=Float(Round(n_freq/48.)) ;default of 48 output frequency bins
n_freq_use=Floor(n_freq/n_avg)
IF Keyword_Set(ps_beam_threshold) THEN beam_threshold=ps_beam_threshold ELSE beam_threshold=0.2

IF Keyword_Set(ps_kbinsize) THEN kbinsize=ps_kbinsize ELSE $
    IF Keyword_Set(ps_fov) THEN kbinsize=!RaDeg/ps_FoV ELSE kbinsize=Mean(obs_arr.kpix)
FoV_use=!RaDeg/kbinsize
 
IF Keyword_Set(ps_kspan) THEN dimension_use=ps_kspan/kbinsize ELSE $
    IF Keyword_Set(ps_dimension) THEN dimension_use=ps_dimension ELSE $
    IF Keyword_Set(ps_degpix) THEN dimension_use=FoV_use/ps_degpix ELSE dimension_use=FoV_use/Mean(obs_arr.degpix)

degpix_use=FoV_use/dimension_use
pix_sky=4.*!Pi*!RaDeg^2./degpix_use^2.
Nside_chk=2.^(Ceil(ALOG(Sqrt(pix_sky/12.))/ALOG(2))) ;=1024. for 0.1119 degrees/pixel
IF ~Keyword_Set(nside) THEN nside_use=Nside_chk
nside_use=nside_use>Nside_chk
IF Keyword_Set(nside) THEN nside_use=nside ELSE nside=nside_use

residual_flag=Round(Mean(obs_arr.residual)) ;use Mean() not Median() because Median() will give an error if there is only one element
model_flag=intarr(n_obs)+1
hpx_cnv=Ptrarr(n_obs)
psf_arr=Ptrarr(n_obs)
obs_flag=intarr(n_obs)+1

obs_out_arr=obs_arr
psf_out_arr=Ptrarr(n_obs)

FOR obs_i=0.,n_obs-1 DO BEGIN
    file_path_fhd=fhd_file_list[obs_i]
    model_flag[obs_i]=file_test(file_path_fhd+'_fhd.sav')
    obs_out=vis_struct_update_obs(obs_arr[obs_i],n_pol=n_pol,nfreq_avg=n_avg,FoV=FoV_use,dimension=dimension_use)
    obs_out_arr[obs_i]=obs_out
    dimension=obs_out.dimension
    elements=obs_out.elements
    
    psf=beam_setup(obs,file_path_fhd,/no_save,/silent)
    psf_arr[obs_i]=Ptr_new(psf)
    ps_psf_resolution=Round(psf.resolution*obs_out.kpix/obs.kpix)
    psf_out=beam_setup(obs_out,file_path_fhd,/no_save,psf_resolution=ps_psf_resolution,/silent)
    psf_out_arr[obs_i]=Ptr_new(psf_out)
    
    beam_base=Ptrarr(n_pol<2)
    FOR pol_i=0,n_pol-1 DO beam_base[pol_i]=Ptr_new(beam_image(psf_out,obs_out,pol_i=pol_i,/fast))
    beam_mask=fltarr(dimension,elements)+1.
    FOR pol_i=0,(n_pol<2)-1 DO BEGIN
        mask0=fltarr(dimension,elements)
        mask_i=region_grow(*beam_base[pol_i],Floor(obs_out.obsx)+Floor(dimension)*Floor(obs_out.obsy),thresh=[beam_threshold,max(*beam_base[pol_i])])
        mask0[mask_i]=1
        beam_mask*=mask0
    ENDFOR
    IF Total(beam_mask) EQ 0 THEN BEGIN
        obs_flag[obs_i]=0
        CONTINUE
    ENDIF

    hpx_cnv[obs_i]=healpix_cnv_generate(obs_out,file_path_fhd=file_path_fhd,nside=nside_use,mask=beam_mask,/no_save,/pointer,_Extra=extra) 
ENDFOR
model_flag=Min(model_flag)
IF Total(obs_flag) EQ 0 THEN RETURN
hpx_ind_map=healpix_combine_inds(hpx_cnv,hpx_inds=hpx_inds);,reverse_ind=reverse_inds)

n_hpx=N_Elements(hpx_inds)
t_hpx=Systime(1)


IF model_flag AND ~residual_flag THEN dirty_flag=1 ELSE dirty_flag=0
residual_hpx_arr=Ptrarr(n_pol,n_freq_use)
model_hpx_arr=Ptrarr(n_pol,n_freq_use)
dirty_hpx_arr=Ptrarr(n_pol,n_freq_use)
weights_hpx_arr=Ptrarr(n_pol,n_freq_use)
variance_hpx_arr=Ptrarr(n_pol,n_freq_use)
beam_hpx_arr=Ptrarr(n_pol,n_freq_use)
FOR pol_i=0,n_pol-1 DO BEGIN
    FOR freq_i=0,n_freq_use-1 DO BEGIN
        residual_hpx_arr[pol_i,freq_i]=Ptr_new(fltarr(n_hpx))
        IF model_flag THEN model_hpx_arr[pol_i,freq_i]=Ptr_new(fltarr(n_hpx))
        IF dirty_flag THEN dirty_hpx_arr[pol_i,freq_i]=Ptr_new(fltarr(n_hpx))
        weights_hpx_arr[pol_i,freq_i]=Ptr_new(fltarr(n_hpx))
        variance_hpx_arr[pol_i,freq_i]=Ptr_new(fltarr(n_hpx))
        beam_hpx_arr[pol_i,freq_i]=Ptr_new(fltarr(n_hpx))
    ENDFOR
ENDFOR    

t_hpx=Systime(1)-t_hpx
FOR obs_i=0,n_obs-1 DO BEGIN 
    IF obs_flag[obs_i] EQ 0 THEN CONTINUE
    file_path_fhd=fhd_file_list[obs_i]
    obs=obs_arr[obs_i]
    obs_out=obs_out_arr[obs_i]
    psf=*psf_arr[obs_i]  
    psf_out=*psf_out_arr[obs_i]  
    
    print,"Frequency splitting: "+file_basename(file_path_fhd)
    IF model_flag THEN source_array=getvar_savefile(file_path_fhd+'_fhd.sav','source_array')
    residual_arr1=vis_model_freq_split(obs,psf,obs_out=obs_out,psf_out=psf_out,source_list=source_array,rephase_weights=rephase_weights,$
        weights_arr=weights_arr1,variance_arr=variance_arr1,model_arr=model_arr1,n_avg=n_avg,timing=t_split1,/fft,$
        file_path_fhd=file_path_fhd,even_only=even_only,odd_only=odd_only,vis_n_arr=vis_n_arr,/preserve_visibilities,_Extra=extra)
    
    IF dirty_flag THEN BEGIN
        dirty_arr1=residual_arr1
        residual_arr1=Ptrarr(size(residual_arr1,/dimension),/allocate)
    ENDIF
    t_hpx0=Systime(1)
    FOR pol_i=0,n_pol-1 DO FOR freq_i=0,n_freq_use-1 DO BEGIN
        IF dirty_flag THEN IF stddev(*dirty_arr1[pol_i,freq_i]) EQ 0 THEN CONTINUE 
        IF ~dirty_flag THEN IF stddev(*residual_arr1[pol_i,freq_i]) EQ 0 THEN CONTINUE
        (*weights_hpx_arr[pol_i,freq_i])[*hpx_ind_map[obs_i]]+=healpix_cnv_apply((*weights_arr1[pol_i,freq_i]),hpx_cnv[obs_i])
        (*variance_hpx_arr[pol_i,freq_i])[*hpx_ind_map[obs_i]]+=healpix_cnv_apply((*variance_arr1[pol_i,freq_i]),hpx_cnv[obs_i])
        IF dirty_flag THEN *residual_arr1[pol_i,freq_i]=*dirty_arr1[pol_i,freq_i]-*model_arr1[pol_i,freq_i]
        (*residual_hpx_arr[pol_i,freq_i])[*hpx_ind_map[obs_i]]+=healpix_cnv_apply((*residual_arr1[pol_i,freq_i]),hpx_cnv[obs_i])
        IF dirty_flag THEN (*dirty_hpx_arr[pol_i,freq_i])[*hpx_ind_map[obs_i]]+=healpix_cnv_apply((*dirty_arr1[pol_i,freq_i]),hpx_cnv[obs_i])
        IF model_flag THEN (*model_hpx_arr[pol_i,freq_i])[*hpx_ind_map[obs_i]]+=healpix_cnv_apply((*model_arr1[pol_i,freq_i]),hpx_cnv[obs_i])
        (*beam_hpx_arr[pol_i,freq_i])[*hpx_ind_map[obs_i]]+=healpix_cnv_apply((*beam[pol_i,freq_i])^2.,hpx_cnv)
    ENDFOR
    t_hpx1=Systime(1)
    t_hpx+=t_hpx1-t_hpx0
ENDFOR
obs_arr=obs_out_arr

IF dirty_flag THEN BEGIN
    dirty_xx_cube=fltarr(n_hpx,n_freq_use)
    dirty_yy_cube=fltarr(n_hpx,n_freq_use)
    FOR fi=0L,n_freq_use-1 DO BEGIN
        ;write index in much more efficient memory access order
        dirty_xx_cube[n_hpx*fi]=Temporary(*dirty_hpx_arr[0,fi])
        dirty_yy_cube[n_hpx*fi]=Temporary(*dirty_hpx_arr[1,fi])
    ENDFOR
    Ptr_free,dirty_hpx_arr
ENDIF

IF model_flag THEN BEGIN
    model_xx_cube=fltarr(n_hpx,n_freq_use)
    model_yy_cube=fltarr(n_hpx,n_freq_use)
    FOR fi=0L,n_freq_use-1 DO BEGIN
        model_xx_cube[n_hpx*fi]=Temporary(*model_hpx_arr[0,fi])
        model_yy_cube[n_hpx*fi]=Temporary(*model_hpx_arr[1,fi])
    ENDFOR
    Ptr_free,model_hpx_arr
ENDIF

res_xx_cube=fltarr(n_hpx,n_freq_use)
res_yy_cube=fltarr(n_hpx,n_freq_use)
FOR fi=0L,n_freq_use-1 DO BEGIN
    res_xx_cube[n_hpx*fi]=Temporary(*residual_hpx_arr[0,fi])
    res_yy_cube[n_hpx*fi]=Temporary(*residual_hpx_arr[1,fi])
ENDFOR
Ptr_free,residual_hpx_arr

weights_xx_cube=fltarr(n_hpx,n_freq_use)
weights_yy_cube=fltarr(n_hpx,n_freq_use)
FOR fi=0L,n_freq_use-1 DO BEGIN
    weights_xx_cube[n_hpx*fi]=Temporary(*weights_hpx_arr[0,fi])
    weights_yy_cube[n_hpx*fi]=Temporary(*weights_hpx_arr[1,fi])
ENDFOR
Ptr_free,weights_hpx_arr

variance_xx_cube=fltarr(n_hpx,n_freq_use)
variance_yy_cube=fltarr(n_hpx,n_freq_use)
FOR fi=0L,n_freq_use-1 DO BEGIN
    variance_xx_cube[n_hpx*fi]=Temporary(*variance_hpx_arr[0,fi])
    variance_yy_cube[n_hpx*fi]=Temporary(*variance_hpx_arr[1,fi])
ENDFOR
Ptr_free,variance_hpx_arr

beam_xx_cube=fltarr(n_hpx,n_freq_use)
    beam_yy_cube=fltarr(n_hpx,n_freq_use)
    FOR fi=0L,n_freq_use-1 DO BEGIN
        beam_xx_cube[n_hpx*fi]=Temporary(*beam_hpx_arr[0,fi])
        beam_yy_cube[n_hpx*fi]=Temporary(*beam_hpx_arr[1,fi])
    ENDFOR
Ptr_free,beam_hpx_arr

save,filename=cube_filepath,/compress,dirty_xx_cube,model_xx_cube,weights_xx_cube,variance_xx_cube,res_xx_cube,$
    dirty_yy_cube,model_yy_cube,weights_yy_cube,variance_yy_cube,res_yy_cube,beam_xx_cube,beam_yy_cube,$
    obs_arr,nside,hpx_inds,n_avg,psf_arr
END