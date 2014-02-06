PRO healpix_snapshot_cube_generate,obs_in,psf_in,params,vis_arr,vis_model_ptr=vis_model_ptr,$
    file_path_fhd=file_path_fhd,ps_dimension=ps_dimension,ps_fov=ps_fov,ps_degpix=ps_degpix,$
    ps_kbinsize=ps_kbinsize,ps_kspan=ps_kspan,ps_beam_threshold=ps_beam_threshold,$
    rephase_weights=rephase_weights,n_avg=n_avg,flag_arr=flag_arr,split_ps_export=split_ps_export,_Extra=extra

t0=Systime(1)

IF N_Elements(silent) EQ 0 THEN silent=0
pol_names=['xx','yy','xy','yx']
flags_filepath=file_path_fhd+'_flags.sav'
params_filepath=file_path_fhd+'_params.sav'
psf_filepath=file_path_fhd+'_beams.sav'
obs_filepath=file_path_fhd+'_obs.sav'
vis_filepath=file_path_fhd+'_vis_'
IF N_Elements(obs_in) EQ 0 THEN obs_in=getvar_savefile(obs_filepath,'obs')
IF N_Elements(psf_in) EQ 0 THEN psf_in=beam_setup(obs_in,file_path_fhd,/no_save,/silent)
IF N_Elements(params) EQ 0 THEN params=getvar_savefile(params_filepath,'params')

n_pol=obs_in.n_pol
n_freq=obs_in.n_freq

IF N_Elements(n_avg) EQ 0 THEN n_avg=Float(Round(n_freq/48.)) ;default of 48 output frequency bins
n_freq_use=Floor(n_freq/n_avg)
IF Keyword_Set(ps_beam_threshold) THEN beam_threshold=ps_beam_threshold ELSE beam_threshold=0.2

IF Keyword_Set(ps_kbinsize) THEN kbinsize=ps_kbinsize ELSE $
    IF Keyword_Set(ps_fov) THEN kbinsize=!RaDeg/ps_FoV ELSE kbinsize=obs_in.kpix
FoV_use=!RaDeg/kbinsize
 
IF Keyword_Set(ps_kspan) THEN dimension_use=ps_kspan/kbinsize ELSE $
    IF Keyword_Set(ps_dimension) THEN dimension_use=ps_dimension ELSE $
    IF Keyword_Set(ps_degpix) THEN dimension_use=FoV_use/ps_degpix ELSE dimension_use=FoV_use/obs_in.degpix

degpix_use=FoV_use/dimension_use
pix_sky=4.*!Pi*!RaDeg^2./degpix_use^2.
Nside_chk=2.^(Ceil(ALOG(Sqrt(pix_sky/12.))/ALOG(2))) ;=1024. for 0.1119 degrees/pixel
IF ~Keyword_Set(nside) THEN nside_use=Nside_chk
nside_use=nside_use>Nside_chk
IF Keyword_Set(nside) THEN nside_use=nside ELSE nside=nside_use

obs_out=vis_struct_update_obs(obs_in,n_pol=n_pol,nfreq_avg=n_avg,FoV=FoV_use,dimension=dimension_use)
ps_psf_resolution=Round(psf_in.resolution*obs_out.kpix/obs_in.kpix)
psf_out=beam_setup(obs_out,file_path_fhd,/no_save,psf_resolution=ps_psf_resolution,/silent)

beam=Ptrarr(n_pol,n_freq_use,/allocate)
beam_mask=fltarr(dimension_use,dimension_use)+1.
FOR pol_i=0,n_pol-1 DO FOR fi=0L,n_freq_use-1 DO BEGIN
    *beam[pol_i,fi]=Sqrt(beam_image(psf_out,obs_out,pol_i=pol_i,freq_i=fi,/square)>0.)
    b_i=obs_out.obsx+obs_out.obsy*dimension_use
    beam_i=region_grow(*beam[pol_i,fi],b_i,thresh=[0,max(*beam[pol_i,fi])])
    beam_mask1=fltarr(dimension_use,dimension_use)
    beam_mask1[beam_i]=1.
    beam_mask*=beam_mask1
ENDFOR

fhd_log_settings,file_path_fhd+'_ps',obs=obs_out,psf=psf_out
hpx_cnv=healpix_cnv_generate(obs_out,file_path_fhd=file_path_fhd,nside=nside_use,$
    mask=beam_mask,restore_last=0,/no_save,hpx_radius=FoV_use/sqrt(2.))
hpx_inds=hpx_cnv.inds
n_hpx=N_Elements(hpx_inds)


IF N_Elements(flag_arr) LT n_pol THEN flag_arr=getvar_savefile(flags_filepath,'flag_arr')
flags_use=Ptrarr(n_pol,/allocate)

bin_start=(*obs_out.baseline_info).bin_offset
nt=N_Elements(bin_start)
nb=(size(*flag_arr[0],/dimension))[1]
bin_end=fltarr(nt)
bin_end[0:nt-2]=bin_start[1:nt-1]-1
bin_end[nt-1]=nb-1
bin_i=lonarr(nb)-1
nt2=Floor(nt/2)
FOR t_i=0,2*nt2-1 DO bin_i[bin_start[t_i]:bin_end[t_i]]=t_i
bi_n=findgen(nb)

IF Keyword_Set(split_ps_export) THEN BEGIN
    n_iter=2
    bi_use=Ptrarr(n_iter,/allocate)
    *bi_use[0]=where(bin_i mod 2 EQ 0)
    *bi_use[1]=where(bin_i mod 2 EQ 1)
    filepath_cube=file_path_fhd+['_even_cube.sav','_odd_cube.sav']
ENDIF ELSE BEGIN
    n_iter=1
    bi_use=Ptrarr(n_iter,/allocate)
    *bi_use[0]=lindgen(nb)
    filepath_cube=file_path_fhd+'_cube.sav'
ENDELSE

IF Min(Ptr_valid(vis_arr)) EQ 0 THEN vis_arr=Ptrarr(n_pol,/allocate)
IF N_Elements(*vis_arr[0]) EQ 0 THEN FOR pol_i=0,n_pol-1 DO vis_arr[pol_i]=$
    getvar_savefile(vis_filepath+pol_names[pol_i]+'.sav','vis_ptr',verbose=~silent)

residual_flag=obs_out.residual
model_flag=0
vis_noise_calc,obs_out,vis_arr,flag_arr

IF Min(Ptr_valid(vis_model_ptr)) THEN IF N_Elements(*vis_model_ptr[0]) GT 0 THEN model_flag=1
IF residual_flag EQ 0 THEN IF model_flag EQ 0 THEN BEGIN
    model_flag=1
    vis_model_ptr=Ptrarr(n_pol)
    FOR pol_i=0,n_pol-1 DO model_flag*=file_test(vis_filepath+'model_'+pol_names[pol_i]+'.sav')
    IF model_flag EQ 1 THEN FOR pol_i=0,n_pol-1 DO $
        vis_model_ptr[pol_i]=getvar_savefile(vis_filepath+'model_'+pol_names[pol_i]+'.sav','vis_ptr',verbose=~silent)
ENDIF
IF model_flag AND ~residual_flag THEN dirty_flag=1 ELSE dirty_flag=0

t_hpx=0.
t_split=0.
FOR iter=0,n_iter-1 DO BEGIN
    FOR pol_i=0,n_pol-1 DO BEGIN
        flag_arr1=fltarr(size(*flag_arr[pol_i],/dimension))
        flag_arr1[*,*bi_use[iter]]=(*flag_arr[pol_i])[*,*bi_use[iter]]
        *flags_use[pol_i]=flag_arr1
    ENDFOR
    obs=obs_out ;will have some values over-written!
    psf=psf_out
    
    residual_arr1=vis_model_freq_split(obs_in,psf_in,params,flags_use,obs_out=obs,psf_out=psf,/rephase_weights,$
        weights_arr=weights_arr1,variance_arr=variance_arr1,model_arr=model_arr1,n_avg=n_avg,timing=t_split1,/fft,$
        file_path_fhd=file_path_fhd,vis_n_arr=vis_n_arr,/preserve_visibilities,vis_data_arr=vis_arr,vis_model_arr=vis_model_ptr)
    t_split+=t_split1
    IF dirty_flag THEN BEGIN
        dirty_arr1=residual_arr1
        residual_arr1=Ptrarr(size(residual_arr1,/dimension),/allocate)
    ENDIF
    
    residual_hpx_arr=Ptrarr(n_pol,n_freq_use,/allocate)
    model_hpx_arr=Ptrarr(n_pol,n_freq_use,/allocate)
    dirty_hpx_arr=Ptrarr(n_pol,n_freq_use,/allocate)
    weights_hpx_arr=Ptrarr(n_pol,n_freq_use,/allocate)
    variance_hpx_arr=Ptrarr(n_pol,n_freq_use,/allocate)
    beam_hpx_arr=Ptrarr(n_pol,n_freq_use,/allocate)
    t_hpx0=Systime(1)
    FOR pol_i=0,n_pol-1 DO FOR freq_i=0,n_freq_use-1 DO BEGIN
        *weights_hpx_arr[pol_i,freq_i]=healpix_cnv_apply((*weights_arr1[pol_i,freq_i]),hpx_cnv)
        *variance_hpx_arr[pol_i,freq_i]=healpix_cnv_apply((*variance_arr1[pol_i,freq_i]),hpx_cnv)
        IF dirty_flag THEN *residual_arr1[pol_i,freq_i]=*dirty_arr1[pol_i,freq_i]-*model_arr1[pol_i,freq_i]
        *residual_hpx_arr[pol_i,freq_i]=healpix_cnv_apply((*residual_arr1[pol_i,freq_i]),hpx_cnv)
        IF dirty_flag THEN *dirty_hpx_arr[pol_i,freq_i]=healpix_cnv_apply((*dirty_arr1[pol_i,freq_i]),hpx_cnv)
        IF model_flag THEN *model_hpx_arr[pol_i,freq_i]=healpix_cnv_apply((*model_arr1[pol_i,freq_i]),hpx_cnv)
        *beam_hpx_arr[pol_i,freq_i]=healpix_cnv_apply((*beam[pol_i,freq_i])^2.,hpx_cnv)
    ENDFOR
    t_hpx+=Systime(1)-t_hpx0
    
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
    
    save,filename=filepath_cube[iter],/compress,dirty_xx_cube,model_xx_cube,weights_xx_cube,variance_xx_cube,res_xx_cube,$
        dirty_yy_cube,model_yy_cube,weights_yy_cube,variance_yy_cube,res_yy_cube,beam_xx_cube,beam_yy_cube,$
        obs,nside,hpx_inds,n_avg,psf
ENDFOR
timing=Systime(1)-t0
IF ~Keyword_Set(silent) THEN print,'HEALPix cube export timing: ',timing,t_split,t_hpx
END