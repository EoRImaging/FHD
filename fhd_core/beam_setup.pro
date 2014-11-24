FUNCTION beam_setup,obs,status_str,antenna,file_path_fhd=file_path_fhd,restore_last=restore_last,timing=timing,$
    residual_tolerance=residual_tolerance,residual_threshold=residual_threshold,beam_mask_threshold=beam_mask_threshold,$
    silent=silent,psf_dim=psf_dim,psf_resolution=psf_resolution,psf_image_resolution=psf_image_resolution,$
    swap_pol=swap_pol,no_complex_beam=no_complex_beam,no_save=no_save,beam_pol_test=beam_pol_test,$
    beam_model_version=beam_model_version,beam_dim_fit=beam_dim_fit,_Extra=extra

compile_opt idl2,strictarrsubs  
t00=Systime(1)

antenna_flag=Arg_present(antenna)
IF N_Elements(file_path_fhd) EQ 0 THEN file_path_fhd=''
IF Keyword_Set(restore_last) THEN BEGIN
    fhd_save_io,status_str,psf,var='psf',/restore,file_path_fhd=file_path_fhd
    IF antenna_flag THEN fhd_save_io,status_str,antenna,var='antenna',/restore,file_path_fhd=file_path_fhd
    IF Keyword_Set(psf) THEN RETURN,psf $
        ELSE IF not Keyword_Set(silent) THEN print,"Saved beam model not found. Recalculating."
ENDIF

IF N_Elements(obs) EQ 0 THEN fhd_save_io,status_str,obs,var='obs',/restore,file_path_fhd=file_path_fhd
;Fixed parameters 
;extract information from the structures
n_tiles=obs.n_tile
n_freq=obs.n_freq
n_pol=obs.n_pol

;obsra=obs.obsra
;obsdec=obs.obsdec
;zenra=obs.zenra
;zendec=obs.zendec
;phasera=obs.phasera
;phasedec=obs.phasedec
;Jdate=obs.Jd0
;frequency_array=(*obs.baseline_info).freq
freq_bin_i=(*obs.baseline_info).fbin_i
nfreq_bin=Max(freq_bin_i)+1

tile_A=(*obs.baseline_info).tile_A
tile_B=(*obs.baseline_info).tile_B
nbaselines=obs.nbaselines

dimension=obs.dimension
elements=obs.elements
;kbinsize=obs.kpix
;kx_span=kbinsize*dimension ;Units are # of wavelengths
;ky_span=kx_span
degpix=obs.degpix
;astr=obs.astr

antenna=fhd_struct_init_antenna(obs,beam_model_version=beam_model_version,psf_resolution=psf_resolution,psf_dim=psf_dim,$
    psf_intermediate_res=psf_intermediate_res,psf_image_resolution=psf_image_resolution,timing=t_ant,_Extra=extra)

;IF tag_exist(obs,'delays') THEN delay_settings=obs.delays
;IF Tag_exist(obs,'alpha') THEN alpha=obs.alpha ELSE alpha=0.

IF Keyword_Set(swap_pol) THEN pol_arr=[[1,1],[0,0],[1,0],[0,1]] ELSE pol_arr=[[0,0],[1,1],[0,1],[1,0]] 

psf_image_dim=psf_dim*psf_image_resolution*psf_intermediate_res ;use a larger box to build the model than will ultimately be used, to allow higher resolution in the initial image space beam model

;;residual_tolerance is residual as fraction of psf_base above which to include 
;IF N_Elements(residual_tolerance) EQ 0 THEN residual_tolerance=1./100.  
;;residual_threshold is minimum residual above which to include
;IF N_Elements(residual_threshold) EQ 0 THEN residual_threshold=0.
IF N_Elements(beam_mask_threshold) EQ 0 THEN beam_mask_threshold=1E2

;;begin forming psf
psf_xvals=Ptrarr(psf_resolution,psf_resolution,/allocate)
psf_yvals=Ptrarr(psf_resolution,psf_resolution,/allocate)
xvals_i=Reform(meshgrid(psf_dim,psf_dim,1)*psf_resolution,psf_dim^2.)
yvals_i=Reform(meshgrid(psf_dim,psf_dim,2)*psf_resolution,psf_dim^2.)
FOR i=0,psf_resolution-1 DO FOR j=0,psf_resolution-1 DO BEGIN 
    *psf_xvals[i,j]=meshgrid(psf_dim,psf_dim,1)-psf_dim/2.+Float(i)/psf_resolution
    *psf_yvals[i,j]=meshgrid(psf_dim,psf_dim,2)-psf_dim/2.+Float(j)/psf_resolution
ENDFOR

;;set up coordinates to generate the high uv resolution model. 
;;Remember that field of view = uv resolution, image pixel scale = uv span. 
;;So, the cropped uv span (psf_dim) means we do not need to calculate at full image resolution, 
;;   while the increased uv resolution can correspond to super-horizon scales. We construct the beam model in image space, 
;;   and while we don't need the full image resolution we need to avoid quantization errors that come in if we make too small an image and then take the FFT
psf_intermediate_res=(Ceil(Sqrt(psf_resolution)/2)*2.)<psf_resolution
psf_image_dim=psf_dim*psf_image_resolution*psf_intermediate_res ;use a larger box to build the model than will ultimately be used, to allow higher resolution in the initial image space beam model
image_res_scale=dimension*psf_intermediate_res/psf_image_dim
zen_int_x=(obs.zenx-obs.obsx)/image_res_scale+psf_image_dim/2
zen_int_y=(obs.zeny-obs.obsy)/image_res_scale+psf_image_dim/2
psf_superres_dim=psf_dim*psf_resolution
xvals_uv_superres=meshgrid(psf_superres_dim,psf_superres_dim,1)/(Float(psf_resolution)/psf_intermediate_res)-Floor(psf_dim/2)*psf_intermediate_res+Floor(psf_image_dim/2)
yvals_uv_superres=meshgrid(psf_superres_dim,psf_superres_dim,2)/(Float(psf_resolution)/psf_intermediate_res)-Floor(psf_dim/2)*psf_intermediate_res+Floor(psf_image_dim/2)

complex_flag_arr=intarr(n_pol,nfreq_bin)
beam_arr=Ptrarr(n_pol,nfreq_bin,nbaselines)
ant_A_list=tile_A[0:nbaselines-1]
ant_B_list=tile_B[0:nbaselines-1]
baseline_mod=2.^(Ceil(Alog(Sqrt(nbaselines*2.-n_tiles))/Alog(2.)))>(Max(ant_A_list)>Max(ant_B_list))
bi_list=ant_B_list+ant_A_list*baseline_mod
bi_hist0=histogram(bi_list,min=0,omax=bi_max,/binsize,reverse_indices=ri_bi)

group_arr=Lonarr(n_pol,nfreq_bin,nbaselines)-1
FOR pol_i=0,n_pol-1 DO BEGIN

    ant_pol1=pol_arr[0,pol_i]
    ant_pol1x=Abs(1-ant_pol1)
    ant_pol2=pol_arr[1,pol_i]
    ant_pol2x=Abs(1-ant_pol2)
    
    group1=antenna.group_id[ant_pol1,*]
    group2=antenna.group_id[ant_pol2,*]
    
    hgroup1=histogram(group1,min=0,/binsize,reverse=gri1)
    hgroup2=histogram(group2,min=0,/binsize,reverse=gri2)
    ng1=N_Elements(hgroup1)
    ng2=N_Elements(hgroup2)
    group_matrix=hgroup1#hgroup2
    gi_use=where(group_matrix,n_group)
    freq_center=antenna[0].freq ;all antennas need to have the same frequency coverage, so just take the first    
    
    FOR freq_i=0,nfreq_bin-1 DO BEGIN        
        t2_a=Systime(1)
        
        FOR g_i=0L,n_group-1 DO BEGIN
            g_i1=gi_use[g_i] mod ng1
            g_i2=Floor(gi_use[g_i]/ng1)
            
            baseline_group_n=group_matrix[g_i1,g_i2]
            IF baseline_group_n LE 0 THEN CONTINUE
            
            ant_1_arr=gri1[gri1[g_i1]:gri1[g_i1+1]-1]
            ant_2_arr=gri2[gri2[g_i2]:gri2[g_i2+1]-1]
            ant_1=ant_1_arr[0]
            ant_2=ant_2_arr[0]
            
            ant_1_n=hgroup1[g_i1]
            ant_2_n=hgroup2[g_i2]
            
            bi_use=Reform(rebin((ant_2_arr+1),ant_1_n,ant_2_n)+Rebin(Transpose(ant_1_arr+1),ant_1_n,ant_2_n)*baseline_mod,baseline_group_n)
            IF Max(bi_use) GT bi_max THEN bi_use=bi_use[where(bi_use LE bi_max)]
            bi_use_i=where(bi_hist0[bi_use],n_use)
            IF n_use GT 0 THEN bi_use=bi_use[bi_use_i]
            baseline_group_n=N_Elements(bi_use)
            bi_inds=ri_bi[ri_bi[bi_use]] ;use these indices to index the reverse indices of the original baseline index histogram
            group_arr[pol_i,freq_i,bi_inds]=g_i
            
            psf_base_superres=beam_power(antenna[ant_1],antenna[ant_2],ant_pol1=ant_pol1,ant_pol2=ant_pol2,$
                freq_i=freq_i,psf_image_dim=psf_image_dim,psf_intermediate_res=psf_intermediate_res,$
                xvals_uv_superres=xvals_uv_superres,yvals_uv_superres=yvals_uv_superres,$
                beam_mask_threshold=beam_mask_threshold,zen_int_x=zen_int_x,zen_int_y=zen_int_y,_Extra=extra)
            
            psf_single=Ptrarr(psf_resolution,psf_resolution)
            FOR i=0,psf_resolution-1 DO FOR j=0,psf_resolution-1 DO psf_single[psf_resolution-1-i,psf_resolution-1-j]=Ptr_new(psf_base_superres[xvals_i+i,yvals_i+j]) 
            psf_single=Ptr_new(psf_single)
            FOR bii=0L,baseline_group_n-1 DO beam_arr[pol_i,freq_i,bi_inds[bii]]=psf_single
        ENDFOR
    ENDFOR
ENDFOR

;higher than necessary psf_dim is VERY computationally expensive, but we also don't want to crop the beam if there is real signal
;   So, in case a larger than necessary psf_dim was specified above, reduce it now if that is safe
IF Keyword_Set(beam_dim_fit) THEN beam_dim_fit,beam_arr,psf_dim=psf_dim,psf_resolution=psf_resolution,beam_mask_threshold=beam_mask_threshold,$
    psf_xvals=psf_xvals,psf_yvals=psf_yvals,_Extra=extra

complex_flag=1
beam_ptr=Ptr_new(beam_arr)
psf=fhd_struct_init_psf(beam_ptr=beam_ptr,xvals=psf_xvals,yvals=psf_yvals,fbin_i=freq_bin_i,$
    psf_resolution=psf_resolution,psf_dim=psf_dim,complex_flag=complex_flag,pol_norm=pol_norm,freq_norm=freq_norm,$
    n_pol=n_pol,n_freq=nfreq_bin,freq_cen=freq_center,group_arr=group_arr)
    
fhd_save_io,status_str,psf,var='psf',/compress,file_path_fhd=file_path_fhd,no_save=no_save
fhd_save_io,status_str,antenna,var='antenna',/compress,file_path_fhd=file_path_fhd,no_save=no_save
IF not antenna_flag THEN undefine_fhd,antenna
timing=Systime(1)-t00
RETURN,psf
END
