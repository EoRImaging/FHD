FUNCTION vis_source_model,source_list,obs,psf,params,flag_ptr,cal,jones,model_uv_arr=model_uv_arr,file_path_fhd=file_path_fhd,$
    timing=timing,silent=silent,uv_mask=uv_mask,galaxy_calibrate=galaxy_calibrate,error=error,beam_arr=beam_arr,$
    fill_model_vis=fill_model_vis,use_pointing_center=use_pointing_center,_Extra=extra

t0=Systime(1)
IF N_Elements(error) EQ 0 THEN error=0
IF N_Elements(file_path_fhd) EQ 0 THEN file_path_fhd=''
flags_filepath=file_path_fhd+'_flags.sav'
;vis_filepath=file_path_fhd+'_vis.sav'
params_filepath=file_path_fhd+'_params.sav'
psf_filepath=file_path_fhd+'_beams.sav'
obs_filepath=file_path_fhd+'_obs.sav'
IF N_Elements(silent) EQ 0 THEN silent=1

IF N_Elements(obs) EQ 0 THEN obs=getvar_savefile(obs_filepath,'obs')
IF N_Elements(psf) EQ 0 THEN psf=getvar_savefile(psf_filepath,'psf')
IF N_Elements(params) EQ 0 THEN params=getvar_savefile(params_filepath,'params')
IF N_Elements(flag_ptr) EQ 0 THEN flag_ptr=getvar_savefile(flags_filepath,'flag_arr')
IF N_Elements(jones) EQ 0 THEN jones=fhd_struct_init_jones(obs,file_path_fhd=file_path_fhd,/restore)

heap_gc

;IF Keyword_Set(flag_ptr) THEN flag_switch=1 ELSE flag_switch=0

pol_names=['xx','yy','xy','yx']

;extract information from the structures
n_pol=obs.n_pol
dimension=obs.dimension
elements=obs.elements
degpix=obs.degpix
kbinsize=obs.kpix
kx_span=kbinsize*dimension ;Units are # of wavelengths
ky_span=kx_span
icomp=Complex(0,1)

xvals=meshgrid(dimension,elements,1)-dimension/2
yvals=meshgrid(dimension,elements,2)-elements/2

;only the LOWER half of the u-v plane is used for gridding/degridding. 
; Visibilities that would land in the upper half use the complex conjugate of their mirror in the lower half 
IF ~Keyword_Set(uv_mask) THEN BEGIN
    uv_mask=Fltarr(dimension,elements)
    vis_count=visibility_count(obs,psf,params,flag_ptr=flag_ptr,no_conjugate=1,fill_model_vis=fill_model_vis)
    mask_i_use=where(vis_count)
    uv_mask[mask_i_use]=1
ENDIF ELSE uv_mask[*,elements/2+psf.dim:*]=0. 

IF Tag_exist(obs,'fbin_i') THEN freq_bin_i=obs.fbin_i ELSE freq_bin_i=(*obs.baseline_info).fbin_i
nfreq_bin=Max(freq_bin_i)+1
bin_offset=(*obs.baseline_info).bin_offset
IF Tag_exist(obs,'freq') THEN frequency_array=obs.freq ELSE frequency_array=(*obs.baseline_info).freq

kx_arr=params.uu/kbinsize
ky_arr=params.vv/kbinsize
baseline_i=params.baseline_arr
nbaselines=bin_offset[1]
n_samples=N_Elements(bin_offset)
n_freq=N_Elements(frequency_array)
n_freq_bin=N_Elements(freq_bin_i)

vis_dimension=Float(nbaselines*n_samples)
n_sources=N_Elements(source_list)
IF N_Elements(cal) NE 0 THEN BEGIN
    cal.n_cal_src=n_sources
    cal.galaxy_cal=Keyword_Set(galaxy_calibrate)
ENDIF

IF not Keyword_Set(model_uv_arr) THEN BEGIN
    ;convert Stokes entries to instrumental polarization (weighted by one factor of the beam) 
    ;NOTE this is for record-keeping purposes, since the Stokes flux values will actually be used
    source_list=stokes_cnv(source_list,jones,beam_arr=beam_arr,/inverse,_Extra=extra) 
    model_uv_arr=source_dft_model(obs,jones,source_list,t_model=t_model,sigma_threshold=2.,uv_mask=uv_mask)
    IF ~Keyword_Set(silent) THEN print,"DFT timing: "+strn(t_model)+" (",strn(n_sources)+" sources)"
ENDIF

IF Keyword_Set(galaxy_calibrate) THEN BEGIN
    gal_model_uv=fhd_galaxy_model(obs,jones,antialias=1,/uv_return,_Extra=extra)
    FOR pol_i=0,n_pol-1 DO *model_uv_arr[pol_i]+=*gal_model_uv[pol_i]
ENDIF

vis_arr=Ptrarr(n_pol)

psf_base=psf.base
psf_residuals_n=psf.res_n
psf_residuals_i=psf.res_i
psf_residuals_val=psf.res_val
psf_dim=Sqrt((Size(*psf_base[0],/dimension))[0])
psf_resolution=(Size(psf_base,/dimension))[2]

valid_test=fltarr(n_pol)
FOR pol_i=0,n_pol-1 DO valid_test[pol_i]=Total(Abs(*model_uv_arr[pol_i]))
IF min(valid_test) EQ 0 THEN BEGIN
    error=1
    print,"ERROR: Invalid calibration model."
    timing=Systime(1)-t0
    RETURN,vis_arr
ENDIF

t_degrid=Fltarr(n_pol)
FOR pol_i=0,n_pol-1 DO BEGIN
    vis_arr[pol_i]=visibility_degrid(*model_uv_arr[pol_i],flag_ptr[pol_i],obs,psf,params,silent=silent,$
        timing=t_degrid0,polarization=pol_i,fill_model_vis=fill_model_vis)
    t_degrid[pol_i]=t_degrid0
ENDFOR
IF ~Keyword_Set(silent) THEN print,"Degridding timing: ",strn(t_degrid)

timing=Systime(1)-t0

RETURN,vis_arr
END