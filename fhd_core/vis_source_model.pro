FUNCTION vis_source_model,source_list,obs,status_str,psf,params,flag_ptr,cal,jones,model_uv_arr=model_uv_arr,file_path_fhd=file_path_fhd,$
    timing=timing,silent=silent,uv_mask=uv_mask,galaxy_calibrate=galaxy_calibrate,error=error,beam_arr=beam_arr,$
    fill_model_vis=fill_model_vis,use_pointing_center=use_pointing_center,vis_model_ptr=vis_model_ptr,$
    galaxy_model=galaxy_model,calibration_flag=calibration_flag,diffuse_calibrate=diffuse_calibrate,diffuse_model=diffuse_model,_Extra=extra

t0=Systime(1)
IF N_Elements(error) EQ 0 THEN error=0
IF N_Elements(file_path_fhd) EQ 0 THEN file_path_fhd=''
IF N_Elements(silent) EQ 0 THEN silent=1

IF N_Elements(obs) EQ 0 THEN fhd_save_io,status_str,obs,var='obs',/restore,file_path_fhd=file_path_fhd,_Extra=extra
IF N_Elements(psf) EQ 0 THEN fhd_save_io,status_str,psf,var='psf',/restore,file_path_fhd=file_path_fhd,_Extra=extra
IF N_Elements(params) EQ 0 THEN fhd_save_io,status_str,params,var='params',/restore,file_path_fhd=file_path_fhd,_Extra=extra
IF N_Elements(flag_ptr) EQ 0 THEN fhd_save_io,status_str,flag_ptr,var='flag_arr',/restore,file_path_fhd=file_path_fhd,_Extra=extra
IF N_Elements(jones) EQ 0 THEN fhd_save_io,status_str,jones,var='jones',/restore,file_path_fhd=file_path_fhd,_Extra=extra

galaxy_flag=0
IF Keyword_Set(calibration_flag) THEN BEGIN
    IF Keyword_Set(galaxy_calibrate) THEN galaxy_flag=1
    IF Keyword_Set(diffuse_calibrate) THEN diffuse_filepath=diffuse_calibrate
ENDIF ELSE BEGIN
    IF Keyword_Set(galaxy_model) THEN galaxy_flag=1
    IF Keyword_Set(diffuse_model) THEN diffuse_filepath=diffuse_model
ENDELSE
heap_gc

;IF Keyword_Set(flag_ptr) THEN flag_switch=1 ELSE flag_switch=0

pol_names=obs.pol_names

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
;IF ~Keyword_Set(uv_mask) THEN BEGIN
;    uv_mask=Fltarr(dimension,elements)
;    vis_count=visibility_count(obs,psf,params,flag_ptr=flag_ptr,no_conjugate=1,fill_model_vis=fill_model_vis,file_path_fhd=file_path_fhd)
;    mask_i_use=where(vis_count)
;    uv_mask[mask_i_use]=1
;ENDIF ELSE 
IF Keyword_Set(uv_mask) THEN uv_mask_use=uv_mask ELSE uv_mask_use=Fltarr(dimension,elements)+1
uv_mask_use[*,elements/2+psf.dim:*]=0. 

freq_bin_i=(*obs.baseline_info).fbin_i
nfreq_bin=Max(freq_bin_i)+1
frequency_array=(*obs.baseline_info).freq

;kx_arr=params.uu/kbinsize
;ky_arr=params.vv/kbinsize
;baseline_i=params.baseline_arr
nbaselines=obs.nbaselines
n_samples=obs.n_time
n_freq=obs.n_freq
n_freq_bin=N_Elements(freq_bin_i)
IF N_Elements(vis_model_ptr) LT n_pol THEN vis_model_ptr=intarr(n_pol)

vis_dimension=Float(nbaselines*n_samples)
n_sources=N_Elements(source_list)
IF size(cal,/type) EQ 8 THEN BEGIN
    cal.n_cal_src=n_sources
    cal.galaxy_cal=Keyword_Set(galaxy_calibrate)
ENDIF


IF Min(Ptr_valid(model_uv_arr)) EQ 0 THEN BEGIN
    model_uv_arr=Ptrarr(n_pol,/allocate)
    FOR pol_i=0,n_pol-1 DO *model_uv_arr[pol_i]=Complexarr(dimension,elements)
ENDIF

IF n_sources GT 1 THEN BEGIN ;test that there are actual sources in the source list
    ;convert Stokes entries to instrumental polarization (weighted by one factor of the beam) 
    ;NOTE this is for record-keeping purposes, since the Stokes flux values will actually be used
    source_list=stokes_cnv(source_list,jones,beam_arr=beam_arr,/inverse,_Extra=extra) 
    model_uv_arr1=source_dft_model(obs,jones,source_list,t_model=t_model,sigma_threshold=2.,uv_mask=uv_mask_use,_Extra=extra)
    FOR pol_i=0,n_pol-1 DO *model_uv_arr[pol_i]+=*model_uv_arr1[pol_i]
    undefine_fhd,model_uv_arr1
    IF ~Keyword_Set(silent) THEN print,"DFT timing: "+strn(t_model)+" (",strn(n_sources)+" sources)"
ENDIF


IF galaxy_flag THEN gal_model_uv=fhd_galaxy_model(obs,jones,antialias=1,/uv_return,_Extra=extra)
IF Min(Ptr_valid(gal_model_uv)) GT 0 THEN FOR pol_i=0,n_pol-1 DO *model_uv_arr[pol_i]+=*gal_model_uv[pol_i]*uv_mask_use

IF Keyword_Set(diffuse_filepath) THEN BEGIN
    IF file_test(diffuse_filepath) EQ 0 THEN diffuse_filepath=(file_search(diffuse_filepath+'*'))[0]
    IF Keyword_Set(calibration_flag) THEN print,"Reading diffuse model file for calibration: "+diffuse_filepath $
        ELSE print,"Reading diffuse model file for model subtraction: "+diffuse_filepath
    diffuse_model_uv=fhd_diffuse_model(obs,jones,/uv_return,model_filepath=diffuse_filepath,_Extra=extra)
    IF Max(Ptr_valid(diffuse_model_uv)) EQ 0 THEN print,"Error reading or building diffuse model. Null pointer returned!"
ENDIF
IF Min(Ptr_valid(diffuse_model_uv)) GT 0 THEN FOR pol_i=0,n_pol-1 DO *model_uv_arr[pol_i]+=*diffuse_model_uv[pol_i]*uv_mask_use

vis_arr=Ptrarr(n_pol)

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
        timing=t_degrid0,polarization=pol_i,fill_model_vis=fill_model_vis,vis_input_ptr=vis_model_ptr[pol_i])
    t_degrid[pol_i]=t_degrid0
ENDFOR
IF ~Keyword_Set(silent) THEN print,"Degridding timing: ",strn(t_degrid)

timing=Systime(1)-t0

RETURN,vis_arr
END