FUNCTION fhd_init,obs,skymodel,file_path_fhd=file_path_fhd,gain_factor=gain_factor,$
    max_iter=max_iter,check_iter=check_iter,max_sources=max_sources,$
    smooth_width=smooth_width,beam_threshold=beam_threshold,deconvolution_add_threshold=deconvolution_add_threshold,$
    independent_fit=independent_fit,deconvolution_filter=deconvolution_filter,$
    beam_max_threshold=beam_max_threshold,deconvolution_convergence_sigma=deconvolution_convergence_sigma,local_max_radius=local_max_radius,$
    reject_pol_sources=reject_pol_sources,filter_background=filter_background,$
    galaxy_model_fit=galaxy_model_fit,transfer_mapfn=transfer_mapfn,$
    subtract_sidelobe_catalog=subtract_sidelobe_catalog,return_sidelobe_catalog=return_sidelobe_catalog,$
    joint_deconvolution_list=joint_deconvolution_list,dft_deconvolution_threshold=dft_deconvolution_threshold,$
    deconvolution_over_resolution=deconvolution_over_resolution,deconvolution_horizon_threshold=deconvolution_horizon_threshold


IF N_Elements(obs) EQ 0 THEN BEGIN
    dimension=1024. 
    n_pol=4
    beam_width=1.
    IF N_Elements(dft_deconvolution_threshold) EQ 0 THEN dft_deconvolution_threshold=1. 
ENDIF ELSE BEGIN
    IF N_Elements(dft_deconvolution_threshold) EQ 0 THEN $
        IF Keyword_Set(obs.dft_threshold) THEN dft_deconvolution_threshold=obs.dft_threshold ELSE dft_deconvolution_threshold=1. 
    dimension=obs.dimension
    n_pol=obs.n_pol
    beam_width=beam_width_calculate(obs,/fwhm)/2.
ENDELSE
;explicitly set to 0 to use analytic DFT. ;1/2 value of kernel_test along either axis at the edge of the image.
IF dft_deconvolution_threshold GE 1 THEN dft_deconvolution_threshold=1./((2.*!Pi)^2.*dimension) $ 
    ELSE dft_deconvolution_threshold=Float(dft_deconvolution_threshold)

IF N_Elements(file_path_fhd) EQ 0 THEN file_path_fhd=''
; Smoothing scale of the background filtering to remove large-scale fluctation. Equivalent to a smoothing radius in pixels.
IF N_Elements(smooth_width) EQ 0 THEN smooth_width=32 
IF N_Elements(filter_background) EQ 0 THEN filter_background=1 ; Flag to determine whether to subtract large-scale background fluctuations

; End iterative deconvolution if the S/N of the brightest component is below this threshold
IF N_Elements(deconvolution_convergence_sigma) EQ 0 THEN deconvolution_convergence_sigma=2.

IF N_Elements(beam_threshold) EQ 0 THEN beam_threshold=0.05 ;0.05 is really as far down as you should go with our current beam models!
IF N_Elements(beam_max_threshold) EQ 0 THEN beam_max_threshold=1e-4 ; Completely mask all pixels below this beam threshold.

IF N_Elements(max_sources) EQ 0 THEN max_sources=100000L ELSE max_sources=Long(max_sources) ; Maximum number of source components to deconvolve
IF N_Elements(gain_factor) EQ 0 THEN gain_factor=0.15 ; "clean gain" applied to detected sources before subtraction
IF N_Elements(deconvolution_add_threshold) EQ 0 THEN deconvolution_add_threshold=0.8 ;also fit additional components brighter than this threshold
IF N_Elements(local_max_radius) EQ 0 THEN local_max_radius=3. ; Sources must be brighter than all other pixels within this radius to be detected
IF N_Elements(max_iter) EQ 0 THEN max_iter=Ceil(Sqrt(max_sources))>10 ELSE max_iter=Fix(max_iter)
IF N_Elements(check_iter) EQ 0 THEN check_iter=Round(1./gain_factor)<5 ELSE check_iter=Fix(check_iter)

IF N_Elements(independent_fit) EQ 0 THEN independent_fit=0 ;set to 1 to fit I, Q, (U, V) seperately. Otherwise, only I (and U) is fit
IF N_Elements(reject_pol_sources) EQ 0 THEN reject_pol_sources=0 ;set to exclude source candidates with high Stokes Q/I

IF N_Elements(transfer_mapfn) EQ 0 THEN transfer_mapfn='False'
IF N_Elements(joint_deconvolution_list) LE 1 THEN decon_mode='Single snapshot' ELSE decon_mode='HEALPix integrated'
IF N_Elements(joint_deconvolution_list) EQ 0 THEN joint_obs=file_basename(file_path_fhd) ELSE joint_obs=file_basename(joint_deconvolution_list)

IF N_Elements(deconvolution_over_resolution) EQ 0 THEN over_resolution=2 ELSE over_resolution=deconvolution_over_resolution
IF N_Elements(deconvolution_horizon_threshold) EQ 0 THEN deconvolution_horizon_threshold=10. ;degrees above the horizon to exclude from deconvolution
IF N_Elements(deconvolution_filter) EQ 0 THEN deconvolution_filter='filter_uv_uniform' ; Filter function name of the weighting applied to the UV plane

IF N_Elements(galaxy_model_fit) EQ 0 THEN galaxy_model_fit=0 ; Set to model and subtract galactic emission prior to deconvolution
IF N_Elements(subtract_sidelobe_catalog) EQ 0 THEN sidelobe_subtract='' ELSE BEGIN ; Set to subtract sources outside the primary beam prior to deconvolution
    IF size(subtract_sidelobe_catalog,/type) EQ 7 THEN sidelobe_subtract=subtract_sidelobe_catalog ELSE BEGIN
        IF Keyword_Set(subtract_sidelobe_catalog) THEN IF N_Elements(skymodel) GT 0 THEN sidelobe_subtract=skymodel.catalog_name ELSE BEGIN
            IF N_Elements(obs) GT 0 THEN sidelobe_subtract=obs.instrument+'_calibration_source_list' ELSE sidelobe_subtract='' 
        ENDELSE
    ENDELSE
ENDELSE
IF N_Elements(return_sidelobe_catalog) EQ 0 THEN sidelobe_return=0 $
    ELSE sidelobe_return=Keyword_Set(sidelobe_subtract) ? return_sidelobe_catalog:0
end_condition='Not successfully run'
n_iter=0
n_components=0L
n_sources=0L
detection_threshold=0.
convergence=0.
info=Ptr_new()

fhd_params={npol:n_pol,beam_threshold:beam_threshold,max_iter:max_iter,max_sources:max_sources,check_iter:check_iter,$
    gain_factor:gain_factor,add_threshold:deconvolution_add_threshold,$
    over_resolution:over_resolution,dft_threshold:dft_deconvolution_threshold,independent_fit:independent_fit,$
    reject_pol_sources:reject_pol_sources,beam_max_threshold:beam_max_threshold,horizon_threshold:deconvolution_horizon_threshold,smooth_width:smooth_width,$
    sigma_cut:deconvolution_convergence_sigma,local_max_radius:local_max_radius,transfer_mapfn:transfer_mapfn,$
    galaxy_subtract:galaxy_model_fit,sidelobe_subtract:sidelobe_subtract,sidelobe_return:sidelobe_return,$
    filter_background:filter_background,decon_filter:deconvolution_filter,decon_mode:decon_mode,$
    joint_obs:joint_obs,end_condition:end_condition,n_iter:n_iter,n_components:n_components,n_sources:n_sources,$
    detection_threshold:detection_threshold,convergence:convergence,info:info}

RETURN,fhd_params
END
