FUNCTION visibility_grid_wrap,vis_arr,vis_weights,obs,status_str,psf,params,file_path_fhd=file_path_fhd,vis_model_arr=vis_model_arr,$
    deconvolve=deconvolve,model_flag=model_flag,snapshot_healpix_export=snapshot_healpix_export,mapfn_recalculate=mapfn_recalculate,$
    save_visibilities=save_visibilities,error=error,no_save=no_save,weights_arr=weights_arr,model_uv_holo=model_uv_holo,wstacking=wstacking,$
    aw_projection=aw_projection,_Extra=extra
    
n_pol=obs.n_pol
t_grid=fltarr(n_pol)
t_mapfn_gen=fltarr(n_pol)
print,'Gridding visibilities'
IF Keyword_Set(deconvolve) THEN map_fn_arr=Ptrarr(n_pol)
image_uv_arr=Ptrarr(n_pol,/allocate)
weights_arr=Ptrarr(n_pol,/allocate)

if keyword_set(wstacking) OR keyword_set(aw_projection) then begin
    ; If performing analysis for combining uvw planes across observations, make sure the visibilities
    ; are phased to the common phase centre across observations and pointings (i.e. EoR0 for EoR0 observations)
    rephase_uv_data, vis_arr, vis_model_arr, vis_weights, obs=obs, params=params
    obs = structure_update(obs, _Extra={w_phasera: obs.w_phasera, w_phasedec: obs.w_phasedec})
endif
if keyword_set(aw_projection) then begin
    ; If performing aw projection, pre-compute x and y pierce-point grids for the beam pointer
    psf_dim3 = LONG64(psf.dim*psf.dim)
    xvals_i=Reform(meshgrid(psf.dim,psf.dim,1)*psf.resolution,psf_dim3)
    yvals_i=Reform(meshgrid(psf.dim,psf.dim,2)*psf.resolution,psf_dim3)
    ; Precompute index arrays for main block
    x_grid = ulong(rebin(xvals_i, psf_dim3, psf.resolution) + rebin(transpose(lindgen(psf.resolution)), psf_dim3, psf.resolution))
    y_grid = ulong(rebin(yvals_i, psf_dim3, psf.resolution) + rebin(transpose(lindgen(psf.resolution)), psf_dim3, psf.resolution))
endif

IF Keyword_Set(model_flag) THEN model_uv_holo=Ptrarr(n_pol,/allocate)
IF N_Elements(weights_grid) EQ 0 THEN weights_grid=1
FOR pol_i=0,n_pol-1 DO BEGIN
    IF Keyword_Set(model_flag) THEN model_return=1
    IF Keyword_Set(snapshot_healpix_export) THEN preserve_visibilities=1 ELSE preserve_visibilities=0
    IF Keyword_Set(preserve_visibilities) THEN return_mapfn=0 ELSE return_mapfn=mapfn_recalculate
    IF Keyword_Set(mapfn_recalculate) AND Keyword_Set(save_visibilities) THEN preserve_vis_grid=0 ELSE preserve_vis_grid=preserve_visibilities
    IF pol_i EQ 0 THEN uniform_filter=1 ELSE uniform_filter=0
    IF pol_i GT 1 THEN no_conjugate=1 ELSE no_conjugate=0 
    grid_uv=visibility_grid(vis_arr[pol_i],vis_weights[pol_i],obs,status_str,psf,params,file_path_fhd=file_path_fhd,$
        timing=t_grid0,polarization=pol_i,weights=weights_grid,silent=silent,uniform_filter=uniform_filter,$
        mapfn_recalculate=mapfn_recalculate,return_mapfn=return_mapfn,error=error,no_save=no_save,$
        model_return=model_return,model_ptr=vis_model_arr[pol_i],preserve_visibilities=preserve_vis_grid,$
        no_conjugate=no_conjugate,wstacking=wstacking,$
            aw_projection=aw_projection,x_grid=x_grid,y_grid=y_grid,_Extra=extra)
    IF Keyword_Set(error) THEN BEGIN
        print,"Error occured during gridding. Returning."
        IF Keyword_Set(!Journal) THEN Journal ;write and close log file if present
        RETURN,image_uv_arr
    ENDIF
    t_grid[pol_i]=t_grid0
    IF pol_i EQ 0 THEN fhd_save_io,status_str,uniform_filter,var='vis_count',/compress,file_path_fhd=file_path_fhd,_Extra=extra

    IF Keyword_Set(deconvolve) THEN IF Keyword_Set(return_mapfn) THEN map_fn_arr[pol_i]=Ptr_new(return_mapfn,/no_copy)
    *image_uv_arr[pol_i]=Temporary(grid_uv)
    IF Keyword_Set(model_flag) THEN $
        *model_uv_holo[pol_i]=Temporary(model_return)
    IF N_Elements(weights_grid) GT 0 THEN BEGIN
        *weights_arr[pol_i]=Temporary(weights_grid)
        weights_grid=1
    ENDIF
ENDFOR

image_uv_arr=crosspol_reformat(image_uv_arr)
weights_arr=crosspol_reformat(weights_arr)
IF Keyword_Set(model_flag) THEN $
    model_uv_holo=crosspol_reformat(model_uv_holo)
FOR pol_i=0, n_pol-1 DO BEGIN
    fhd_save_io,status_str,*image_uv_arr[pol_i],var='grid_uv',/compress,file_path_fhd=file_path_fhd,pol_i=pol_i,obs=obs,_Extra=extra
    fhd_save_io,status_str,*weights_arr[pol_i],var='weights_uv',/compress,file_path_fhd=file_path_fhd,pol_i=pol_i,obs=obs,_Extra=extra
    IF Keyword_Set(model_flag) THEN $
        fhd_save_io,status_str,*model_uv_holo[pol_i],var='grid_uv_model',/compress,file_path_fhd=file_path_fhd,pol_i=pol_i,obs=obs,_Extra=extra
ENDFOR

fhd_save_io,status_str,obs,var='obs',/compress,file_path_fhd=file_path_fhd,_Extra=extra ;need to save here to enter nf_vis
IF ~Keyword_Set(silent) THEN print,'Gridding time:',t_grid
RETURN,image_uv_arr
END