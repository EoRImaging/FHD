FUNCTION visibility_patch_grid,visibility_ptr,flag_ptr,obs,psf,params,file_path_fhd,weights_ptr=weights_ptr,variance_ptr=variance_ptr,$
    timing=timing,polarization=polarization,silent=silent,time_arr=time_arr,no_save=no_save,$
    error=error,inds_patch=inds_patch,phase_x=phase_x,phase_y=phase_y,rephase_vis_flag=rephase_vis_flag,$
    obs_patch=obs_patch,psf_patch=psf_patch,_Extra=extra

kbinsize=obs.kpix
degpix=obs.degpix
dimension=obs.dimension
elements=obs.elements
n_pol=obs.n_pol
dummy_string=''
IF N_Elements(rephase_vis_flag) EQ 0 THEN rephase_vis_flag=1
IF N_Elements(phase_x) EQ 0 THEN phase_x=dimension/2.
IF N_Elements(phase_y) EQ 0 THEN phase_y=elements/2.

xy_inds=array_indices([dimension,elements],inds_patch,/dimension)
xvals=Reform(xy_inds[0,*])
yvals=Reform(xy_inds[1,*])
pdim=Max(xvals)-Min(xvals)+1
pelem=Max(yvals)-Min(yvals)+1
xvals=Reform(xvals,pdim,pelem)
yvals=Reform(yvals,pdim,pelem)
IF pdim LT pelem THEN pelem=pdim
IF pelem LT pdim THEN pdim=pelem
IF (pdim mod 2) EQ 1 THEN pdim-=1
IF (pelem mod 2) EQ 1 THEN pelem-=1
xvals=xvals[0:pdim-1,0:pelem-1]
yvals=yvals[0:pdim-1,0:pelem-1]

IF Keyword_Set(rephase_vis_flag) THEN BEGIN
;astr:astr}    
    obs_patch=obs
    astr=obs.astr
    obs_patch.dimension=pdim
    obs_patch.elements=pelem
    kbinsize*=pdim/dimension
    obs_patch.kpix=kbinsize
    xy2ad,phase_x,phase_y,astr,phasera,phasedec
    obs_patch.phasera=phasera
    obs_patch.phasedec=phasedec
    dx=dimension/2-(phase_x-pdim/2)
    dy=elements/2-(phase_y-pelem/2)
    obs_patch.obsx-=dx
    obs_patch.obsy-=dy
    obs_patch.zenx-=dx
    obs_patch.zeny-=dy
    astr.crpix-=[dx,dy]
    obs_patch.astr=astr
    
    psf_patch=beam_setup(obs_patch,dummy_string,restore_last=0,silent=silent,/no_save,_Extra=extra)
    
    kx_arr=params.uu/kbinsize
    ky_arr=params.vv/kbinsize
    ucen=Float(obs.freq#kx_arr)
    vcen=Float(obs.freq#ky_arr)
    phase_shift=Exp((2.*!Pi*Complex(0,1)/dimension)*((dx)*ucen+(dy)*vcen))
    FOR pol_i=0,n_pol-1 DO *visibility_ptr[pol_i]*=phase_shift
    rephase_vis_flag=0
ENDIF

image_uv_arr=Ptrarr(n_pol,/allocate)
IF Arg_present(weights_ptr) THEN weights_ptr=Ptrarr(n_pol,/allocate)
IF Arg_present(variance_ptr) THEN variance_ptr=Ptrarr(n_pol,/allocate)
FOR pol_i=0,n_pol-1 DO BEGIN
    IF Keyword_Set(weights_ptr) THEN weights=1 ELSE weights=0
    IF Keyword_Set(variance_ptr) THEN variance=1 ELSE variance=0
    *image_uv_arr[pol_i]=visibility_grid(visibility_ptr[pol_i],flag_ptr[pol_i],obs_patch,psf_patch,params,dummy_string,$
        timing=t_grid0,polarization=pol_i,weights=weights,variance=variance,silent=silent,$
        mapfn_recalculate=0,return_mapfn=0,error=error,/no_save,_Extra=extra)
    *weights_ptr[pol_i]=weights
    *variance_ptr[pol_i]=variance
ENDFOR    
;IF Keyword_Set(error) THEN RETURN
RETURN,image_uv
END