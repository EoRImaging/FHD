PRO fhd_multi,obs_arr,_Extra=extra

compile_opt idl2,strictarrsubs  

n_obs=N_Elements(obs_arr)
fhd=fhd_init(*obs_arr[0],_Extra=extra) ;use the same deconvolution parameters for all observations

n_pol=fhd.npol
baseline_threshold=fhd.baseline_threshold
gain_factor=fhd.gain_factor
mapfn_interval=fhd.mapfn_interval
max_iter=fhd.max_iter
max_sources=fhd.max_sources
check_iter=fhd.check_iter
mapfn_threshold=fhd.mapfn_threshold
beam_threshold=fhd.beam_threshold
add_threshold=fhd.add_threshold
max_add_sources=fhd.max_add_sources
local_max_radius=fhd.local_max_radius
pol_use=fhd.pol_use
independent_fit=fhd.independent_fit
reject_pol_sources=fhd.reject_pol_sources

icomp=Complex(0,1)
beam_max_threshold=fhd.beam_max_threshold
smooth_width=fhd.smooth_width

beam=Ptrarr(n_pol,n_obs,/allocate)
beam_corr=Ptrarr(n_pol,n_obs,/allocate)
weights_arr=Ptrarr(n_pol,n_obs,/allocate) ;this one will be in Healpix pixels
;weights_inv_arr=Ptrarr(n_pol,n_obs,/allocate) ;this one will be in Healpix pixels
map_fn_arr=Ptrarr(n_pol,n_obs,/allocate)
dirty_uv_arr=Ptrarr(n_pol,n_obs,/allocate) 
model_uv_holo=Ptrarr(n_pol,n_obs,/allocate)
model_uv_full=Ptrarr(n_pol,n_obs,/allocate)

uv_mask_arr=Ptrarr(n_obs,/allocate)
comp_arr=Ptrarr(n_obs,/allocate)
ind_arr=Ptrarr(n_obs,/allocate)
hpx_cnv=Ptrarr(n_obs,/allocate)
xv_arr=Ptrarr(n_obs,/allocate)
yv_arr=Ptrarr(n_obs,/allocate)

box_coords=Lonarr(n_obs,4)

FOR obs_i=0.,n_obs-1 DO BEGIN
    obs=*obs_arr[obs_i]
;    vis_coordinates,obs,ra_arr,dec_arr,astr=astr
    dimension=obs.dimension
    elements=obs.elements
    xvals=meshgrid(dimension,elements,1)-dimension/2
    yvals=meshgrid(dimension,elements,2)-elements/2
    
    psf=beam_setup(obs,restore_last=1,silent=1)
    FOR pol_i=0,n_pol-1 DO *beam[pol_i,obs_i]=beam_image(psf,pol_i=pol_i,dimension=obs.dimension)
    
    beam_mask=fltarr(obs.dimension,obs.elements)+1
    FOR pol_i=0,(n_pol<2)-1 DO BEGIN
        mask0=fltarr(obs.dimension,obs.elements)
        mask_i=region_grow(*beam[pol_i,obs_i],obs.obsx+obs.dimension*obs.obsy,thresh=[0.05,max(*beam[pol_i,obs_i])])
        mask0[mask_i]=1
        beam_mask*=mask0
    ENDFOR
    FOR pol_i=0,n_pol-1 DO *beam_corr[pol_i,obs_i]=weight_invert(*beam[pol_i,obs_i]*beam_mask)
    
    *hpx_cnv[obs_i]=healpix_cnv_generate(obs,nside=nside,mask=beam_mask,radius=radius,restore_last=1)
    
    source_comp_init,comp_arr0,n_sources=max_sources
    *comp_arr[obs_i]=comp_arr0
    
    FOR pol_i=0,n_pol-1 DO BEGIN
        restore,filename=file_path+'_uv_'+pol_names[pol_i]+'.sav' ; dirty_uv,weights_grid
        *dirty_uv_arr[pol_i,obs_i]=dirty_uv*obs.cal[pol_i]
        *model_uv_full[pol_i,obs_i]=Complexarr(dimension,elements)
        *model_uv_holo[pol_i,obs_i]=Complexarr(dimension,elements)
        *weights_arr[pol_i,obs_i]=healpix_cnv_apply(*beam[pol_i,obs_i],*hpx_cnv[obs_i])
;        *weights_inv_arr[pol_i,obs_i]=healpix_cnv_apply(*beam_corr[pol_i,obs_i],*hpx_cnv[obs_i])
    ENDFOR
    
    source_uv_mask=fltarr(dimension,elements)
    FOR pol_i=0,n_pol-1 DO BEGIN
        holo_mapfn_generate,obs,/restore_last,map_fn=map_fn_single,polarization=pol_i
        *map_fn_arr[pol_i,obs_i]=map_fn_single
        weights_single=real_part(holo_mapfn_apply(complexarr(dimension,elements)+1,*map_fn_arr[pol_i,obs_i]))
        source_uv_mask[where(weights_single)]=1.
    ENDFOR
    *uv_mask_arr[obs_i]=source_uv_mask
    
    
    uv_i_use=where(source_uv_mask,n_uv_use)
    uv_use_frac=Float(n_uv_use)/(dimension*elements)
;    print,"Fractional uv coverage: ",uv_use_frac,"normalization: ",normalization
    *xv_arr[obs_i]=xvals[uv_i_use]
    *yv_arr[obs_i]=yvals[uv_i_use]
    
    box_coords[obs_i,0]=(Min((*xv_arr[obs_i])[where(source_mask)])+dimension/2.-smooth_width)>0
    box_coords[obs_i,1]=(Max((*xv_arr[obs_i])[where(source_mask)])+dimension/2.+smooth_width)<(dimension-1)
    box_coords[obs_i,2]=(Min((*yv_arr[obs_i])[where(source_mask)])+elements/2.-smooth_width)>0
    box_coords[obs_i,3]=(Max((*yv_arr[obs_i])[where(source_mask)])+elements/2.+smooth_width)<(elements-1)
ENDFOR

;healpix indices are in sparse format. Need to combine them
hpx_min=Lon64arr(n_obs) 
hpx_max=Lon64arr(n_obs) 
FOR obs_i=0,n_obs-1 DO BEGIN
    hpx_min[obs_i]=Min((*hpx_cnv[obs_i]).inds)
    hpx_max[obs_i]=Max((*hpx_cnv[obs_i]).inds)
ENDFOR
hpx_min=Min(hpx_min)
hpx_max=Max(hpx_max)
n_hpx=hpx_max-hpx_min+1

ind_hist=lonarr(n_hpx)
;ri_arr=Ptrarr(n_obs,/allocate)
hist_arr=Ptrarr(n_obs,/allocate)
FOR obs_i=0,n_obs-1 DO BEGIN
    ind_hist1=histogram((*hpx_cnv[obs_i]).inds,min=hpx_min,max=hpx_max,/bin);,reverse=ri) ;should not actually need ri
    *hist_arr[obs_i]=ind_hist1
;    *ri_arr[obs_i]=ri
    ind_hist+=ind_hist1
ENDFOR
ind_hist1=0 ;free memory
;ri=0 ;free memory

ind_use=where(ind_hist,n_hpx_use)
ind_hist=0 ;free memory
hpx_inds=ind_use+hpx_min

hpx_ind_map=Ptrarr(n_obs,/allocate)
FOR obs_i=0,n_obs-1 DO BEGIN
    ind_use2=where((*hist_arr[obs_i])[ind_use],n_use2)
;    ri=*ri_arr[obs_i]
;    ind_map=ri[ri[ind_use[ind_use2]]]
    *hpx_ind_map[obs_i]=ind_use2;[Sort(ind_map)]
    *hist_arr[obs_i]=0  ;free memory
;    *ri_arr[obs_i]=0  ;free memory
ENDFOR
ind_use2=0
Ptr_free,hist_arr;,ri_arr

    
converge_check=Fltarr(Ceil(max_iter/check_iter))
converge_check2=Fltarr(max_iter)

t1=0 ;generation of model_images and image_use for source detection
t2=0 ;source extraction
t3=0 ;fit the brightest source(s) to each polarization/etc...
t4=0 ;update model and run Holo mapping function
i2=0. & i3=0.
t0=Systime(1)

si=0L
healpix_map=Ptrarr(n_pol,/allocate)
weights_map=Ptrarr(n_pol,/allocate)
weights_corr_map=Ptrarr(n_pol,/allocate)
smooth_map=Ptrarr(n_pol,/allocate)
FOR pol_i=0,n_pol-1 DO BEGIN
    FOR obs_i=0,n_obs-1 DO BEGIN
        (*weights_map[pol_i])[*hpx_ind_map[obs_i]]+=*weights_arr[pol_i,obs_i]
    ENDFOR
    *weights_corr_map[pol_i]=weight_invert(*weights_map[pol_i])
ENDFOR

FOR i=0L,max_iter-1 DO BEGIN 
    t1_0=Systime(1)
    FOR pol_i=0,n_pol-1 DO BEGIN
        *healpix_map[pol_i]=Fltarr(n_hpx)
    ENDFOR
    
    FOR pol_i=0,n_pol-1 DO BEGIN
        IF i mod Floor(1./gain_factor) EQ 0 THEN *smooth_map[pol_i]=Fltarr(n_hpx)
        FOR obs_i=0,n_obs-1 DO BEGIN
            residual=dirty_image_generate(*dirty_uv_arr[pol_i,obs_i]-*model_uv_holo[pol_i,obs_i])
            residual_hpx=healpix_cnv_apply(residual,*hpx_cnv[obs_i])
            (*healpix_map[pol_i])[*hpx_ind_map[obs_i]]+=residual_hpx
            IF i mod Floor(1./gain_factor) EQ 0 THEN BEGIN
                ;smooth image changes slowly and the median function takes a lot of time, so only re-calculate every few iterations
                image_smooth=Median(residual[box_coords[obs_i,0]:box_coords[obs_i,1],box_coords[obs_i,2]:box_coords[obs_i,3]]*$
                    (*beam_corr[pol_i,obs_i])[box_coords[obs_i,0]:box_coords[obs_i,1],box_coords[obs_i,2]:box_coords[obs_i,3]],smooth_width,/even)*$
                    (*beam[pol_i,obs_i])[box_coords[obs_i,0]:box_coords[obs_i,1],box_coords[obs_i,2]:box_coords[obs_i,3]]
                residual[box_coords[obs_i,0]:box_coords[obs_i,1],box_coords[obs_i,2]:box_coords[obs_i,3]]-=image_smooth
                smooth_hpx=healpix_cnv_apply(residual,*hpx_cnv[obs_i])
                (*smooth_map[pol_i])[*hpx_ind_map[obs_i]]+=smooth_hpx
            ENDIF
        ENDFOR
    ENDFOR
    
    ;NOTE healpix_map and smooth_hpx are in instrumental polarization, weighted by the beam squared
    
    ;convert to Stokes I
    
    ;detect sources
    
    ;update models
    
    ;apply HMF
    
    ;check convergence
    
ENDFOR
END