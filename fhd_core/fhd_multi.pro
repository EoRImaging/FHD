PRO fhd_multi,fhd_file_list,source_array,comp_arr,fhd=fhd,obs_arr=obs_arr,weights_arr=weights_arr,timing=timing,nside=nside,$
    residual_array=residual_array,dirty_uv_arr=dirty_uv_arr,model_uv_full=model_uv_full,model_uv_holo=model_uv_holo,$
    silent=silent,beam_model=beam_model,beam_corr=beam_corr,norm_arr=norm_arr,source_mask=source_mask,hpx_inds=hpx_inds,$
    transfer_mapfn=transfer_mapfn,_Extra=extra
except=!except
!except=0
compile_opt idl2,strictarrsubs  
t00=Systime(1)

IF N_Elements(obs_arr) EQ 0 THEN BEGIN
    n_obs=N_Elements(fhd_file_list)
    FOR obs_i=0,n_obs-1 DO BEGIN
        file_path=fhd_file_list[obs_i]
        restore,file_path+'_obs.sav'
        IF obs_i EQ 0 THEN obs_arr=Replicate(obs,n_obs)
        obs_arr[obs_i]=obs
    ENDFOR
ENDIF

n_obs=N_Elements(obs_arr)
fhd=fhd_init(obs_arr[0],_Extra=extra) ;use the same deconvolution parameters for all observations. obs is used for very little here!
FOR obs_i=0,n_obs-1 DO BEGIN
    file_path=fhd_file_list[obs_i]
    save,fhd,filename=file_path+'_fhd_params.sav'
ENDFOR


n_pol=fhd.npol
baseline_threshold=fhd.baseline_threshold
gain_factor=fhd.gain_factor
;gain_factor_use=gain_factor*(!RaDeg/(obs_arr.MAX_BASELINE/obs_arr.KPIX)/obs_arr.degpix)^2. ;correct by approx. beam area
mapfn_interval=fhd.mapfn_interval
max_iter=fhd.max_iter
max_sources=fhd.max_sources
check_iter=fhd.check_iter
beam_threshold=fhd.beam_threshold
beam_model_threshold=0.05
add_threshold=fhd.add_threshold
max_add_sources=fhd.max_add_sources
local_max_radius=fhd.local_max_radius
pol_use=fhd.pol_use
independent_fit=fhd.independent_fit
reject_pol_sources=fhd.reject_pol_sources
local_radius=local_max_radius*Mean(obs_arr.degpix)
source_alias_radius=Mean(obs_arr.degpix*obs_arr.dimension)/4.

icomp=Complex(0,1)
beam_max_threshold=fhd.beam_max_threshold
smooth_width=fhd.smooth_width
pol_names=['xx','yy','xy','yx','I','Q','U','V']

beam_model=Ptrarr(n_pol,n_obs,/allocate)
beam_corr=Ptrarr(n_pol,n_obs,/allocate)
beam_mask_arr=Ptrarr(n_obs,/allocate)
source_mask_arr=Ptrarr(n_obs,/allocate)
beam_sourcefind_mask_arr=Ptrarr(n_obs,/allocate)
beam_model_hpx_arr=Ptrarr(n_pol,n_obs,/allocate) ;this one will be in Healpix pixels
;weights_inv_arr=Ptrarr(n_pol,n_obs,/allocate) ;this one will be in Healpix pixels
map_fn_arr=Ptrarr(n_pol,n_obs,/allocate)
dirty_uv_arr=Ptrarr(n_pol,n_obs,/allocate) 
model_uv_holo=Ptrarr(n_pol,n_obs,/allocate)
model_uv_full=Ptrarr(n_pol,n_obs,/allocate)
model_uv_stks=Ptrarr(4,/allocate)
weights_arr=Ptrarr(n_pol,n_obs,/allocate)

uv_mask_arr=Ptrarr(n_obs,/allocate)
comp_arr=Ptrarr(n_obs,/allocate)
ind_arr=Ptrarr(n_obs,/allocate)
hpx_cnv=Ptrarr(n_obs,/allocate)
xv_arr=Ptrarr(n_obs,/allocate)
yv_arr=Ptrarr(n_obs,/allocate)
uv_i_arr=Ptrarr(n_obs,/allocate)

box_coords=Lonarr(n_obs,4)
norm_arr=Fltarr(n_obs)
IF Keyword_Set(transfer_mapfn) THEN BEGIN
    file_path_mapfn=filepath(transfer_mapfn+'_mapfn_',root=file_dirname(fhd_file_list[0])) 
    print,String(format='("Transferring mapfn from: ",A)',transfer_mapfn)
    FOR pol_i=0,n_pol-1 DO BEGIN
        restore,file_path_mapfn+pol_names[pol_i]+'.sav' ;restores a variable named map_fn
        map_fn_ptr=Ptr_new(map_fn)
        FOR obs_i=0L,n_obs-1 DO map_fn_arr[pol_i,obs_i]=map_fn_ptr
    ENDFOR
ENDIF

FOR obs_i=0.,n_obs-1 DO BEGIN
    file_path_fhd=fhd_file_list[obs_i]
    obs=obs_arr[obs_i]
    dimension=obs.dimension
    elements=obs.elements
    xvals=meshgrid(dimension,elements,1)-dimension/2
    yvals=meshgrid(dimension,elements,2)-elements/2
    
    psf=beam_setup(obs,file_path_fhd,restore_last=1,silent=1)
    FOR pol_i=0,n_pol-1 DO *beam_model[pol_i,obs_i]=Sqrt(beam_image(psf,pol_i=pol_i,dimension=obs.dimension,/square))
    
    beam_sourcefind_mask=(beam_mask=fltarr(obs.dimension,obs.elements)+1)
    FOR pol_i=0,(n_pol<2)-1 DO BEGIN
        mask0=(mask1=fltarr(obs.dimension,obs.elements))
        ref_pix=Long(obs.obsx)+Long(obs.dimension)*Long(obs.obsy)
        mask_i=region_grow(*beam_model[pol_i,obs_i],ref_pix,thresh=[beam_threshold,max(*beam_model[pol_i,obs_i])])
        mask0[mask_i]=1
        beam_sourcefind_mask*=mask0
                
        mask_i=where(*beam_model[pol_i,obs_i] GE beam_model_threshold)
        mask1[mask_i]=1
        beam_mask*=mask1
    ENDFOR
    *beam_sourcefind_mask_arr[obs_i]=beam_sourcefind_mask
    *beam_mask_arr[obs_i]=beam_mask
    *source_mask_arr[obs_i]=beam_mask
    FOR pol_i=0,n_pol-1 DO *beam_corr[pol_i,obs_i]=weight_invert(*beam_model[pol_i,obs_i]*beam_mask)

    ;supply beam_mask in case file is missing and needs to be generated
    *hpx_cnv[obs_i]=healpix_cnv_generate(obs,file_path_fhd=file_path_fhd,nside=nside_chk,mask=beam_sourcefind_mask,radius=radius,restore_last=0) 
    IF N_Elements(nside) EQ 0 THEN nside=nside_chk
    IF nside_chk NE nside THEN *hpx_cnv[obs_i]=healpix_cnv_generate(obs,file_path_fhd=file_path_fhd,nside=nside,mask=beam_sourcefind_mask,radius=radius,restore_last=0)
    
    source_comp_init,comp_arr0,n_sources=max_sources
    *comp_arr[obs_i]=comp_arr0
    
    FOR pol_i=0,n_pol-1 DO BEGIN
;        restore,filename=file_path_fhd+'_uv_'+pol_names[pol_i]+'.sav' ; dirty_uv,weights_grid
        *dirty_uv_arr[pol_i,obs_i]=getvar_savefile(file_path_fhd+'_uv_'+pol_names[pol_i]+'.sav','dirty_uv')*obs.cal[pol_i];dirty_uv*obs.cal[pol_i]
        *model_uv_full[pol_i,obs_i]=Complexarr(dimension,elements)
        *model_uv_holo[pol_i,obs_i]=Complexarr(dimension,elements)
        *beam_model_hpx_arr[pol_i,obs_i]=healpix_cnv_apply(*beam_model[pol_i,obs_i],*hpx_cnv[obs_i])
    ENDFOR
    
    source_uv_mask=fltarr(dimension,elements)
    source_uv_mask2=fltarr(dimension,elements)
    normalization_arr=fltarr(n_pol)
    FOR pol_i=0,n_pol-1 DO BEGIN
;        restore,filename=file_path_fhd+'_mapfn_'+pol_names[pol_i]+'.sav' ;map_fn
;        *map_fn_arr[pol_i,obs_i]=getvar_savefile(file_path_fhd+'_mapfn_'+pol_names[pol_i]+'.sav','map_fn');map_fn
        IF N_Elements(*map_fn_arr[pol_i,obs_i]) EQ 0 THEN BEGIN
            file_path_mapfn=file_path_fhd+'_mapfn_'
            restore,file_path_mapfn+pol_names[pol_i]+'.sav' ;map_fn
            *map_fn_arr[pol_i,obs_i]=Temporary(map_fn)
        ENDIF
        weights_single=holo_mapfn_apply(complexarr(dimension,elements)+1,map_fn_arr[pol_i,obs_i],/no_conj,/indexed,_Extra=extra)
        weights_single_conj=Conj(Shift(Reverse(Reverse(weights_single,1),2),1,1))
        *weights_arr[pol_i,obs_i]=(weights_single+weights_single_conj)/2.
        normalization_arr[pol_i]=1./(dirty_image_generate(*weights_arr[pol_i,obs_i],degpix=obs.degpix))[dimension/2.,elements/2.]
        normalization_arr[pol_i]*=((*beam_model[pol_i,obs_i])[obs.obsx,obs.obsy])^2.
        source_uv_mask[where(*weights_arr[pol_i,obs_i])]=1.
        source_uv_mask2[where(weights_single)]=1.
    ENDFOR
    *uv_mask_arr[obs_i]=source_uv_mask
    norm_arr[obs_i]=Mean(normalization_arr[0:n_pol-1])
    
    uv_i_use=where(source_uv_mask,n_uv_use)
    uv_use_frac=Float(n_uv_use)/(dimension*elements)
;    print,"Fractional uv coverage: ",uv_use_frac,"normalization: ",normalization
    *uv_i_arr[obs_i]=where(source_uv_mask2,n_uv_use2)
    *xv_arr[obs_i]=xvals[*uv_i_arr[obs_i]]
    *yv_arr[obs_i]=yvals[*uv_i_arr[obs_i]]
    
    box_coords[obs_i,0]=(Min(xvals[where(beam_mask)])+dimension/2.-smooth_width)>0
    box_coords[obs_i,1]=(Max(xvals[where(beam_mask)])+dimension/2.+smooth_width)<(dimension-1)
    box_coords[obs_i,2]=(Min(yvals[where(beam_mask)])+elements/2.-smooth_width)>0
    box_coords[obs_i,3]=(Max(yvals[where(beam_mask)])+elements/2.+smooth_width)<(elements-1)
ENDFOR
gain_factor_use=gain_factor*norm_arr
print,"Gain normalization factors used: ",norm_arr
;print,"Normalization factors (ignored!): ",norm_arr 
;norm_arr[*]=1.
;FFT normalization factors:
;norm_arr=(obs_arr.degpix*!DtoR)^2.*(obs_arr.dimension*obs_arr.elements)
;print,"FFT Normalization factors used: ",norm_arr
;healpix indices are in sparse format. Need to combine them
hpx_ind_map=healpix_combine_inds(hpx_cnv,hpx_inds=hpx_inds,reverse_ind=reverse_inds)
n_hpx=N_Elements(hpx_inds)
n_hpx_full=nside2npix(nside)
degpix_hpx=Sqrt((4*!Pi*!Radeg^2.)/n_hpx_full)


pix2vec_ring,nside,hpx_inds,pix_coords
vec2ang,pix_coords,dec_hpx,ra_hpx,/astro

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
beam_map=Ptrarr(n_pol,/allocate)
beam_map2=Ptrarr(n_pol,/allocate)
beam_corr_map=Ptrarr(n_pol,/allocate)
beam_corr_map2=Ptrarr(n_pol,/allocate)
smooth_map=Ptrarr(n_pol,/allocate)
source_mask=Fltarr(n_hpx)+1.
FOR pol_i=0,n_pol-1 DO BEGIN
    *beam_map[pol_i]=Fltarr(n_hpx)
    *beam_map2[pol_i]=Fltarr(n_hpx)
    FOR obs_i=0,n_obs-1 DO BEGIN
        (*beam_map[pol_i])[*hpx_ind_map[obs_i]]+=*beam_model_hpx_arr[pol_i,obs_i]
        (*beam_map2[pol_i])[*hpx_ind_map[obs_i]]+=*beam_model_hpx_arr[pol_i,obs_i]^2.
    ENDFOR
    *beam_corr_map[pol_i]=weight_invert(*beam_map[pol_i])
    *beam_corr_map2[pol_i]=weight_invert(*beam_map2[pol_i])
ENDFOR
FOR pol_i=0,n_pol-1 DO BEGIN
    zero_ind=where(*beam_map[pol_i] EQ 0,n_zero)
    IF n_zero GT 0 THEN source_mask[zero_ind]=0
ENDFOR
;beam_map_avg=*beam_map[0]
;beam_corr_avg=*beam_corr_map[0]
;IF n_pol GT 1 THEN BEGIN
;    beam_map_avg=(beam_map_avg+*beam_map[1])/2.
;    beam_corr_avg=(beam_corr_avg+*beam_corr_map[1])/2.
;ENDIF

res_arr=Ptrarr(n_pol,n_obs,/allocate)
smooth_arr=Ptrarr(n_pol,n_obs,/allocate)
recalc_flag=Intarr(n_obs)+1
FOR i=0L,max_iter-1 DO BEGIN 
    FOR pol_i=0,n_pol-1 DO BEGIN
        *healpix_map[pol_i]=Fltarr(n_hpx)
        IF i mod Floor(1./gain_factor) EQ 0 THEN *smooth_map[pol_i]=Fltarr(n_hpx)
        FOR obs_i=0,n_obs-1 DO BEGIN
            t1_0=Systime(1)
            residual=dirty_image_generate(*dirty_uv_arr[pol_i,obs_i]-*model_uv_holo[pol_i,obs_i],degpix=obs_arr[obs_i].degpix)
            
            t2_0a=Systime(1)
            t1+=t2_0a-t1_0
;            IF i mod Floor(1./gain_factor) EQ 0 THEN BEGIN
                smooth0=fltarr(size(residual,/dimension))
                image_smooth=Median(residual[box_coords[obs_i,0]:box_coords[obs_i,1],box_coords[obs_i,2]:box_coords[obs_i,3]]$
                    *(*beam_corr[pol_i,obs_i])[box_coords[obs_i,0]:box_coords[obs_i,1],box_coords[obs_i,2]:box_coords[obs_i,3]],smooth_width,/even)$
                    *(*beam_model[pol_i,obs_i])[box_coords[obs_i,0]:box_coords[obs_i,1],box_coords[obs_i,2]:box_coords[obs_i,3]]
                smooth0[box_coords[obs_i,0]:box_coords[obs_i,1],box_coords[obs_i,2]:box_coords[obs_i,3]]=image_smooth
                
                *smooth_arr[pol_i,obs_i]=smooth0
                smooth_hpx=healpix_cnv_apply(smooth0*(*beam_sourcefind_mask_arr[obs_i]),*hpx_cnv[obs_i])
                (*smooth_map[pol_i])[*hpx_ind_map[obs_i]]+=smooth_hpx
;            ENDIF
            
            *res_arr[pol_i,obs_i]=residual-*smooth_arr[pol_i,obs_i]
            residual_use=residual*(*beam_sourcefind_mask_arr[obs_i]);l*(*source_mask_arr[obs_i])
            residual_hpx=healpix_cnv_apply(residual_use,*hpx_cnv[obs_i])
            (*healpix_map[pol_i])[*hpx_ind_map[obs_i]]+=residual_hpx
            t2_0b=Systime(1)
            t2+=t2_0b-t2_0a
        ENDFOR
    ENDFOR
    
;    IF i mod Floor(1./gain_factor) EQ 0 THEN BEGIN
;        FOR pol_i=0,n_pol-1 DO BEGIN
;            *smooth_map[pol_i]=Fltarr(n_hpx)
;            FOR obs_i=0,n_obs-1 DO BEGIN
;                ;smooth image changes slowly and the median function takes a lot of time, so only re-calculate every few iterations
;                residual=*res_arr[pol_i,obs_i]
;                smooth0=fltarr(size(residual,/dimension))
;                image_smooth=Median(residual[box_coords[obs_i,0]:box_coords[obs_i,1],box_coords[obs_i,2]:box_coords[obs_i,3]]$
;                    *(*beam_corr[pol_i,obs_i])[box_coords[obs_i,0]:box_coords[obs_i,1],box_coords[obs_i,2]:box_coords[obs_i,3]],smooth_width,/even)$
;                    *(*beam_model[pol_i,obs_i])[box_coords[obs_i,0]:box_coords[obs_i,1],box_coords[obs_i,2]:box_coords[obs_i,3]]
;                smooth0[box_coords[obs_i,0]:box_coords[obs_i,1],box_coords[obs_i,2]:box_coords[obs_i,3]]=image_smooth
;                smooth_hpx=healpix_cnv_apply(smooth0*(*beam_sourcefind_mask_arr[obs_i]),*hpx_cnv[obs_i])
;                (*smooth_map[pol_i])[*hpx_ind_map[obs_i]]+=smooth_hpx
;                
;            ENDFOR
;        ENDFOR
;    ENDIF
    ;NOTE healpix_map and smooth_hpx are in instrumental polarization, weighted by the beam squared
    
    ;convert to Stokes I
    source_find_hpx=(*healpix_map[0]-*smooth_map[0])*(*beam_corr_map[0])
    IF n_pol GT 1 THEN source_find_hpx+=(*healpix_map[1]-*smooth_map[1])*(*beam_corr_map[1])
    
;    IF i GT 0 THEN BEGIN
;        diverge_check1=source_find_hpx
;        diverge_tolerance=gain_factor/2. ;somewhat arbitrary
;        
;        diverge_check=Abs(diverge_check1)-Abs(diverge_check0)
;        diverge_test=where(diverge_check*source_mask GT flux_ref*diverge_tolerance,n_diverge) 
;        
;        IF n_diverge GT 0 THEN BEGIN
;            src_list=Reverse(si-lindgen(n_src)) ;n_src is still around from last iteration
;            FOR div_i=0L,n_diverge-1 DO BEGIN
;                ra_div=ra_hpx[diverge_test[div_i]]
;                dec_div=dec_hpx[diverge_test[div_i]]
;                dist_test=min(abs(angle_difference(dec_div,ra_div,dec_arr,ra_arr,/degree)),min_i) ;dec_arr,ra_arr are still around from last iteration
;                IF dist_test GT 2.*local_radius THEN CONTINUE
;                Query_disc,nside,pix_coords[source_i[min_i],*],local_radius/4.,region_inds,ninds,/deg ;source_i is still around from last iteration
;                region_i=reverse_inds[region_inds]
;                reg_i_i=where(region_i GE 0,n_reg)
;                IF n_reg GT 0 THEN region_i=region_i[reg_i_i] ELSE region_i=source_i[min_i]
;                source_mask[region_i]=0
;            ENDFOR
;        ENDIF
;        
;        diverge_check0=diverge_check1
;    ENDIF ELSE diverge_check0=source_find_hpx
;    
;    IF Mean(source_mask) LT 0.75 THEN BEGIN
;        print,String(format='("Failure to centroid after",I," iterations")',i)
;        converge_check2=converge_check2[0:i-1]
;        converge_check=converge_check[0:i2]
;        BREAK
;    ENDIF
    
    source_find_hpx*=source_mask
    residual_I=(*healpix_map[0]-*smooth_map[0])*(*beam_corr_map2[0])
    IF n_pol GT 1 THEN residual_I+=(*healpix_map[1]-*smooth_map[1])*(*beam_corr_map2[1])
    IF n_pol GT 2 THEN residual_U=(*healpix_map[2]-*smooth_map[2])*(*beam_corr_map2[2])$
        +(*healpix_map[3]-*smooth_map[3])*(*beam_corr_map2[3])
    IF Keyword_Set(independent_fit) AND (n_pol GT 1) THEN BEGIN
        residual_Q=(*healpix_map[0]-*smooth_map[0])*(*beam_corr_map2[0])-(*healpix_map[1]-*smooth_map[1])*(*beam_corr_map2[1])
        IF n_pol GT 3 THEN residual_V=(*healpix_map[2]-*smooth_map[2])*(*beam_corr_map2[2])$
            -(*healpix_map[3]-*smooth_map[3])*(*beam_corr_map2[3])
    ENDIF ELSE BEGIN
        residual_Q=fltarr(n_hpx)
        IF n_pol GT 2 THEN residual_V=fltarr(n_hpx)
    ENDELSE
    
    converge_check2[i]=Stddev(source_find_hpx[where(source_mask)],/nan)
    IF i EQ 0 THEN converge_check[0]=converge_check2[0]
    
    ;detect sources
    flux_ref=Max(source_find_hpx*source_mask,max_i)
    flux_ref1=flux_ref*add_threshold
    source_i=where(source_find_hpx*source_mask GT flux_ref1,n_src)
    source_list=source_find_hpx[source_i]
    source_i=source_i[Reverse(Sort(source_list))]
    
    source_ra=ra_hpx[source_i]
    source_dec=dec_hpx[source_i]
    source_i_use=lonarr(n_src) & source_i_use[0]=1
    FOR src_i=1L,n_src-1 DO BEGIN
        dist_test=angle_difference(source_dec[src_i],source_ra[src_i],source_dec[0:src_i-1],source_ra[0:src_i-1],/degree)
        dist_test=Min(dist_test)
        IF dist_test GT local_radius THEN source_i_use[src_i]=1
    ENDFOR
    source_i=source_i[where(source_i_use,n_src)]
    source_ra=ra_hpx[source_i]
    source_dec=dec_hpx[source_i]
    
    IF (n_src<max_add_sources)+si GE max_sources THEN max_add_sources=max_sources-(si+1)
    IF n_src GT max_add_sources THEN source_i=source_i[0:max_add_sources-1]
    n_src=Long(N_Elements(source_i))
    
    flux_src_arr=residual_I[source_i]
    ra_arr=fltarr(n_src)
    dec_arr=fltarr(n_src)
    source_cut_arr=fltarr(n_src)
    FOR src_i=0L,n_src-1 DO BEGIN
        Query_disc,nside,pix_coords[source_i[src_i],*],local_radius,region_inds,ninds,/deg
        region_i=reverse_inds[region_inds]
        region_i=region_i[where(region_i GE 0)] ;guaranteed at least the center pixel
        ra1=ra_hpx[region_i]
        dec1=dec_hpx[region_i]
        dist_weights=Exp(-(angle_difference(source_dec[src_i],source_ra[src_i],dec1,ra1,/degree)/local_radius)^2.)
        simg1=source_find_hpx[region_i]>0;*dist_weights>0.
        
        ra_arr[src_i]=Total(ra1*simg1)/Total(simg1)
        dec_arr[src_i]=Total(dec1*simg1)/Total(simg1)
    ENDFOR
    t3_0=Systime(1)
    t2+=t3_0-t2_0b
    
    ;update models
    flux_I=residual_I[source_i]
    flux_Q=residual_Q[source_i]
    IF n_pol GT 2 THEN BEGIN
        flux_U=residual_U[source_i]
        flux_V=residual_V[source_i]
    ENDIF
    FOR obs_i=0L,n_obs-1 DO BEGIN
        ad2xy,ra_arr,dec_arr,obs_arr[obs_i].astr,x_arr,y_arr
        dist_test=angle_difference(obs_arr[obs_i].obsdec,obs_arr[obs_i].obsra,dec_arr,ra_arr,/degree)
        dist_cut=where(dist_test GT source_alias_radius,n_dist_cut)

        comp_arr1=*comp_arr[obs_i]
        dimension=obs_arr[obs_i].dimension
        elements=obs_arr[obs_i].elements
        beam_mask=*beam_mask_arr[obs_i]
        
        si_use=lonarr(n_src)-1
        si_cut=lonarr(n_src)
        IF n_dist_cut GT 0 THEN si_cut[dist_cut]=1
        residual_test=*res_arr[0,obs_i] & IF n_pol GT 1 THEN residual_test=residual_test<*res_arr[1,obs_i]
        FOR src_i=0L,n_src-1 DO BEGIN    
            flux_arr=fltarr(4)
            si1=si+src_i
            beam_corr_src=fltarr(n_pol)
            beam_src=fltarr(n_pol)
            xv=x_arr[src_i]
            yv=y_arr[src_i]
            IF si_cut[src_i] THEN CONTINUE
            source_use_flag=1
            IF xv<yv GE 0 AND xv>yv LE (dimension<elements)-1 THEN BEGIN 
                IF beam_mask[xv,yv] EQ 0 THEN source_use_flag=0
                residual_test_val=residual_test[xv,yv]-$
                    Median(residual_test[(xv-smooth_width/2.)>0:(xv+smooth_width/2.)<(dimension-1),(yv-smooth_width/2.)>0:(yv+smooth_width/2.)<(elements-1)])
                IF residual_test_val LE 0 THEN BEGIN
                  (*source_mask_arr[obs_i])[xv,yv]=0
                  source_use_flag=0
                ENDIF
                IF Keyword_Set(source_use_flag) THEN BEGIN
                    source_cut_arr[src_i]+=1.
                    FOR pol_i=0,n_pol-1 DO BEGIN   
                        beam_corr_src[pol_i]=(*beam_corr[pol_i,obs_i])[xv,yv]
                        beam_src[pol_i]=(*beam_model[pol_i,obs_i])[xv,yv]
                        
                         IF Keyword_Set(independent_fit) THEN BEGIN
                            sign=(pol_i mod 2) ? -1:1
                            IF pol_i LE 1 THEN flux_use=flux_I[src_i]+sign*flux_Q[src_i]
                            IF pol_i GE 2 THEN flux_use=flux_U[src_i]+sign*flux_V[src_i]
                        ENDIF ELSE IF pol_i LE 1 THEN flux_use=flux_I[src_i] ELSE flux_use=flux_U[src_i]
                        
                        flux_use*=gain_factor_use[obs_i]/2.
                        comp_arr1[si1].flux.(pol_i)=flux_use*beam_src[pol_i] ;Apparent brightness, instrumental polarization X gain (a scalar)
                        flux_arr[pol_i]=flux_use;"True sky" instrumental pol
                    ENDFOR
                ENDIF
            ENDIF
            
            comp_arr1[si1].flux.I=flux_arr[0]+flux_arr[1]
            comp_arr1[si1].flux.Q=flux_arr[0]-flux_arr[1]
            comp_arr1[si1].flux.U=flux_arr[2]+flux_arr[3]
            comp_arr1[si1].flux.V=flux_arr[2]-flux_arr[3]
            
            comp_arr1[si1].x=xv
            comp_arr1[si1].y=yv
            comp_arr1[si1].ra=ra_arr[src_i]
            comp_arr1[si1].dec=dec_arr[src_i]
            si_use[src_i]=si1
            
            ;Make sure to update source uv model in "true sky" instrumental polarization i.e. 1/beam^2 frame.
;            IF Total(Abs(flux_arr)) GT 0 THEN BEGIN
;;                source_uv_vals=Exp(icomp*(2.*!Pi/dimension)*((comp_arr1[si1].x-dimension/2.)*(*xv_arr[obs_i])+(comp_arr1[si1].y-elements/2.)*(*yv_arr[obs_i])))
;                source_uv_vals=source_dft(comp_arr1[si1].x,comp_arr1[si1].y,*xv_arr[obs_i],*yv_arr[obs_i],$
;                    dimension=dimension,elements=elements,degpix=obs_arr[obs_i].degpix)
;                FOR pol_i=0,n_pol-1 DO $
;                    (*model_uv_full[pol_i,obs_i])[*uv_i_arr[obs_i]]+=flux_arr[pol_i]*source_uv_vals
;            ENDIF
        ENDFOR
        
        si_use_i=where(si_use GE 0,n_si_use)
        IF n_si_use EQ 0 THEN BEGIN
            ;do something to end loop if n_mask EQ 0
            
            recalc_flag[obs_i]=0
            CONTINUE
        ENDIF ELSE recalc_flag[obs_i]=1
        
        si_use=si_use[si_use_i]
                ;Make sure to update source uv model in "true sky" instrumental polarization i.e. 1/beam^2 frame.
        flux_I_use=comp_arr1[si_use].flux.I
        flux_Q_use=comp_arr1[si_use].flux.Q
        flux_U_use=comp_arr1[si_use].flux.U
        flux_V_use=comp_arr1[si_use].flux.V
        x_vec=comp_arr1[si_use].x
        y_vec=comp_arr1[si_use].y
        *model_uv_stks[0]=source_dft(x_vec,y_vec,*xv_arr[obs_i],*yv_arr[obs_i],dimension=dimension,elements=elements,degpix=degpix,flux=flux_I_use)
        IF Total(flux_Q_use) EQ 0 THEN *model_uv_stks[1]=0. $
            ELSE *model_uv_stks[1]=source_dft(x_vec,y_vec,*xv_arr[obs_i],*yv_arr[obs_i],dimension=dimension,elements=elements,degpix=degpix,flux=flux_Q_use) 
        IF Total(flux_U_use) EQ 0 THEN *model_uv_stks[2]=0. $
            ELSE *model_uv_stks[2]=source_dft(x_vec,y_vec,*xv_arr[obs_i],*yv_arr[obs_i],dimension=dimension,elements=elements,degpix=degpix,flux=flux_U_use)
        IF Total(flux_V_use) EQ 0 THEN *model_uv_stks[3]=0. $
            ELSE *model_uv_stks[3]=source_dft(x_vec,y_vec,*xv_arr[obs_i],*yv_arr[obs_i],dimension=dimension,elements=elements,degpix=degpix,flux=flux_V_use)
        SWITCH n_pol OF
            4:(*model_uv_full[3,obs_i])[*uv_i_arr[obs_i]]+=(*model_uv_stks[2]-*model_uv_stks[3])/2.
            3:(*model_uv_full[2,obs_i])[*uv_i_arr[obs_i]]+=(*model_uv_stks[2]+*model_uv_stks[3])/2.
            2:(*model_uv_full[1,obs_i])[*uv_i_arr[obs_i]]+=(*model_uv_stks[0]-*model_uv_stks[1])/2.
            1:(*model_uv_full[0,obs_i])[*uv_i_arr[obs_i]]+=(*model_uv_stks[0]+*model_uv_stks[1])/2.
        ENDSWITCH
        
        *comp_arr[obs_i]=comp_arr1
    ENDFOR
    si_use=where(source_cut_arr,n_src_use,complement=si_mask,ncomplement=n_si_mask)
    IF n_si_mask GT 0 THEN source_mask[source_i[si_mask]]=0.
    
    si+=n_src_use
    t4_0=Systime(1)
    t3+=t4_0-t3_0
    
    ;apply HMF
    FOR obs_i=0L,n_obs-1 DO BEGIN
        IF recalc_flag[obs_i] EQ 0 THEN CONTINUE
        FOR pol_i=0,n_pol-1 DO BEGIN
            *model_uv_holo[pol_i,obs_i]=holo_mapfn_apply(*model_uv_full[pol_i,obs_i],map_fn_arr[pol_i,obs_i],/indexed,_Extra=extra);*norm_arr[obs_i]
        ENDFOR
    ENDFOR
    t4+=Systime(1)-t4_0
    
    IF (si+1) GE max_sources THEN BEGIN
        i2+=1                                        
        t10=Systime(1)-t0
        conv_chk=Stddev(source_find_hpx[where(source_mask)],/nan)
        print,StrCompress(String(format='("Max sources found by iteration ",I," after ",I," seconds (convergence:",F,")")',i,t10,conv_chk))
        converge_check[i2]=conv_chk
        BREAK
    ENDIF
    
    ;check convergence
    IF (Round(i mod check_iter) EQ 0) AND (i GT 0) THEN BEGIN
        i2+=1
        t10=Systime(1)-t0
        conv_chk=Stddev(source_find_hpx[where(source_mask)],/nan)
        IF ~Keyword_Set(silent) THEN print,StrCompress(String(format='(I," : ",I," : ",I," : ",F)',i,si,t10,conv_chk))
        converge_check[i2]=conv_chk
        IF 2.*converge_check[i2] GT flux_ref THEN BEGIN
            print,StrCompress(String(format='("Break after iteration ",I," from low signal to noise after ",I," seconds (convergence:",F,")")',i,t10,conv_chk))
            converge_check2=converge_check2[0:i]
            converge_check=converge_check[0:i2]
            BREAK
        ENDIF
        IF converge_check[i2] GE Max(converge_check[((i2-Ceil(Alog10(i)))>0):i2-1]) THEN BEGIN ;add more tolerance for small variations
            print,StrCompress(String(format='("Break after iteration ",I," from lack of convergence after ",I," seconds (convergence:",F,")")',i,t10,conv_chk))
            converge_check2=converge_check2[0:i]
            converge_check=converge_check[0:i2]
            BREAK
        ENDIF
    ENDIF
ENDFOR

;condense clean components
residual_array=Ptrarr(n_pol,n_obs,/allocate)
source_array=Ptrarr(n_obs)
FOR obs_i=0L,n_obs-1 DO *comp_arr[obs_i]=(*comp_arr[obs_i])[0:si-1] ;truncate component list to include only components actually deconvolved
FOR obs_i=0L,n_obs-1 DO BEGIN
    FOR pol_i=0,n_pol-1 DO BEGIN
        *residual_array[pol_i,obs_i]=dirty_image_generate(*dirty_uv_arr[pol_i,obs_i]-*model_uv_holo[pol_i,obs_i],$
            degpix=obs_arr[obs_i].degpix)*(*beam_corr[pol_i,obs_i])
    ENDFOR
    
    image_use=*residual_array[0,obs_i] & IF n_pol GT 1 THEN image_use+=*residual_array[1,obs_i]
    image_use-=Median(image_use,smooth_width)
    beam_avg=*beam_model[0,obs_i] & IF n_pol GT 1 THEN beam_avg=(beam_avg+*beam_model[1,obs_i])/2.
    noise_map=Stddev(image_use[where(*beam_mask_arr[obs_i])],/nan)*weight_invert(beam_avg)
    comp_arr1=*comp_arr[obs_i]
    source_array1=Components2Sources(comp_arr1,radius=(local_max_radius/2.)>0.5,noise_map=noise_map)
    source_array[obs_i]=Ptr_new(source_array1)
ENDFOR

t00=Systime(1)-t00
print,'Deconvolution timing [per iteration]'
print,String(format='("FFT:",A,"[",A,"]")',Strn(Round(t1)),Strn(Round(t1*100/i)/100.))
print,String(format='("Filtering:",A,"[",A,"]")',Strn(Round(t2)),Strn(Round(t2*100/i)/100.))
print,String(format='("DFT source modeling:",A,"[",A,"]")',Strn(Round(t3)),Strn(Round(t3*100/i)/100.))
print,String(format='("Applying HMF:",A,"[",A,"]")',Strn(Round(t4)),Strn(Round(t4*100/i)/100.))
Ptr_free,map_fn_arr,hpx_cnv,hpx_ind_map,res_arr,smooth_arr,healpix_map
timing=[t00,t1,t2,t3,t4]
!except=except
END