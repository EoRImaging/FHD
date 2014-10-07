FUNCTION fhd_source_detect_healpix,obs_arr,jones_arr,fhd_params,source_find_hpx,residual_I=residual_I,residual_Q=residual_Q,$
    residual_U=residual_U,residual_V=residual_V,beam_model=beam_model,ra_hpx=ra_hpx,dec_hpx=dec_hpx,gain_factor_use=gain_factor_use,$
    beam_mask_arr=beam_mask_arr,source_mask_arr=source_mask_arr,recalc_flag=recalc_flag,n_sources=n_sources,$
    nside=nside,region_inds=region_inds,pix_coords=pix_coords,reverse_inds=reverse_inds,res_arr=res_arr,$
    source_mask_hpx=source_mask_hpx

n_obs=N_Elements(obs_arr)
recalc_flag=Intarr(n_obs)
beam_width=(!RaDeg/Median(obs_arr.degpix*obs_arr.MAX_BASELINE/obs_arr.KPIX))>1.
local_max_radius=beam_width*2.
local_radius=local_max_radius*Mean(obs_arr.degpix)
smooth_width=Ceil(local_max_radius*2.)
filter_background=fhd_params.filter_background
IF N_Elements(gain_factor_use) EQ 0 THEN gain_factor_use=fhd_params.gain_factor
IF N_Elements(gain_factor_use) EQ 1 THEN gain_factor_use=Replicate(gain_factor_use,n_obs)

add_threshold=fhd_params.add_threshold
max_add_sources=fhd_params.max_add_sources
pol_use=fhd_params.pol_use
independent_fit=fhd_params.independent_fit
reject_pol_sources=fhd_params.reject_pol_sources
sigma_threshold=2.

n_pol=fhd_params.npol
source_alias_radius=Median(obs_arr.degpix*obs_arr.dimension)/4.

;detect sources
flux_ref=Max(source_find_hpx,max_i)
flux_ref1=flux_ref*add_threshold
source_i=where(source_find_hpx GT flux_ref1,n_src)
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

;IF (n_src<max_add_sources)+si GE max_sources THEN max_add_sources=max_sources-(si+1)
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
    IF Max(ra1)-Min(ra1) GT 100. THEN ra1[where(ra1 GE (Min(ra1)+100))]-=360. ;watch out for branch cut at 360 degrees
    dec1=dec_hpx[region_i]
    dist_weights=Exp(-(angle_difference(source_dec[src_i],source_ra[src_i],dec1,ra1,/degree)/(2.*local_radius))^2.)
    simg1=(source_find_hpx[region_i]*dist_weights)>0.
    
    ra_arr[src_i]=Total(ra1*simg1)/Total(simg1)
    dec_arr[src_i]=Total(dec1*simg1)/Total(simg1)
ENDFOR

;update models
flux_I=residual_I[source_i]
flux_Q=residual_Q[source_i]
IF n_pol GT 2 THEN BEGIN
    flux_U=residual_U[source_i]
    flux_V=residual_V[source_i]
ENDIF

source_array=Ptrarr(n_obs)
FOR obs_i=0L,n_obs-1 DO BEGIN
    si=0L
    ad2xy,ra_arr,dec_arr,obs_arr[obs_i].astr,x_arr,y_arr
    dist_test=angle_difference(obs_arr[obs_i].obsdec,obs_arr[obs_i].obsra,dec_arr,ra_arr,/degree)
    dist_cut=where(dist_test GT source_alias_radius,n_dist_cut)

    comp_arr1=source_comp_init(n_sources=n_src)
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
;                    beam_corr_src[pol_i]=(*beam_corr[pol_i,obs_i])[xv,yv]
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
    ENDFOR
    
    source_array[obs_i]=Ptr_new(comp_arr1)
    si_use_i=where(si_use GE 0,n_si_use)
    IF n_si_use EQ 0 THEN BEGIN
        ;do something to end loop if n_mask EQ 0
        
        recalc_flag[obs_i]=0
        CONTINUE
    ENDIF ELSE recalc_flag[obs_i]=1
    
    si_use=si_use[si_use_i]
;    source_dft_multi,obs,comp_arr1[si_use],model_uv_full[*,obs_i],xvals=*xv_arr[obs_i],yvals=*xvy_arr[obs_i],uv_i_use=*uv_i_arr[obs_i]
    
ENDFOR

si_use=where(source_cut_arr,n_sources,complement=si_mask,ncomplement=n_si_mask)
IF n_si_mask GT 0 THEN BEGIN
    source_mask_hpx[source_i[si_mask]]=0.
    FOR obs_i=0L,n_obs-1 DO BEGIN
        *source_array[obs_i]=(*source_array[obs_i])[si_use]
    ENDFOR
ENDIF
RETURN,source_array
END