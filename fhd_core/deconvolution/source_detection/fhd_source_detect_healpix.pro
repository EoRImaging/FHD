FUNCTION fhd_source_detect_healpix,obs_arr,jones_arr,fhd_params,source_find_hpx,residual_stokes_hpx=residual_stokes_hpx,$
    beam_model=beam_model,ra_hpx=ra_hpx,dec_hpx=dec_hpx,gain_factor_use=gain_factor_use,$
    beam_mask_arr=beam_mask_arr,source_mask_arr=source_mask_arr,recalc_flag=recalc_flag,n_sources=n_sources,$
    nside=nside,region_inds=region_inds,pix_coords=pix_coords,reverse_inds=reverse_inds,res_stokes_arr=res_stokes_arr,$
    source_mask_hpx=source_mask_hpx,si_start=si_start

IF N_Elements(si_start) EQ 0 THEN si_start=0L
n_sources=0
max_iter=5
n_obs=N_Elements(obs_arr)
recalc_flag=Intarr(n_obs)
beam_width=beam_width_calculate(obs_arr,min_restored_beam_width=1.,/FWHM)
local_max_radius=beam_width
local_radius=local_max_radius*Mean(obs_arr.degpix)
smooth_width=Ceil(local_max_radius*10.)
;filter_background=fhd_params.filter_background
IF N_Elements(gain_factor_use) EQ 0 THEN gain_factor_use=fhd_params.gain_factor

add_threshold=fhd_params.add_threshold
max_add_sources=fhd_params.max_add_sources
independent_fit=fhd_params.independent_fit
reject_pol_sources=fhd_params.reject_pol_sources
sigma_threshold=2.
freq_use=Median(obs_arr.freq_center)

n_pol=fhd_params.npol
source_alias_radius=Median(obs_arr.degpix*obs_arr.dimension)/4.

iter=0
WHILE n_sources LE 0 DO BEGIN
    ;detect sources
    flux_ref=Max(source_find_hpx*source_mask_hpx,max_i)
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
    
    ;IF (n_src<max_add_sources)+si GE max_deconvolution_components THEN max_add_sources=max_deconvolution_components-(si+1)
    IF n_src GT max_add_sources THEN source_i=source_i[0:max_add_sources-1]
    n_src=Long(N_Elements(source_i))
    
    ;flux_src_arr=residual_I[source_i]
    ra_arr=fltarr(n_src)
    dec_arr=fltarr(n_src)
    source_cut_arr=fltarr(n_src)
    FOR src_i=0L,n_src-1 DO BEGIN
        Query_disc,nside,pix_coords[source_i[src_i],*],local_radius,region_inds,ninds,/deg
        region_i=reverse_inds[region_inds]
        i_use=where(region_i GE 0,n_use)
        IF n_use EQ 0 THEN BEGIN
            source_cut_arr[src_i]+=1
            CONTINUE
        ENDIF
        region_i=region_i[i_use] ;guaranteed at least the center pixel
        ra1=ra_hpx[region_i]
        IF Max(ra1)-Min(ra1) GT 100. THEN ra1[where(ra1 GE (Min(ra1)+100))]-=360. ;watch out for branch cut at 360 degrees
        dec1=dec_hpx[region_i]
        dist_weights=Exp(-(angle_difference(source_dec[src_i],source_ra[src_i],dec1,ra1,/degree)/(2.*local_radius))^2.)
        simg1=((*residual_stokes_hpx[0])[region_i]*dist_weights)>0.
        
        ra_arr[src_i]=Total(ra1*simg1)/Total(simg1)
        dec_arr[src_i]=Total(dec1*simg1)/Total(simg1)
    ENDFOR
    source_i_i=where(source_cut_arr EQ 0,n_src)
    source_i=source_i[source_i_i]
    ra_arr=ra_arr[source_i_i]
    dec_arr=dec_arr[source_i_i]
    source_cut_arr=Fltarr(n_src)
    flux_vals=Ptrarr(n_pol)
    FOR pol_i=0,n_pol-1 DO flux_vals[pol_i]=Ptr_new((*residual_stokes_hpx[pol_i])[source_i]*gain_factor_use)
    IF ~Keyword_Set(independent_fit) THEN BEGIN
        IF n_pol GE 2 THEN *flux_vals[1]=Fltarr(n_src)
        IF n_pol GE 4 THEN *flux_vals[3]=Fltarr(n_src)
    ENDIF
    
    ;update models
    
    comp_arr=source_comp_init(n_sources=n_src,id=Lindgen(n_src),ra=ra_arr,dec=dec_arr,freq=freq_use)
    FOR pol_i=0,n_pol-1 DO comp_arr.flux.(pol_i+4)=*flux_vals[pol_i]
    source_array=Ptrarr(n_obs)
    si_i_arr=Ptrarr(n_obs)
    FOR obs_i=0L,n_obs-1 DO BEGIN
        si=0L
        apply_astrometry, obs_arr[obs_i], ra_arr=ra_arr, dec_arr=dec_arr, x_arr=x_arr, y_arr=y_arr, /ad2xy, /refraction
    ;    dist_test=angle_difference(obs_arr[obs_i].obsdec,obs_arr[obs_i].obsra,dec_arr,ra_arr,/degree)
    ;    dist_cut=where(dist_test GT source_alias_radius,n_dist_cut)
    
        comp_arr1=comp_arr
        dimension=obs_arr[obs_i].dimension
        elements=obs_arr[obs_i].elements
        beam_mask=*beam_mask_arr[obs_i]
        comp_arr1.x=x_arr
        comp_arr1.y=y_arr
        
        si_use=lonarr(n_src)
    ;    si_cut=lonarr(n_src)
    ;    IF n_dist_cut GT 0 THEN si_cut[dist_cut]=1
        residual_test=*res_stokes_arr[obs_i] 
        
        ;first, cut any sources that are physically outside of the image
        src_i_use1=where((comp_arr1.x GE 0) AND (comp_arr1.x LE dimension-1) AND (comp_arr1.y GE 0) AND (comp_arr1.y LE elements-1),n_src_use1)
        IF n_src_use1 EQ 0 THEN CONTINUE
        comp_arr1=comp_arr1[src_i_use1]
        
        ;Second, cut sources that are outside the beam mask region
        src_i_use2=where(beam_mask[comp_arr1.x,comp_arr1.y],n_src_use2)
        IF n_src_use2 EQ 0 THEN CONTINUE
        comp_arr1=comp_arr1[src_i_use2]
        
        ;Finally, cut sources that are already negative in Stokes I
        src_i_use3=where(residual_test[comp_arr1.x,comp_arr1.y] GT 0,n_src_use3)
        IF n_src_use3 EQ 0 THEN CONTINUE
        comp_arr1=comp_arr1[src_i_use3]
            
        source_array[obs_i]=Ptr_new(stokes_cnv(comp_arr1,jones_arr[obs_i],obs_arr[obs_i],beam_arr=beam_model[*,obs_i],/square,/inverse,_Extra=extra))
        comp_arr1=0 ;free memory
        recalc_flag[obs_i]=1
        
        si_use[src_i_use1[src_i_use2[src_i_use3]]]=1
        si_i_arr[obs_i]=Ptr_new(where(si_use))
    ENDFOR
    
    si_use=Intarr(n_src)
    obs_i_use=Where(Ptr_valid(si_i_arr),n_obs_use)
    
    FOR obs_i=0L,n_obs_use-1 DO si_use[*si_i_arr[obs_i_use[obs_i]]]+=1
    si_i_use=where(si_use,n_sources,complement=si_cut,ncomplement=n_cut)
    IF n_cut GT 0 THEN source_mask_hpx[source_i[si_cut]]=0
    iter+=1
    IF iter GE max_iter THEN BREAK
ENDWHILE
IF n_sources GT 0 THEN si_use[si_i_use]=si_i_use

FOR obs_i=0L,n_obs_use-1 DO BEGIN
;    (*source_array[obs_i_use[obs_i]])=(*source_array[obs_i_use[obs_i]])[si_i_use]
    (*source_array[obs_i_use[obs_i]]).id+=si_start
ENDFOR
Ptr_free,si_i_arr,flux_vals
;si_use=where(source_cut_arr,n_sources,complement=si_mask,ncomplement=n_si_mask)
;IF n_si_mask GT 0 THEN BEGIN
;    source_mask_hpx[source_i[si_mask]]=0.
;    FOR obs_i=0L,n_obs-1 DO BEGIN
;        *source_array[obs_i]=(*source_array[obs_i])[si_use]
;    ENDFOR
;ENDIF
RETURN,source_array
END