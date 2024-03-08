FUNCTION components2sources,comp_arr,obs,fhd_params,detection_threshold=detection_threshold,radius=radius,noise_map=noise_map,$
    reject_sigma_threshold=reject_sigma_threshold,clean_bias_threshold=clean_bias_threshold,gain_factor=gain_factor,$
    reject_outlier_components=reject_outlier_components,extend_threshold=extend_threshold,regrid_extended_sources=regrid_extended_sources,$
    source_mask=source_mask,max_deconvolution_components=max_deconvolution_components,_Extra=extra
compile_opt idl2,strictarrsubs  

astr=obs.astr
dimension=obs.dimension
elements=obs.elements
n_pol=obs.n_pol

;Set up defaults
pregroup_flag=0
IF Keyword_Set(fhd_params) THEN BEGIN
    IF N_Elements(detection_threshold) EQ 0 THEN detection_threshold=fhd_params.detection_threshold
    IF N_Elements(reject_sigma_threshold) EQ 0 THEN $
        IF Keyword_Set(noise_map) THEN reject_sigma_threshold=fhd_params.sigma_cut ELSE reject_sigma_threshold=0  
    IF Tag_exist(fhd_params,"sidelobe_return") THEN $
        IF Keyword_Set(fhd_params.sidelobe_return) THEN pregroup_flag=1
ENDIF ELSE BEGIN
    IF N_Elements(detection_threshold) EQ 0 THEN detection_threshold=Min(comp_arr[comp_i_use].flux.I)/2.
    IF N_Elements(reject_sigma_threshold) EQ 0 THEN $
        IF Keyword_Set(noise_map) THEN reject_sigma_threshold=2. ELSE reject_sigma_threshold=0  
ENDELSE
IF N_Elements(extend_threshold) EQ 0 THEN extend_threshold=0.2
IF N_Elements(reject_outlier_components) EQ 0 THEN reject_outlier_components=0

gauss_sigma=beam_width_calculate(obs)
gauss_width=beam_width_calculate(obs,/fwhm)
gauss_area=beam_width_calculate(obs,/area)
IF N_Elements(radius) EQ 0 THEN radius=gauss_width
comp_i_use=where(comp_arr.flux.I GT 0,n_comp_use)
IF Keyword_Set(max_deconvolution_components) THEN BEGIN
    IF n_comp_use GT max_deconvolution_components THEN BEGIN
        n_comp_use=max_deconvolution_components
        comp_i_use=comp_i_use[0:max_deconvolution_components-1]
    ENDIF
ENDIF
IF n_comp_use EQ 0 THEN RETURN,source_comp_init(n_sources=0)

IF Keyword_Set(regrid_extended_sources) THEN BEGIN
    nside_obs=obs.healpix.nside
    pix_sky=4.*!Pi*!RaDeg^2./Product(Abs(astr.cdelt))
    Nside_chk=2.^(Ceil(ALOG(Sqrt(pix_sky/12.))/ALOG(2)))
    nside=nside_obs>nside_chk
    hpx_cnv=healpix_cnv_generate(obs,/no_save,/divide_pixel_area,nside=nside,/pointer_return)
ENDIF

IF pregroup_flag THEN gi0=Max(comp_arr.id)+1 ELSE BEGIN
    gi0=0L
    comp_arr.id=-1
ENDELSE
group_id=group_source_components(obs,comp_arr,radius=radius,gain_factor=gain_factor)

IF max(group_id) LE 0 THEN BEGIN
    IF Keyword_Set(hpx_cnv) THEN undefine_fhd,hpx_cnv
    RETURN,source_comp_init(n_sources=0)
ENDIF
hgroup=histogram(group_id,binsize=1,min=0,reverse_ind=gri)
group_inds=where(hgroup GT 1,ng)

ungroup_i=where(group_id LT 0,n_ungroup)
IF n_ungroup GT 0 THEN BEGIN
    IF Tag_exist(comp_arr,'flag') THEN comp_arr[ungroup_i].flag=1
    IF ~Keyword_Set(reject_outlier_components)THEN BEGIN
        g0=Max(group_id)+1
        group_id_sub=group_source_components(obs,comp_arr[ungroup_i],radius=radius,gain_factor=gain_factor)
        group_id_sub_i=where(group_id_sub GE 0,n_sub_use)
        IF n_sub_use GT 0 THEN BEGIN
            group_id[ungroup_i[group_id_sub_i]]=group_id_sub[group_id_sub_i]+g0
            IF Tag_exist(comp_arr,'flag') THEN comp_arr[ungroup_i[group_id_sub_i]].flag=2
        ENDIF
    ENDIF
ENDIF

hgroup=histogram(group_id,binsize=1,min=0,reverse_ind=gri)
group_inds=where(hgroup GT 1,ng)
source_arr=source_comp_init(n_sources=ng)
FOR gi=0L,ng-1 DO BEGIN
;    IF hgroup[gi] EQ 0 THEN CONTINUE
    gi_in=group_inds[gi]
    si_g=gri[gri[gi_in]:gri[gi_in+1]-1]; guaranteed at least one source per group
    flux_I=(comp_arr[si_g].flux.I)>0.
    IF Total(flux_I) LE 0 THEN CONTINUE
    
    ;flux_I is guaranteed to be non-zero from above
    sx=Total(comp_arr[si_g].x*flux_I)/Total(flux_I)
    sy=Total(comp_arr[si_g].y*flux_I)/Total(flux_I)
    
    apply_astrometry, obs, x_arr=sx, y_arr=sy, ra_arr=sra, dec_arr=sdec, /xy2ad, /refraction
    source_arr[gi].x=sx
    source_arr[gi].y=sy
    source_arr[gi].ra=sra
    source_arr[gi].dec=sdec
    FOR pol_i=0,7 DO source_arr[gi].flux.(pol_i)=Total(comp_arr[si_g].flux.(pol_i))
    source_arr[gi].id=gi+gi0
    comp_arr[si_g].id=gi+gi0
    IF Keyword_Set(noise_map) THEN BEGIN
        ;need some sort of error checking here first!!!
        IF N_Elements(noise_map) EQ 1 THEN nm0=noise_map ELSE nm0=noise_map[source_arr[gi].x,source_arr[gi].y]
        IF nm0 GT 0 THEN source_arr[gi].ston=Total(flux_I)/nm0 ELSE source_arr[gi].ston=0.
    ENDIF ELSE source_arr[gi].ston=Max(comp_arr[si_g].ston)
    source_arr[gi].alpha=Total(comp_arr[si_g].alpha*flux_I)/Total(flux_I)
    source_arr[gi].freq=Total(comp_arr[si_g].freq*flux_I)/Total(flux_I)    
    IF (1.-(1.-gain_factor)^N_Elements(si_g)) LT 0.5 THEN flag_min=1 ELSE flag_min=0 
    source_arr[gi].flag=Max(comp_arr[si_g].flag)>flag_min
    
    extend_test=Mean(Sqrt((sx-comp_arr[si_g].x)^2.+(sy-comp_arr[si_g].y)^2.))
    IF extend_test GE extend_threshold THEN BEGIN
        IF Keyword_Set(regrid_extended_sources) THEN BEGIN
            comp_arr_hpx = source_array_regrid(comp_arr[si_g],obs,hpx_cnv=hpx_cnv,_Extra=extra)
            source_arr[gi].extend=Ptr_new(comp_arr_hpx)
        ENDIF ELSE source_arr[gi].extend=Ptr_new(comp_arr[si_g])
    ENDIF
ENDFOR

IF Keyword_Set(hpx_cnv) THEN undefine_fhd,hpx_cnv

comp_arr_use=comp_arr
IF Keyword_Set(reject_sigma_threshold) THEN BEGIN
    IF N_Elements(noise_map) EQ 0 THEN si_use=where(source_arr.flux.I GE reject_sigma_threshold,n_use) $
        ELSE si_use=where(source_arr.ston GE reject_sigma_threshold,n_use)
    IF n_use EQ 0 THEN RETURN,source_arr
    source_arr=source_arr[si_use]
ENDIF

IF Keyword_Set(clean_bias_threshold) THEN BEGIN
    IF clean_bias_threshold GE 1 THEN clean_bias_threshold=0.5
    ns=N_Elements(source_arr)
    comp_gi=comp_arr_use.id
    hcomp_gi=histogram(comp_gi,min=0,/bin,reverse_ind=c_ri)
    gain_factor_arr=comp_arr_use.gain
    id_use=where(hcomp_gi,n_id_use)
    
    flux_frac_arr=1.-(1.-gain_factor)^hcomp_gi[source_arr.id]
    
    si_use=where(flux_frac_arr GE Abs(clean_bias_threshold) AND (hcomp_gi[source_arr.id] GT 1),n_use)
    IF n_use EQ 0 THEN RETURN,source_arr ; return if no valid values found
    source_arr=source_arr[si_use]
    flux_frac_arr=flux_frac_arr[si_use]
    
    IF clean_bias_threshold LT 0 THEN RETURN,source_arr ; return trimmed source array if threshold is specified as negative, but don't correct any fluxes
    
    comp_weight=comp_arr_use
    comp_weight.flux.I=comp_arr_use.gain
    weight_img=source_image_generate(comp_weight,dimension=obs.dimension,restored_beam_width=gauss_sigma,pol=4,threshold=Mean(gain_factor_arr))
    undefine_fhd,comp_weight
    IF N_Elements(source_mask) NE dimension*elements THEN source_mask=replicate(1.,dimension,elements)
    FOR si = 0L,n_use-1L DO BEGIN
        gi=source_arr[si].id
        ncomp=hcomp_gi[gi]
        
        IF Ptr_valid(source_arr[si].extend) THEN BEGIN
            ;still need to handle extended sources properly!
            ext_comp=*(source_arr[si].extend)
            
            ext_gain=ext_comp & ext_gain.flux.I=ext_gain.gain/Mean(ext_gain.gain)
            ext_n_single=source_image_generate(ext_gain,obs,dimension=obs.dimension,restored_beam_width=gauss_sigma,pol=4,threshold=Mean(ext_gain.gain))
            ext_n_img=weight_img/Mean(ext_comp.gain)
            flux_frac_ext=1.-(1.-Mean(ext_comp.gain))^ext_n_img
            pix_i=where((flux_frac_ext*source_mask GE Abs(clean_bias_threshold)) AND (ext_n_single GE 1./gauss_area),n_pix)
            IF n_pix EQ 0 THEN CONTINUE
            sx=Float(pix_i mod dimension)
            sy=Float(Floor(pix_i/dimension))
            apply_astrometry, obs, x_arr=sx, y_arr=sy, ra_arr=sra, dec_arr=sdec, /xy2ad, /refraction
            salpha=Replicate(source_arr[si].alpha,n_pix)
            sfreq=Replicate(source_arr[si].freq,n_pix)
            ci=N_Elements(ext_comp)
            ;this will append a new component to the end of comp_arr_use
            ext_comp_new=source_comp_init(ext_comp,xv=sx,yv=sy,ra=sra,dec=sdec,freq=sfreq,alpha=salpha,id=gi)
            FOR pol_i=0,7 DO BEGIN
                IF source_arr[si].flux.(pol_i) EQ 0 THEN CONTINUE
                ext_img=source_image_generate(ext_comp,obs,pol=pol_i,dimension=obs.dimension,restored_beam_width=gauss_sigma)
                ext_img*=source_mask*(1.-flux_frac_ext)/gauss_area
                ext_comp_new[ci:*].flux.(pol_i)=ext_img[pix_i]
                source_arr[si].flux.(pol_i)+=Total(ext_img[pix_i])
            ENDFOR
            *(source_arr[si].extend)=ext_comp_new
        ENDIF ELSE BEGIN
            sx=source_arr[si].x & sy=source_arr[si].y
            sra=source_arr[si].ra & sdec=source_arr[si].dec
            salpha=source_arr[si].alpha & sfreq=source_arr[si].freq
            IF source_mask[sx,sy] EQ 0 THEN CONTINUE
            ;this will append a new component to the end of comp_arr_use
            comp_arr_use=source_comp_init(comp_arr_use,xv=sx,yv=sy,ra=sra,dec=sdec,freq=sfreq,alpha=salpha,id=gi)
            ci=N_Elements(comp_arr_use)-1
            FOR pol_i=0,7 DO BEGIN
                comp_arr_use[ci].flux.(pol_i)=source_arr[si].flux.(pol_i)*(1.-flux_frac_arr[si])/gauss_area
                source_arr[si].flux.(pol_i)+=comp_arr_use[ci].flux.(pol_i)/gauss_area
            ENDFOR
        ENDELSE
        
    ENDFOR
    comp_arr=comp_arr_use
ENDIF

print,String(format='(A," sources detected with maximum signal-to-noise of ",A)',Strn(N_Elements(source_arr)),Strn(max(source_arr.ston)))
RETURN,source_arr
END