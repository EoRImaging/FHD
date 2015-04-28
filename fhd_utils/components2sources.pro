FUNCTION Components2Sources,comp_arr,obs,detection_threshold=detection_threshold,radius=radius,noise_map=noise_map,$
    reject_sigma_threshold=reject_sigma_threshold,clean_bias_threshold=clean_bias_threshold,gain_array=gain_array,$
    reject_outlier_components=reject_outlier_components,extend_threshold=extend_threshold
compile_opt idl2,strictarrsubs  

IF N_Elements(reject_outlier_components) EQ 0 THEN reject_outlier_components=0
astr=obs.astr
dimension=obs.dimension
elements=obs.elements
n_pol=obs.n_pol
IF N_Elements(extend_threshold) EQ 0 THEN extend_threshold=0.2


gauss_sigma=beam_width_calculate(obs)
gauss_width=beam_width_calculate(obs,/fwhm)
comp_i_use=where(comp_arr.flux.I GT 0,n_comp_use)
IF n_comp_use EQ 0 THEN RETURN,source_comp_init(n_sources=0)
IF N_Elements(detection_threshold) EQ 0 THEN detection_threshold=Min(comp_arr[comp_i_use].flux.I)/2.

group_id=group_source_components(obs,comp_arr,radius=radius,gain_array=gain_array)
;source_image=source_image_generate(comp_arr,obs,pol_i=4,restored_beam_width=gauss_sigma,resolution=16,threshold=1E-2)
;
;cx=comp_arr.x
;cy=comp_arr.y
;weight_arr=source_comp_init(xvals=cx,yvals=cy,flux=1.)
;weight_arr=weight_arr[comp_i_use]
;component_intensity=source_image_generate(weight_arr,obs,pol_i=4,restored_beam_width=gauss_sigma,resolution=16,threshold=1E-2)
;undefine_fhd,weight_arr ;make sure this isn't a memory leak
;CASE N_Elements(gain_array) OF
;    0: gain_min=(gain_array=0.15)
;    1: gain_min=gain_array
;    ELSE: gain_min=Min(gain_array[where(gain_array GT 0)]) 
;ENDCASE
;component_intensity=1.-(1.-gain_array)^component_intensity
;
;source_intensity_threshold=gain_min<0.5
;local_max_radius=Ceil(2.*radius)>2.
;max_image=max_filter(source_image,local_max_radius+1.,/circle)
;source_candidate_i=where((source_image EQ max_image) AND (component_intensity GT source_intensity_threshold),n_candidates)
;source_candidate_i=Reverse(source_candidate_i[Sort(source_image[source_candidate_i])]) ;order from brightest to faintest
;
;;determine the extent of pixels that are associated with each source candidate
;influence_map=fltarr(dimension,elements)
;candidate_map=lonarr(dimension,elements)-1
;xvals=meshgrid(dimension,elements,1)
;yvals=meshgrid(dimension,elements,2)
;candidate_vals=source_image[source_candidate_i]
;t0=0.
;t1=0.
;t2=0.
;t3=0.
;t4=0.
;t5=0.
;influence_inds=Region_grow(component_intensity,source_candidate_i,threshold=[gain_min,1.])
;ind_map=lindgen(dimension,elements)
;zoom_x=Minmax(Floor(influence_inds) mod dimension)+[-2,2]
;zoom_y=Minmax(Floor(influence_inds/dimension))+[-2,2]
;ind_map=ind_map[zoom_x[0]:zoom_x[1],zoom_y[0]:zoom_y[1]]
;intensity_zoom=component_intensity[zoom_x[0]:zoom_x[1],zoom_y[0]:zoom_y[1]]
;source_candidate_x=source_candidate_i mod dimension & source_candidate_x-=zoom_x[0] 
;source_candidate_y=Floor(source_candidate_i/dimension) & source_candidate_y-=zoom_y[0]
;zoom_dim=Long(zoom_x[1]-zoom_x[0]+1)
;source_candidate_i2=source_candidate_x+source_candidate_y*zoom_dim
;c_i0=0L
;FOR c_i=0L,n_candidates-1 DO BEGIN
;    t0a=Systime(1)
;    influence_i=Region_grow(intensity_zoom,source_candidate_i2[c_i],threshold=[gain_min,1.])
;    IF min(influence_i) EQ -1 THEN CONTINUE
;    influence_i=ind_map[influence_i]
;    t1a=Systime(1)
;    t0+=t1a-t0a
;    c_i_i=source_candidate_i[c_i]
;    dist_arr=sqrt((xvals[c_i_i]-xvals[influence_i])^2.+(yvals[c_i_i]-yvals[influence_i])^2.)/gauss_width
;    t2a=Systime(1)
;    t1+=t2a-t1a
;    single_influence=candidate_vals[c_i]*Exp(-dist_arr);/(dist_arr+1)
;    t3a=Systime(1)
;    t2+=t3a-t2a
;    primary_i=where(single_influence GT influence_map[influence_i],n_primary)
;    t4a=Systime(1)
;    t3+=t4a-t3a
;    IF n_primary EQ 0 THEN CONTINUE
;    candidate_map[influence_i[primary_i]]=c_i
;    influence_map[influence_i[primary_i]]=single_influence[primary_i]
;    c_i0+=1
;    t5+=Systime(1)-t4a
;ENDFOR
;
;;group source components by their associated source candidate. 
;;Components without a source candidate will not be included
;
;source_pix_i=Round(cx)+dimension*Round(cy)
;group_id=candidate_map[source_pix_i]

;IF max(group_id) LE 0 THEN RETURN,source_comp_init(n_sources=0)
;hgroup=histogram(group_id,binsize=1,min=0,reverse_ind=gri)
;group_inds=where(hgroup GT 1,ng)
;
;cx_arr=source_candidate_i mod dimension
;cy_arr=Floor(source_candidate_i/dimension)
;sub_pad=5. ;don't change this. Padding is required around the sub_image for source_image_generate and then region_grow to work properly
;
;FOR gi=0L,ng-1 DO BEGIN
;;    IF hgroup[gi] EQ 0 THEN CONTINUE
;    gi_in=group_inds[gi]
;    si_g=gri[gri[gi_in]:gri[gi_in+1]-1]; guaranteed at least one source per group
;    
;    x_offset=Floor(Min(comp_arr[si_g].x))-sub_pad
;    y_offset=Floor(Min(comp_arr[si_g].y))-sub_pad
;    sub_x=comp_arr[si_g].x-x_offset
;    sub_y=comp_arr[si_g].y-y_offset
;    comp_arr1=source_comp_init(xvals=sub_x,yvals=sub_y,flux=comp_arr[si_g].flux.I)
;    sub_dim=Max(Ceil(sub_x))+sub_pad
;    sub_elem=Max(Ceil(sub_y))+sub_pad
;    single_source_image=source_image_generate(comp_arr1,pol_i=4,dimension=sub_dim,elements=sub_elem,restored_beam_width=gauss_sigma,resolution=16,threshold=1E-2)
;    gauss_img=gauss2Dfit_errchk(single_source_image,gauss_params,/tilt,status=fit_error)
;    IF fit_error GT 0 THEN BEGIN
;        gcntrd,single_source_image,cx_arr[gi_in]-x_offset,cy_arr[gi_in]-y_offset,xcen_sub,ycen_sub,gauss_width,/silent
;        IF Abs(xcen_sub-gauss_params[4])>Abs(ycen_sub-gauss_params[5]) LT 0.1 THEN BEGIN
;            x_ext=(gauss_params[2]*2.*Sqrt(2.*Alog(2.))/gauss_width)>1.
;            y_ext=(gauss_params[3]*2.*Sqrt(2.*Alog(2.))/gauss_width)>1.
;            ext_factor=Sqrt(x_ext*y_ext)
;        ENDIF ELSE ext_factor=-99.
;    ENDIF ELSE BEGIN
;        xcen_sub=gauss_params[4]
;        ycen_sub=gauss_params[5]
;        x_ext=(gauss_params[2]*2.*Sqrt(2.*Alog(2.))/gauss_width)>1.
;        y_ext=(gauss_params[3]*2.*Sqrt(2.*Alog(2.))/gauss_width)>1.
;        ext_factor=Sqrt(x_ext*y_ext)
;    ENDELSE
;    IF xcen_sub LE 0 OR xcen_sub GE sub_dim-1 OR ycen_sub LE 0 OR ycen_sub GE sub_elem THEN BEGIN
;        maxval=Max(comp_arr[si_g].flux.I,max_i)
;        xcen_sub=sub_x[max_i]
;        ycen_sub=sub_y[max_i]
;        ext_factor=-99.
;    ENDIF   
;    
;    sub_expand=(2./ext_factor^2.)>(1./gauss_width)
;    n_sub=N_Elements(si_g)
;    
;    sub_dim2=Ceil(sub_dim*sub_expand)
;    sub_elem2=Ceil(sub_elem*sub_expand)
;    sub_x2=Round(sub_x*sub_expand)
;    sub_y2=Round(sub_y*sub_expand)
;    xcen_sub2=Round(xcen_sub*sub_expand)
;    ycen_sub2=Round(ycen_sub*sub_expand)
;    sub_image=intarr(sub_dim2,sub_elem2)-1
;    sub_image[sub_x2,sub_y2]=1
;    sub_image[xcen_sub2,ycen_sub2]=1
;    sub_i=sub_x2+sub_y2*sub_dim2
;    sub_hist=histogram(sub_i,min=0,/binsize,reverse_ind=ri_sub)
;    cen_sub_i=xcen_sub2+ycen_sub2*sub_dim2
;    sub_i_use=Region_grow(sub_image,cen_sub_i,/all_neighbors,threshold=[0,1])
;    sub_use_test=intarr(n_sub)
;    n_sub_test=N_Elements(sub_i_use)
;    IF Max(sub_i_use) EQ -1 THEN n_sub_test=0
;    FOR si_i=0L,n_sub_test-1 DO BEGIN
;        sub_i_i=sub_i_use[si_i]
;        IF sub_i_i GE N_Elements(sub_hist) THEN CONTINUE
;        IF sub_hist[sub_i_i] GT 0 THEN sub_use_test[ri_sub[ri_sub[sub_i_i]:ri_sub[sub_i_i+1]-1]]=1
;    ENDFOR
;    comp_i_sub=where(sub_use_test,n_sub_use,ncomplement=n_cut,complement=cut_i)
;    
;    IF n_cut GT 0 THEN group_id[si_g[cut_i]]=-1
;;    IF n_sub_use GT 0 THEN si_g=si_g[comp_i_sub]
;ENDFOR

IF max(group_id) LE 0 THEN RETURN,source_comp_init(n_sources=0)
hgroup=histogram(group_id,binsize=1,min=0,reverse_ind=gri)
group_inds=where(hgroup GT 1,ng)

ungroup_i=where(group_id LT 0,n_ungroup)
IF n_ungroup GT 0 THEN BEGIN
    IF Tag_exist(comp_arr,'flag') THEN comp_arr[ungroup_i].flag=1
    IF ~Keyword_Set(reject_outlier_components)THEN BEGIN
        g0=Max(group_id)+1
        group_id_sub=group_source_components(obs,comp_arr[ungroup_i],radius=radius,gain_array=gain_array)
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
    
    sx=Total(comp_arr[si_g].x*flux_I)/Total(flux_I)
    sy=Total(comp_arr[si_g].y*flux_I)/Total(flux_I)
    
    xy2ad,sx,sy,astr,sra,sdec
    source_arr[gi].x=sx ;flux_I is guaranteed to be non-zero from above
    source_arr[gi].y=sy ;flux_I is guaranteed to be non-zero from above
    source_arr[gi].ra=sra ;flux_I is guaranteed to be non-zero from above
    source_arr[gi].dec=sdec ;flux_I is guaranteed to be non-zero from above
    FOR pol_i=0,7 DO source_arr[gi].flux.(pol_i)=Total(comp_arr[si_g].flux.(pol_i))
    source_arr[gi].id=gi
    comp_arr[si_g].id=gi
    IF Keyword_Set(noise_map) THEN BEGIN
        nm0=noise_map[source_arr[gi].x,source_arr[gi].y] ;need some sort of error checking here first!!!
        IF nm0 GT 0 THEN source_arr[gi].ston=Total(flux_I)/nm0 ELSE source_arr[gi].ston=0.
    ENDIF ELSE source_arr[gi].ston=Max(comp_arr[si_g].ston)
    source_arr[gi].alpha=Total(comp_arr[si_g].alpha*flux_I)/Total(flux_I)
    source_arr[gi].freq=Total(comp_arr[si_g].freq*flux_I)/Total(flux_I)    
    IF Tag_exist(comp_arr,'flag') THEN source_arr[gi].flag=Max(comp_arr[si_g].flag)
    
    extend_test=Mean(Sqrt((sx-comp_arr[si_g].x)^2.+(sy-comp_arr[si_g].y)^2.))
    IF extend_test GE extend_threshold THEN BEGIN
        (source_arr[gi].extend)=Ptr_new(comp_arr[si_g])
    ENDIF
ENDFOR

comp_arr_use=comp_arr
IF Keyword_Set(reject_sigma_threshold) THEN BEGIN
    IF N_Elements(noise_map) EQ 0 THEN si_use=where(source_arr.flux.I GE reject_sigma_threshold,n_use) $
        ELSE si_use=where(source_arr.ston GE reject_sigma_threshold,n_use)
    source_arr=source_arr[si_use]
ENDIF

IF Keyword_Set(clean_bias_threshold) THEN BEGIN
    IF clean_bias_threshold GE 1 THEN clean_bias_threshold=0.5
    ns=N_Elements(source_arr)
    comp_gi=comp_arr_use.id
    hcomp_gi=histogram(comp_gi,min=0,/bin,reverse_ind=c_ri)
    IF N_Elements(gain_array) EQ 1 THEN gain_factor=gain_array ELSE gain_factor=gain_array[source_arr.x,source_arr.y]
    gain_factor_arr=comp_arr_use.gain
    id_use=where(hcomp_gi,n_id_use)
    flux_frac_arr=Fltarr(ns)+1.
    FOR i=0L,n_id_use-1 DO BEGIN
        id_i=id_use[i]
        ;What was this supposed to do???
    ENDFOR
;    product(
    flux_frac_arr=1.-(1.-gain_factor)^hcomp_gi[source_arr.id]
    
    si_use=where(flux_frac_arr GE Abs(clean_bias_threshold),n_use)
    IF n_use EQ 0 THEN RETURN,source_arr ; return if no valid values found
    source_arr=source_arr[si_use]
    flux_frac_arr=flux_frac_arr[si_use]
    
    IF clean_bias_threshold LT 0 THEN RETURN,source_arr ; return trimmed source array if threshold is specified as negative, but don't correct any fluxes
    
    FOR si = 0L,n_use-1L DO BEGIN
        gi=source_arr[si].id
        ncomp=hcomp_gi[gi]
        
        IF Ptr_valid(source_arr[si].extend) THEN BEGIN
            ;still need to handle extended sources properly!
        ENDIF ELSE BEGIN
            sx=source_arr[si].x & sy=source_arr[si].y
            sra=source_arr[si].ra & sdec=source_arr[si].dec
            salpha=source_arr[si].alpha & sfreq=source_arr[si].freq
            comp_arr_use=source_comp_init(comp_arr_use,xv=sx,yv=sy,ra=sra,dec=sdec,freq=sfreq,alpha=salpha,id=gi) ;this will append a new component to the end of comp_arr_use
            ci=N_Elements(comp_arr_use)-1
            FOR pol_i=0,7 DO BEGIN
                comp_arr_use[ci].flux.(pol_i)=source_arr[si].flux.(pol_i)*(1.-flux_frac_arr[si])
                source_arr[si].flux.(pol_i)+=comp_arr_use[ci].flux.(pol_i)
            ENDFOR
        ENDELSE
        
    ENDFOR
    comp_arr=comp_arr_use
ENDIF

print,String(format='(A," sources detected with maximum signal-to-noise of ",A)',Strn(N_Elements(source_arr)),Strn(max(source_arr.ston)))
RETURN,source_arr
END