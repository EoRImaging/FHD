FUNCTION Components2Sources,comp_arr,obs,detection_threshold=detection_threshold,radius=radius,noise_map=noise_map,extend_allow=extend_allow,$
    reject_sigma_threshold=reject_sigma_threshold,clean_bias_threshold=clean_bias_threshold,gain_array=gain_array
compile_opt idl2,strictarrsubs  

IF N_Elements(radius) EQ 0 THEN radius=1.
astr=obs.astr
dimension=obs.dimension
elements=obs.elements
n_pol=obs.n_pol

n_sources=(size(comp_arr,/dimension))[0]

gauss_width=1.
comp_i_use=where(comp_arr.flux.I GT 0,n_comp_use)
IF N_Elements(detection_threshold) EQ 0 THEN detection_threshold=Min(comp_arr[comp_i_use].flux.I)/2.
source_image=Fltarr(dimension,elements)
FOR pol_i=0,(n_pol<2)-1 DO source_image+=$
    source_image_generate(comp_arr,obs,pol_i=pol_i,restored_beam_width=gauss_width,resolution=16,threshold=1E-2)
IF n_pol EQ 1 THEN source_image*=2.
cx=comp_arr.x
cy=comp_arr.y
weight_arr=source_comp_init(xvals=cx,yvals=cy,flux=1.)
weight_arr=weight_arr[comp_i_use]
component_intensity=source_image_generate(weight_arr,obs,pol_i=4,restored_beam_width=gauss_width,resolution=16,threshold=1E-2)
undefine_fhd,weight_arr ;make sure this isn't a memory leak
component_intensity=1.-(1.-gain_array)^component_intensity
source_intensity_threshold=0.5
local_max_radius=Ceil(2.*radius)>2.
max_image=max_filter(source_image,2.*local_max_radius+1.,/circle)
source_candidate_i=where((source_image EQ max_image) AND (component_intensity GT source_intensity_threshold),n_candidates)
source_candidate_i=Reverse(source_candidate_i[Sort(source_image[source_candidate_i])]) ;order from brightest to faintest

;determine the extent of pixels that are associated with each source candidate
influence_map=fltarr(dimension,elements)
candidate_map=lonarr(dimension,elements)-1
gain_min=Min(gain_array[where(gain_array GT 0)])
xvals=meshgrid(dimension,elements,1)
yvals=meshgrid(dimension,elements,2)
candidate_vals=source_image[source_candidate_i]
FOR c_i=0L,n_candidates-1 DO BEGIN
    c_i_i=source_candidate_i[c_i]
    influence_i=Region_grow(component_intensity,c_i_i,threshold=[gain_min,1.])
    dist_arr=((xvals[c_i_i]-xvals[influence_i])^2.+(yvals[c_i_i]-yvals[influence_i])^2.)/gauss_width^2. ;NOTE: not taking square root here
    single_influence=candidate_vals[c_i]*Exp(-dist_arr)
    primary_i=where(single_influence GT influence_map[influence_i],n_primary)
    IF n_primary EQ 0 THEN CONTINUE
    candidate_map[influence_i[primary_i]]=c_i
    influence_map[influence_i[primary_i]]=single_influence[primary_i]
ENDFOR

;group source components by their associated source candidate. Components without a source candidate will not be included
;source_mask_i=region_grow(component_intensity,source_candidate,threshold=[gain_min,1.])
;source_mask=fltarr(dimension,elements) & source_mask[source_mask_i]=1

source_pix_i=Round(cx)+dimension*Round(cy)
pix_hist=histogram(source_pix_i,min=0,/binsize,reverse_ind=pri)
pix_i_use=where(pix_hist,n_pix)
group_id=Lonarr(n_sources)-1


debug_point=1
;group_id=fltarr(n_sources)-1
;g_id=0
;FOR si=0L,n_sources-1 DO BEGIN 
;    IF group_id[si] GE 0 THEN CONTINUE ;skip sources already grouped
;    IF comp_arr[si].flux.I EQ 0 THEN CONTINUE
;    si_use=where(group_id EQ -1,n_use)  
;    dx=comp_arr[si].x-comp_arr[si_use].x
;    dy=comp_arr[si].y-comp_arr[si_use].y
;    dr=sqrt(dx^2.+dy^2.)
;    group_i=where(dr LT radius,n_group) ;guaranteed at least one since si is included in si_use
;    group_id[si_use[group_i]]=g_id
;    g_id+=1
;ENDFOR
;
;ng=max(group_id)
;IF ng LE 0 THEN RETURN,source_comp_init(n_sources=1)
;hgroup=histogram(group_id,binsize=1,min=0,reverse_ind=gri)
;group_inds=where(hgroup)

;si_primary=gri[gri[group_inds]]
;prim_x=source_array[si_primary].x
;prim_y=source_array[si_primary].y
;
;source_map=lonarr(dimension,elements)
;source_map[prim_x,prim_y]=lindgen(ng)+1
;background_dist=morph_distance(source_map,neighbor=3)
;extended_pix=where(background_dist GT (beam_width>1.),n_extend)
;extended_flag=intarr(n_sources)
;IF n_extend GT 0 THEN BEGIN
;    src_inds=source_map[extended_pix]-1
;    FOR ext_i=0L,n_extend-1 DO BEGIN
;        src_i=src_inds[ext_i]
;        IF extended_flag[src_i] NE 0 THEN CONTINUE ;skip sources already dealt with
;        pix_i=region_grow(background_dist,extended_pix[ext_i],thresh=[1,dimension])
;        add_i_use=source_map[pix_i]-1
;        flux_vals=image_I_flux[additional_i[add_i_use]]
;        ii_use=where(flux_vals GE Max(flux_vals)/2.,nii_use,complement=ii_unused,ncomplement=nii_unused)
;        IF nii_use GT 3 THEN BEGIN
;            extended_flag[add_i_use[ii_use]]=1.
;            IF nii_unused GT 0 THEN extended_flag[add_i_use[ii_unused]]=-1.
;        ENDIF ELSE extended_flag[add_i_use]=-1
;    ENDFOR
;ENDIF


source_arr=source_comp_init(n_sources=ng)
FOR gi=0L,ng-1 DO BEGIN
    si_g=gri[gri[gi]:gri[gi+1]-1]; guaranteed at least one source per group
    
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
    
    dist_test=Sqrt((source_arr[gi].x-comp_arr[si_g].x)^2.+(source_arr[gi].y-comp_arr[si_g].y)^2.)
    IF Stddev(dist_test) GE radius/4. THEN BEGIN
        (source_arr[gi].extend)=Ptr_new(comp_arr[si_g])
    ENDIF
;    source_arr[gi].extend=0 ;need to add some way to handle extended sources!
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
    gain_factor=gain_array[source_arr.x,source_arr.y]
    gain_factor_arr=comp_arr_use.gain
    id_use=where(hcomp_gi,n_id_use)
    flux_frac_arr=Fltarr(ns)+1.
    FOR i=0L,n_id_use-1 DO BEGIN
        id_i=id_use[i]
        
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


RETURN,source_arr
END