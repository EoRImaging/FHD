FUNCTION Components2Sources,comp_arr,obs,fhd,radius=radius,noise_map=noise_map,extend_allow=extend_allow,$
    reject_sigma_threshold=reject_sigma_threshold,clean_bias_threshold=clean_bias_threshold,gain_array=gain_array
compile_opt idl2,strictarrsubs  

IF N_Elements(radius) EQ 0 THEN radius=1.
astr=obs.astr

ns=(size(comp_arr,/dimension))[0]
group_id=fltarr(ns)-1
g_id=0
FOR si=0L,ns-1 DO BEGIN 
    IF group_id[si] GE 0 THEN CONTINUE ;skip sources already grouped
    IF comp_arr[si].flux.I EQ 0 THEN CONTINUE
    si_use=where(group_id EQ -1,n_use)  
    dx=comp_arr[si].x-comp_arr[si_use].x
    dy=comp_arr[si].y-comp_arr[si_use].y
    dr=sqrt(dx^2.+dy^2.)
    group_i=where(dr LT radius,n_group) ;guaranteed at least one since si is included in si_use
    group_id[si_use[group_i]]=g_id
    g_id+=1
ENDFOR

ng=max(group_id)
IF ng LE 0 THEN RETURN,source_comp_init(n_sources=1)

hgroup=histogram(group_id,binsize=1,min=0,reverse_ind=gri)

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
    
    
    dist_test=Sqrt((source_arr[gi].x-comp_arr[si_g].x)^2.+(source_arr[gi].y-comp_arr[si_g].y)^2.)
    IF Stddev(dist_test) GE radius/4. THEN BEGIN
        (source_arr[gi].extend)=Ptr_new(comp_arr[si_g])
    ENDIF
;    source_arr[gi].extend=0 ;need to add some way to handle extended sources!
ENDFOR

IF Keyword_Set(reject_sigma_threshold) THEN BEGIN
    IF N_Elements(noise_map) EQ 0 THEN si_use=where(source_arr.flux.I GE reject_sigma_threshold,n_use) $
        ELSE si_use=where(source_arr.ston GE reject_sigma_threshold,n_use)
    source_arr=source_arr[si_use]
ENDIF

IF Keyword_Set(clean_bias_threshold) THEN BEGIN
    IF clean_bias_threshold GE 1 THEN clean_bias_threshold=0.5
    ns=N_Elements(source_arr)
    comp_gi=comp_arr.id
    hcomp_gi=histogram(comp_gi,min=0,/bin)
    gain_factor=gain_array[source_arr.x,source_arr.y]
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
            comp_arr=source_comp_init(comp_arr,xv=sx,yv=sy,ra=sra,dec=sdec,freq=sfreq,alpha=salpha,id=gi)
            ci=N_Elements(comp_arr)-1
            FOR pol_i=0,7 DO BEGIN
                comp_arr[ci].flux.(pol_i)=source_arr[si].flux.(pol_i)*(1.-flux_frac_arr[si])
                source_arr[si].flux.(pol_i)+=comp_arr[ci].flux.(pol_i)
            ENDFOR
        ENDELSE
        
    ENDFOR
ENDIF

RETURN,source_arr
END