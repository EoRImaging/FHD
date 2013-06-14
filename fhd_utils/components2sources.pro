FUNCTION Components2Sources,comp_arr,fhd,radius=radius,noise_map=noise_map,extend_allow=extend_allow,$
    gain_recapture=gain_recapture,reject_sigma_threshold=reject_sigma_threshold
compile_opt idl2,strictarrsubs  

IF N_Elements(radius) EQ 0 THEN radius=1.

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

hgroup=histogram(group_id,binsize=1,min=0,reverse_ind=gri)
ng=max(group_id)

source_comp_init,source_arr,n_sources=ng
FOR gi=0L,ng-1 DO BEGIN
    si_g=gri[gri[gi]:gri[gi+1]-1]; guaranteed at least one source per group
    
    flux_I=(comp_arr[si_g].flux.I)>0.
    IF Total(flux_I) LE 0 THEN CONTINUE
    source_arr[gi].x=Total(comp_arr[si_g].x*flux_I)/Total(flux_I) ;flux_I is guaranteed to be non-zero from above
    source_arr[gi].y=Total(comp_arr[si_g].y*flux_I)/Total(flux_I) ;flux_I is guaranteed to be non-zero from above
    source_arr[gi].ra=Total(comp_arr[si_g].ra*flux_I)/Total(flux_I) ;flux_I is guaranteed to be non-zero from above
    source_arr[gi].dec=Total(comp_arr[si_g].dec*flux_I)/Total(flux_I) ;flux_I is guaranteed to be non-zero from above
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

;IF Keyword_Set(gain_recapture) THEN BEGIN
;    IF ~Keyword_Set(fhd) THEN BREAK
;    gain_factor=fhd.gain_factor
;    ns=N_Elements(source_arr)
;    comp_gi=comp_arr.id
;    hcomp_gi=histogram(comp_gi,min=0,/bin)
;    FOR si = 0L,ns-1L DO BEGIN
;        gi=source_arr[si].id
;        IF Ptr_valid(source_arr[si].extend) THEN BEGIN
;        
;        ENDIF ELSE BEGIN
;            ncomp=hcomp_gi[gi]
;            flux_frac=1.-(1.-gain_factor)^ncomp
;            FOR pol_i=0,7 DO source_arr[gi].flux.(pol_i)=source_arr[gi].flux.(pol_i)/flux_frac
;        ENDELSE
;        
;    ENDFOR
;ENDIF

RETURN,source_arr
END