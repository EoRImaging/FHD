FUNCTION group_source_components,obs,comp_arr,radius=radius,gain_factor=gain_factor

; Source components are grouped by gridding and merging footprints above the threshold of the deconvolution gain_factor
; Orphan components are culled
gauss_sigma=beam_width_calculate(obs)
gauss_width=beam_width_calculate(obs,/fwhm)
IF N_Elements(radius) EQ 0 THEN radius=gauss_width
dimension=obs.dimension
elements=obs.elements


comp_i_use=where(comp_arr.flux.I GT 0,n_comp_use)
IF N_Elements(detection_threshold) EQ 0 THEN detection_threshold=Min(comp_arr[comp_i_use].flux.I)/2.

source_image=source_image_generate(comp_arr,obs,pol_i=4,restored_beam_width=gauss_sigma,resolution=16,threshold=1E-2)

cx=comp_arr.x
cy=comp_arr.y
weight_arr=source_comp_init(xvals=cx,yvals=cy,flux=1.)
weight_arr=weight_arr[comp_i_use]
component_intensity=source_image_generate(weight_arr,obs,pol_i=4,restored_beam_width=gauss_sigma,resolution=16,threshold=1E-2)
undefine_fhd,weight_arr ;make sure this isn't a memory leak
IF N_Elements(gain_factor) EQ 0 THEN gain_factor=0.15
component_intensity=1.-(1.-gain_factor)^component_intensity

source_intensity_threshold=gain_factor<0.5
local_max_radius=Ceil(2.*radius)>2.
max_image=max_filter(source_image,local_max_radius+1.,/circle)
source_candidate_i=where((source_image EQ max_image) AND (component_intensity GT source_intensity_threshold),n_candidates)
source_candidate_i=Reverse(source_candidate_i[Sort(source_image[source_candidate_i])]) ;order from brightest to faintest

;determine the extent of pixels that are associated with each source candidate
influence_map=fltarr(dimension,elements)
candidate_map=lonarr(dimension,elements)-1
xvals=meshgrid(dimension,elements,1)
yvals=meshgrid(dimension,elements,2)
candidate_vals=source_image[source_candidate_i]
t0=0.
t1=0.
t2=0.
t3=0.
t4=0.
t5=0.
influence_inds=Region_grow(component_intensity,source_candidate_i,threshold=[gain_factor,1.])
ind_map=lindgen(dimension,elements)
zoom_x=Minmax(Floor(influence_inds) mod dimension)+[-2,2]
zoom_y=Minmax(Floor(influence_inds/dimension))+[-2,2]
ind_map=ind_map[zoom_x[0]:zoom_x[1],zoom_y[0]:zoom_y[1]]
intensity_zoom=component_intensity[zoom_x[0]:zoom_x[1],zoom_y[0]:zoom_y[1]]
source_candidate_x=source_candidate_i mod dimension & source_candidate_x-=zoom_x[0] 
source_candidate_y=Floor(source_candidate_i/dimension) & source_candidate_y-=zoom_y[0]
zoom_dim=Long(zoom_x[1]-zoom_x[0]+1)
source_candidate_i2=source_candidate_x+source_candidate_y*zoom_dim
FOR c_i=0,n_candidates-1 DO BEGIN
    t0a=Systime(1)
    influence_i=Region_grow(intensity_zoom,source_candidate_i2[c_i],threshold=[gain_factor,1.])
    IF min(influence_i) EQ -1 THEN CONTINUE
    influence_i=ind_map[influence_i]
    t1a=Systime(1)
    t0+=t1a-t0a
    c_i_i=source_candidate_i[c_i]
    dist_arr=sqrt((xvals[c_i_i]-xvals[influence_i])^2.+(yvals[c_i_i]-yvals[influence_i])^2.)/gauss_width
    t2a=Systime(1)
    t1+=t2a-t1a
    single_influence=candidate_vals[c_i]*Exp(-dist_arr)
    t3a=Systime(1)
    t2+=t3a-t2a
    primary_i=where(single_influence GT influence_map[influence_i],n_primary)
    t4a=Systime(1)
    t3+=t4a-t3a
    IF n_primary EQ 0 THEN CONTINUE
    candidate_map[influence_i[primary_i]]=c_i
    influence_map[influence_i[primary_i]]=single_influence[primary_i]
    t5+=Systime(1)-t4a
ENDFOR

;group source components by their associated source candidate. 
;Components without a source candidate will not be included

source_pix_i=Round(cx)+dimension*Round(cy)
group_id=candidate_map[source_pix_i]

IF max(group_id) LE 0 THEN RETURN,group_id

hgroup=histogram(group_id,binsize=1,min=0,reverse_ind=gri)
group_inds=where(hgroup GT 1,ng)

cx_arr=source_candidate_i mod dimension
cy_arr=Floor(source_candidate_i/dimension)
sub_pad=5. ;don't change this. Padding is required around the sub_image for source_image_generate and then region_grow to work properly

FOR gi=0L,ng-1 DO BEGIN
;    IF hgroup[gi] EQ 0 THEN CONTINUE
    gi_in=group_inds[gi]
    si_g=gri[gri[gi_in]:gri[gi_in+1]-1]; guaranteed at least one source per group
    
    x_offset=Floor(Min(comp_arr[si_g].x))-sub_pad
    y_offset=Floor(Min(comp_arr[si_g].y))-sub_pad
    sub_x=comp_arr[si_g].x-x_offset
    sub_y=comp_arr[si_g].y-y_offset
    comp_arr1=source_comp_init(xvals=sub_x,yvals=sub_y,flux=comp_arr[si_g].flux.I)
    sub_dim=Max(Ceil(sub_x))+sub_pad
    sub_elem=Max(Ceil(sub_y))+sub_pad
    single_source_image=source_image_generate(comp_arr1,pol_i=4,dimension=sub_dim,elements=sub_elem,restored_beam_width=gauss_sigma,resolution=16,threshold=1E-2)
    gauss_img=gauss2Dfit_errchk(single_source_image,gauss_params,/tilt,status=fit_error)
    IF fit_error GT 0 THEN BEGIN
        gcntrd,single_source_image,cx_arr[gi_in]-x_offset,cy_arr[gi_in]-y_offset,xcen_sub,ycen_sub,gauss_width,/silent
        IF Abs(xcen_sub-gauss_params[4])>Abs(ycen_sub-gauss_params[5]) LT 0.1 THEN BEGIN
            x_ext=(gauss_params[2]*2.*Sqrt(2.*Alog(2.))/gauss_width)>1.
            y_ext=(gauss_params[3]*2.*Sqrt(2.*Alog(2.))/gauss_width)>1.
            ext_factor=Sqrt(x_ext*y_ext)
        ENDIF ELSE ext_factor=-99.
    ENDIF ELSE BEGIN
        xcen_sub=gauss_params[4]
        ycen_sub=gauss_params[5]
        x_ext=(gauss_params[2]*2.*Sqrt(2.*Alog(2.))/gauss_width)>1.
        y_ext=(gauss_params[3]*2.*Sqrt(2.*Alog(2.))/gauss_width)>1.
        ext_factor=Sqrt(x_ext*y_ext)
    ENDELSE
    IF xcen_sub LE 0 OR xcen_sub GE sub_dim-1 OR ycen_sub LE 0 OR ycen_sub GE sub_elem THEN BEGIN
        maxval=Max(comp_arr[si_g].flux.I,max_i)
        xcen_sub=sub_x[max_i]
        ycen_sub=sub_y[max_i]
        ext_factor=-99.
    ENDIF   
    
    sub_expand=(2./ext_factor^2.)>(1./gauss_width)
    IF sub_expand LE 1 THEN BEGIN
        print, 'WARNING: sub_expand is less than one. Manually resetting to be 1.
        sub_expand=1
    ENDIF
    n_sub=N_Elements(si_g)
    
    sub_dim2=Ceil(sub_dim*sub_expand)
    sub_elem2=Ceil(sub_elem*sub_expand)
    sub_x2=Round(sub_x*sub_expand)
    sub_y2=Round(sub_y*sub_expand)
    xcen_sub2=Round(xcen_sub*sub_expand)
    ycen_sub2=Round(ycen_sub*sub_expand)
    sub_image=intarr(sub_dim2,sub_elem2)-1
    sub_image[sub_x2,sub_y2]=1
    sub_image[xcen_sub2,ycen_sub2]=1
    sub_i=sub_x2+sub_y2*sub_dim2
    sub_hist=histogram(sub_i,min=0,/binsize,reverse_ind=ri_sub)
    cen_sub_i=xcen_sub2+ycen_sub2*sub_dim2
    sub_i_use=Region_grow(sub_image,cen_sub_i,/all_neighbors,threshold=[0,1])
    sub_use_test=intarr(n_sub)
    n_sub_test=N_Elements(sub_i_use)
    IF Max(sub_i_use) EQ -1 THEN n_sub_test=0
    FOR si_i=0L,n_sub_test-1 DO BEGIN
        sub_i_i=sub_i_use[si_i]
        IF sub_i_i GE N_Elements(sub_hist) THEN CONTINUE
        IF sub_hist[sub_i_i] GT 0 THEN sub_use_test[ri_sub[ri_sub[sub_i_i]:ri_sub[sub_i_i+1]-1]]=1
    ENDFOR
    comp_i_sub=where(sub_use_test,n_sub_use,ncomplement=n_cut,complement=cut_i)
    
    IF n_cut GT 0 THEN group_id[si_g[cut_i]]=-1
ENDFOR

RETURN,group_id
END