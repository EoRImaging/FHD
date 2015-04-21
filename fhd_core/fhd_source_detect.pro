FUNCTION fhd_source_detect,obs,fhd_params,jones,source_find_image,image_I_flux=image_I_flux,image_Q_flux=image_Q_flux,$
    image_U_flux=image_U_flux,image_V_flux=image_V_flux,beam_arr=beam_arr,beam_corr_avg=beam_corr_avg,$
    beam_mask=beam_mask,source_mask=source_mask,gain_array=gain_array,n_sources=n_sources,detection_threshold=detection_threshold,$
    model_I_image=model_I_image,_Extra=extra
;NOTE: if supplied, model_I_image should be in the same units and weighting scheme as source_find_image

add_threshold=fhd_params.add_threshold
max_add_sources=fhd_params.max_add_sources
pol_use=fhd_params.pol_use
independent_fit=fhd_params.independent_fit
reject_pol_sources=fhd_params.reject_pol_sources
sigma_threshold=2.
frequency=obs.freq_center
alpha_use=obs.alpha ;spectral index used for the subtracted component

n_pol=fhd_params.npol
dimension=obs.dimension
elements=obs.elements
degpix=obs.degpix
astr=obs.astr
beam_width=beam_width_calculate(obs,/fwhm)
beam_area=beam_width_calculate(obs,/area)
local_max_radius=beam_width*2.
box_radius=Ceil(local_max_radius)

IF N_Elements(beam_mask) EQ 0 THEN beam_mask=Fltarr(dimension,elements)+1.
IF N_Elements(image_I_flux) EQ 0 THEN image_I_flux=source_find_image
IF N_Elements(gain_array) EQ 1 THEN gain_array=replicate(gain_array[0],dimension,elements)
converge_check=Stddev(source_find_image[where(beam_mask)],/nan)

IF N_Elements(source_mask) EQ 0 THEN source_mask0=beam_mask ELSE source_mask0=source_mask
neg_i=where(source_find_image LE -Max(source_find_image),n_neg)
IF n_neg GT 0 THEN BEGIN
    source_neg=fltarr(dimension,elements)
    source_neg[neg_i]=1.
    source_neg=Smooth(source_neg,Ceil(beam_width*4.))*Ceil(beam_width*4.)^2
    source_mask0[where(source_neg)]=0
ENDIF

source_mask1=beam_mask*source_mask0
flux_offset=Mean(source_find_image[where(source_mask0)])
source_find_image-=flux_offset

;IF N_Elements(model_I_image) EQ N_Elements(source_find_image) THEN BEGIN
;    mask_test_i=where((source_find_image LT -5.*converge_check) AND (model_I_image GT 5.*converge_check),n_mask)
;    IF n_mask GT 0 THEN BEGIN
;        mask_test=fltarr(dimension,elements)
;        mask_test[mask_test_i]=1
;        mask_test=smooth(mask_test,2.*local_max_radius+1,/edge_truncate)
;        mask_i=where(mask_test,n_mask)
;        source_mask1[mask_i]=0
;    ENDIF
;ENDIF
    
;    Find additional sources:
;       require that they be isolated ; This is local_max_radius
;       should put some cap on the absolute number of them ; This is max_add_sources
;       all within some range of the brightest pixels flux, say 95%; This is add_threshold


n_sources=0
max_iter=5
iter=0
WHILE n_sources EQ 0 DO BEGIN
    source_find_image_use=source_find_image*source_mask1
    source_flux=Max(source_find_image_use*source_mask0,source_i)
    flux_ref1=source_find_image_use[source_i]*add_threshold
    flux_ref2=source_find_image_use[source_i]*0.5
    
    additional_i1=where(source_find_image_use GE flux_ref1,n_sources1)
    additional_i2=where((source_find_image_use GE 5.*converge_check) AND (source_find_image_use GE flux_ref2),n_sources2)
    IF n_sources1 GT n_sources2 THEN BEGIN
        additional_i=additional_i1
        detection_threshold=flux_ref1
        n_sources=n_sources1
    ENDIF ELSE BEGIN
        additional_i=additional_i2
        detection_threshold=flux_ref2
        n_sources=n_sources2
    ENDELSE
    
    ;output={n_sources1:n_sources1,n_sources2:n_sources2,source_find_image:source_find_image,beam_mask:beam_mask,source_mask:source_mask,converge_check:converge_check}
    ;save,output,filename=fhd.joint_obs+'_test_output.sav'
    
    additional_i=additional_i[reverse(Sort(source_find_image_use[additional_i]))] ;order from brightest to faintest
    add_x=additional_i mod dimension
    add_y=Float(Floor(additional_i/dimension))
    add_dist=fltarr(n_sources)+dimension
    FOR addi=1,n_sources-1 DO add_dist[addi]=Min(Sqrt((add_x[addi]-add_x[0:addi-1])^2+(add_y[addi]-add_y[0:addi-1])^2.))
        
    source_map=lonarr(dimension,elements)
    source_map[add_x,add_y]=lindgen(n_sources)+1
    background_dist=morph_distance(source_map,neighbor=3)
    extended_pix=where(background_dist GT (beam_width>1.),n_extend)
    extended_flag=fltarr(n_sources)
    IF n_extend GT 0 THEN BEGIN
        src_inds=source_map[extended_pix]-1
        FOR ext_i=0L,n_extend-1 DO BEGIN
            src_i=src_inds[ext_i]
            IF extended_flag[src_i] NE 0 THEN CONTINUE ;skip sources already dealt with
            pix_i=region_grow(background_dist,extended_pix[ext_i],thresh=[1,dimension])
            add_i_use=source_map[pix_i]-1
            flux_vals=image_I_flux[additional_i[add_i_use]]
            ii_use=where(flux_vals GE Max(flux_vals)/2.,nii_use,complement=ii_unused,ncomplement=nii_unused)
            IF nii_use GT 3 THEN BEGIN
                extended_flag[add_i_use[ii_use]]=1.
                IF nii_unused GT 0 THEN extended_flag[add_i_use[ii_unused]]=-1.
            ENDIF ELSE extended_flag[add_i_use]=-1
        ENDFOR
    ENDIF
    
    additional_i_usei=where((add_dist GE local_max_radius) OR (extended_flag GT 0),n_sources)
    additional_i=additional_i[additional_i_usei] ;guaranteed at least one, so this is safe
    add_dist=add_dist[additional_i_usei]
    extended_flag=extended_flag[additional_i_usei]
    
    ;IF (n_sources<max_add_sources)+si GT max_sources THEN max_add_sources=max_sources-si
    
    IF max_add_sources EQ 0 THEN BEGIN
        source_list=source_comp_init(n_sources=0,frequency=frequency,alpha=alpha_use)
        n_sources=0
        RETURN,source_list
    ENDIF
    
    IF n_sources GT max_add_sources THEN BEGIN
        additional_i=additional_i[0:max_add_sources-1]
        n_sources=max_add_sources
    ENDIF
    n_mask=0
    comp_arr=source_comp_init(n_sources=n_sources,frequency=frequency,alpha=alpha_use)
    
    ;fit flux here, and fill comp_arr for each pol
    flux_arr=fltarr(4)
    fit_threshold=-2.*converge_check
    source_box_xvals=meshgrid(2.*local_max_radius+1,2.*local_max_radius+1,1)
    source_box_yvals=meshgrid(2.*local_max_radius+1,2.*local_max_radius+1,2)
    ;source_fit_fn=Exp(-((source_box_xvals-local_max_radius)^2.+(source_box_yvals-local_max_radius)^2.)/(2.*local_max_radius))
    ;source_fit_fn_ref=Total(source_fit_fn)/2.
    
    si_use=Lonarr(n_sources)-1
    sx_arr=additional_i mod dimension
    sy_arr=Float(Floor(additional_i/dimension))
    
    si=0L
    FOR src_i=0L,n_sources-1 DO BEGIN
        sx=sx_arr[src_i]
        sy=sy_arr[src_i]
        IF add_dist[src_i] GE local_max_radius THEN BEGIN
            gcntrd,image_I_flux,sx,sy,xcen,ycen,beam_width,/keepcenter,/silent 
        ENDIF ELSE BEGIN
            IF extended_flag[src_i] EQ 0 THEN BEGIN
            ;if NOT marked as an extended source, skip if too close to a brighter source
                source_mask1[sx,sy]=0
                CONTINUE 
            ENDIF
            xcen=(ycen=-1)
        ENDELSE
        flux_interp_flag=extended_flag[src_i]
        IF Abs(sx-xcen) GT beam_width THEN BEGIN
;            IF extended_flag[src_i] EQ 0 THEN BEGIN
;                ;if NOT marked as an extended source, skip if centroiding failed for either pol
;                source_mask1[sx,sy]=0
;                CONTINUE 
;            ENDIF
            IF xcen EQ -1 THEN xcen=sx
            flux_interp_flag=1
        ENDIF
        IF Abs(sy-ycen) GT beam_width THEN BEGIN
;            IF extended_flag[src_i] EQ 0 THEN BEGIN
;                ;if NOT marked as an extended source, skip if centroiding failed for either pol
;                source_mask1[sx,sy]=0
;                CONTINUE 
;            ENDIF
            IF ycen EQ -1 THEN ycen=sy
            flux_interp_flag=1
        ENDIF
;        IF Abs(sx-xcen)>Abs(sy-ycen) GE box_radius/2. THEN BEGIN
;    ;            n_mask+=Total(source_mask1[sx-1:sx+1,sy-1:sy+1])
;    ;            source_mask1[sx-1:sx+1,sy-1:sy+1]=0
;    ;            CONTINUE
;            xcen=sx
;            ycen=sy
;;            gain_mod=1./beam_width^2. ;divide by the area of the beam for diffuse sources
;        ENDIF ;ELSE gain_mod=1./beam_width^2.
        gain_mod=1.
        sx0=Floor(xcen)
        sy0=Floor(ycen)
        IF source_mask1[sx0,sy0] EQ 0 THEN CONTINUE
        source_box=image_I_flux[sx0-box_radius:sx0+box_radius,sy0-box_radius:sy0+box_radius]
        xcen0=xcen-sx0+box_radius
        ycen0=ycen-sy0+box_radius
        xy2ad,xcen,ycen,astr,ra,dec
        
        IF flux_interp_flag EQ 0 THEN flux_use=Interpolate(source_box,xcen0,ycen0,cubic=-0.5)>image_I_flux[sx,sy] $
            ELSE flux_use=image_I_flux[sx,sy]
        IF flux_use LE 0 THEN BEGIN
            n_mask+=Total(source_mask1[sx-box_radius:sx+box_radius,sy-box_radius:sy+box_radius])
            source_mask1[sx-box_radius:sx+box_radius,sy-box_radius:sy+box_radius]=0
            CONTINUE
        ENDIF
        
        gain_factor_use=gain_array[sx,sy]*gain_mod
        IF Keyword_Set(scale_gain) THEN BEGIN
            ston_single=flux_use/(converge_check*gain_normalization)
            gain_factor_use=(((1.-(1.-gain_factor)^(ston_single/2.-1))<(1.-1./ston_single))*gain_normalization)>gain_factor_use
        ENDIF
        
        comp_arr[si].x=xcen
        comp_arr[si].y=ycen
        comp_arr[si].ra=ra
        comp_arr[si].dec=dec
        comp_arr[si].flux.I=flux_use*gain_factor_use;/beam_area
        comp_arr[si].gain=gain_factor_use
        IF Keyword_Set(independent_fit) AND (N_Elements(image_Q_flux) EQ N_Elements(source_find_image_use)) THEN comp_arr[si].flux.Q=$
            Interpolate(image_Q_flux[sx0-box_radius:sx0+box_radius,sy0-box_radius:sy0+box_radius],xcen0,ycen0,cubic=-0.5)*gain_factor_use
        IF (N_Elements(image_U_flux) EQ N_Elements(source_find_image_use)) THEN comp_arr[si].flux.U=$
            Interpolate(image_U_flux[sx0-box_radius:sx0+box_radius,sy0-box_radius:sy0+box_radius],xcen0,ycen0,cubic=-0.5)*gain_factor_use
        IF Keyword_Set(independent_fit) AND (N_Elements(image_V_flux) EQ N_Elements(source_find_image_use)) THEN comp_arr[si].flux.V=$
            Interpolate(image_V_flux[sx0-box_radius:sx0+box_radius,sy0-box_radius:sy0+box_radius],xcen0,ycen0,cubic=-0.5)*gain_factor_use
        si_use[src_i]=si
        si+=1
    ENDFOR
    n_sources=si
    iter+=1
    IF iter GE max_iter THEN BREAK
ENDWHILE

source_mask=source_mask1
IF n_sources EQ 0 THEN RETURN,source_comp_init(n_sources=0,frequency=frequency,alpha=alpha_use)

source_list=stokes_cnv(comp_arr[0:n_sources-1],jones,beam_arr=beam_arr,/inverse,_Extra=extra)

RETURN,source_list
END
