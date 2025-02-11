FUNCTION fhd_source_detect,obs,fhd_params,jones,source_find_image,image_I_flux=image_I_flux,image_Q_flux=image_Q_flux,$
    image_U_flux=image_U_flux,image_V_flux=image_V_flux,beam_arr=beam_arr,beam_corr_avg=beam_corr_avg,$
    beam_mask=beam_mask,source_mask=source_mask,gain_factor=gain_factor,n_sources=n_sources,detection_threshold=detection_threshold,_Extra=extra

add_threshold=fhd_params.add_threshold
independent_fit=fhd_params.independent_fit
reject_pol_sources=fhd_params.reject_pol_sources
sigma_threshold=2.
frequency=obs.freq_center
alpha_use=obs.alpha ;spectral index used for the subtracted component
over_resolution=fhd_params.over_resolution

n_pol=fhd_params.npol
dimension=obs.dimension
elements=obs.elements
degpix=obs.degpix
astr=obs.astr
beam_width=beam_width_calculate(obs,/fwhm)
beam_area=beam_width_calculate(obs,/area)
local_max_radius=beam_width>1.;*2.
box_radius=Ceil(local_max_radius*2.)
xvals=meshgrid(dimension,elements,1)
yvals=meshgrid(dimension,elements,2)

IF N_Elements(beam_mask) EQ 0 THEN beam_mask=Fltarr(dimension,elements)+1.
IF N_Elements(image_I_flux) EQ 0 THEN image_I_flux=source_find_image
converge_check=Stddev(source_find_image[where(beam_mask)],/nan)

IF N_Elements(source_mask) EQ 0 THEN source_mask0=beam_mask ELSE source_mask0=source_mask
neg_i=where(source_find_image LE -Max(source_find_image),n_neg)

source_mask1=beam_mask*source_mask0
flux_offset=Mean(source_find_image[where(source_mask0)])
source_find_image-=flux_offset
    
;    Find additional sources:
;       require that they be isolated ; This is local_max_radius
;       all within some range of the brightest pixels flux, say 80%; This is add_threshold

circle_i=where(Sqrt((xvals-dimension/2)^2.+(yvals-elements/2)^2.) LE local_max_radius*sqrt(2.))
circle_i-=Long(dimension*(1.+elements)/2)
n_sources=0
max_iter=5
iter=0
WHILE n_sources EQ 0 DO BEGIN
    source_find_image_use=source_find_image*source_mask1
    source_flux=Max(source_find_image_use,source_i)
    flux_ref1=source_find_image_use[source_i]*add_threshold
    flux_ref2=(source_find_image_use[source_i]*0.5)>(100.*converge_check)
    
    additional_i1=where(source_find_image_use GE flux_ref1,n_sources1)
    additional_i2=where(source_find_image_use GE flux_ref2,n_sources2)
    IF n_sources1 GT n_sources2 THEN BEGIN
        additional_i=additional_i1
        detection_threshold=flux_ref1
        n_sources=n_sources1
    ENDIF ELSE BEGIN
        additional_i=additional_i2
        detection_threshold=flux_ref2
        n_sources=n_sources2
    ENDELSE
    
    additional_i=additional_i[reverse(Sort(source_find_image_use[additional_i]))] ;order from brightest to faintest
    add_x=additional_i mod Long(dimension)
    add_y=Long(Floor(additional_i/dimension))
    add_dist=fltarr(n_sources)+dimension
    FOR addi=1L,n_sources-1 DO add_dist[addi]=Min(Sqrt(Float(add_x[addi]-add_x[0:addi-1])^2+Float(add_y[addi]-add_y[0:addi-1])^2.))
        
    source_map=lonarr(dimension,elements)
    source_map[add_x,add_y]=lindgen(n_sources)+1
    background_dist=morph_distance(source_map,neighbor=3)
    extended_pix=where(background_dist GT local_max_radius,n_extend)
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
    
    n_mask=0
    comp_arr=source_comp_init(n_sources=n_sources,frequency=frequency,alpha=alpha_use)
    
    ;fit flux here, and fill comp_arr for each pol
    flux_arr=fltarr(4)
    fit_threshold=-2.*converge_check
    
    si_use=Lonarr(n_sources)-1
    sx_arr=additional_i mod Long(dimension)
    sy_arr=Float(Floor(additional_i/dimension))
    
    si=0L
    FOR src_i=0L,n_sources-1 DO BEGIN
        sx=sx_arr[src_i]
        sy=sy_arr[src_i]
        IF add_dist[src_i] GE local_max_radius THEN BEGIN
            ;FWHM here is used to set the size of the fitting box. Will not work right if the box is too small, even if that's the correct beam size
            gcntrd,(image_I_flux),sx,sy,xcen,ycen,beam_width>2.,/keepcenter,/silent   
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
            IF xcen EQ -1 THEN xcen=sx
            flux_interp_flag=1
        ENDIF
        IF Abs(sy-ycen) GT beam_width THEN BEGIN
            IF ycen EQ -1 THEN ycen=sy
            flux_interp_flag=1
        ENDIF
        gain_mod=1.
        sx0=Long(Floor(xcen))
        sy0=Long(Floor(ycen))
        IF source_mask1[sx0,sy0] EQ 0 THEN BEGIN
            n_mask+=source_mask1[sx,sy]
            source_mask1[sx,sy]=0
            source_mask[sx,sy]=0
            CONTINUE
        ENDIF
        source_box=image_I_flux[sx0-box_radius:sx0+box_radius,sy0-box_radius:sy0+box_radius]
        xcen0=xcen-sx0+box_radius
        ycen0=ycen-sy0+box_radius
        apply_astrometry, obs, x_arr=xcen, y_arr=ycen, ra_arr=ra, dec_arr=dec, /xy2ad, /refraction
        flux_min=Min(source_box[box_radius-local_max_radius:box_radius+local_max_radius,box_radius-local_max_radius:box_radius+local_max_radius])<0
        
        IF flux_interp_flag EQ 0 THEN flux_use=Interpolate(source_box,xcen0,ycen0,cubic=-0.5)>image_I_flux[sx,sy] $
            ELSE flux_use=image_I_flux[sx,sy]
        IF flux_use LE -flux_min THEN BEGIN
            mask_i=sx0+sy0*Long(dimension)+circle_i
            n_mask+=Total(source_mask[mask_i])
            source_mask[mask_i]=0
            source_mask1[mask_i]=0
            CONTINUE
        ENDIF
        
        gain_factor_use=gain_factor*gain_mod
        IF Keyword_Set(scale_gain) THEN BEGIN
            ston_single=flux_use/(converge_check*gain_normalization)
            gain_factor_use=(((1.-(1.-gain_factor)^(ston_single/2.-1))<(1.-1./ston_single))*gain_normalization)>gain_factor_use
        ENDIF
        
        comp_arr[si].x=xcen/over_resolution
        comp_arr[si].y=ycen/over_resolution
        comp_arr[si].ra=ra
        comp_arr[si].dec=dec
        comp_arr[si].flux.I=flux_use*gain_factor_use
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

IF n_sources EQ 0 THEN RETURN,source_comp_init(n_sources=0,frequency=frequency,alpha=alpha_use)

source_list=stokes_cnv(comp_arr[0:n_sources-1],jones,beam_arr=beam_arr,/inverse,_Extra=extra)

RETURN,source_list
END
