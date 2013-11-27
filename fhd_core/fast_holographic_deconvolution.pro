
;+
; :Description:
;    Deconvolution algorithm to fit multiple polarizations simultaneously using the Holographic Mapping function
;    
;    Fits multiple components simultaneously
;
; :Params:
;    obs - structure containing details of the observation
;    
;    psf - structure containing gridded beam models for all polarizations/frequencies/baselines at high resolution
;    
;    image_uv_arr - pointer array to the gridded visibility data 
;    
;
; :Author: isullivan May 4, 2012
;-
PRO fast_holographic_deconvolution,fhd,obs,psf,params,cal,image_uv_arr,source_array,comp_arr,timing=timing,weights_arr=weights_arr,$
    residual_array=residual_array,dirty_array=dirty_array,model_uv_full=model_uv_full,model_uv_holo=model_uv_holo,$
    ra_arr=ra_arr,dec_arr=dec_arr,astr=astr,silent=silent,map_fn_arr=map_fn_arr,transfer_mapfn=transfer_mapfn,$
    beam_base=beam_base,beam_correction=beam_correction,file_path_fhd=file_path_fhd,$
    galaxy_model_fit=galaxy_model_fit,scale_gain=scale_gain,model_uv_arr=model_uv_arr,_Extra=extra
;calibration_model_subtract is passed through the fhd structure
compile_opt idl2,strictarrsubs  

t00=Systime(1)    
;vis_path_default,data_directory,filename,file_path,obs=obs
;image_uv_arr is a pointer array with dimensions (n_pol) 

n_pol=fhd.npol
baseline_threshold=fhd.baseline_threshold
gain_factor=fhd.gain_factor
mapfn_interval=fhd.mapfn_interval
max_iter=fhd.max_iter
max_sources=fhd.max_sources
check_iter=fhd.check_iter
mapfn_threshold=fhd.mapfn_threshold
beam_threshold=fhd.beam_threshold
add_threshold=fhd.add_threshold
max_add_sources=fhd.max_add_sources
;local_max_radius=fhd.local_max_radius
pol_use=fhd.pol_use
independent_fit=fhd.independent_fit
reject_pol_sources=fhd.reject_pol_sources
sigma_threshold=2.
calibration_model_subtract=fhd.cal_subtract
filter_background=fhd.filter_background

icomp=Complex(0,1)
beam_max_threshold=fhd.beam_max_threshold
smooth_width=fhd.smooth_width
;color_frequency_correction=fltarr(nfreq)+1. ;remove same component from all frequencies, but allow to be different in the future

dimension=obs.dimension
elements=obs.elements
degpix=obs.degpix
astr=obs.astr
beam_width=(!RaDeg/(obs.MAX_BASELINE/obs.KPIX)/obs.degpix)>1.
local_max_radius=beam_width*2.
box_radius=Ceil(local_max_radius)
xvals=meshgrid(dimension,elements,1)-dimension/2
yvals=meshgrid(dimension,elements,2)-elements/2

;the particular set of beams read will be the ones specified by file_path_fhd.
;that will include all polarizations and frequencies, at ONE time snapshot
IF N_Elements(psf) EQ 0 THEN psf=beam_setup(obs,/restore_last,/silent)
nfreq_beam=(size(psf.base,/dimension))[1]
beam_base=Ptrarr(n_pol,/allocate)
beam_correction=Ptrarr(n_pol,/allocate)
beam_i=Ptrarr(n_pol,/allocate)
beam_mask=Ptrarr(n_pol,/allocate)
source_mask=fltarr(dimension,elements)+1.; & source_mask[valid_radec_i]=1.
gain_use=gain_factor
beam_avg=fltarr(dimension,elements)
alias_mask=fltarr(dimension,elements) 
alias_mask[dimension/4:3.*dimension/4.,elements/4:3.*elements/4.]=1
nbeam_avg=0
FOR pol_i=0,n_pol-1 DO BEGIN ;this should be by frequency! and also by time
    *beam_base[pol_i]=Sqrt(beam_image(psf,obs,pol_i=pol_i,dimension=dimension,/square))
;    *beam_base[pol_i]=beam_image(psf,obs,pol_i=pol_i,dimension=dimension,/square)
    *beam_mask[pol_i]=fltarr(dimension,elements)
    
    beam_mask_test=*beam_base[pol_i]
;    *beam_i[pol_i]=region_grow(beam_mask_test,dimension/2.+dimension*elements/2.,threshold=[beam_threshold,Max(beam_mask_test)])
    *beam_i[pol_i]=where(beam_mask_test GE beam_threshold)
    (*beam_mask[pol_i])[*beam_i[pol_i]]=1.
    IF pol_i LE 1 THEN BEGIN
        nbeam_avg+=1
        source_mask*=*beam_mask[pol_i]
        beam_avg+=(*beam_base[pol_i])^2.
    ENDIF
    
    *beam_correction[pol_i]=weight_invert(*beam_base[pol_i],beam_max_threshold)
ENDFOR
source_mask*=alias_mask
beam_avg/=nbeam_avg
beam_avg=Sqrt(beam_avg>0.)
beam_corr_avg=weight_invert(beam_avg,beam_threshold)
source_taper=beam_avg*(beam_corr_avg<1./sqrt(beam_threshold))

IF N_Elements(map_fn_arr) EQ 0 THEN map_fn_arr=Ptrarr(n_pol,/allocate)
weights_arr=Ptrarr(n_pol,/allocate)
dirty_array=Ptrarr(n_pol,/allocate)
residual_array=Ptrarr(n_pol,/allocate)
model_arr=Ptrarr(n_pol,/allocate)
normalization_arr=fltarr(n_pol) ;factor to normalize holo_mapfn_apply
model_uv_full=Ptrarr(n_pol,/allocate)
model_uv_holo=Ptrarr(n_pol,/allocate)
IF Tag_exist(obs,'alpha') THEN alpha=obs.alpha ELSE alpha=0.

pol_names=['xx','yy','xy','yx','I','Q','U','V'] 

;load holo map functions and initialize output arrays
dirty_image_composite=fltarr(dimension,elements)
dirty_image_composite_Q=fltarr(dimension,elements)
dirty_image_composite_U=fltarr(dimension,elements)
dirty_image_composite_V=fltarr(dimension,elements)
source_uv_mask=fltarr(dimension,elements)
source_uv_mask2=fltarr(dimension,elements)
IF Keyword_Set(transfer_mapfn) THEN BEGIN
    file_path_mapfn=filepath(transfer_mapfn+'_mapfn_',root=file_dirname(file_path_fhd)) 
    print,String(format='("Transferring mapfn from: ",A)',transfer_mapfn)
ENDIF ELSE file_path_mapfn=file_path_fhd+'_mapfn_'

FOR pol_i=0,n_pol-1 DO BEGIN
    IF N_Elements(*map_fn_arr[pol_i]) EQ 0 THEN BEGIN
        restore,file_path_mapfn+pol_names[pol_i]+'.sav' ;map_fn
        *map_fn_arr[pol_i]=Temporary(map_fn)
    ENDIF
ENDFOR

FOR pol_i=0,n_pol-1 DO BEGIN
    weights_single=holo_mapfn_apply(complexarr(dimension,elements)+1,map_fn_arr[pol_i],/no_conj,/indexed,_Extra=extra)
    weights_single_conj=Conj(Shift(Reverse(Reverse(weights_single,1),2),1,1))
    source_uv_mask[where(*image_uv_arr[pol_i])]=1.
    source_uv_mask2[where(weights_single)]=1
    weights_single=(weights_single+weights_single_conj)/2.
    *weights_arr[pol_i]=weights_single
ENDFOR

IF Keyword_Set(galaxy_model_fit) THEN BEGIN
    gal_model_holo=fhd_galaxy_deconvolve(obs,image_uv_arr,map_fn_arr=map_fn_arr,beam_base=beam_base,$
        galaxy_model_uv=galaxy_model_uv,file_path_fhd=file_path_fhd,restore=0,_Extra=extra)
;    gal_model_composite=fltarr(dimension,elements)
;    FOR pol_i=0,n_pol-1 DO gal_model_composite+=(*gal_model_holo[pol_i])*(*beam_correction[pol_i])^2.
ENDIF 

filter_arr=Ptrarr(n_pol)
FOR pol_i=0,n_pol-1 DO BEGIN    
    filter_single=1
    dirty_image_single=dirty_image_generate(*image_uv_arr[pol_i],degpix=degpix,obs=obs,psf=psf,params=params,$
        weights=*weights_arr[pol_i],image_filter='filter_uv_uniform2',filter=filter_single)*(*beam_correction[pol_i])^2.
;    dirty_image_single=dirty_image_generate(*image_uv_arr[pol_i],degpix=degpix,obs=obs,psf=psf,params=params,$
;        weights=*weights_arr[pol_i])*(*beam_correction[pol_i])^2.
    IF Ptr_valid(filter_single) THEN filter_arr[pol_i]=filter_single
    IF Keyword_Set(galaxy_model_fit) THEN dirty_image_single-=*gal_model_holo[pol_i]*(*beam_correction[pol_i])^2.
    *dirty_array[pol_i]=dirty_image_single*(*beam_base[pol_i])
    
    ;xx, yy and xy, yx polarizations are treated seperately
    IF pol_i LE 1 THEN dirty_image_composite+=dirty_image_single
    IF pol_i GE 2 THEN dirty_image_composite_U+=dirty_image_single
    CASE pol_i OF 
        0:dirty_image_composite_Q+=dirty_image_single
        1:dirty_image_composite_Q-=dirty_image_single 
        2:dirty_image_composite_V+=dirty_image_single
        3:dirty_image_composite_V-=dirty_image_single
    ENDCASE
    *model_uv_full[pol_i]=complexarr(dimension,elements)
    *model_uv_holo[pol_i]=complexarr(dimension,elements)
ENDFOR

FOR pol_i=0,n_pol-1 DO BEGIN    
    normalization_arr[pol_i]=1./(dirty_image_generate(*weights_arr[pol_i],degpix=degpix,obs=obs,psf=psf,params=params,$
        weights=*weights_arr[pol_i],image_filter='filter_uv_uniform2',filter=filter_arr[pol_i]))[dimension/2.,elements/2.]
;    normalization_arr[pol_i]=1./(dirty_image_generate(weights_single,degpix=degpix,obs=obs,psf=psf,params=params,$
;        weights=*weights_arr[pol_i]))[dimension/2.,elements/2.]
    normalization_arr[pol_i]*=((*beam_base[pol_i])[obs.obsx,obs.obsy])^2.
ENDFOR
gain_normalization=mean(normalization_arr[0:n_pol-1]);/2. ;factor of two accounts for complex conjugate
;pix_area_cnv=pixel_area(astr,dimension=dimension);/degpix^2.
;gain_normalization=(!RaDeg/(obs.MAX_BASELINE/obs.KPIX)/obs.degpix);^2.
gain_use*=gain_normalization
gain_array=source_taper*gain_use
uv_i_use=where(source_uv_mask,n_uv_use)
uv_use_frac=Float(n_uv_use)/(dimension*elements)
print,"Fractional uv coverage: ",uv_use_frac

uv_i_use2=where(source_uv_mask2,n_uv_use2)
xvals2=xvals[uv_i_use2]
yvals2=yvals[uv_i_use2]

t1=0 ;generation of model_images and image_use for source detection
t2=0 ;filtering and source extraction
t3=0 ;DFT
t4=0 ;Holographic mapping function
i2=0. 
t0=Systime(1)

converge_check=Fltarr(Ceil(float(max_iter)/float(check_iter>1))>2+1)
converge_check2=Fltarr(max_iter>2+1)

sm_xmin=(Min(xvals[where(source_mask)])+dimension/2.-smooth_width)>0
sm_xmax=(Max(xvals[where(source_mask)])+dimension/2.+smooth_width)<(dimension-1)
sm_ymin=(Min(yvals[where(source_mask)])+elements/2.-smooth_width)>0
sm_ymax=(Max(yvals[where(source_mask)])+elements/2.+smooth_width)<(elements-1)
beam_avg_box=beam_avg[sm_xmin:sm_xmax,sm_ymin:sm_ymax]
beam_corr_box=beam_corr_avg[sm_xmin:sm_xmax,sm_ymin:sm_ymax]

source_box_xvals=meshgrid(2.*local_max_radius+1,2.*local_max_radius+1,1)
source_box_yvals=meshgrid(2.*local_max_radius+1,2.*local_max_radius+1,2)
source_fit_fn=Exp(-((source_box_xvals-local_max_radius)^2.+(source_box_yvals-local_max_radius)^2.)/(2.*local_max_radius))

image_filtered=dirty_image_composite
IF Keyword_Set(filter_background) THEN BEGIN
    image_smooth=Median(image_filtered[sm_xmin:sm_xmax,sm_ymin:sm_ymax]*beam_avg_box,smooth_width,/even)*beam_corr_box
    image_filtered[sm_xmin:sm_xmax,sm_ymin:sm_ymax]-=image_smooth
ENDIF
source_find_image=image_filtered*beam_avg*source_mask*source_taper
converge_check[0]=Stddev(source_find_image[where(source_mask)],/nan)
converge_check2[0]=Stddev(source_find_image[where(source_mask)],/nan)
print,"Gain factor used:",Strn(fhd.gain_factor)
print,"Initial convergence:",Strn(converge_check[0])

si=0L
i0=0L
IF Keyword_Set(calibration_model_subtract) THEN BEGIN
    print,String(format='("Calibration source model subtracted (",A3,"%)")',Strn(calibration_model_subtract*100.,length=3))
    n_cal_src=cal.n_cal_src
    si+=n_cal_src
    i2+=1
    max_sources+=n_cal_src
    
    comp_arr=source_comp_init(n_sources=max_sources,alpha=alpha,freq=obs.freq_center)
    cal_sources=cal.source_list
    IF calibration_model_subtract LT 1 THEN BEGIN
        FOR tag_i=0,n_tags(cal_sources.flux)-1 DO cal_sources.flux.(tag_i)*=(0.>calibration_model_subtract<1.)
    ENDIF
    IF n_cal_src GT 0 THEN comp_arr[0:n_cal_src-1]=cal_sources ;if this breaks, use a FOR loop
    
    FOR pol_i=0,n_pol-1 DO *model_uv_full[pol_i]+=*model_uv_arr[pol_i]*(0.>calibration_model_subtract<1.) ;this allows you to subtract less than 100% of the model!
    FOR pol_i=0,n_pol-1 DO BEGIN
        *model_uv_holo[pol_i]=holo_mapfn_apply(*model_uv_full[pol_i],map_fn_arr[pol_i],_Extra=extra,/indexed)
    ENDFOR
    
    model_image_composite=fltarr(dimension,elements)
    FOR pol_i=0,(n_pol<2)-1 DO BEGIN 
        model_image_holo=dirty_image_generate(*model_uv_holo[pol_i],degpix=degpix,filter=filter_arr[pol_i])
        model_image=(model_image_holo)*(*beam_correction[pol_i])^2.
        model_image_composite+=model_image
    ENDFOR
    
    image_filtered=dirty_image_composite-model_image_composite
    IF Keyword_Set(filter_background) THEN BEGIN
        image_smooth=Median(image_filtered[sm_xmin:sm_xmax,sm_ymin:sm_ymax]*beam_avg_box,smooth_width,/even)*beam_corr_box
        image_filtered[sm_xmin:sm_xmax,sm_ymin:sm_ymax]-=image_smooth
    ENDIF
    source_find_image=image_filtered*beam_avg*source_mask
    converge_check[i2]=Stddev(source_find_image[where(source_mask)],/nan)
    converge_check2[i2]=Stddev(source_find_image[where(source_mask)],/nan)
    print,"Convergence after subtracting input source model:",Strn(converge_check[i2])
ENDIF ELSE comp_arr=source_comp_init(n_sources=max_sources,alpha=alpha,freq=obs.freq_center)

IF ~Keyword_Set(silent) THEN print,'Iteration # : Component # : Elapsed time : Convergence'

recalc_flag=1
t_init=Systime(1)-t00
FOR i=i0,max_iter-1 DO BEGIN 
    IF Keyword_Set(recalc_flag) THEN BEGIN
        t1_0=Systime(1)
        model_image_composite=fltarr(dimension,elements)
        model_image_composite_Q=fltarr(dimension,elements)
        model_image_composite_U=fltarr(dimension,elements)
        model_image_composite_V=fltarr(dimension,elements)
        FOR pol_i=0,n_pol-1 DO BEGIN 
            model_image_holo=dirty_image_generate(*model_uv_holo[pol_i],degpix=degpix,filter=filter_arr[pol_i])
            model_image=(model_image_holo)*(*beam_correction[pol_i])^2.
            
            *model_arr[pol_i]=model_image
            IF pol_i LE 1 THEN model_image_composite+=model_image $
                ELSE model_image_composite_U+=model_image
            IF Keyword_Set(independent_fit) OR Keyword_Set(reject_pol_sources) THEN BEGIN   
                CASE pol_i OF
                    0:model_image_composite_Q+=model_image
                    1:model_image_composite_Q-=model_image
                    2:model_image_composite_V+=model_image
                    3:model_image_composite_V-=model_image
                ENDCASE
            ENDIF
        ENDFOR
        
        t2_0=Systime(1)
        t1+=t2_0-t1_0 
        
        image_unfiltered=dirty_image_composite-model_image_composite
        image_filtered=image_unfiltered
        IF Keyword_Set(filter_background) THEN BEGIN
            image_smooth=Median(image_unfiltered[sm_xmin:sm_xmax,sm_ymin:sm_ymax]*beam_avg_box,smooth_width,/even)*beam_corr_box
            image_filtered=fltarr(dimension,elements)
            image_filtered[sm_xmin:sm_xmax,sm_ymin:sm_ymax]=image_unfiltered[sm_xmin:sm_xmax,sm_ymin:sm_ymax]-image_smooth
        ENDIF
        
        IF Keyword_Set(independent_fit) THEN BEGIN
            image_use_Q=dirty_image_composite_Q-model_image_composite_Q
            image_use_U=dirty_image_composite_U-model_image_composite_U
            image_use_V=dirty_image_composite_V-model_image_composite_V
;            image_smooth_Q=Median(image_use_Q[sm_xmin:sm_xmax,sm_ymin:sm_ymax]*beam_avg_box,smooth_width,/even)*beam_corr_box
;            image_use_Q[sm_xmin:sm_xmax,sm_ymin:sm_ymax]-=image_smooth_Q
;            image_smooth_U=Median(image_use_U[sm_xmin:sm_xmax,sm_ymin:sm_ymax]*beam_avg_box,smooth_width,/even)*beam_corr_box
;            image_use_U[sm_xmin:sm_xmax,sm_ymin:sm_ymax]-=image_smooth_U
;            image_smooth_V=Median(image_use_V[sm_xmin:sm_xmax,sm_ymin:sm_ymax]*beam_avg_box,smooth_width,/even)*beam_corr_box
;            image_use_V[sm_xmin:sm_xmax,sm_ymin:sm_ymax]-=image_smooth_V            
        ENDIF ELSE IF n_pol GT 2 THEN BEGIN
            image_use_U=dirty_image_composite_U-model_image_composite_U
;            image_smooth_U=Median(image_use_U[sm_xmin:sm_xmax,sm_ymin:sm_ymax]*beam_avg_box,smooth_width,/even)*beam_corr_box
;            image_use_U[sm_xmin:sm_xmax,sm_ymin:sm_ymax]-=image_smooth_U
        ENDIF  
    ENDIF ELSE t2_0=Systime(1)
    source_find_image=image_filtered*beam_avg*source_mask*source_taper
    image_use=image_filtered*beam_avg*source_mask
   
    IF i EQ 0 THEN converge_check[i]=Stddev(image_use[where(source_mask)],/nan)
    converge_check2[i]=Stddev(image_use[where(source_mask)],/nan)
    ;use the composite image to locate sources, but then fit for flux independently
    source_flux=Max(source_find_image,source_i)
    
;    Find additional sources:
;       require that they be isolated ; This is local_max_radius
;       should put some cap on the absolute number of them ; This is max_add_sources
;       all within some range of the brightest pixels flux, say 95%; This is add_threshold

    flux_ref=source_find_image[source_i]*add_threshold
    additional_i1=where(source_find_image GE flux_ref,n_sources1)
    additional_i2=where((source_find_image GE 5.*converge_check2[i]) AND (source_find_image GE source_find_image[source_i]/10.),n_sources2)
    additional_i=(n_sources1 GT n_sources2) ? additional_i1:additional_i2 
    n_sources=n_sources1>n_sources2
    additional_i=additional_i[reverse(Sort(source_find_image[additional_i]))] ;order from brightest to faintest
    add_x=additional_i mod dimension
    add_y=Floor(additional_i/dimension)
    add_dist=fltarr(n_sources)-1
    FOR addi=1,n_sources-1 DO add_dist[addi]=(local_max_radius-Min(abs(add_x[addi]-add_x[0:addi-1])))<(local_max_radius-Min(abs(add_y[addi]-add_y[0:addi-1])))
    additional_i_usei=where(add_dist LT 0,n_sources)
    additional_i=additional_i[additional_i_usei] ;guaranteed at least one, so this is safe
    
    IF (n_sources<max_add_sources)+si GT max_sources THEN max_add_sources=max_sources-si
    IF max_add_sources EQ 0 THEN BREAK
    IF n_sources GT max_add_sources THEN BEGIN
        additional_i=additional_i[0:max_add_sources-1]
        n_sources=max_add_sources
    ENDIF
    n_mask=0
    
    ;fit flux here, and fill comp_arr for each pol
    flux_arr=fltarr(4)
    fit_threshold=-2.*converge_check2[i]
    source_fit_fn_ref=Total(source_fit_fn)/2.
    
    si_use=Lonarr(n_sources)-1
    sx_arr=additional_i mod dimension
    sy_arr=Floor(additional_i/dimension)
    FOR src_i=0L,n_sources-1 DO BEGIN
        sx=sx_arr[src_i]
        sy=sy_arr[src_i]
        gcntrd,image_use,sx,sy,xcen,ycen,beam_width,/keepcenter,/silent
;        gcntrd,image_use,sx,sy,xcen,ycen,beam_width,/silent
        source_box=image_use[sx-box_radius:sx+box_radius,sy-box_radius:sy+box_radius];*source_fit_fn
;        box_i=where(source_box GT fit_threshold,n_fit)
;        IF n_fit EQ 0 THEN BEGIN
;            n_mask+=Total(source_mask[sx-1:sx+1,sy-1:sy+1])
;            source_mask[sx-1:sx+1,sy-1:sy+1]=0
;            CONTINUE
;        ENDIF
;        IF Total(source_fit_fn[box_i]) LT source_fit_fn_ref THEN BEGIN
;            n_mask+=Total(source_mask[sx-1:sx+1,sy-1:sy+1])
;            source_mask[sx-1:sx+1,sy-1:sy+1]=0
;            CONTINUE
;        ENDIF
;        
;        source_box=source_box>0
;;        source_box-=Min(source_box)
;;        xcen0=Total(source_box[box_i]*source_box_xvals[box_i])/Total(source_box[box_i])
;;        ycen0=Total(source_box[box_i]*source_box_yvals[box_i])/Total(source_box[box_i])
;        xcen0=Total(source_box*source_box_xvals)/Total(source_box)
;        ycen0=Total(source_box*source_box_yvals)/Total(source_box)
;        xcen=sx-box_radius+xcen0
;        ycen=sy-box_radius+ycen0
        IF Abs(sx-xcen)>Abs(sy-ycen) GE box_radius/2. THEN BEGIN
            n_mask+=Total(source_mask[sx-1:sx+1,sy-1:sy+1])
            source_mask[sx-1:sx+1,sy-1:sy+1]=0
            CONTINUE
        ENDIF
        xcen0=xcen-sx+box_radius
        ycen0=ycen-sy+box_radius
        xy2ad,xcen,ycen,astr,ra,dec
        
        beam_corr_src=fltarr(n_pol)
        beam_src=fltarr(n_pol)
        beam_corr_avg_src=beam_corr_avg[additional_i[src_i]]
        FOR pol_i=0,n_pol-1 DO BEGIN   
;            beam_corr_src[pol_i]=(*beam_correction[pol_i])[additional_i[src_i]]
            beam_src[pol_i]=(*beam_base[pol_i])[additional_i[src_i]]
            
            IF Keyword_Set(independent_fit) THEN BEGIN
                sign=(pol_i mod 2) ? -1:1
                IF pol_i EQ 0 THEN sbQ=image_use_Q[sx-box_radius:sx+box_radius,sy-box_radius:sy+box_radius]*source_fit_fn
                IF pol_i EQ 2 THEN BEGIN
                    sbU=image_use_U[sx-box_radius:sx+box_radius,sy-box_radius:sy+box_radius]*source_fit_fn
                    sbV=image_use_V[sx-box_radius:sx+box_radius,sy-box_radius:sy+box_radius]*source_fit_fn
                ENDIF
                IF pol_i LE 1 THEN flux_use=Interpolate(source_box,xcen0,ycen0,cubic=-0.5)+sign*Interpolate(sbQ,xcen0,ycen0,cubic=-0.5)
                IF pol_i GE 2 THEN flux_use=Interpolate(sbU,xcen0,ycen0,cubic=-0.5)+sign*Interpolate(sbV,xcen0,ycen0,cubic=-0.5)
            ENDIF ELSE IF pol_i LE 1 THEN flux_use=Interpolate(source_box,xcen0,ycen0,cubic=-0.5) $
                ELSE flux_use=Interpolate(image_use_U[sx-box_radius:sx+box_radius,sy-box_radius:sy+box_radius],xcen0,ycen0,cubic=-0.5)
            
            flux_arr[pol_i]=flux_use*beam_corr_avg_src/2. ;"True sky" instrumental pol
        ENDFOR
        
        IF (flux_arr[0]+flux_arr[1]) LE 0 THEN BEGIN
            n_mask+=Total(source_mask[sx-1:sx+1,sy-1:sy+1])
            source_mask[sx-1:sx+1,sy-1:sy+1]=0
            CONTINUE
        ENDIF
        
        gain_factor_use=gain_array[sx,sy]
        IF Keyword_Set(scale_gain) THEN BEGIN
            ston_single=(flux_arr[0]+flux_arr[1])/(converge_check2[i]*gain_normalization)
            gain_factor_use=(((1.-(1.-gain_factor)^(ston_single/2.-1))<(1.-1./ston_single))*gain_normalization)>gain_factor_use
        ENDIF
        flux_arr*=gain_factor_use
        
        ;Apparent brightness, instrumental polarization X gain (a scalar)
        FOR pol_i=0,n_pol-1 DO comp_arr[si].flux.(pol_i)=flux_arr[pol_i]*beam_src[pol_i]
        comp_arr[si].x=xcen
        comp_arr[si].y=ycen
        comp_arr[si].ra=ra
        comp_arr[si].dec=dec
        comp_arr[si].flux.I=flux_arr[0]+flux_arr[1]
        comp_arr[si].flux.Q=flux_arr[0]-flux_arr[1]
        comp_arr[si].flux.U=flux_arr[2]+flux_arr[3]
        comp_arr[si].flux.V=flux_arr[2]-flux_arr[3]
        si_use[src_i]=si
        si+=1
    ENDFOR
    
    si_use_i=where(si_use GE 0,n_si_use)
    IF n_si_use EQ 0 THEN BEGIN
        ;do something to end loop if n_mask EQ 0
        
        recalc_flag=0
        CONTINUE
    ENDIF ELSE recalc_flag=1
    
    si_use=si_use[si_use_i]
            ;Make sure to update source uv model in "true sky" instrumental polarization i.e. 1/beam^2 frame.
    t3_0=Systime(1)
    t2+=t3_0-t2_0
    x_vec=comp_arr[si_use].x
    y_vec=comp_arr[si_use].y
;    area_cnv=pix_area_cnv[x_vec,y_vec]
    flux_I=comp_arr[si_use].flux.I;*area_cnv
    flux_Q=comp_arr[si_use].flux.Q;*area_cnv
    flux_U=comp_arr[si_use].flux.U;*area_cnv
    flux_V=comp_arr[si_use].flux.V;*area_cnv
    model_uv_stks=Ptrarr(4,/allocate)
    *model_uv_stks[0]=source_dft(x_vec,y_vec,xvals2,yvals2,dimension=dimension,elements=elements,degpix=degpix,flux=flux_I,/conserve_memory)
    IF Total(flux_Q) EQ 0 THEN *model_uv_stks[1]=0. $
        ELSE *model_uv_stks[1]=source_dft(x_vec,y_vec,xvals2,yvals2,dimension=dimension,elements=elements,degpix=degpix,flux=flux_Q,/conserve_memory) 
    IF Total(flux_U) EQ 0 THEN *model_uv_stks[2]=0. $
        ELSE *model_uv_stks[2]=source_dft(x_vec,y_vec,xvals2,yvals2,dimension=dimension,elements=elements,degpix=degpix,flux=flux_U,/conserve_memory)
    IF Total(flux_V) EQ 0 THEN *model_uv_stks[3]=0. $
        ELSE *model_uv_stks[3]=source_dft(x_vec,y_vec,xvals2,yvals2,dimension=dimension,elements=elements,degpix=degpix,flux=flux_V,/conserve_memory)
    SWITCH n_pol OF
        4:(*model_uv_full[3])[uv_i_use2]+=(*model_uv_stks[2]-*model_uv_stks[3])/2.
        3:(*model_uv_full[2])[uv_i_use2]+=(*model_uv_stks[2]+*model_uv_stks[3])/2.
        2:(*model_uv_full[1])[uv_i_use2]+=(*model_uv_stks[0]-*model_uv_stks[1])/2.
        1:(*model_uv_full[0])[uv_i_use2]+=(*model_uv_stks[0]+*model_uv_stks[1])/2.
    ENDSWITCH
    Ptr_free,model_uv_stks
    
    t4_0=Systime(1)
    t3+=t4_0-t3_0
    FOR pol_i=0,n_pol-1 DO BEGIN
        *model_uv_holo[pol_i]=holo_mapfn_apply(*model_uv_full[pol_i],map_fn_arr[pol_i],_Extra=extra,/indexed)
    ENDFOR
    t4+=Systime(1)-t4_0    
    
    IF si GE max_sources THEN BEGIN
        i2+=1                                        
        t10=Systime(1)-t0
        print,StrCompress(String(format='("Max sources found by iteration ",I," after ",I," seconds with ",I," sources (convergence:",F,")")',$
            i,t10,si,Stddev(image_use[where(source_mask)],/nan)))
        converge_check[i2]=Stddev(image_use[where(source_mask)],/nan)
        BREAK
    ENDIF
    
    IF (Round(i mod check_iter) EQ 0) AND (i GT 0) THEN BEGIN
        i2+=1
        t10=Systime(1)-t0
        IF ~Keyword_Set(silent) THEN print,StrCompress(String(format='(I," : ",I," : ",I," : ",F)',$
            i,si,t10,Stddev(image_use[where(source_mask)],/nan)))
        converge_check[i2]=Stddev(image_use[where(source_mask)],/nan)
        IF sigma_threshold*converge_check[i2] GT image_use[source_i] THEN BEGIN
            print,StrCompress(String(format='("Break after iteration ",I," from low signal to noise after ",I," seconds with ",I," sources (convergence:",F,")")',$
                i,t10,si,Stddev(image_use[where(source_mask)],/nan)))
            converge_check2=converge_check2[0:i]
            converge_check=converge_check[0:i2]
            BREAK
        ENDIF
        IF converge_check[i2] GE converge_check[i2-1] THEN BEGIN
            print,StrCompress(String(format='("Break after iteration ",I," from lack of convergence after ",I," seconds with ",I," sources (convergence:",F,")")',$
                i,t10,si,Stddev(image_use[where(source_mask)],/nan)))
            converge_check2=converge_check2[0:i]
            converge_check=converge_check[0:i2]
            BREAK
        ENDIF
    ENDIF
ENDFOR
IF i EQ max_iter THEN BEGIN
    t10=Systime(1)-t0
    print,StrCompress(String(format='("Max iteration ",I," reached after ",I," seconds with ",I," sources (convergence:",F,")")',$
        i,t10,si,Stddev(image_use[where(source_mask)],/nan)))
ENDIF

;condense clean components
noise_map=Stddev(image_use[where(source_mask)],/nan)*beam_corr_avg
noise_map*=gain_normalization
IF Keyword_Set(independent_fit) THEN noise_map*=Sqrt(2.)
comp_arr=comp_arr[0:si-1]
source_array=Components2Sources(comp_arr,radius=(local_max_radius/2.)>0.5,noise_map=noise_map,reject_sigma_threshold=sigma_threshold)
t3_0=Systime(1)
model_uv_full=source_dft_model(obs,source_array,t_model=t_model,uv_mask=source_uv_mask2,/conserve_memory)
t4_0=Systime(1)
t3+=t4_0-t3_0
FOR pol_i=0,n_pol-1 DO BEGIN
    *model_uv_holo[pol_i]=holo_mapfn_apply(*model_uv_full[pol_i],map_fn_arr[pol_i],_Extra=extra,/indexed)
ENDFOR
t1_0=Systime(1)
t4+=t1_0-t4_0    
FOR pol_i=0,n_pol-1 DO BEGIN
    *residual_array[pol_i]=dirty_image_generate(*image_uv_arr[pol_i]-*model_uv_holo[pol_i],degpix=degpix)*(*beam_correction[pol_i])
ENDFOR  
t1+=Systime(1)-t1_0

t00=Systime(1)-t00
print,'Deconvolution timing [per iteration]'
print,String(format='("Setup:",A," ")',Strn(Round(t_init)))
print,String(format='("FFT:",A,"[",A,"]")',Strn(Round(t1)),Strn(Round(t1*100/i)/100.))
print,String(format='("Filtering:",A,"[",A,"]")',Strn(Round(t2)),Strn(Round(t2*100/i)/100.))
print,String(format='("DFT source modeling:",A,"[",A,", or ",A," per 100 sources]")',Strn(Round(t3)),Strn(Round(t3*100/i)/100.),Strn(Round(t3*10000./si)/100.))
print,String(format='("Applying HMF:",A,"[",A,"]")',Strn(Round(t4)),Strn(Round(t4*100/i)/100.))
timing=[t00,t1,t2,t3,t4]
;print,timing

END  
