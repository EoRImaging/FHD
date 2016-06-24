
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
PRO fast_holographic_deconvolution,fhd_params,obs,psf,params,cal,jones,image_uv_arr,source_array,component_array,timing=timing,weights_arr=weights_arr,$
    residual_array=residual_array,dirty_array=dirty_array,model_uv_full=model_uv_full,model_uv_holo=model_uv_holo,$
    ra_arr=ra_arr,dec_arr=dec_arr,astr=astr,silent=silent,map_fn_arr=map_fn_arr,transfer_mapfn=transfer_mapfn,$
    beam_base=beam_base,beam_correction=beam_correction,file_path_fhd=file_path_fhd,no_condense_sources=no_condense_sources,$
    source_mask=source_mask,scale_gain=scale_gain,model_uv_arr=model_uv_arr,use_pointing_center=use_pointing_center,_Extra=extra
;calibration_model_subtract is passed through the fhd_params structure
compile_opt idl2,strictarrsubs  

t00=Systime(1)    
;vis_path_default,data_directory,filename,file_path,obs=obs
;image_uv_arr is a pointer array with dimensions (n_pol) 
;most parameters are set in fhd_init()
n_pol=fhd_params.npol
gain_factor=fhd_params.gain_factor
max_iter=fhd_params.max_iter
max_sources=fhd_params.max_sources
check_iter=fhd_params.check_iter
beam_threshold=fhd_params.beam_threshold
add_threshold=fhd_params.add_threshold
dft_threshold=fhd_params.dft_threshold
over_resolution=fhd_params.over_resolution
return_kernel=1
max_add_sources=fhd_params.max_add_sources
;local_max_radius=fhd_params.local_max_radius
pol_use=fhd_params.pol_use
independent_fit=fhd_params.independent_fit
reject_pol_sources=fhd_params.reject_pol_sources
sigma_threshold=fhd_params.sigma_cut
filter_background=fhd_params.filter_background
decon_filter=fhd_params.decon_filter
galaxy_model_fit=fhd_params.galaxy_subtract
subtract_sidelobe_catalog=fhd_params.sidelobe_subtract
return_sidelobe_catalog=fhd_params.sidelobe_return

component_array=source_comp_init(n_sources=max_sources,alpha=obs.alpha,freq=obs.freq_center,gain_factor=gain_factor)

IF over_resolution GT 1 THEN obs_fit=fhd_struct_update_obs(obs,dimension=obs.dimension*over_resolution,kbin=obs.kpix) $
    ELSE obs_fit=obs

icomp=Complex(0,1)
beam_max_threshold=fhd_params.beam_max_threshold
smooth_width=fhd_params.smooth_width
;color_frequency_correction=fltarr(nfreq)+1. ;remove same component from all frequencies, but allow to be different in the future

dimension=obs.dimension
elements=obs.elements
dimension_fit=obs_fit.dimension
elements_fit=obs_fit.elements
degpix=obs.degpix
astr=obs.astr
beam_width=beam_width_calculate(obs_fit,min_restored_beam_width=1.,/FWHM)
;beam_width=(!RaDeg/(obs.MAX_BASELINE/obs.KPIX)/obs.degpix)>1.
local_max_radius=beam_width*2.
box_radius=Ceil(local_max_radius)
xvals=meshgrid(dimension,elements,1)-dimension/2
yvals=meshgrid(dimension,elements,2)-elements/2
xvals_fit=meshgrid(dimension_fit,elements_fit,1)-dimension_fit/2
yvals_fit=meshgrid(dimension_fit,elements_fit,2)-elements_fit/2

;the particular set of beams read will be the ones specified by file_path_fhd.
;that will include all polarizations and frequencies, at ONE time snapshot
;IF N_Elements(psf) EQ 0 THEN psf=beam_setup(obs,/restore_last,/silent)
nfreq_beam=psf.n_freq
beam_base=Ptrarr(n_pol,/allocate)
beam_base_fit=Ptrarr(n_pol,/allocate)
beam_correction=Ptrarr(n_pol,/allocate)
beam_correction_fit=Ptrarr(n_pol,/allocate)
beam_i=Ptrarr(n_pol,/allocate)
beam_mask=fltarr(dimension_fit,elements_fit)+1.; 
source_mask=fltarr(dimension_fit,elements_fit)+1.
beam_avg=fltarr(dimension_fit,elements_fit)
alias_mask=fltarr(dimension_fit,elements_fit) 
alias_mask[dimension_fit/4:3.*dimension_fit/4.,elements_fit/4:3.*elements_fit/4.]=1
nbeam_avg=0
FOR pol_i=0,n_pol-1 DO BEGIN ;this should be by frequency! and also by time
    *beam_base[pol_i]=Sqrt(beam_image(psf,obs,pol_i=pol_i,dimension=dimension,/square))
    IF over_resolution GT 1 THEN *beam_base_fit[pol_i]=Rebin(*beam_base[pol_i],dimension_fit,elements_fit) ELSE *beam_base_fit[pol_i]=*beam_base[pol_i]
;    *beam_base[pol_i]=beam_image(psf,obs,pol_i=pol_i,dimension=dimension,/square)
    beam_mask0=fltarr(dimension_fit,elements_fit)
    
    beam_mask_test=*beam_base_fit[pol_i]
;    *beam_i[pol_i]=region_grow(beam_mask_test,dimension/2.+dimension*elements/2.,threshold=[beam_threshold,Max(beam_mask_test)])
    *beam_i[pol_i]=where(beam_mask_test GE beam_threshold)
    beam_mask0[*beam_i[pol_i]]=1.
    IF pol_i LE 1 THEN BEGIN
        nbeam_avg+=1
        beam_mask*=beam_mask0
        beam_avg+=(*beam_base_fit[pol_i])^2.
    ENDIF
    
    *beam_correction[pol_i]=weight_invert(*beam_base[pol_i],beam_max_threshold)
    IF over_resolution GT 1 THEN *beam_correction_fit[pol_i]=Rebin(*beam_correction[pol_i],dimension_fit,elements_fit) ELSE *beam_correction_fit[pol_i]=*beam_correction[pol_i]
ENDFOR
beam_mask*=alias_mask
beam_avg/=nbeam_avg
beam_avg=Sqrt(beam_avg>0.)
beam_corr_avg=weight_invert(beam_avg,beam_threshold)
source_taper=Sqrt((beam_avg*(beam_corr_avg<1./sqrt(beam_threshold)))>0.)

horizon_mask=intarr(dimension,elements)+1

;ignore refraction since it's only being used to calculate a pixel mask for the horizon
apply_astrometry, obs, x_arr=meshgrid(dimension,elements,1), y_arr=meshgrid(dimension,elements,2), ra_arr=ra_arr, dec_arr=dec_arr, /xy2ad, /ignore_refraction
nan_i=where(Finite(ra_arr,/nan),n_nan,complement=horizon_i)
IF n_nan GT 0 THEN horizon_mask[nan_i]=0
ra_use=ra_arr[horizon_i]
dec_use=dec_arr[horizon_i]
Eq2Hor,ra_use,dec_use,obs.Jd0,alt_arr1,az_arr1,lat=obs.lat,lon=obs.lon,alt=obs.alt,precess=1,/nutate, refract=0
IF Tag_exist(fhd_params,'horizon_threshold') THEN horizon_threshold=fhd_params.horizon_threshold ELSE horizon_threshold=10. ;degrees above the horizon to exclude
horizon_cut=where(alt_arr1 LT horizon_threshold,n_hor_cut)
IF n_hor_cut GT 0 THEN horizon_mask[horizon_i[horizon_cut]]=0
beam_mask*=rebin(horizon_mask,dimension_fit,elements_fit,/sample)

IF N_Elements(map_fn_arr) EQ 0 THEN map_fn_arr=Ptrarr(n_pol,/allocate)
weights_arr=Ptrarr(n_pol,/allocate)
dirty_array=Ptrarr(n_pol,/allocate)
residual_array=Ptrarr(n_pol,/allocate)
model_arr=Ptrarr(n_pol,/allocate)
normalization_arr=fltarr(n_pol) ;factor to normalize holo_mapfn_apply
model_uv_full=Ptrarr(n_pol,/allocate)
model_uv_holo=Ptrarr(n_pol,/allocate)

pol_names=obs.pol_names

;load holo map functions and initialize output arrays
dirty_image_composite=fltarr(dimension_fit,elements_fit)
dirty_image_composite_Q=fltarr(dimension_fit,elements_fit)
dirty_image_composite_U=fltarr(dimension_fit,elements_fit)
dirty_image_composite_V=fltarr(dimension_fit,elements_fit)
source_uv_mask=fltarr(dimension,elements)
source_uv_mask2=fltarr(dimension,elements)

FOR pol_i=0,n_pol-1 DO BEGIN
    weights_single=holo_mapfn_apply(complexarr(dimension,elements)+1,map_fn_arr[pol_i],/no_conj,/indexed,_Extra=extra)
    weights_single_conj=conjugate_mirror(weights_single)
    source_uv_mask[where(*image_uv_arr[pol_i])]=1.
    source_uv_mask2[where(weights_single)]=1
    weights_single=(weights_single+weights_single_conj)/2.
    *weights_arr[pol_i]=weights_single
ENDFOR

filter_arr=Ptrarr(n_pol,/allocate)
gain_normalization = get_image_renormalization(obs,psf=psf,params=params,weights_arr=weights_arr,$
    beam_base=beam_base,filter_arr=filter_arr,image_filter_fn=decon_filter,degpix=degpix,file_path_fhd=file_path_fhd,/antialias)

FOR pol_i=0,n_pol-1 DO BEGIN 
;    filter_single=filter_arr[pol_i]
    *dirty_array[pol_i]=dirty_image_generate(*image_uv_arr[pol_i],degpix=degpix,obs=obs_fit,psf=psf,params=params,$
        weights=*weights_arr[pol_i],image_filter=decon_filter,filter=filter_arr[pol_i],pad_uv=over_resolution,/antialias,norm=gain_normalization);*(*beam_correction_fit[pol_i])
;    filter_arr[pol_i]=filter_single
ENDFOR

gain_array=replicate(gain_factor,dimension_fit,elements_fit)

jones_fit=fhd_struct_init_jones(obs_fit,status_str,jones,file_path_fhd=file_path_fhd,mask=beam_mask,/update)
dirty_stokes_arr=stokes_cnv(dirty_array,jones_fit,beam_arr=beam_base_fit,/square,_Extra=extra)
dirty_image_composite=*dirty_stokes_arr[0]
IF n_pol GT 1 THEN dirty_image_composite_Q=*dirty_stokes_arr[1]
IF n_pol GT 2 THEN BEGIN
    dirty_image_composite_V=*dirty_stokes_arr[3]
    dirty_image_composite_U=*dirty_stokes_arr[2]
ENDIF

FOR pol_i=0,n_pol-1 DO BEGIN 
    *model_uv_full[pol_i]=complexarr(dimension,elements)
    *model_uv_holo[pol_i]=complexarr(dimension,elements)
ENDFOR

IF Keyword_Set(galaxy_model_fit) THEN BEGIN
;    gal_model_holo=fhd_galaxy_deconvolve(obs,image_uv_arr,map_fn_arr=map_fn_arr,beam_base=beam_base,file_path_fhd=file_path_fhd,$
;        galaxy_model_uv=galaxy_model_uv,restore=0,image_filter=decon_filter,filter_arr=filter_arr,_Extra=extra)
    gal_model_uv=fhd_galaxy_model(obs,jones,file_path_fhd=file_path_fhd,/uv_return,_Extra=extra)
    FOR pol_i=0,n_pol-1 DO BEGIN
        *model_uv_full[pol_i]+=*gal_model_uv[pol_i]
        *model_uv_holo[pol_i]=holo_mapfn_apply(*model_uv_full[pol_i],map_fn_arr[pol_i],_Extra=extra,/indexed)
    ENDFOR
ENDIF 

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

sm_xmin=(Min(xvals_fit[where(beam_mask)])+dimension_fit/2.-smooth_width)>0
sm_xmax=(Max(xvals_fit[where(beam_mask)])+dimension_fit/2.+smooth_width)<(dimension_fit-1)
sm_ymin=(Min(yvals_fit[where(beam_mask)])+elements_fit/2.-smooth_width)>0
sm_ymax=(Max(yvals_fit[where(beam_mask)])+elements_fit/2.+smooth_width)<(elements_fit-1)
beam_avg_box=beam_avg[sm_xmin:sm_xmax,sm_ymin:sm_ymax]
beam_corr_box=beam_corr_avg[sm_xmin:sm_xmax,sm_ymin:sm_ymax]

image_filtered=dirty_image_composite
IF Keyword_Set(filter_background) THEN BEGIN
    IF smooth_width GT 11 AND (dimension/smooth_width EQ Floor(dimension/smooth_width)) THEN BEGIN
        image_rebin=Rebin(image_filtered*source_mask,dimension/smooth_width,elements/smooth_width)
        mask_rebin=Rebin(source_mask,dimension/smooth_width,elements/smooth_width)
        mask_i_use=where(mask_rebin)
        filter_i=where(Abs(image_rebin[mask_i_use]-Mean(image_rebin[mask_i_use])) GT 5.*Stddev(image_rebin[mask_i_use]),n_filter)

        IF n_filter GT 0 THEN BEGIN
            image_rebin2=Median(image_rebin,4,/even)
            image_rebin[mask_i_use[filter_i]]=image_rebin2[mask_i_use[filter_i]]
        ENDIF
        image_smooth=Rebin(image_rebin,dimension_fit,elements_fit)
        image_filtered=(image_filtered-image_smooth)*source_mask
    ENDIF ELSE BEGIN
        image_smooth=Median(image_filtered[sm_xmin:sm_xmax,sm_ymin:sm_ymax]*beam_avg_box,smooth_width,/even)*beam_corr_box
        image_filtered[sm_xmin:sm_xmax,sm_ymin:sm_ymax]-=image_smooth
    ENDELSE
ENDIF
source_find_image=image_filtered*beam_avg*beam_mask*source_taper
converge_check[0]=Stddev(source_find_image[where(beam_mask)],/nan)
converge_check2[0]=Stddev(source_find_image[where(beam_mask)],/nan)
print,"Gain factor used:",Strn(fhd_params.gain_factor)
print,"Initial convergence:",Strn(converge_check[0])

comp_i=0L
model_holo_arr=Ptrarr(n_pol,/allocate)
source_n_arr=Lonarr(max_iter)
detection_threshold_arr=Fltarr(max_iter)
IF Keyword_Set(subtract_sidelobe_catalog) THEN BEGIN
    source_arr_sidelobe=generate_source_cal_list(obs_fit,psf,catalog_path=subtract_sidelobe_catalog,$
        mask=1-beam_mask,/allow_sidelobe_model_sources,/model_visibilities,_Extra=extra)
    sidelobe_extend_i=where(Ptr_valid(source_arr_sidelobe.extend),n_sidelobe_extend)
    IF over_resolution GT 1 THEN BEGIN
        source_arr_sidelobe.x/=over_resolution
        source_arr_sidelobe.y/=over_resolution
        IF n_sidelobe_extend GT 0 THEN BEGIN
            FOR ext_i=0L,n_sidelobe_extend-1 DO BEGIN
                source_arr_sub=*source_arr_sidelobe[sidelobe_extend_i[ext_i]].extend
                source_arr_sub.x/=over_resolution
                source_arr_sub.y/=over_resolution
                *source_arr_sidelobe[sidelobe_extend_i[ext_i]].extend=source_arr_sub
            ENDFOR
        ENDIF
    ENDIF
    n_sidelobe_src=N_Elements(source_arr_sidelobe)
    empty_test=(n_sidelobe_src EQ 1) AND (source_arr_sidelobe[0].flux.I EQ 0)
    IF not empty_test THEN BEGIN 
        source_arr_sidelobe.id = indgen(n_sidelobe_src)
        IF n_sidelobe_extend GT 0 THEN BEGIN
            FOR ext_i=0L,n_sidelobe_extend-1 DO BEGIN
                (*source_arr_sidelobe[sidelobe_extend_i[ext_i]].extend).id =source_arr_sidelobe[sidelobe_extend_i[ext_i]].id 
            ENDFOR
        ENDIF
        source_arr_sidelobe=source_list_expand(source_arr_sidelobe)
        n_sidelobe_src=N_Elements(source_arr_sidelobe)
        print,'Subtracting source model from the sidelobes: '+Strn(n_sidelobe_src) + " source components"
        IF Keyword_Set(return_sidelobe_catalog) THEN BEGIN
            print, 'Sidelobe sources included in source list"
            component_array = [source_arr_sidelobe, component_array]
            max_sources += n_sidelobe_src
            comp_i += n_sidelobe_src
        ENDIF ELSE print, 'Sidelobe sources not included in source list'
        model_uv_sidelobe=source_dft_model(obs,jones,source_arr_sidelobe,t_model=t_model,uv_mask=source_uv_mask2,_Extra=extra)
        t3+=t_model
        FOR pol_i=0,n_pol-1 DO *model_uv_full[pol_i]+=*model_uv_sidelobe[pol_i]
        FOR pol_i=0,n_pol-1 DO BEGIN
            *model_uv_holo[pol_i]=holo_mapfn_apply(*model_uv_full[pol_i],map_fn_arr[pol_i],_Extra=extra,/indexed)
        ENDFOR
    ENDIF ELSE print, "Sidelobe catalog pre-subtraction was set, but no valid sources were found in catalog: " + subtract_sidelobe_catalog
ENDIF

IF ~Keyword_Set(silent) THEN print,'Iteration # : Component # : Elapsed time : Convergence'

recalc_flag=1
t_init=Systime(1)-t00
FOR iter=0L,max_iter-1 DO BEGIN 
    IF Keyword_Set(recalc_flag) THEN BEGIN
        t1_0=Systime(1)
        FOR pol_i=0,n_pol-1 DO *model_holo_arr[pol_i]=dirty_image_generate(*model_uv_holo[pol_i],degpix=degpix,filter=filter_arr[pol_i],pad_uv=over_resolution,/antialias,norm=gain_normalization)
        undefine_fhd,model_stokes_arr
        model_stokes_arr=stokes_cnv(model_holo_arr,jones_fit,beam_arr=beam_base_fit,/square,_Extra=extra)
        model_image_composite=*model_stokes_arr[0]
        IF n_pol GT 1 THEN model_image_composite_Q=*model_stokes_arr[1]
        IF n_pol GT 2 THEN model_image_composite_U=*model_stokes_arr[2]
        IF n_pol GT 3 THEN model_image_composite_V=*model_stokes_arr[3]
        
        t2_0=Systime(1)
        t1+=t2_0-t1_0 
        
        image_unfiltered=dirty_image_composite-model_image_composite
        IF Keyword_Set(filter_background) THEN BEGIN
            IF smooth_width GT 11 AND (dimension/smooth_width EQ Floor(dimension/smooth_width)) THEN BEGIN
                image_rebin=Rebin(image_unfiltered*beam_mask,dimension/smooth_width,elements/smooth_width)
                mask_rebin=Rebin(beam_mask,dimension/smooth_width,elements/smooth_width)
                mask_i_use=where(mask_rebin)
                filter_i=where(Abs(image_rebin[mask_i_use]-Mean(image_rebin[mask_i_use])) GT 5.*Stddev(image_rebin[mask_i_use]),n_filter)

                model_rebin=Rebin(model_image_composite,dimension/smooth_width,elements/smooth_width)
                IF n_filter GT 0 THEN BEGIN
                    image_rebin2=Median(image_rebin,4,/even)
                    image_rebin[mask_i_use[filter_i]]=image_rebin2[mask_i_use[filter_i]]
                ENDIF
                image_smooth=Rebin(image_rebin,dimension_fit,elements_fit)
                image_filtered=(image_unfiltered-image_smooth)*beam_mask
            ENDIF ELSE BEGIN
                image_smooth=Median(image_unfiltered[sm_xmin:sm_xmax,sm_ymin:sm_ymax]*beam_avg_box,smooth_width,/even)*beam_corr_box
                image_filtered=fltarr(dimension_fit,elements_fit)
                image_filtered[sm_xmin:sm_xmax,sm_ymin:sm_ymax]=image_unfiltered[sm_xmin:sm_xmax,sm_ymin:sm_ymax]-image_smooth
                model_smooth=Median(model_image_composite[sm_xmin:sm_xmax,sm_ymin:sm_ymax]*beam_avg_box,smooth_width,/even)*beam_corr_box
            ENDELSE
        ENDIF ELSE BEGIN
            image_filtered=image_unfiltered
        ENDELSE
        
        IF Keyword_Set(independent_fit) THEN BEGIN
            image_use_Q=dirty_image_composite_Q-model_image_composite_Q
            image_use_U=dirty_image_composite_U-model_image_composite_U
            image_use_V=dirty_image_composite_V-model_image_composite_V           
        ENDIF 
    ENDIF ELSE t2_0=Systime(1)
    source_find_image=image_filtered*beam_avg*source_taper*source_mask
    image_use=image_unfiltered*beam_avg*beam_mask*source_mask
   
    component_array1=fhd_source_detect(obs_fit,fhd_params,jones,source_find_image,image_I=image_filtered,$
        image_Q=image_use_Q,image_U=image_use_U,image_V=image_use_V,$
        gain_array=gain_array,beam_mask=beam_mask,source_mask=source_mask,n_sources=n_sources,detection_threshold=detection_threshold,$
        beam_arr=beam_base,beam_corr_avg=beam_corr_avg,_Extra=extra)
    
    source_n_arr[iter]=n_sources
    detection_threshold_arr[iter]=detection_threshold
    image_use*=source_mask
    source_find_image*=source_mask
    IF iter EQ 0 THEN converge_check[iter]=Stddev(image_use[where(beam_mask*source_mask)],/nan)
    converge_check2[iter]=Stddev(image_use[where(beam_mask*source_mask)],/nan)
    ;use the composite image to locate sources, but then fit for flux independently
    
    IF comp_i+n_sources GE max_sources THEN BEGIN
        n_sources=max_sources-comp_i-1
        IF n_sources GT 0 THEN component_array1=component_array1[0:n_sources-1]
    ENDIF
    IF n_sources LE 0 THEN BEGIN
        ;do something to end loop if n_mask EQ 0
        
        i2+=1
        t10=Systime(1)-t0
        converge_check[i2]=Stddev(image_use[where(beam_mask*source_mask)],/nan)
        fhd_params.end_condition='Source fit failure'
        print,StrCompress(String(format='("Break after iteration ",I," from failure to fit any sources after ",I," seconds with ",I," sources (convergence:",F,")")',$
            iter,t10,comp_i+1,Stddev(image_use[where(beam_mask*source_mask)],/nan)))
        BREAK
        recalc_flag=0
    ENDIF ELSE recalc_flag=1
    
    component_array[comp_i:comp_i+n_sources-1]=component_array1
    comp_i+=n_sources
            ;Make sure to update source uv model in "true sky" instrumental polarization i.e. 1/beam^2 frame.
    t3_0=Systime(1)
    t2+=t3_0-t2_0
    source_dft_multi,obs,jones,component_array1,model_uv_full,xvals=xvals2,yvals=yvals2,uv_i_use=uv_i_use2,$
        /silent,dft_threshold=dft_threshold,return_kernel=return_kernel,_Extra=extra
    
    t4_0=Systime(1)
    t3+=t4_0-t3_0
    FOR pol_i=0,n_pol-1 DO BEGIN
        *model_uv_holo[pol_i]=holo_mapfn_apply(*model_uv_full[pol_i],map_fn_arr[pol_i],_Extra=extra,/indexed)
    ENDFOR
    t4+=Systime(1)-t4_0
    
    IF comp_i+1 GE max_sources THEN BEGIN
        i2+=1                                        
        t10=Systime(1)-t0
        fhd_params.end_condition='Max components'
        print,StrCompress(String(format='("Max sources found by iteration ",I," after ",I," seconds with ",I," sources (convergence:",F,")")',$
            iter,t10,comp_i+1,Stddev(image_use[where(beam_mask*source_mask)],/nan)))
        converge_check[i2]=Stddev(image_use[where(beam_mask*source_mask)],/nan)
        BREAK
    ENDIF
    
    IF (Round(iter mod check_iter) EQ 0) AND (iter GT 0) THEN BEGIN
        i2+=1
        t10=Systime(1)-t0
        IF ~Keyword_Set(silent) THEN print,StrCompress(String(format='(I," : ",I," : ",I," : ",F)',$
            iter,comp_i+1,t10,Stddev(image_use[where(beam_mask*source_mask)],/nan)))
        converge_check[i2]=Stddev(image_use[where(beam_mask*source_mask)],/nan)
        IF sigma_threshold*converge_check[i2] GT Max(source_find_image) THEN BEGIN
            fhd_params.end_condition='Low SNR'
            print,StrCompress(String(format='("Break after iteration ",I," from low signal to noise after ",I," seconds with ",I," sources (convergence:",F,")")',$
                iter,t10,comp_i+1,Stddev(image_use[where(beam_mask*source_mask)],/nan)))
            BREAK
        ENDIF
        IF converge_check[i2] GE converge_check[i2-1] THEN BEGIN
            fhd_params.end_condition='Convergence'
            print,StrCompress(String(format='("Break after iteration ",I," from lack of convergence after ",I," seconds with ",I," sources (convergence:",F,")")',$
                iter,t10,comp_i+1,Stddev(image_use[where(beam_mask*source_mask)],/nan)))
            BREAK
        ENDIF
    ENDIF
ENDFOR
IF iter EQ max_iter THEN BEGIN
    t10=Systime(1)-t0
    fhd_params.end_condition='Max iterations'
    print,StrCompress(String(format='("Max iteration ",I," reached after ",I," seconds with ",I," sources (convergence:",F,")")',$
        iter,t10,comp_i+1,Stddev(image_use[where(beam_mask*source_mask)],/nan)))
ENDIF ELSE iter+=1 ;increment iter by one if the loop was exited by a BREAK statement
converge_check2=converge_check2[0:iter-1]
converge_check=converge_check[0:i2]

;condense clean components
fhd_params.convergence=Stddev(image_use[where(beam_mask*source_mask)],/nan)
noise_map=fhd_params.convergence*beam_corr_avg
IF over_resolution GT 1 THEN BEGIN
    noise_map=Rebin(noise_map,dimension,elements)
    gain_array=Rebin(gain_array,dimension,elements)
    beam_width=beam_width_calculate(obs,min_restored_beam_width=1.,/FWHM)
ENDIF
;noise_map*=gain_normalization
IF Keyword_Set(independent_fit) THEN noise_map*=Sqrt(n_pol)
;component_array=component_array[0:comp_i]
comp_i_use=where(component_array.flux.I GT 0)
component_array=component_array[comp_i_use]
fhd_params.n_iter=iter
fhd_params.n_components=N_Elements(component_array)
fhd_params.detection_threshold=detection_threshold
source_n_arr=source_n_arr[0:iter-1]
detection_threshold_arr=detection_threshold_arr[0:iter-1]
source_mask=Rebin(source_mask,dimension,elements,/sample)
source_array=Components2Sources(component_array,obs,fhd_params,radius=beam_width>1.5,noise_map=noise_map,$
    gain_array=gain_array,source_mask=source_mask,_Extra=extra) ;;Note that gain_array=gain_factor*source_taper
fhd_params.n_sources=N_Elements(source_array)
info_struct={convergence_iter:converge_check2,source_n_iter:source_n_arr,detection_threshold_iter:detection_threshold_arr}
fhd_params.info=Ptr_new(info_struct)
t3_0=Systime(1)
IF Keyword_Set(no_condense_sources) THEN BEGIN
    comp_i_use=where(component_array.id GE 0)
    component_array_use=component_array[comp_i_use]
    model_uv_full=source_dft_model(obs,jones,component_array_use,t_model=t_model,uv_mask=source_uv_mask2,return_kernel=return_kernel,$
        dft_threshold=dft_threshold,_Extra=extra) 
ENDIF ELSE BEGIN
    model_uv_full=source_dft_model(obs,jones,source_array,t_model=t_model,uv_mask=source_uv_mask2,return_kernel=return_kernel,$
        dft_threshold=dft_threshold,_Extra=extra)
ENDELSE
IF size(return_kernel,/type) EQ 10 THEN Ptr_free,return_kernel
IF Keyword_Set(galaxy_model_fit) THEN FOR pol_i=0,n_pol-1 DO *model_uv_full[pol_i]+=*gal_model_uv[pol_i]
IF Keyword_Set(subtract_sidelobe_catalog) THEN  FOR pol_i=0,n_pol-1 DO *model_uv_full[pol_i]+=*model_uv_sidelobe[pol_i]
t4_0=Systime(1)
t3+=t4_0-t3_0
FOR pol_i=0,n_pol-1 DO *model_uv_holo[pol_i]=holo_mapfn_apply(*model_uv_full[pol_i],map_fn_arr[pol_i],_Extra=extra,/indexed)

t1_0=Systime(1)
t4+=t1_0-t4_0    
undefine_fhd,beam_correction_fit,beam_base_fit
FOR pol_i=0,n_pol-1 DO *residual_array[pol_i]=$
    dirty_image_generate(*image_uv_arr[pol_i]-*model_uv_holo[pol_i],degpix=degpix,filter=filter_arr[pol_i],/antialias,norm=gain_normalization)*(*beam_correction[pol_i])
t1+=Systime(1)-t1_0

t00=Systime(1)-t00
print,'Deconvolution timing [per iteration]'
print,String(format='("Setup:",A," ")',Strn(Round(t_init)))
print,String(format='("FFT:",A,"[",A,"]")',Strn(Round(t1)),Strn(Round(t1*100/iter)/100.))
print,String(format='("Filtering:",A,"[",A,"]")',Strn(Round(t2)),Strn(Round(t2*100/iter)/100.))
print,String(format='("DFT source modeling:",A,"[",A,", or ",A," per 100 sources]")',Strn(Round(t3)),Strn(Round(t3*100/iter)/100.),Strn(Round(t3*10000./(comp_i+1))/100.))
print,String(format='("Applying HMF:",A,"[",A,"]")',Strn(Round(t4)),Strn(Round(t4*100/iter)/100.))
timing=[t00,t1,t2,t3,t4]
;print,timing

END  
