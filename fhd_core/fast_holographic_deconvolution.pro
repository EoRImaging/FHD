
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
;    source_array - condensed source list. Columns are 0:x, 1:y, 2:RA (degrees), 3:Dec(degrees), 4: flux density (image), 5: pixel index, 6: u,v plane amplitude
;    
;    comp_arr - uncondensed source list
;
; :Keywords:
;    weights_arr
;    pol_use
;    freq_use
;    time_i_use
;    gain_factor
;    mapfn_interval
;    max_iter
;    check_iter
;    max_add_sources
;    max_sources
;    mapfn_threshold
;    baseline_threshold
;    beam_threshold
;    add_threshold
;    data_directory
;    filename
;    timing
;    residual_array
;    dirty_array
;    model_uv_full
;    model_uv_holo
;    polarization_map
;    polarization_correction
;    ra_arr
;    dec_arr
;    astr
;    beam_base
;    beam_correction
;    normalization_arr
;    independent_fit
;    reject_pol_sources
;
; :Author: isullivan May 4, 2012
;-
PRO fast_holographic_deconvolution,fhd,obs,psf,image_uv_arr,source_array,comp_arr,weights_arr=weights_arr,timing=timing,$
    residual_array=residual_array,dirty_array=dirty_array,model_uv_full=model_uv_full,model_uv_holo=model_uv_holo,$
    ra_arr=ra_arr,dec_arr=dec_arr,astr=astr,silent=silent,map_fn_arr=map_fn_arr,$
    beam_base=beam_base,beam_correction=beam_correction,normalization=normalization

compile_opt idl2,strictarrsubs  

t00=Systime(1)    
vis_path_default,data_directory,filename,file_path,obs=obs
;image_uv_arr is a pointer array with dimensions (npol) 

npol=fhd.npol
;ntimes=fhd.ntimes
;nfreq=fhd.nfreq
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
local_max_radius=fhd.local_max_radius
pol_use=fhd.pol_use
independent_fit=fhd.independent_fit
reject_pol_sources=fhd.reject_pol_sources

icomp=Complex(0,1)
beam_max_threshold=fhd.beam_max_threshold
;beam_deriv_threshold=0.1
smooth_width=fhd.smooth_width
;color_frequency_correction=fltarr(nfreq)+1. ;remove same component from all frequencies, but allow to be different in the future

dimension=obs.dimension
elements=obs.elements
xvals=meshgrid(dimension,elements,1)-dimension/2
yvals=meshgrid(dimension,elements,2)-elements/2
rvals=Sqrt(xvals^2.+yvals^2.)

vis_coordinates,obs,ra_arr,dec_arr,astr=astr
;IF N_Elements(ra_arr) EQ 0 THEN vis_coordinates,obs,ra_arr,dec_arr,astr=astr,valid_i=valid_radec_i $
;    ELSE valid_radec_i=where(ra_arr AND dec_arr)

ps_not_used=1./2.
pc_not_used=1.


;;TEMPORARY addition to fix polarization for off-zenith pointings
;polarization_map=vis_jones_matrix(obs,polarization_correction,ra_arr=ra_arr,dec_arr=dec_arr)
;p_map_simple=[polarization_map[0,0],polarization_map[0,1],polarization_map[2,2],polarization_map[2,3]]
;p_corr_simple=[polarization_correction[0,0],polarization_correction[0,1],polarization_correction[2,2],polarization_correction[2,3]]
;;FOR pol_i=0,1 DO BEGIN
;;    *p_map_simple[pol_i]*=0.5/(*p_map_simple[pol_i])[dimension/2,elements/2.]
;;    *p_corr_simple[pol_i]*=1.0/(*p_corr_simple[pol_i])[dimension/2,elements/2.]
;;;    *p_map_simple[pol_i]*=0.5/(*p_map_simple[pol_i])[obs.zenx,obs.zeny]
;;;    *p_corr_simple[pol_i]*=1.0/(*p_corr_simple[pol_i])[obs.zenx,obs.zeny]    
;;ENDFOR

;dxc=dimension/2.-obs.zenx
;dyc=elements/2.-obs.zeny
;IF Abs(obs.obsra-obs.zenra) GT 90. THEN lon_offset=obs.obsra-((obs.obsra GT obs.zenra) ? 360.:(-360.))-obs.zenra ELSE lon_offset=obs.obsra-obs.zenra
;lat_offset=-(obs.zendec-obs.obsdec)
;degpix_use=[Cos(lon_offset*!DtoR*Cos(obs.obsdec*!DtoR)),Cos(lat_offset*!DtoR)]*obs.degpix
;xcvals=((xvals+dxc)*Cos(!DtoR*obs.rotation)-(yvals+dyc)*Sin(!DtoR*obs.rotation))*degpix_use[0]
;ycvals=((yvals+dyc)*Cos(!DtoR*obs.rotation)+(xvals+dxc)*Sin(!DtoR*obs.rotation))*degpix_use[1]
;
;p_map_simple=Ptrarr(4,/allocate)
;p_corr_simple=Ptrarr(4,/allocate)
;*p_map_simple[0]=0.5*Cos(xcvals*!DtoR)^2.
;*p_map_simple[1]=0.5*Cos(ycvals*!DtoR)^2.
;*p_map_simple[2]=0.5*Cos(xcvals*!DtoR)*Cos(ycvals*!DtoR)
;*p_map_simple[3]=0.5*Cos(xcvals*!DtoR)*Cos(ycvals*!DtoR)
;FOR pol_i=0,3 DO *p_corr_simple[pol_i]=0.5*weight_invert(*p_map_simple[pol_i])

;the particular set of beams read will be the ones specified by file_path.
;that will include all polarizations and frequencies, at ONE time snapshot
IF N_Elements(psf) EQ 0 THEN psf=beam_setup(obs,/restore_last) 
nfreq_beam=(size(psf.base,/dimension))[1]
beam_base=Ptrarr(npol,/allocate)
beam_correction=Ptrarr(npol,/allocate)
beam_i=Ptrarr(npol,/allocate)
beam_mask=Ptrarr(npol,/allocate)
source_mask=fltarr(dimension,elements)+1.; & source_mask[valid_radec_i]=1.
gain_array=fltarr(dimension,elements)+gain_factor
beam_avg=fltarr(dimension,elements)
nbeam_avg=0
FOR pol_i=0,npol-1 DO BEGIN ;this should be by frequency! and also by time
    *beam_base[pol_i]=beam_image(psf.base,pol_i=pol_i,dimension=dimension)
    *beam_mask[pol_i]=fltarr(dimension,elements)
    
    beam_mask_test=*beam_base[pol_i];*(*p_map_simple[pol_i]);*(ps_not_used*2.)
    *beam_i[pol_i]=region_grow(beam_mask_test,dimension/2.+dimension*elements/2.,threshold=[beam_threshold,Max(beam_mask_test)])
    (*beam_mask[pol_i])[*beam_i[pol_i]]=1.
    IF pol_i LE 1 THEN BEGIN
        nbeam_avg+=1
        source_mask*=*beam_mask[pol_i]
        beam_avg+=*beam_base[pol_i];*(*p_map_simple[pol_i]);*(ps_not_used*2.)
    ENDIF
        
    *beam_correction[pol_i]=fltarr(dimension,elements)
    beam_max_i=where(abs(*beam_base[pol_i]) GT beam_max_threshold,complement=beam_under_i,ncomplement=n_beam_under)
    (*beam_correction[pol_i])[beam_max_i]=1./(*beam_base[pol_i])[beam_max_i]
ENDFOR

beam_avg/=nbeam_avg
beam_corr_avg=weight_invert(beam_avg,beam_threshold)
;gain_array*=Sqrt(beam_avg>0.)*source_mask


IF N_Elements(map_fn_arr) EQ 0 THEN map_fn_arr=Ptrarr(npol,/allocate)
weights_arr=Ptrarr(npol,/allocate)
dirty_array=Ptrarr(npol,/allocate)
residual_array=Ptrarr(npol,/allocate)
model_arr=Ptrarr(npol,/allocate)
;scale_arr=fltarr(npol)
normalization_arr=fltarr(npol) ;factor to normalize dirty_image_generate
;gain_factor_correct=fltarr(npol) ;factor to normalize the Holo map function 
model_uv=Ptrarr(npol,/allocate)
model_uv_full=Ptrarr(npol,/allocate)
model_uv_holo=Ptrarr(npol,/allocate)
;;columns of source_array are: 0:x, 1:y, 2:RA, 3:Dec, 4:estimated flux, 5:pixel index, 6:clean component used 
;source_array=Fltarr(7,npol,max_sources)

source_comp_init,comp_arr,n_sources=max_sources
pol_names=['xx','yy','xy','yx','I','Q','U','V'] ;not used, but here for reference

pol_cut=1-histogram(pol_use,min=0,bin=1,nbins=npol)

;load holo map functions and initialize output arrays
dirty_image_composite=fltarr(dimension,elements)
;dirty_image_apparent=fltarr(dimension,elements)
dirty_image_composite_Q=fltarr(dimension,elements)
dirty_image_composite_U=fltarr(dimension,elements)
dirty_image_composite_V=fltarr(dimension,elements)
source_uv_mask=fltarr(dimension,elements)
FOR pol_i=0,npol-1 DO BEGIN
    IF pol_cut[pol_i] THEN CONTINUE
    IF N_Elements(*map_fn_arr[pol_i]) EQ 0 THEN BEGIN
        holo_mapfn_generate,obs,/restore_last,map_fn=map_fn_single,polarization=pol_i
        *map_fn_arr[pol_i]=map_fn_single
    ENDIF
    weights_single=real_part(holo_mapfn_apply(complexarr(dimension,elements)+1,*map_fn_arr[pol_i]))
    normalization_arr[pol_i]=$
        1./(dirty_image_generate(weights_single,baseline_threshold=baseline_threshold))[dimension/2.,elements/2.]
        
    normalization_arr[pol_i]*=((*beam_base[pol_i])[dimension/2.,elements/2.])^2.
    
;    gain_factor_correct[pol_i]=$
;        1./(dirty_image_generate(weights_single,baseline_threshold=baseline_threshold))[dimension/2.,elements/2.]
    *weights_arr[pol_i]=weights_single
    dirty_image_single=dirty_image_generate(*image_uv_arr[pol_i],baseline=baseline_threshold)*(*beam_correction[pol_i])
    
    source_uv_mask[where(weights_single)]=1.
    
    ;THIS IS THE ONE LINE TO UNCOMMENT IF RE-INSERTING POLARIZATION CORRECTION EFFECTS
;    ;TEMPORARY addition to fix polarization for off-zenith pointings
;    dirty_image_single*=*p_corr_simple[pol_i]
    
    ;xx, yy and xy, yx polarizations are treated seperately
;    IF pol_i LE 1 THEN dirty_image_apparent+=dirty_image_single
    IF pol_i LE 1 THEN dirty_image_composite+=dirty_image_single*(*beam_correction[pol_i]);*pc_not_used
    IF pol_i GE 2 THEN dirty_image_composite_U+=dirty_image_single*(*beam_correction[pol_i]);*pc_not_used
    CASE pol_i OF 
        0:dirty_image_composite_Q+=dirty_image_single*(*beam_correction[pol_i]);*pc_not_used
        1:dirty_image_composite_Q-=dirty_image_single*(*beam_correction[pol_i]);*pc_not_used 
        2:dirty_image_composite_V+=dirty_image_single*(*beam_correction[pol_i]);*pc_not_used
        3:dirty_image_composite_V-=dirty_image_single*(*beam_correction[pol_i]);*pc_not_used
    ENDCASE
    *dirty_array[pol_i]=dirty_image_single
    *model_uv[pol_i]=complexarr(dimension,elements)
    *model_uv_full[pol_i]=complexarr(dimension,elements)
    *model_uv_holo[pol_i]=complexarr(dimension,elements)
ENDFOR
normalization=mean(normalization_arr[0:npol-1])/2. ;factor of two accounts for complex conjugate

uv_i_use=where(source_uv_mask,n_uv_use)
uv_use_frac=Float(n_uv_use)/(dimension*elements)
print,"Fractional uv coverage: ",uv_use_frac,"normalization: ",normalization
xvals1=xvals[uv_i_use]
yvals1=yvals[uv_i_use]

;;TEMPORARY HACK!!!
dxx=*dirty_array[0]*(*beam_correction[0])*source_mask;*beam_avg
dyy=*dirty_array[1]*(*beam_correction[1])*source_mask;*beam_avg
stdxx=Stddev(dxx)
stdyy=Stddev(dyy)
;dirty_image_composite=fltarr(dimension,elements)
;dirty_image_composite_Q=fltarr(dimension,elements)
;dirty_image_composite_U=fltarr(dimension,elements)
;dirty_image_composite_V=fltarr(dimension,elements)
;sclyy=Sqrt(stdxx/stdyy)
;sclxx=Sqrt(stdyy/stdxx)
;FOR pol_i=0,(1<(npol-1)) DO BEGIN
;    dirty_image_single=dirty_image_generate(*image_uv_arr[pol_i],baseline=baseline_threshold)*(*beam_correction[pol_i])
;    dirty_image_single*=*p_corr_simple[pol_i]
;    IF pol_i EQ 0 THEN dirty_image_single*=sclxx
;    IF pol_i EQ 1 THEN dirty_image_single*=sclyy
;    IF pol_i LE 1 THEN dirty_image_composite+=dirty_image_single*(*beam_correction[pol_i]);*pc_not_used
;    IF pol_i GE 2 THEN dirty_image_composite_U+=dirty_image_single*(*beam_correction[pol_i]);*pc_not_used
;    CASE pol_i OF 
;        0:dirty_image_composite_Q+=dirty_image_single*(*beam_correction[pol_i]);*pc_not_used
;        1:dirty_image_composite_Q-=dirty_image_single*(*beam_correction[pol_i]);*pc_not_used 
;        2:dirty_image_composite_V+=dirty_image_single*(*beam_correction[pol_i]);*pc_not_used
;        3:dirty_image_composite_V-=dirty_image_single*(*beam_correction[pol_i]);*pc_not_used
;    ENDCASE
;    *dirty_array[pol_i]=dirty_image_single
;    *model_uv[pol_i]=complexarr(dimension,elements)
;    *model_uv_full[pol_i]=complexarr(dimension,elements)
;    *model_uv_holo[pol_i]=complexarr(dimension,elements)
;ENDFOR


t1=0 ;generation of model_images and image_use for source detection
t2=0 ;source extraction
t3=0 ;fit the brightest source(s) to each polarization/etc...
t4=0 ;update model and run Holo mapping function
i2=0. & i3=0.
t0=Systime(1)
converge_check=Fltarr(Ceil(max_iter/check_iter))
converge_check2=Fltarr(max_iter)

sm_xmin=(Min(xvals[where(source_mask)])+dimension/2.-smooth_width)>0
sm_xmax=(Max(xvals[where(source_mask)])+dimension/2.+smooth_width)<(dimension-1)
sm_ymin=(Min(yvals[where(source_mask)])+elements/2.-smooth_width)>0
sm_ymax=(Max(yvals[where(source_mask)])+elements/2.+smooth_width)<(elements-1)
beam_avg_box=beam_avg[sm_xmin:sm_xmax,sm_ymin:sm_ymax]
beam_corr_box=beam_corr_avg[sm_xmin:sm_xmax,sm_ymin:sm_ymax]

dirty_image_composite_smooth=fltarr(dimension,elements)
dirty_image_composite_smooth[sm_xmin:sm_xmax,sm_ymin:sm_ymax]=Median(dirty_image_composite[sm_xmin:sm_xmax,sm_ymin:sm_ymax]*beam_avg_box,smooth_width,/even) *beam_corr_box
converge_check[0]=(converge_check2[0]=Stddev(((dirty_image_composite-dirty_image_composite_smooth)*beam_avg)[where(source_mask,n_pix0)],/nan))
print,"Initial convergence:",converge_check[0]

IF not Keyword_Set(silent) THEN print,'Iteration # : Component # : Elapsed time : Convergence'

si=0L
FOR i=0L,max_iter-1 DO BEGIN 
    t1_0=Systime(1)
    model_image_composite=fltarr(dimension,elements)
;    model_image_apparent=fltarr(dimension,elements)
    model_image_composite_Q=fltarr(dimension,elements)
    model_image_composite_U=fltarr(dimension,elements)
    model_image_composite_V=fltarr(dimension,elements)
    FOR pol_i=0,npol-1 DO BEGIN 
        IF pol_cut[pol_i] THEN CONTINUE
        model_image_holo=dirty_image_generate(*model_uv_holo[pol_i])
;        model_image_est=dirty_image_generate(*model_uv[pol_i]*(*weights_arr[pol_i]))*normalization;*(*beam_base[pol_i])^2.
;        model_image=(model_image_holo+model_image_est)*(*beam_correction[pol_i])^2.;*pc_not_used
        model_image=(model_image_holo)*(*beam_correction[pol_i])^2.
        
        
;        ;TEMPORARY addition to fix polarization for off-zenith pointings
;        model_image*=*p_corr_simple[pol_i]
        
;        IF pol_i LE 1 THEN model_image_apparent+=model_image
        *model_arr[pol_i]=model_image
        IF pol_i LE 1 THEN model_image_composite+=model_image $;*(*beam_correction[pol_i]) $
            ELSE model_image_composite_U+=model_image;*(*beam_correction[pol_i])
        IF Keyword_Set(independent_fit) OR Keyword_Set(reject_pol_sources) THEN BEGIN   
            CASE pol_i OF
                0:model_image_composite_Q+=model_image;*(*beam_correction[pol_i])
                1:model_image_composite_Q-=model_image;*(*beam_correction[pol_i])
                2:model_image_composite_V+=model_image;*(*beam_correction[pol_i])
                3:model_image_composite_V-=model_image;*(*beam_correction[pol_i])
            ENDCASE
        ENDIF
    ENDFOR
    
    t2_0=Systime(1)
    t1+=t2_0-t1_0 
    
    IF i mod Floor(1./gain_factor) EQ 0 THEN BEGIN
        image_use=dirty_image_composite-model_image_composite
        
;        im_sm_x_holo=dirty_image_generate((*image_uv_arr[0]-*model_uv_holo[0])*hanning(dimension,elements))
;        im_sm_y_holo=dirty_image_generate((*image_uv_arr[1]-*model_uv_holo[1])*hanning(dimension,elements))
;;        im_sm_x_holo=Real_part(fft_shift(FFT(fft_shift((*image_uv_arr[0]-*model_uv_holo[0])*hanning(dimension,elements)))))/(dimension*elements)
;;        im_sm_y_holo=Real_part(fft_shift(FFT(fft_shift((*image_uv_arr[1]-*model_uv_holo[1])*hanning(dimension,elements)))))/(dimension*elements)
;        image_smooth=im_sm_x_holo*(*beam_correction[0])^2.*(*p_corr_simple[0])+im_sm_y_holo*(*beam_correction[1])^2.*(*p_corr_simple[1])
;        image_use-=image_smooth
;;        image_smooth1=Median(image_use*beam_avg,smooth_width,/even)*beam_corr_avg;Max_filter(image_use,smooth_width,/median,/circle)
        image_smooth=Median(image_use[sm_xmin:sm_xmax,sm_ymin:sm_ymax]*beam_avg_box,smooth_width,/even)*beam_corr_box;Max_filter(image_use,smooth_width,/median,/circle)
        image_use[sm_xmin:sm_xmax,sm_ymin:sm_ymax]-=image_smooth
        
        source_find_image=image_use*beam_avg
        
        IF Keyword_Set(independent_fit) THEN BEGIN
            image_use_Q=dirty_image_composite_Q-model_image_composite_Q
            image_use_U=dirty_image_composite_U-model_image_composite_U
            image_use_V=dirty_image_composite_V-model_image_composite_V
            image_smooth_Q=Median(image_use_Q[sm_xmin:sm_xmax,sm_ymin:sm_ymax]*beam_avg_box,smooth_width,/even)*beam_corr_box
            image_use_Q[sm_xmin:sm_xmax,sm_ymin:sm_ymax]-=image_smooth_Q
            image_smooth_U=Median(image_use_U[sm_xmin:sm_xmax,sm_ymin:sm_ymax]*beam_avg_box,smooth_width,/even)*beam_corr_box
            image_use_U[sm_xmin:sm_xmax,sm_ymin:sm_ymax]-=image_smooth_U
            image_smooth_V=Median(image_use_V[sm_xmin:sm_xmax,sm_ymin:sm_ymax]*beam_avg_box,smooth_width,/even)*beam_corr_box
            image_use_V[sm_xmin:sm_xmax,sm_ymin:sm_ymax]-=image_smooth_V            
        ENDIF ELSE IF npol GT 2 THEN BEGIN
            image_use_U=dirty_image_composite_U-model_image_composite_U
            image_smooth_U=Median(image_use_U[sm_xmin:sm_xmax,sm_ymin:sm_ymax]*beam_avg_box,smooth_width,/even)*beam_corr_box;Max_filter(image_use_U,smooth_width,/median,/circle)
            image_use_U[sm_xmin:sm_xmax,sm_ymin:sm_ymax]-=image_smooth_U
        ENDIF    
    ENDIF ELSE BEGIN
        image_use=dirty_image_composite-model_image_composite
;        image_use-=image_smooth ;uses previously calculated image_smooth!
        image_use[sm_xmin:sm_xmax,sm_ymin:sm_ymax]-=image_smooth ;uses previously calculated image_smooth!
        source_find_image=image_use*beam_avg

        IF Keyword_Set(independent_fit) THEN BEGIN
            image_use_Q=dirty_image_composite_Q-model_image_composite_Q
            image_use_U=dirty_image_composite_U-model_image_composite_U
            image_use_V=dirty_image_composite_V-model_image_composite_V
            image_use_Q[sm_xmin:sm_xmax,sm_ymin:sm_ymax]-=image_smooth_Q ;uses previously calculated image_smooth!
            image_use_U[sm_xmin:sm_xmax,sm_ymin:sm_ymax]-=image_smooth_U ;uses previously calculated image_smooth!
            image_use_V[sm_xmin:sm_xmax,sm_ymin:sm_ymax]-=image_smooth_V ;uses previously calculated image_smooth!
        ENDIF ELSE IF npol GT 2 THEN BEGIN
            image_use_U=dirty_image_composite_U-model_image_composite_U
            image_use_U[sm_xmin:sm_xmax,sm_ymin:sm_ymax]-=image_smooth_U ;uses previously calculated image_smooth!
        ENDIF    
    ENDELSE
    
    ;new addition to reject strongly polarized sources. The point of this is to primarily reject artifacts from sidelobes of bright sources outside the FOV
;    IF Keyword_Set(reject_pol_sources) THEN BEGIN
;        image_test_Q=dirty_image_composite_Q-model_image_composite_Q
;        source_find_image=image_use-Abs(image_test_Q)          
;    ENDIF ELSE source_find_image=image_use
    
   
    ;use the composite image to locate sources, but then fit for flux independently
    source_flux=Max(source_find_image*source_mask,source_i)
    sx=(source_i mod dimension)
    sy=Floor(source_i/dimension)
    gcntrd,source_find_image,sx,sy,xcen,ycen,local_max_radius,/silent,/keepcenter
    IF (xcen EQ -1) OR (ycen EQ -1) THEN BEGIN
        source_mask[sx,sy]=0
        unmask_i=where(source_mask GT 0,n_pix)
        IF Float(n_pix)/Float(n_pix0) LE 0.75 THEN BEGIN
            print,String(format='("Failure to centroid after",I," iterations")',i)
            FOR pol_i=0,npol-1 DO BEGIN
                *model_uv_holo[pol_i]=holo_mapfn_apply(*model_uv_full[pol_i],*map_fn_arr[pol_i])*normalization
;                *model_uv[pol_i]=Dcomplexarr(dimension,elements)
            ENDFOR
            converge_check2=converge_check2[0:i-1]
            converge_check=converge_check[0:i2]
            BREAK
        ENDIF
        CONTINUE
    ENDIF    
    
;    Find additional sources:
;       require that they be isolated ; This is local_max_radius
;       should put some cap on the absolute number of them ; This is max_add_sources
;       all within some range of the brightest pixels flux, say 95%; This is add_threshold
;    
    flux_ref1=source_find_image[source_i]*add_threshold
    additional_i=where(source_find_image*source_mask GT flux_ref1,n_add)
    additional_i=additional_i[reverse(Sort(source_find_image[additional_i]))] ;order from brightest to faintest
    add_x=additional_i mod dimension
    add_y=Floor(additional_i/dimension)
    add_dist=fltarr(n_add)-1
    FOR addi=1,n_add-1 DO add_dist[addi]=(local_max_radius-Min(abs(add_x[addi]-add_x[0:addi-1])))<(local_max_radius-Min(abs(add_y[addi]-add_y[0:addi-1])))
    additional_i_usei=where(add_dist LT 0,n_sources)
    IF n_sources GT max_add_sources THEN BEGIN
        additional_i_usei=additional_i_usei[0:max_add_sources-1]
        n_sources=max_add_sources
    ENDIF
    additional_i=additional_i[additional_i_usei] ;guaranteed at least one, so this is safe
    
    converge_check2[i]=Stddev((image_use*beam_avg)[where(source_mask)],/nan)
    ;fit flux here, and fill comp_arr for each pol
    t3_0=Systime(1)
    t2+=t3_0-t2_0
    flux_arr=fltarr(4)
    flux_arr2=fltarr(4)
    FOR src_i=0L,n_sources-1 DO BEGIN
        IF src_i GT 0 THEN BEGIN
            sx=(additional_i[src_i] mod dimension)
            sy=Floor(additional_i[src_i]/dimension)
            gcntrd,source_find_image,sx,sy,xcen,ycen,local_max_radius,/silent,/keepcenter
            IF (xcen EQ -1) OR (ycen EQ -1) THEN BEGIN
                source_mask[additional_i[src_i]]=0
                CONTINUE
            ENDIF 
        ENDIF
;        IF Keyword_Set(reject_pol_sources) THEN BEGIN
;            pol_thresh=0.2
;            IF Abs(image_test_Q[additional_i[src_i]]) GE pol_thresh*image_use[additional_i[src_i]] THEN BEGIN
;                source_mask[additional_i[src_i]]=0
;                CONTINUE
;            ENDIF
;        ENDIF 
        xy2ad,xcen,ycen,astr,ra,dec
;        ra=Interpolate(ra_arr,xcen,ycen)
;        dec=Interpolate(dec_arr,xcen,ycen)
        comp_arr[si].x=xcen
        comp_arr[si].y=ycen
        comp_arr[si].ra=ra
        comp_arr[si].dec=dec
        
        beam_corr_src=fltarr(npol)
        beam_src=fltarr(npol)
        gain_factor_use=gain_array[additional_i[src_i]]
        FOR pol_i=0,npol-1 DO BEGIN   
            beam_corr_src[pol_i]=(*beam_correction[pol_i])[additional_i[src_i]]
            beam_src[pol_i]=(*beam_base[pol_i])[additional_i[src_i]]
            
            IF Keyword_Set(independent_fit) THEN BEGIN
                sign=(pol_i mod 2) ? -1:1
                IF pol_i LE 1 THEN flux_use=image_use[additional_i[src_i]]+sign*image_use_Q[additional_i[src_i]]
                IF pol_i GE 2 THEN flux_use=image_use_U[additional_i[src_i]]+sign*image_use_V[additional_i[src_i]]
            ENDIF ELSE IF pol_i LE 1 THEN flux_use=image_use[additional_i[src_i]] ELSE flux_use=image_use_U[additional_i[src_i]]
;            flux_arr2[pol_i]=flux_use 
            
            flux_use*=gain_factor_use/2.
            comp_arr[si].flux.(pol_i)=flux_use*beam_src[pol_i];*ps_not_used ;Apparent brightness, instrumental polarization X gain (a scalar)
;            comp_arr[si].flux.(pol_i)=flux_use*gain_factor_use*beam_src[pol_i]*(*p_map_simple[pol_i])[additional_i[src_i]]
;            flux_use*=ps_not_used*(*beam_correction[pol_i])[additional_i[src_i]]
            flux_arr[pol_i]=flux_use;*beam_corr_src[pol_i] ;"True sky" instrumental pol
        ENDFOR
        
        comp_arr[si].flux.I=flux_arr[0]+flux_arr[1]
        comp_arr[si].flux.Q=flux_arr[0]-flux_arr[1]
        comp_arr[si].flux.U=flux_arr[2]+flux_arr[3]
        comp_arr[si].flux.V=flux_arr[2]-flux_arr[3]
        
;        ;this will catch negatives in this form (negatives should not be possible, anyway)
;        IF max(flux_arr[0:(npol<2)-1]) GT 3.*min(flux_arr[0:(npol<2)-1]) THEN BEGIN
;            source_mask[additional_i[src_i]]=0
;            CONTINUE
;        ENDIF
        ;Make sure to update source uv model in "true sky" instrumental polarization i.e. 1/beam^2 frame.
        source_uv_vals=Exp(icomp*(2.*!Pi/dimension)*((comp_arr[si].x-dimension/2.)*xvals1+(comp_arr[si].y-elements/2.)*yvals1))
        FOR pol_i=0,npol-1 DO BEGIN
;            *model_uv[pol_i]+=comp_arr[si].flux.(pol_i)*beam_corr_src[pol_i]*source_uv 
            (*model_uv_full[pol_i])[uv_i_use]+=comp_arr[si].flux.(pol_i)*beam_corr_src[pol_i]*source_uv_vals
;            (*model_uv_full[pol_i])[uv_i_use]+=comp_arr[si].flux.(pol_i)*source_uv_vals
;            (*model_uv_full[pol_i])[uv_i_use]+=flux_arr[pol_i]*source_uv_vals
        ENDFOR
        
        si+=1
        IF si GE max_sources THEN BEGIN
            t4_0=Systime(1)
            t3+=t4_0-t3_0
            FOR pol_i=0,npol-1 DO BEGIN
                *model_uv_holo[pol_i]=holo_mapfn_apply(*model_uv_full[pol_i],*map_fn_arr[pol_i])*normalization
;                *model_uv[pol_i]=Dcomplexarr(dimension,elements)
            ENDFOR
            i3=0    
            t4+=Systime(1)-t4_0
            i2+=1                                        
            t10=Systime(1)-t0
            print,StrCompress(String(format='("Max sources found by iteration ",I," after ",I," seconds (convergence:",F,")")',i,t10,Stddev((image_use*beam_avg)[where(source_mask)],/nan)))
            converge_check[i2]=Stddev((image_use*beam_avg)[where(source_mask)],/nan)
            BREAK
        ENDIF
    ENDFOR
    IF si GE max_sources THEN BREAK
    t4_0=Systime(1)
    t3+=t4_0-t3_0
    IF i3 EQ 0 THEN flux_ref=Median(flux_arr[0:(npol<2)-1,*,*]) ;replace with median across all pol/freq/time
    IF (i3 GE mapfn_interval) OR (Median(flux_arr[0:(npol<2)-1,*,*]) LT flux_ref*mapfn_threshold) OR ((i+1) mod check_iter EQ 0) THEN BEGIN
        FOR pol_i=0,npol-1 DO BEGIN
            *model_uv_holo[pol_i]=holo_mapfn_apply(*model_uv_full[pol_i],*map_fn_arr[pol_i])*normalization
;            *model_uv[pol_i]=Dcomplexarr(dimension,elements)
        ENDFOR
        i3=0
    ENDIF ELSE i3+=1
    t4+=Systime(1)-t4_0
    
    IF (Round(i mod check_iter) EQ 0) AND (i GT 0) THEN BEGIN
        i2+=1
        t10=Systime(1)-t0
        IF not Keyword_Set(silent) THEN print,StrCompress(String(format='(I," : ",I," : ",I," : ",F)',i,si,t10,Stddev((image_use*beam_avg)[where(source_mask)],/nan)))
;        print,StrCompress(String(format='("At iteration ",I," with ",I," components after ",I," seconds (convergence:",F,")")',i,si,t10,Stddev(image_use[where(source_mask)],/nan)))
        converge_check[i2]=Stddev((image_use*beam_avg)[where(source_mask)],/nan)
        IF 2.*converge_check[i2] GT image_use[source_i] THEN BEGIN
            print,'Break after iteration',i,' from low signal to noise'
            converge_check2=converge_check2[0:i]
            converge_check=converge_check[0:i2]
            BREAK
        ENDIF
        IF converge_check[i2] GT converge_check[i2-1] THEN BEGIN
            print,'Break after iteration',i,' from lack of convergence'
            converge_check2=converge_check2[0:i]
            converge_check=converge_check[0:i2]
            BREAK
        ENDIF
    ENDIF
ENDFOR

;condense clean components
noise_map=Stddev((image_use*beam_avg)[where(source_mask)],/nan)*beam_corr_avg
source_array=Components2Sources(comp_arr,radius=0.5,noise_map=noise_map)

FOR pol_i=0,npol-1 DO BEGIN
    *residual_array[pol_i]=(*dirty_array[pol_i]-dirty_image_generate(*model_uv_holo[pol_i])*(*beam_correction[pol_i]))
ENDFOR  

t00=Systime(1)-t00
timing=[t00,t1,t2,t3,t4]
print,timing

END  