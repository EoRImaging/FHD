
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
    beam_base=beam_base,beam_correction=beam_correction,normalization=normalization,file_path_fhd=file_path_fhd

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
local_max_radius=fhd.local_max_radius
pol_use=fhd.pol_use
independent_fit=fhd.independent_fit
reject_pol_sources=fhd.reject_pol_sources

icomp=Complex(0,1)
beam_max_threshold=fhd.beam_max_threshold
smooth_width=fhd.smooth_width
;color_frequency_correction=fltarr(nfreq)+1. ;remove same component from all frequencies, but allow to be different in the future

dimension=obs.dimension
elements=obs.elements
xvals=meshgrid(dimension,elements,1)-dimension/2
yvals=meshgrid(dimension,elements,2)-elements/2
rvals=Sqrt(xvals^2.+yvals^2.)

vis_coordinates,obs,ra_arr,dec_arr,astr=astr

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

;the particular set of beams read will be the ones specified by file_path_fhd.
;that will include all polarizations and frequencies, at ONE time snapshot
IF N_Elements(psf) EQ 0 THEN psf=beam_setup(obs,/restore_last) 
nfreq_beam=(size(psf.base,/dimension))[1]
beam_base=Ptrarr(n_pol,/allocate)
beam_correction=Ptrarr(n_pol,/allocate)
beam_i=Ptrarr(n_pol,/allocate)
beam_mask=Ptrarr(n_pol,/allocate)
source_mask=fltarr(dimension,elements)+1.; & source_mask[valid_radec_i]=1.
gain_array=fltarr(dimension,elements)+gain_factor
beam_avg=fltarr(dimension,elements)
nbeam_avg=0
FOR pol_i=0,n_pol-1 DO BEGIN ;this should be by frequency! and also by time
    *beam_base[pol_i]=beam_image(psf,pol_i=pol_i,dimension=dimension)
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


IF N_Elements(map_fn_arr) EQ 0 THEN map_fn_arr=Ptrarr(n_pol,/allocate)
weights_arr=Ptrarr(n_pol,/allocate)
dirty_array=Ptrarr(n_pol,/allocate)
residual_array=Ptrarr(n_pol,/allocate)
model_arr=Ptrarr(n_pol,/allocate)
normalization_arr=fltarr(n_pol) ;factor to normalize holo_mapfn_apply
;model_uv=Ptrarr(n_pol,/allocate)
model_uv_full=Ptrarr(n_pol,/allocate)
model_uv_holo=Ptrarr(n_pol,/allocate)

source_comp_init,comp_arr,n_sources=max_sources
pol_names=['xx','yy','xy','yx','I','Q','U','V'] 

pol_cut=1-histogram(pol_use,min=0,bin=1,nbins=n_pol)

;load holo map functions and initialize output arrays
dirty_image_composite=fltarr(dimension,elements)
dirty_image_composite_Q=fltarr(dimension,elements)
dirty_image_composite_U=fltarr(dimension,elements)
dirty_image_composite_V=fltarr(dimension,elements)
source_uv_mask=fltarr(dimension,elements)
FOR pol_i=0,n_pol-1 DO BEGIN
    IF pol_cut[pol_i] THEN CONTINUE
    IF N_Elements(*map_fn_arr[pol_i]) EQ 0 THEN BEGIN
        file_name_base='_mapfn_'+pol_names[pol_i]
        restore,file_path_fhd+file_name_base+'.sav' ;map_fn
;        holo_mapfn_generate,obs,/restore_last,map_fn=map_fn_single,polarization=pol_i
        *map_fn_arr[pol_i]=map_fn
    ENDIF
    weights_single=real_part(holo_mapfn_apply(complexarr(dimension,elements)+1,*map_fn_arr[pol_i]))
    normalization_arr[pol_i]=1./(dirty_image_generate(weights_single,baseline_threshold=baseline_threshold))[dimension/2.,elements/2.]
    normalization_arr[pol_i]*=((*beam_base[pol_i])[dimension/2.,elements/2.])^2.
    
    *weights_arr[pol_i]=weights_single
    source_uv_mask[where(weights_single)]=1.
ENDFOR
;normalization=mean(normalization_arr[0:n_pol-1])/2. ;factor of two accounts for complex conjugate
normalization=.25

FOR pol_i=0,n_pol-1 DO BEGIN    
    dirty_image_single=dirty_image_generate(*image_uv_arr[pol_i],baseline=baseline_threshold)*(*beam_correction[pol_i])^2.
    
;    ;TEMPORARY!
;    dirty_image_single*=normalization
    
    ;THIS IS THE ONE LINE TO UNCOMMENT IF RE-INSERTING POLARIZATION CORRECTION EFFECTS
;    ;TEMPORARY addition to fix polarization for off-zenith pointings
;    dirty_image_single*=*p_corr_simple[pol_i]
    
    ;xx, yy and xy, yx polarizations are treated seperately
    IF pol_i LE 1 THEN dirty_image_composite+=dirty_image_single
    IF pol_i GE 2 THEN dirty_image_composite_U+=dirty_image_single
    CASE pol_i OF 
        0:dirty_image_composite_Q+=dirty_image_single
        1:dirty_image_composite_Q-=dirty_image_single 
        2:dirty_image_composite_V+=dirty_image_single
        3:dirty_image_composite_V-=dirty_image_single
    ENDCASE
;    *model_uv[pol_i]=complexarr(dimension,elements)
    *model_uv_full[pol_i]=complexarr(dimension,elements)
    *model_uv_holo[pol_i]=complexarr(dimension,elements)
ENDFOR

;;TEMPORARY!
;normalization=normalization^2.

uv_i_use=where(source_uv_mask,n_uv_use)
uv_use_frac=Float(n_uv_use)/(dimension*elements)
print,"Fractional uv coverage: ",uv_use_frac,"normalization: ",normalization
xvals1=xvals[uv_i_use]
yvals1=yvals[uv_i_use]

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
dirty_image_composite_smooth[sm_xmin:sm_xmax,sm_ymin:sm_ymax]=$
    Median(dirty_image_composite[sm_xmin:sm_xmax,sm_ymin:sm_ymax]*beam_avg_box,smooth_width,/even) *beam_corr_box
converge_check[0]=(converge_check2[0]=$
    Stddev(((dirty_image_composite-dirty_image_composite_smooth)*beam_avg)[where(source_mask,n_pix0)],/nan))
print,"Initial convergence:",converge_check[0]

IF not Keyword_Set(silent) THEN print,'Iteration # : Component # : Elapsed time : Convergence'

si=0L
FOR i=0L,max_iter-1 DO BEGIN 
    t1_0=Systime(1)
    model_image_composite=fltarr(dimension,elements)
    model_image_composite_Q=fltarr(dimension,elements)
    model_image_composite_U=fltarr(dimension,elements)
    model_image_composite_V=fltarr(dimension,elements)
    FOR pol_i=0,n_pol-1 DO BEGIN 
        IF pol_cut[pol_i] THEN CONTINUE
        model_image_holo=dirty_image_generate(*model_uv_holo[pol_i])
        model_image=(model_image_holo)*(*beam_correction[pol_i])^2.
        
;        ;TEMPORARY addition to fix polarization for off-zenith pointings
;        model_image*=*p_corr_simple[pol_i]
        
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
    
    IF i mod Floor(1./gain_factor) EQ 0 THEN BEGIN
        image_use=dirty_image_composite-model_image_composite
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
        ENDIF ELSE IF n_pol GT 2 THEN BEGIN
            image_use_U=dirty_image_composite_U-model_image_composite_U
            image_smooth_U=Median(image_use_U[sm_xmin:sm_xmax,sm_ymin:sm_ymax]*beam_avg_box,smooth_width,/even)*beam_corr_box;Max_filter(image_use_U,smooth_width,/median,/circle)
            image_use_U[sm_xmin:sm_xmax,sm_ymin:sm_ymax]-=image_smooth_U
        ENDIF    
    ENDIF ELSE BEGIN
        image_use=dirty_image_composite-model_image_composite
        image_use[sm_xmin:sm_xmax,sm_ymin:sm_ymax]-=image_smooth ;uses previously calculated image_smooth!
        source_find_image=image_use*beam_avg

        IF Keyword_Set(independent_fit) THEN BEGIN
            image_use_Q=dirty_image_composite_Q-model_image_composite_Q
            image_use_U=dirty_image_composite_U-model_image_composite_U
            image_use_V=dirty_image_composite_V-model_image_composite_V
            image_use_Q[sm_xmin:sm_xmax,sm_ymin:sm_ymax]-=image_smooth_Q ;uses previously calculated image_smooth!
            image_use_U[sm_xmin:sm_xmax,sm_ymin:sm_ymax]-=image_smooth_U ;uses previously calculated image_smooth!
            image_use_V[sm_xmin:sm_xmax,sm_ymin:sm_ymax]-=image_smooth_V ;uses previously calculated image_smooth!
        ENDIF ELSE IF n_pol GT 2 THEN BEGIN
            image_use_U=dirty_image_composite_U-model_image_composite_U
            image_use_U[sm_xmin:sm_xmax,sm_ymin:sm_ymax]-=image_smooth_U ;uses previously calculated image_smooth!
        ENDIF    
    ENDELSE
        
   
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
            FOR pol_i=0,n_pol-1 DO BEGIN
                *model_uv_holo[pol_i]=holo_mapfn_apply(*model_uv_full[pol_i],*map_fn_arr[pol_i])*normalization
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
        xy2ad,xcen,ycen,astr,ra,dec
        comp_arr[si].x=xcen
        comp_arr[si].y=ycen
        comp_arr[si].ra=ra
        comp_arr[si].dec=dec
        
        beam_corr_src=fltarr(n_pol)
        beam_src=fltarr(n_pol)
        gain_factor_use=gain_array[additional_i[src_i]]
        FOR pol_i=0,n_pol-1 DO BEGIN   
            beam_corr_src[pol_i]=(*beam_correction[pol_i])[additional_i[src_i]]
            beam_src[pol_i]=(*beam_base[pol_i])[additional_i[src_i]]
            
            IF Keyword_Set(independent_fit) THEN BEGIN
                sign=(pol_i mod 2) ? -1:1
                IF pol_i LE 1 THEN flux_use=image_use[additional_i[src_i]]+sign*image_use_Q[additional_i[src_i]]
                IF pol_i GE 2 THEN flux_use=image_use_U[additional_i[src_i]]+sign*image_use_V[additional_i[src_i]]
            ENDIF ELSE IF pol_i LE 1 THEN flux_use=image_use[additional_i[src_i]] ELSE flux_use=image_use_U[additional_i[src_i]]
            
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

        ;Make sure to update source uv model in "true sky" instrumental polarization i.e. 1/beam^2 frame.
        source_uv_vals=Exp(icomp*(2.*!Pi/dimension)*((comp_arr[si].x-dimension/2.)*xvals1+(comp_arr[si].y-elements/2.)*yvals1))
        FOR pol_i=0,n_pol-1 DO BEGIN
            (*model_uv_full[pol_i])[uv_i_use]+=comp_arr[si].flux.(pol_i)*beam_corr_src[pol_i]*source_uv_vals
;            (*model_uv_full[pol_i])[uv_i_use]+=comp_arr[si].flux.(pol_i)*source_uv_vals
;            (*model_uv_full[pol_i])[uv_i_use]+=flux_arr[pol_i]*source_uv_vals
        ENDFOR
        
        si+=1
        IF si GE max_sources THEN BEGIN
            t4_0=Systime(1)
            t3+=t4_0-t3_0
            FOR pol_i=0,n_pol-1 DO BEGIN
                *model_uv_holo[pol_i]=holo_mapfn_apply(*model_uv_full[pol_i],*map_fn_arr[pol_i])*normalization
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
    IF i3 EQ 0 THEN flux_ref=Median(flux_arr[0:(n_pol<2)-1,*,*]) ;replace with median across all pol/freq/time
    IF (i3 GE mapfn_interval) OR (Median(flux_arr[0:(n_pol<2)-1,*,*]) LT flux_ref*mapfn_threshold) OR ((i+1) mod check_iter EQ 0) THEN BEGIN
        FOR pol_i=0,n_pol-1 DO BEGIN
            *model_uv_holo[pol_i]=holo_mapfn_apply(*model_uv_full[pol_i],*map_fn_arr[pol_i])*normalization
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

FOR pol_i=0,n_pol-1 DO BEGIN
    *residual_array[pol_i]=dirty_image_generate(*image_uv_arr[pol_i]-*model_uv_holo[pol_i])*(*beam_correction[pol_i])
ENDFOR  

t00=Systime(1)-t00
timing=[t00,t1,t2,t3,t4]
print,timing

END  