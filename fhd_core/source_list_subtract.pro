PRO source_list_subtract,model_uv_full,model_uv_holo,obs=obs,image_uv_arr=image_uv_arr,source_list=source_list,$
    map_fn_arr=map_fn_arr,beam_base=beam_base,p_map_simple=p_map_simple,p_corr_simple=p_corr_simple,$
    normalization=normalization,fix_flux=fix_flux,independent_fit=independent_fit,max_iter=max_iter
    
vis_path_default,data_directory,filename,file_path,obs=obs,version=version
restore,file_path+'_fhd_params.sav'

npol=fhd.npol
beam_threshold=fhd.beam_threshold
local_max_radius=fhd.local_max_radius
pol_use=fhd.pol_use
;IF N_Elements(independent_fit) EQ 0 THEN independent_fit=fhd.independent_fit
reject_pol_sources=fhd.reject_pol_sources
IF N_Elements(max_iter) EQ 0 THEN max_iter=5.
IF not Keyword_Set(flux_fix) THEN max_iter=1.

icomp=Complex(0,1)
beam_max_threshold=fhd.beam_max_threshold
;beam_deriv_threshold=0.1
smooth_width=fhd.smooth_width
;color_frequency_correction=fltarr(nfreq)+1. ;remove same component from all frequencies, but allow to be different in the future

dimension=obs.dimension
elements=obs.elements
xvals=meshgrid(dimension,elements,1)-dimension/2
yvals=meshgrid(dimension,elements,2)-elements/2
x_use=source_list.x
y_use=source_list.y
ns=N_Elements(x_use)
flux_I_use=fltarr(ns) & FOR si=0,ns-1 DO flux_I_use[si]=(source_list[si].flux).I
flux_use=fltarr(npol,ns)
beam_corr_use=fltarr(npol,ns)
beam_use=fltarr(npol,ns)
p_map_use=fltarr(npol,ns)
p_corr_use=fltarr(npol,ns)
beam_correction=Ptrarr(npol,/allocate)

FOR pol_i=0,npol-1 DO BEGIN
    beam_use[pol_i,*]=(*beam_base[pol_i])[x_use,y_use]
    beam_corr_use[pol_i,*]=1./beam_use[pol_i,*]
    p_map_use[pol_i,*]=(*p_map_simple[pol_i])[x_use,y_use]
    p_corr_use[pol_i,*]=(*p_corr_simple[pol_i])[x_use,y_use]
    FOR si=0L,ns-1 DO flux_use[pol_i,si]=(source_list[si].flux).(pol_i)
    
    *beam_correction[pol_i]=weight_invert(*beam_base[pol_i],beam_threshold)
ENDFOR  

flux_use_fit=flux_use
beam_img_uv=Ptrarr(npol,/allocate)
FOR pol_i=0,npol-1 DO *beam_img_uv[pol_i]=Dirty_image_generate(*beam_base[pol_i])*dimension*elements
FOR iter=0.,max_iter-1 DO BEGIN

    IF size(model_uv_full,/type) EQ 10 THEN Ptr_free,model_uv_full
    IF size(model_uv_holo,/type) EQ 10 THEN Ptr_free,model_uv_holo
    
    model_uv_full=Ptrarr(npol,/allocate)
    FOR pol_i=0,npol-1 DO *model_uv_full[pol_i]=Complexarr(dimension,elements)
    IF Keyword_Set(independent_fit) THEN BEGIN
        FOR si=0L,ns-1 DO BEGIN
            model_uv=Exp(icomp*(2.*!Pi/dimension)*((x_use[si]-dimension/2.)*xvals+(y_use[si]-elements/2.)*yvals))
            FOR pol_i=0,npol-1 DO *model_uv_full[pol_i]+=flux_use_fit[pol_i,si]*beam_corr_use[pol_i,si]*model_uv
        ENDFOR
    ENDIF ELSE BEGIN
        FOR si=0L,ns-1 DO BEGIN
            model_uv=Exp(icomp*(2.*!Pi/dimension)*((x_use[si]-dimension/2.)*xvals+(y_use[si]-elements/2.)*yvals))
            FOR pol_i=0,npol-1 DO *model_uv_full[pol_i]+=flux_I_use[si]*p_map_use[pol_i,si]*model_uv
        ENDFOR
    ENDELSE
    model_uv_holo=Ptrarr(npol,/allocate)
    FOR pol_i=0,npol-1 DO *model_uv_holo[pol_i]=holo_mapfn_apply(*model_uv_full[pol_i],*map_fn_arr[pol_i])*normalization
    
    IF not Keyword_Set(fix_flux) THEN BEGIN
        Ptr_free,model_img_arr,dirty_img_arr,res_arr
        CONTINUE
    ENDIF
    
    
    model_img_arr=Ptrarr(npol,2,/allocate)
    dirty_img_arr=Ptrarr(npol,2,/allocate)
    FOR pol_i=0,npol-1 DO BEGIN
        *model_img_arr[pol_i,0]=dirty_image_generate(*model_uv_holo[pol_i])*(*beam_correction[pol_i])
        *model_img_arr[pol_i,1]=dirty_image_generate(*model_uv_holo[pol_i],/Hanning)*(*beam_correction[pol_i])
        *dirty_img_arr[pol_i,0]=dirty_image_generate(*image_uv_arr[pol_i])*(*beam_correction[pol_i])
        *dirty_img_arr[pol_i,1]=dirty_image_generate(*image_uv_arr[pol_i],/Hanning)*(*beam_correction[pol_i])
    ENDFOR
    
    beam_avg=fltarr(dimension,elements)
    model_Is=Fltarr(dimension,elements)
    dirty_Is=Fltarr(dimension,elements)
    FOR pol_i=0,(npol<2)-1 DO BEGIN
        beam_avg+=*beam_base[pol_i]*(*p_map_simple[pol_i])
        model_Is+=(*model_img_arr[pol_i,0]-*model_img_arr[pol_i,1])*(*p_corr_simple[pol_i])*(*beam_correction[pol_i])
        dirty_Is+=(*dirty_img_arr[pol_i,0]-*dirty_img_arr[pol_i,1])*(*p_corr_simple[pol_i])*(*beam_correction[pol_i])
    ENDFOR
    beam_avg/=(npol<2.)
    
    IF npol EQ 1 THEN BEGIN model_Is*=2. & dirty_Is*=2. & ENDIF
    residual_Is=(dirty_Is-model_Is)
    
    res_arr=Ptrarr(npol,/allocate)
    IF Keyword_Set(independent_fit) THEN BEGIN
        FOR pol_i=0,npol-1 DO *res_arr[pol_i]=((*dirty_img_arr[pol_i,0]-*dirty_img_arr[pol_i,1])-(*model_img_arr[pol_i,0]-*model_img_arr[pol_i,1]))
    ENDIF ELSE BEGIN
        FOR pol_i=0,npol-1 DO *res_arr[pol_i]=residual_Is*(*p_map_simple[pol_i])*(*beam_base[pol_i])
    ENDELSE
    
    FOR si=0L,ns-1 DO BEGIN
        box_xl=Round(x_use[si])-local_max_radius*3.
        box_xh=Round(x_use[si])+local_max_radius*3.
        box_yl=Round(y_use[si])-local_max_radius*3.
        box_yh=Round(y_use[si])+local_max_radius*3.
        box_dim=box_xh-box_xl+1
        window_fn=hanning(box_dim,box_dim)
        FOR pol_i=0,npol-1 DO BEGIN
            beam_box=(Fshift(*beam_img_uv[pol_i],dx=x_use[si]-dimension/2.,dy=y_use[si]-elements/2.))[box_xl:box_xh,box_yl:box_yh]>0
            beam_box/=Max(beam_box)
            residual_box=(*res_arr[pol_i])[box_xl:box_xh,box_yl:box_yh]
            i_use=where(beam_box AND residual_box,n_use)
            order_i=i_use[Sort(beam_box[i_use])]
            flux_fit=(linfit(beam_box[order_i],residual_box[order_i]))[1]
            flux_use_fit[pol_i,si]+=flux_fit
            source_list[si].flux.(pol_i)=flux_use_fit[pol_i,si]
        ENDFOR
        IF npol GT 1 THEN BEGIN
            source_list[si].flux.I=flux_use_fit[0,si]*beam_corr_use[0,si]*p_corr_use[0,si]$
                +flux_use_fit[1,si]*beam_corr_use[1,si]*p_corr_use[1,si]
            IF Keyword_Set(independent_fit) THEN source_list[si].flux.Q=$
                flux_use_fit[0,si]*beam_corr_use[0,si]*p_corr_use[0,si]$
                -flux_use_fit[1,si]*beam_corr_use[1,si]*p_corr_use[1,si]
        ENDIF ELSE BEGIN
            source_list[si].flux.I=2.*flux_use_fit[0,si]*beam_corr_use[0,si]*p_corr_use[0,si]
        ENDELSE
    ENDFOR
    Ptr_free,model_img_arr,dirty_img_arr,res_arr
ENDFOR

END