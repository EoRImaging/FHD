FUNCTION holographic_beam_map,obs,map_fn_ptr,beam_model_ptr=beam_model_ptr,x_use=x_use,y_use=y_use,$
    xvals=xvals,yvals=yvals,interpolate_full=interpolate_full

n_pol=obs.n_pol
dimension=obs.dimension
elements=obs.elements
;astr=obs.astr
degpix=obs.degpix

xvals0=meshgrid(dimension,elements,1)-dimension/2
yvals0=meshgrid(dimension,elements,2)-elements/2

nx=N_Elements(x_use)
IF N_Elements(y_use) EQ 0 THEN y_use=x_use
ny=N_Elements(y_use)
x_arr=Rebin(x_use,nx,ny,/sample)
y_arr=Rebin(transpose(y_use),nx,ny,/sample)

source_uv_mask2=fltarr(dimension,elements)
IF N_Elements(xvals)<N_Elements(yvals) EQ 0 THEN BEGIN
    FOR pol_i=0,n_pol-1 DO BEGIN
        weights_single=holo_mapfn_apply(complexarr(dimension,elements)+1,*map_fn_ptr[pol_i],/no_conj,/indexed)
        source_uv_mask2[where(weights_single)]=1
    ENDFOR
    
    uv_i_use2=where(source_uv_mask2,n_uv_use2)
    xvals=xvals0[uv_i_use2]
    yvals=yvals0[uv_i_use2]
ENDIF

ns=N_Elements(x_arr)
flux_fit_ptr=Ptrarr(n_pol,/allocate)
FOR pol_i=0,n_pol-1 DO *flux_fit_ptr[pol_i]=Fltarr(nx,ny)

FOR si=0L,ns-1 DO BEGIN
    sx=x_arr[si]
    sy=y_arr[si]
    flux_I=1.
    
    model_uv_vals=source_dft(sx,sy,xvals,yvals,dimension=dimension,elements=elements,degpix=degpix,flux=flux_I)
    model_uv_vals/=2. ;Pure Stokes I -> xx and yy instrumental pols
    model_uv=Complexarr(dimension,elements)
    model_uv[xvals+dimension/2,yvals+elements/2]=model_uv_vals
    FOR pol_i=0,n_pol-1 DO BEGIN
        model_uv_holo=holo_mapfn_apply(model_uv,map_fn_ptr[pol_i],/indexed)
        model_img_holo=dirty_image_generate(model_uv_holo,degpix=degpix)
        (*flux_fit_ptr[pol_i])[si]=model_img_holo[sx,sy]
;        [sx-local_max_radius:sx+local_max_radius,sy-local_max_radius:sy+local_max_radius]*source_fit_fn
    ENDFOR
ENDFOR
IF Keyword_Set(interpolate_full) THEN BEGIN
    holo_beam_ptr=Ptrarr(n_pol,/allocate)
    x_vec=Interpol(findgen(nx),x_use,findgen(dimension))
    y_vec=Interpol(findgen(ny),y_use,findgen(elements))
    FOR pol_i=0,n_pol-1 DO BEGIN
        
        holo_beam_map=Interpolate(*flux_fit_ptr[pol_i],x_vec,y_vec,/grid,cubic=-0.5)
        *holo_beam_ptr[pol_i]=holo_beam_map
    ENDFOR
ENDIF ELSE holo_beam_ptr=flux_fit_ptr

RETURN,holo_beam_ptr
END