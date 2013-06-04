FUNCTION source_dft_model,obs,source_list,t_model=t_model,sigma_threshold=sigma_threshold,$
    no_extend=no_extend,unpolarized=unpolarized,uv_mask=uv_mask
t_model0=Systime(1)
n_pol=obs.n_pol
dimension=obs.dimension
elements=obs.elements
degpix=obs.degpix

IF N_Elements(uv_mask) EQ 0 THEN uv_mask=fltarr(dimension,elements)+1
uv_i_use=where(uv_mask)
xvals=Float(uv_i_use mod dimension)-dimension/2
yvals=Float(Floor(uv_i_use/dimension))-elements/2

model_uv_arr=Ptrarr(n_pol,/allocate)
FOR pol_i=0,n_pol-1 DO *model_uv_arr[pol_i]=Complexarr(dimension,elements)
model_uv_stks=Ptrarr(4,/allocate)

src_arr=source_list
IF Keyword_Set(sigma_threshold) THEN BEGIN
    ston_use=where(src_arr.ston GE sigma_threshold,n_use)
    IF n_use GT 0 THEN src_arr=src_arr[ston_use]
ENDIF

extend_i=where(Ptr_valid(src_arr.extend),n_ext,complement=point_i,ncomp=n_point)
IF Keyword_Set(no_extend) THEN n_ext=0
IF n_ext GT 0 THEN BEGIN
    IF n_point GT 0 THEN BEGIN
        flux_I=src_arr[point_i].flux.I
        flux_Q=src_arr[point_i].flux.Q
        flux_U=src_arr[point_i].flux.U
        flux_V=src_arr[point_i].flux.V
        x_vec=src_arr[point_i].x
        y_vec=src_arr[point_i].y
    ENDIF ELSE flux_I=(flux_Q=(flux_U=(flux_V=(x_vec=(y_vec=[0])))))
    FOR ex_i=0L,n_ext-1 DO BEGIN
        component_list=*src_arr[extend_i[ex_i]].extend
        flux_I=[flux_I,component_list.flux.I]
        flux_Q=[flux_Q,component_list.flux.Q]
        flux_U=[flux_U,component_list.flux.U]
        flux_V=[flux_V,component_list.flux.V]
        x_vec=[x_vec,component_list.x]
        y_vec=[y_vec,component_list.y]
    ENDFOR
ENDIF ELSE BEGIN
    flux_I=src_arr.flux.I
    flux_Q=src_arr.flux.Q
    flux_U=src_arr.flux.U
    flux_V=src_arr.flux.V
    x_vec=src_arr.x
    y_vec=src_arr.y
ENDELSE

IF Keyword_Set(unpolarized) THEN flux_Q=(flux_V=0.)

*model_uv_stks[0]=source_dft(x_vec,y_vec,xvals,yvals,dimension=dimension,elements=elements,degpix=degpix,flux=flux_I)
IF Total(flux_Q) EQ 0 THEN *model_uv_stks[1]=0. $
    ELSE *model_uv_stks[1]=source_dft(x_vec,y_vec,xvals,yvals,dimension=dimension,elements=elements,degpix=degpix,flux=flux_Q) 
IF Total(flux_U) EQ 0 THEN *model_uv_stks[2]=0. $
    ELSE *model_uv_stks[2]=source_dft(x_vec,y_vec,xvals,yvals,dimension=dimension,elements=elements,degpix=degpix,flux=flux_U)
IF Total(flux_V) EQ 0 THEN *model_uv_stks[3]=0. $
    ELSE *model_uv_stks[3]=source_dft(x_vec,y_vec,xvals,yvals,dimension=dimension,elements=elements,degpix=degpix,flux=flux_V)
SWITCH n_pol OF
    4:(*model_uv_arr[3])[uv_i_use]+=(*model_uv_stks[2]-*model_uv_stks[3])/2.
    3:(*model_uv_arr[2])[uv_i_use]+=(*model_uv_stks[2]+*model_uv_stks[3])/2.
    2:(*model_uv_arr[1])[uv_i_use]+=(*model_uv_stks[0]-*model_uv_stks[1])/2.
    1:(*model_uv_arr[0])[uv_i_use]+=(*model_uv_stks[0]+*model_uv_stks[1])/2.
ENDSWITCH
t_model=Systime(1)-t_model0

RETURN,model_uv_arr
END