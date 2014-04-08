FUNCTION source_dft_model,obs,jones,source_list,t_model=t_model,sigma_threshold=sigma_threshold,$
    no_extend=no_extend,unpolarized=unpolarized,uv_mask=uv_mask,conserve_memory=conserve_memory,polarization_map=polarization_map
t_model0=Systime(1)
IF N_Elements(conserve_memory) EQ 0 THEN conserve_memory=1
n_pol=obs.n_pol
dimension=obs.dimension
elements=obs.elements
degpix=obs.degpix

IF N_Elements(uv_mask) EQ 0 THEN uv_mask=fltarr(dimension,elements)+1
uv_i_use=where(uv_mask)
xvals=Float(uv_i_use mod dimension)-dimension/2
yvals=Float(Floor(uv_i_use/dimension))-elements/2

src_arr=source_list
IF Keyword_Set(sigma_threshold) THEN BEGIN
    ston_use=where(src_arr.ston GE sigma_threshold,n_use)
    IF n_use GT 0 THEN src_arr=src_arr[ston_use]
ENDIF

extend_i=where(Ptr_valid(src_arr.extend),n_ext,complement=point_i,ncomp=n_point)
IF Keyword_Set(no_extend) THEN n_ext=0

IF n_ext GT 0 THEN BEGIN
    IF n_point GT 0 THEN src_arr_use=[src_arr[point_i],*(src_arr[extend_i[0]].extend)] $
        ELSE src_arr_use=*(src_arr[extend_i[ext_i]].extend)
    FOR ext_i=1L,n_ext-1 DO src_arr_use=[src_arr_use,*(src_arr[extend_i[ext_i]].extend)]
ENDIF ELSE src_arr_use=src_arr

src_arr_use.extend=Ptr_new()
IF Keyword_Set(unpolarized) THEN BEGIN
    src_arr_use.flux.Q=0.
    src_arr_use.flux.V=0.
ENDIF
source_dft_multi,obs,jones,src_arr_use,model_uv_arr,xvals=xvals,yvals=yvals,uv_i_use=uv_i_use,$
    conserve_memory=conserve_memory

undefine_fhd,src_arr_use
t_model=Systime(1)-t_model0
RETURN,model_uv_arr
END