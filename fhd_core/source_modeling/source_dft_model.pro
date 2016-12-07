FUNCTION source_dft_model,obs,jones,source_list,t_model=t_model,sigma_threshold=sigma_threshold,exclude_flagged_sources=exclude_flagged_sources,$
    no_extend=no_extend,unpolarized=unpolarized,uv_mask=uv_mask,spectral_model_uv_arr=spectral_model_uv_arr,_Extra=extra
t_model0=Systime(1)
n_pol=obs.n_pol
dimension=obs.dimension
elements=obs.elements
degpix=obs.degpix

print, 'Source List:'
print, size(source_list)

IF N_Elements(uv_mask) EQ 0 THEN uv_mask=fltarr(dimension,elements)+1
uv_i_use=where(uv_mask)
xvals=Float(uv_i_use mod dimension)-dimension/2
yvals=Float(Floor(uv_i_use/dimension))-elements/2

src_arr=source_list
IF Keyword_Set(sigma_threshold) THEN BEGIN
    ston_use=where(src_arr.ston GE sigma_threshold,n_use)
    IF n_use GT 0 THEN src_arr=src_arr[ston_use]
ENDIF
IF Keyword_Set(exclude_flagged_sources) THEN BEGIN
    IF Tag_Exist(src_arr,'flag') THEN src_i=where(src_arr.flag EQ 0,n_src_use) 
    IF Keyword_Set(n_src_use) THEN src_arr=src_arr[src_i]
ENDIF 
src_arr_use=source_list_expand(src_arr,no_extend=no_extend)
IF Keyword_Set(unpolarized) THEN BEGIN
    src_arr_use.flux.Q=0.
    src_arr_use.flux.V=0.
ENDIF
source_dft_multi,obs,jones,src_arr_use,model_uv_arr,xvals=xvals,yvals=yvals,uv_i_use=uv_i_use,spectral_model_uv_arr=spectral_model_uv_arr,_Extra=extra

undefine_fhd,src_arr_use
t_model=Systime(1)-t_model0
RETURN,model_uv_arr
END
