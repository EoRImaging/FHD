FUNCTION fast_dft,x_vec,y_vec,dimension=dimension,elements=elements,degpix=degpix,flux_arr=flux_arr,silent=silent,$
    dft_threshold=dft_threshold,conserve_memory=conserve_memory,return_kernel=return_kernel,no_fft=no_fft,$
    double_precision=double_precision,_Extra=extra

IF N_Elements(elements) EQ 0 THEN elements=dimension

IF size(flux_arr,/type) EQ 10 THEN BEGIN
    n_dim=Total(Ptr_valid(flux_arr)) 
    flux_arr_use=flux_arr
    mem_free=0
ENDIF ELSE BEGIN
    n_dim=1
    flux_arr_use=Ptr_new(flux_arr)
    mem_free=1
ENDELSE 

IF N_Elements(model_uv_full) LT n_dim THEN model_uv_full=Ptrarr(size(flux_arr,/dimensions))
IF Min(Ptr_valid(model_uv_full[0:n_dim-1])) EQ 0 THEN BEGIN
    IF Keyword_Set(no_fft) THEN init_array=Fltarr(dimension,elements) ELSE init_array=Complexarr(dimension,elements)
    FOR pol_i=0,n_dim-1 DO model_uv_full[pol_i]=Ptr_new(init_array)
ENDIF
model_img=fast_dft_subroutine(x_vec,y_vec,flux_arr_use,dft_threshold=dft_threshold,silent=silent,$
    dimension=dimension,elements=elements,conserve_memory=conserve_memory,return_kernel=return_kernel,$
    double_precision=double_precision)
FOR pol_i=0,n_dim-1 DO BEGIN
    IF Keyword_Set(no_fft) THEN model_uv=*model_img[pol_i] ELSE $
        model_uv=fft_shift(FFT(fft_shift(*model_img[pol_i]),/inverse)) ;normalization seems okay
    *model_uv_full[pol_i]+=model_uv
ENDFOR

IF Keyword_Set(mem_free) THEN undefine_fhd,flux_arr_use
undefine_fhd,model_img
RETURN,model_uv_full
END