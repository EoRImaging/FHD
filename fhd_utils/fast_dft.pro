FUNCTION fast_dft,x_vec,y_vec,dimension=dimension,elements=elements,degpix=degpix,flux_arr=flux_arr,$
    conserve_memory=conserve_memory,over_resolution=over_resolution,dft_kernel=dft_kernel

IF N_Elements(dft_kernel) EQ 0 THEN dft_kernel='sinc' ;grids/interpolates using Sin(pi*x)/(pi*x) function
dimension_over=dimension*over_resolution
elements_over=elements*over_resolution

x_vec_over=x_vec*over_resolution
y_vec_over=y_vec*over_resolution

IF size(flux_arr,/type) EQ 10 THEN BEGIN
    n_pol=Total(Ptr_valid(flux_arr)) 
    flux_arr_use=flux_arr
    mem_free=0
ENDIF ELSE BEGIN
    n_pol=1
    flux_arr_use=Ptr_new(flux_arr)
    mem_free=1
ENDELSE 


IF N_Elements(model_uv_full) LT n_pol THEN model_uv_full=Ptrarr(n_pol)
IF Min(Ptr_valid(model_uv_full[0:n_pol-1])) EQ 0 THEN BEGIN
    FOR pol_i=0,n_pol-1 DO model_uv_full[pol_i]=Ptr_new(Complexarr(dimension,elements))
ENDIF
FOR pol_i=0,n_pol-1 DO BEGIN
    model_img=fast_dft_subroutine(x_vec_over,y_vec_over,*flux_arr_use[pol_i],resolution=32,dimension=dimension_over,$
        dft_kernel=dft_kernel,over_resolution=over_resolution,/conserve_flux,_Extra=extra)
    model_uv=fft_shift(FFT(fft_shift(model_img),/inverse)) ; normalization ??!!??!!
    model_uv=model_uv[dimension_over/2-dimension/2:dimension_over/2+dimension/2-1,elements_over/2-elements/2:elements_over/2+elements/2-1];*over_resolution^2.
    *model_uv_full[pol_i]+=model_uv
ENDFOR

IF Keyword_Set(mem_free) THEN Ptr_free,flux_arr_use

RETURN,model_uv_full
END