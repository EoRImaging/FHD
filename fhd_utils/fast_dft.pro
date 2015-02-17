FUNCTION fast_dft,x_loc,y_loc,xvals,yvals,dimension=dimension,elements=elements,degpix=degpix,flux=flux,$
    conserve_memory=conserve_memory,over_resolution=over_resolution,dft_kernel=dft_kernel


dimension_out=dimension*over_resolution
elements_out=elements*over_resolution

IF N_Elements(model_uv_full) LT n_pol THEN model_uv_full=Ptrarr(n_pol)
IF Min(Ptr_valid(model_uv_full[0:n_pol-1])) EQ 0 THEN BEGIN
    FOR pol_i=0,n_pol-1 DO model_uv_full[pol_i]=Ptr_new(Complexarr(dimension,elements))
ENDIF
FOR pol_i=0,n_pol-1 DO BEGIN
    model_img=fast_dft_subroutine(source_arr_out,pol_i=pol_i,resolution=32,dimension=dimension_out,dft_kernel=dft_kernel,over_resolution=over_resolution,_Extra=extra)
;    model_img=source_image_generate(source_arr_out,pol_i=pol_i,resolution=32,dimension=dimension_out,restored_beam_width=0.5,/conserve_flux,_Extra=extra)
    model_uv=fft_shift(FFT(fft_shift(model_img),/inverse)) ; normalization ??!!??!!
    model_uv=model_uv[dimension_out/2-dimension/2:dimension_out/2+dimension/2-1,elements_out/2-elements/2:elements_out/2+elements/2-1];*over_resolution^2.
    *model_uv_full[pol_i]+=model_uv
ENDFOR

RETURN,model_uv_full
END