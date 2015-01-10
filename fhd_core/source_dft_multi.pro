PRO source_dft_multi,obs,jones,source_array,model_uv_full,xvals=xvals,yvals=yvals,uv_i_use=uv_i_use,$
    conserve_memory=conserve_memory,_Extra=extra
IF N_Elements(conserve_memory) EQ 0 THEN conserve_memory=1
dimension=obs.dimension
elements=obs.elements
degpix=obs.degpix
n_pol=obs.n_pol

IF N_Elements(uv_i_use) EQ 0 THEN uv_i_use=Lindgen(dimension*elements)

IF N_Elements(xvals) NE N_Elements(uv_i_use) THEN xvals=(meshgrid(dimension,elements,1))[uv_i_use]-dimension/2
IF N_Elements(yvals) NE N_Elements(uv_i_use) THEN yvals=(meshgrid(dimension,elements,2))[uv_i_use]-elements/2

x_vec=source_array.x
y_vec=source_array.y

IF Max(Ptr_valid(model_uv_full)) EQ 0 THEN BEGIN
    model_uv_full=Ptrarr(n_pol,/allocate)
    FOR pol_i=0,n_pol-1 DO *model_uv_full[pol_i]=Complexarr(dimension,elements)
ENDIF

;set /no_extend since extended sources will not be read. 
; If you want extended sources, inflate the source list before calling this program
source_array_use=Stokes_cnv(source_array,jones,/inverse,/no_extend,_Extra=extra) 
FOR pol_i=0,n_pol-1 DO BEGIN
    over_resolution=4
    obs_out=fhd_struct_update_obs(obs,dimension=obs.dimension*over_resolution,kbin=obs.kpix)
    dimension_out=obs_out.dimension
    elements_out=obs_out.elements
    astr_out=obs_out.astr
    
    source_arr_out=source_array_use
    ad2xy,source_array_use.ra,source_array_use.dec,astr_out,sx,sy
    source_arr_out.x=sx & source_arr_out.y=sy
    
    extend_test=where(Ptr_valid(source_array_use.extend),n_extend)
    IF n_extend GT 0 THEN BEGIN
        FOR ext_i=0L,n_extend-1 DO BEGIN
            comp_arr_out=*source_array_use[extend_test[ext_i]].extend
            ad2xy,comp_arr_out.ra,comp_arr_out.dec,astr_out,cx,cy
            comp_arr_out.x=cx & comp_arr_out.y=cy
            source_arr_out[extend_test[ext_i]].extend=Ptr_new(comp_arr_out)
        ENDFOR
    ENDIF
    model_img=source_image_generate(source_arr_out,obs_out,pol_i=pol_i,resolution=32,dimension=dimension_out,restored_beam_width=0.5,/conserve_flux,_Extra=extra)
    model_uv=fft_shift(FFT(fft_shift(model_img),/inverse))
    model_uv=model_uv[dimension_out/2-dimension/2:dimension_out/2+dimension/2-1,elements_out/2-elements/2:elements_out/2+elements/2-1];*over_resolution^2.
    *model_uv_full[pol_i]+=model_uv
ENDFOR

;flux_arr=Ptrarr(n_pol)
;FOR pol_i=0,n_pol-1 DO flux_arr[pol_i]=Ptr_new(source_array_use.flux.(pol_i))
;
;model_uv_vals=source_dft(x_vec,y_vec,xvals,yvals,dimension=dimension,elements=elements,degpix=degpix,flux=flux_arr,conserve_memory=conserve_memory)
;FOR pol_i=0,n_pol-1 DO (*model_uv_full[pol_i])[uv_i_use]+=*model_uv_vals[pol_i]
;Ptr_free,model_uv_vals,flux_arr

END