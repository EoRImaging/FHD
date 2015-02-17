PRO source_dft_multi,obs,jones,source_array,model_uv_full,xvals=xvals,yvals=yvals,uv_i_use=uv_i_use,$
    conserve_memory=conserve_memory,dft_approximation=dft_approximation,_Extra=extra
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

IF Keyword_Set(dft_approximation) THEN BEGIN
;only use approximation if it will actually be faster than the DFT
    IF dft_approximation GT 1 THEN over_resolution=dft_approximation ELSE over_resolution=4
    IF (over_resolution^2.)*(dimension*elements) GT Float(N_Elements(x_vec))*Float(N_Elements(uv_i_use)) $
        THEN over_resolution=0 
ENDIF
IF Keyword_Set(over_resolution) THEN BEGIN
    obs_out=fhd_struct_update_obs(obs,dimension=obs.dimension*over_resolution,kbin=obs.kpix) ;this will calculate a new astr structure
    dimension_out=obs_out.dimension
    elements_out=obs_out.elements
    astr_out=obs_out.astr
    ad2xy,source_array_use.ra,source_array_use.dec,astr_out,x_vec,y_vec
    obs_out=0 ;don't use undefine_fhd,obs_out because that would undefine pointers still used by the obs structure
    
    flux_arr=Ptrarr(n_pol)
    FOR pol_i=0,n_pol-1 DO flux_arr[pol_i]=Ptr_new(source_array_use.flux.(pol_i))

    model_uv_new=fast_dft(x_vec,y_vec,xvals,yvals,dimension=dimension,elements=elements,degpix=degpix,flux=flux_arr,$
        conserve_memory=conserve_memory,over_resolution=over_resolution,_Extra=extra)

    *model_uv_full[pol_i]+=*model_uv_new[pol_i]
ENDIF ELSE BEGIN
    flux_arr=Ptrarr(n_pol)
    FOR pol_i=0,n_pol-1 DO flux_arr[pol_i]=Ptr_new(source_array_use.flux.(pol_i))
    
    model_uv_vals=source_dft(x_vec,y_vec,xvals,yvals,dimension=dimension,elements=elements,degpix=degpix,flux=flux_arr,conserve_memory=conserve_memory)
    FOR pol_i=0,n_pol-1 DO (*model_uv_full[pol_i])[uv_i_use]+=*model_uv_vals[pol_i]
    Ptr_free,model_uv_vals,flux_arr
ENDELSE
END