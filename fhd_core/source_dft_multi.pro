PRO source_dft_multi,obs,source_array,model_uv_full,xvals=xvals,yvals=yvals,uv_i_use=uv_i_use,$
    conserve_memory=conserve_memory,polarization_map=polarization_map
IF N_Elements(conserve_memory) EQ 0 THEN conserve_memory=1
dimension=obs.dimension
elements=obs.elements
degpix=obs.degpix
n_pol=obs.n_pol

IF N_Elements(uv_i_use) EQ 0 THEN uv_i_use=Lindgen(dimension*elements/2.+dimension)

IF N_Elements(xvals) NE N_Elements(uv_i_use) THEN xvals=(meshgrid(dimension,elements,1))[uv_i_use]
IF N_Elements(yvals) NE N_Elements(uv_i_use) THEN yvals=(meshgrid(dimension,elements,2))[uv_i_use]

x_vec=source_array.x
y_vec=source_array.y

IF Max(Ptr_valid(model_uv_full)) EQ 0 THEN BEGIN
    model_uv_full=Ptrarr(n_pol,/allocate)
    FOR pol_i=0,n_pol-1 DO *model_uv_full[pol_i]=Complexarr(dimension,elements)
ENDIF

;set /no_extend since extended sources will not be read. 
; If you want extended sources, inflate the source list before calling this program
source_array_use=Stokes_cnv(source_array,p_map=polarization_map,/inverse,/no_extend) 
flux_arr=Ptrarr(n_pol)
FOR pol_i=0,n_pol-1 DO flux_arr[pol_i]=Ptr_new(source_array_use.flux.(pol_i))

model_uv_vals=source_dft(x_vec,y_vec,xvals,yvals,dimension=dimension,elements=elements,degpix=degpix,flux=flux_arr,conserve_memory=conserve_memory)
FOR pol_i=0,n_pol-1 DO (*model_uv_full[pol_i])[uv_i_use]+=*model_uv_vals[pol_i]
Ptr_free,model_uv_vals,flux_arr

END