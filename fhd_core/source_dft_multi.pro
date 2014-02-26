PRO source_dft_multi,obs,source_array,model_uv_full,xvals=xvals,yvals=yvals,uv_i_use=uv_i_use,conserve_memory=conserve_memory
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
flux_I=source_array.flux.I
flux_Q=source_array.flux.Q
flux_U=source_array.flux.U
flux_V=source_array.flux.V
model_uv_stks=Ptrarr(4,/allocate)

IF Max(Ptr_valid(model_uv_full)) EQ 0 THEN BEGIN
    model_uv_full=Ptrarr(n_pol,/allocate)
    FOR pol_i=0,n_pol-1 DO *model_uv_full[pol_i]=Complexarr(dimension,elements)
ENDIF

*model_uv_stks[0]=source_dft(x_vec,y_vec,xvals,yvals,dimension=dimension,elements=elements,degpix=degpix,flux=flux_I,conserve_memory=conserve_memory)
IF Total(flux_Q) EQ 0 THEN *model_uv_stks[1]=0. $
    ELSE *model_uv_stks[1]=source_dft(x_vec,y_vec,xvals,yvals,dimension=dimension,elements=elements,degpix=degpix,flux=flux_Q,conserve_memory=conserve_memory) 
IF Total(flux_U) EQ 0 THEN *model_uv_stks[2]=0. $
    ELSE *model_uv_stks[2]=source_dft(x_vec,y_vec,xvals,yvals,dimension=dimension,elements=elements,degpix=degpix,flux=flux_U,conserve_memory=conserve_memory)
IF Total(flux_V) EQ 0 THEN *model_uv_stks[3]=0. $
    ELSE *model_uv_stks[3]=source_dft(x_vec,y_vec,xvals,yvals,dimension=dimension,elements=elements,degpix=degpix,flux=flux_V,conserve_memory=conserve_memory)
SWITCH n_pol OF
    4:(*model_uv_full[3])[uv_i_use]+=(*model_uv_stks[2]-*model_uv_stks[3])/2.
    3:(*model_uv_full[2])[uv_i_use]+=(*model_uv_stks[2]+*model_uv_stks[3])/2.
    2:(*model_uv_full[1])[uv_i_use]+=(*model_uv_stks[0]-*model_uv_stks[1])/2.
    1:(*model_uv_full[0])[uv_i_use]+=(*model_uv_stks[0]+*model_uv_stks[1])/2.
ENDSWITCH
Ptr_free,model_uv_stks

END