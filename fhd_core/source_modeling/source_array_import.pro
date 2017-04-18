FUNCTION source_array_import,file_path=file_path,no_extend=no_extend,$
    id_column=id_column,x_column=x_column,y_column=y_column,ra_column=ra_column,dec_column=dec_column,$
    ston_column=ston_column,frequency_column=frequency_column,flag_colum=flag_column,parent_column=parent_column,$
    flux_xx_column=flux_xx_column,flux_yy_column=flux_yy_column,$
    flux_xy_column=flux_xy_column,flux_yx_column=flux_yx_column,$
    flux_I_column=flux_I_column,flux_Q_column=flux_Q_column,$
    flux_U_column=flux_U_column,flux_V_column=flux_V_column,_Extra=extra

textfast,source_array,header,first_line=1,file_path=file_path,/read
IF N_Elements(id_column) EQ 0 THEN id_column=(where(header EQ '#id'))[0]
IF N_Elements(x_column) EQ 0 THEN x_column=(where(header EQ 'x_loc'))[0]
IF N_Elements(y_column) EQ 0 THEN y_column=(where(header EQ 'y_loc'))[0]
IF N_Elements(ra_column) EQ 0 THEN ra_column=(where(header EQ 'RA'))[0]
IF N_Elements(dec_column) EQ 0 THEN dec_column=(where(header EQ 'Dec'))[0]
IF N_Elements(ston_column) EQ 0 THEN ston_column=(where(header EQ 'S/N'))[0]
IF N_Elements(frequency_column) EQ 0 THEN frequency_column=(where(header EQ 'frequency'))[0]
IF N_Elements(flag_column) EQ 0 THEN flag_column = (where(header EQ 'Flag'))[0]
IF N_Elements(parent_column) EQ 0 THEN parent_column = (where(header EQ 'Parent'))[0]

IF N_Elements(flux_xx_column) EQ 0 THEN flux_xx_column=(where(header EQ 'XX_apparent'))[0]
IF N_Elements(flux_yy_column) EQ 0 THEN flux_yy_column=(where(header EQ 'YY_apparent'))[0]
IF N_Elements(flux_xy_column) EQ 0 THEN flux_xy_column=(where(header EQ 'XY_apparent'))[0]
IF N_Elements(flux_yx_column) EQ 0 THEN flux_yx_column=(where(header EQ 'YX_apparent'))[0]
IF N_Elements(flux_I_column) EQ 0 THEN flux_I_column=(where(header EQ 'Stokes_I'))[0]
IF N_Elements(flux_Q_column) EQ 0 THEN flux_Q_column=(where(header EQ 'Stokes_Q'))[0]
IF N_Elements(flux_U_column) EQ 0 THEN flux_U_column=(where(header EQ 'Stokes_U'))[0]
IF N_Elements(flux_V_column) EQ 0 THEN flux_V_column=(where(header EQ 'Stokes_V'))[0]

parent_id = Reform(source_array[parent_column,*])
parent_i = where(parent_id EQ -1, n_parent)

source_array_out = source_array_import_subroutine(source_array[*,parent_i],$
    id_column=id_column,x_column=x_column,y_column=y_column,ra_column=ra_column,dec_column=dec_column,$
    ston_column=ston_column,frequency_column=frequency_column,flag_colum=flag_column,parent_column=parent_column,$
    flux_xx_column=flux_xx_column,flux_yy_column=flux_yy_column,$
    flux_xy_column=flux_xy_column,flux_yx_column=flux_yx_column,$
    flux_I_column=flux_I_column,flux_Q_column=flux_Q_column,$
    flux_U_column=flux_U_column,flux_V_column=flux_V_column)

extend_i = where(parent_id GE 0, n_extend)
IF Keyword_Set(no_extend) THEN n_extend=0
IF n_extend GT 0 THEN BEGIN
    parent_hist = histogram(parent_id,binsize=1,reverse_indices=ri,min=0)
    parent_i_use = where(parent_hist, n_parent_use)
    FOR i=0,n_parent_use-1 DO BEGIN
        p_i = parent_i_use[i]
        ext_i_use = ri[ri[p_i]:ri[p_i+1]-1]
        sa = source_array_import_subroutine(source_array[*,ext_i_use],$
            id_column=id_column,x_column=x_column,y_column=y_column,ra_column=ra_column,dec_column=dec_column,$
            ston_column=ston_column,frequency_column=frequency_column,flag_colum=flag_column,parent_column=parent_column,$
            flux_xx_column=flux_xx_column,flux_yy_column=flux_yy_column,$
            flux_xy_column=flux_xy_column,flux_yx_column=flux_yx_column,$
            flux_I_column=flux_I_column,flux_Q_column=flux_Q_column,$
            flux_U_column=flux_U_column,flux_V_column=flux_V_column)
        source_array_out[p_i].extend = Ptr_new(sa)
    ENDFOR
ENDIF

RETURN, source_array_out
END

FUNCTION source_array_import_subroutine,source_array,$
    id_column=id_column,x_column=x_column,y_column=y_column,ra_column=ra_column,dec_column=dec_column,$
    ston_column=ston_column,frequency_column=frequency_column,flag_colum=flag_column,parent_column=parent_column,$
    flux_xx_column=flux_xx_column,flux_yy_column=flux_yy_column,$
    flux_xy_column=flux_xy_column,flux_yx_column=flux_yx_column,$
    flux_I_column=flux_I_column,flux_Q_column=flux_Q_column,$
    flux_U_column=flux_U_column,flux_V_column=flux_V_column


sa = source_comp_init(xvals=Reform(source_array[x_column,*]),$
                      yvals=Reform(source_array[y_column,*]),$
                      frequency=Reform(source_array[frequency_column,*]),$
                      ra=Reform(source_array[ra_column,*]),$
                      dec=Reform(source_array[dec_column,*]),$
                      id=Reform(source_array[id_column,*]),$
                      StoN=Reform(source_array[ston_column,*]))
sa.flux.xx = Reform(source_array[flux_xx_column,*])
sa.flux.yy = Reform(source_array[flux_yy_column,*])
sa.flux.xy = Reform(source_array[flux_xy_column,*])
sa.flux.yx = Reform(source_array[flux_yx_column,*])
sa.flux.I = Reform(source_array[flux_I_column,*])
sa.flux.Q = Reform(source_array[flux_Q_column,*])
sa.flux.U = Reform(source_array[flux_U_column,*])
sa.flux.V = Reform(source_array[flux_V_column,*])
RETURN, sa
END