FUNCTION source_array_import,file_path=file_path,no_extend=no_extend,$
    id_column=id_column,x_column=x_column,y_column=y_column,ra_column=ra_column,dec_column=dec_column,$
    ston_column=ston_column,frequency_column=frequency_column,flag_colum=flag_column,parent_column=parent_column,$
    flux_xx_column=flux_xx_column,flux_yy_column=flux_yy_column,$
    flux_xy_column=flux_xy_column,flux_yx_column=flux_yx_column,$
    flux_I_column=flux_I_column,flux_Q_column=flux_Q_column,$
    flux_U_column=flux_U_column,flux_V_column=flux_V_column,_Extra=extra

textfast,source_array,header,first_line=1,file_path=file_path,/read
id_column = header_column(header, column_use=id_column, colname='#id')
x_column = header_column(header, column_use=x_column, colname='x_loc')
y_column = header_column(header, column_use=y_column, colname='y_loc')
ra_column = header_column(header, column_use=ra_column, colname='RA')
dec_column = header_column(header, column_use=dec_column, colname='Dec')
ston_column = header_column(header, column_use=ston_column, colname='S/N')
frequency_column = header_column(header, column_use=frequency_column, colname='frequency')
flag_column = header_column(header, column_use=flag_column, colname='Flag')
parent_column = header_column(header, column_use=parent_column, colname='Parent')

flux_xx_column = header_column(header, column_use=flux_xx_column, colname='XX_apparent')
flux_yy_column = header_column(header, column_use=flux_yy_column, colname='YY_apparent')
flux_xy_column = header_column(header, column_use=flux_xy_column, colname='XY_apparent')
flux_yx_column = header_column(header, column_use=flux_yx_column, colname='YX_apparent')
flux_I_column = header_column(header, column_use=flux_I_column, colname='Stokes_I')
flux_Q_column = header_column(header, column_use=flux_Q_column, colname='Stokes_Q')
flux_U_column = header_column(header, column_use=flux_U_column, colname='Stokes_U')
flux_V_column = header_column(header, column_use=flux_V_column, colname='Stokes_V')

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

FUNCTION header_column, header, column_use=column_use, colname=colname

    header_entry = where(header EQ colname, n_match)
    IF N_Elements(column_use) EQ 0 THEN BEGIN
        IF n_match NE 1 THEN $
            message,String(format='("Source list header has missing or duplicate entries for ",A)', colname)
        column_use = header_entry[0]
    ENDIF
    RETURN, column_use
END

