FUNCTION source_list_expand,source_arr,no_extend=no_extend

extend_i=where(Ptr_valid(source_arr.extend),n_ext,complement=point_i,ncomp=n_point)
IF Keyword_Set(no_extend) THEN n_ext=0

IF n_ext GT 0 THEN BEGIN
    ;Could include a check to make sure extended source components have all  the information of the primary source
;    FOR ext_i=0L,n_ext-1 DO 

    IF n_point GT 0 THEN source_arr_out=[source_arr[point_i],*(source_arr[extend_i[0]].extend)] $
        ELSE source_arr_out=*(source_arr[extend_i[0]].extend)
    source_arr_out[extend_i].extend=Pointer_copy(source_arr[extend_i].extend)
    FOR ext_i=1L,n_ext-1 DO source_arr_out=[source_arr_out,*(source_arr[extend_i[ext_i]].extend)]
ENDIF ELSE source_arr_out=source_arr

Ptr_free,source_arr_out.extend
cut_i=where(source_arr_out.flux.I EQ 0,n_cut,complement=i_use,ncomplement=n_use)
IF n_use EQ 0 THEN BEGIN
    print,"ERROR: no valid sources found to expand! All sources have zero flux"
    RETURN,source_arr
ENDIF
IF n_cut GT 0 THEN BEGIN
    source_cut=source_arr_out[cut_i]
    undefine_fhd,source_cut
    source_arr_out=source_arr_out[i_use]
ENDIF

RETURN,source_arr_out
END