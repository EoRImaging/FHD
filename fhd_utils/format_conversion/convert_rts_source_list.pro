FUNCTION parse_rts_source_list,rts_text_array,source_i=source_i,default_spectral_index=default_spectral_index
    label_col=StrUPcase(reform(rts_text_array[0,*]))
    component_i=where(label_col EQ 'COMPONENT',n_component)
    endcomponent_i=where(label_col EQ 'ENDCOMPONENT')
    
    IF n_component EQ 0 THEN BEGIN
        freq_i=where(label_col EQ 'FREQ',n_freq)
        freq=rts_text_array[1,freq_i]
        flux=rts_text_array[2,freq_i]
        IF n_freq EQ 1 THEN alpha=default_spectral_index
            
        ENDIF ELSE BEGIN
        
        ENDELSE
        source_component=source_comp_init(n_source=1)
    ENDIF ELSE BEGIN
        endcomponent_i=[component_i[0]-1,endcomponent_i]
        component_i=[0,component_i]
        n_comp=n_component+1 ;include the main source
        FOR c_i=0,n_comp-1 DO BEGIN
            freq_i=where(label_col[component_i[c_i]:endcomponent_i[c_i]] EQ 'FREQ',n_freq)+component_i[c_i]
            
        ENDFOR
    ENDELSE

RETURN,source_component
END

PRO convert_rts_source_list,to_rts=to_rts,to_fhd=to_fhd,rts_filepath=rts_filepath,fhd_filepath=fhd_filepath,default_spectral_index=default_spectral_index

rts_filepath='C:\Users\Ian\Desktop\compare\srclist_puma-v2_complete_1061316296_peel3000.txt'
to_fhd=1

IF N_Elements(default_spectral_index) EQ 0 THEN default_spectral_index=-0.8
IF Keyword_Set(to_rts) THEN to_fhd=0
IF Keyword_Set(to_fhd) THEN to_rts=0
IF N_Elements(to_rts) EQ 0 THEN to_rts=1

IF Keyword_Set(to_rts) THEN BEGIN
    
ENDIF ELSE BEGIN
    Textfast,data_array,/read,/string,delimiter=' ',column_list=indgen(6),file_path=rts_filepath
    label_col=StrUPcase(reform(data_array[0,*]))
    len=N_Elements(label_col)
    
    group_i=where(label_col EQ 'SOURCE' OR label_col EQ 'COMPONENT',n_group)
    group_length=intarr(n_group)
    IF n_group EQ 1 THEN group_length[0]=len ELSE BEGIN
        group_length[0:n_group-2]=group_i[1:*]-group_i[0:n_group-2]
        group_length[n_group-1]=len-group_i[n_group-1]
    ENDELSE
    source_i=where(label_col EQ 'SOURCE',n_source)
    endsource_i=where(label_col EQ 'ENDSOURCE')
    component_i=where(label_col EQ 'COMPONENT',n_component)
    
;    ra_arr=Float(Reform(data_array[2,source_i]))*15. ;RTS Right Ascension is in decimal hours
;    dec_arr=Float(Reform(data_array[3,source_i]))
    source_array=source_comp_init(n_sources=n_source,id=lindgen(n_source));,ra=ra_arr,dec=dec_arr)
    
    FOR s_i=0L,n_source-1 DO source_array[s_i]=parse_rts_source_list(data_array[*,source_i[s_i]:endsource_i[s_i]],source_i=s_i,default_spectral_index=default_spectral_index)
    
;    FOR g_i=0L,n_group-1 DO BEGIN
;        gtype=label_col[group_i[g_i]]
;        CASE gtype OF
;            'SOURCE':BEGIN
;                s_i+=1
;            END
;            'COMPONENT':
;            ELSE:
;        ENDCASE 
;    ENDFOR
ENDELSE

END