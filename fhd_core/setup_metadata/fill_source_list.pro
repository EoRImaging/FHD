FUNCTION fill_source_list,source_list_in, tags, tag_ids
    ; Create a new source list structure from only the tags common to both the current definition and the input structure

    FOR tag_i=0,N_Elements(tags)-1 DO BEGIN
        IF N_Elements(struct) EQ 0 THEN struct=create_struct(tags[tag_i],source_list_in.(tag_ids[tag_i])) $
            ELSE struct=create_struct(tags[tag_i],source_list_in.(tag_ids[tag_i]),struct)
    ENDFOR
    source_list = source_comp_init(_Extra=struct)
    RETURN, source_list
END
