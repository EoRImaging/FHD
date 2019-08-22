FUNCTION load_source_catalog, catalog_filepath, varname=varname
    ; Restore a source catalog from a .sav file, being careful about changed structure definitions

    IF N_Elements(varname) EQ 0 THEN varname="catalog"

    ;define structure BEFORE restoring, in case the definition has changed
    cat_init=source_comp_init(n_sources=0)
    source_cat = getvar_savefile(catalog_filepath, varname, /compatibility_mode)

    extend_i=where(Ptr_valid(source_cat.extend),n_ext,complement=point_i,ncomp=n_point)
    ignore_tags = ['EXTEND']

    ; If the source list structure definition has changed we have to update the structure ourselves
    ; compatibility_mode is supposed to take care of that, but does not appear to work for source catalogs
    source_tags = tag_names(cat_init)
    restored_tags = tag_names(source_cat)
    ns_tags = N_Elements(source_tags)
    nr_tags = N_Elements(restored_tags)

    n_missing = 0
    n_extra = 0
    missing_tags = []
    extra_tags = []
    good_tag_ids = []
    good_tag_names = []
    FOR tag_i=0,ns_tags-1 DO BEGIN
        IF Max(strmatch(restored_tags,source_tags[tag_i],/fold_case)) EQ 0 THEN BEGIN
            n_missing += 1
            missing_tags = [missing_tags, source_tags[tag_i]]
        ENDIF
    ENDFOR
    FOR tag_i=0,nr_tags-1 DO BEGIN
        IF Max(strmatch(source_tags,restored_tags[tag_i],/fold_case)) EQ 0 THEN BEGIN
            n_extra += 1
            extra_tags = [extra_tags, restored_tags[tag_i]]
        ENDIF ELSE BEGIN
            IF Max(strmatch(ignore_tags,restored_tags[tag_i],/fold_case)) EQ 0 THEN BEGIN
                good_tag_ids = [good_tag_ids, tag_i]
                good_tag_names = [good_tag_names, restored_tags[tag_i]]
            ENDIF
        ENDELSE
    ENDFOR
    IF n_missing + n_extra GT 0 THEN BEGIN
        IF n_missing GT 0 THEN BEGIN
            print,"WARNING! Mis-matched source list structure definition for file", catalog_filepath
            print,"Missing tags replaced with default values:", missing_tags
        ENDIF

        IF n_extra GT 0 THEN BEGIN
            print,"WARNING! Mis-matched source list structure definition for file", catalog_filepath
            print,"Extraneous tags have been removed:", extra_tags
        ENDIF
        source_cat_mod = fill_source_list(source_cat, good_tag_names, good_tag_ids)
        FOR ext_i=0,n_ext-1 DO source_cat_mod[extend_i[ext_i]].extend = Ptr_new(fill_source_list(*source_cat[extend_i[ext_i]].extend, good_tag_names, good_tag_ids))
        RETURN, source_cat_mod
    ENDIF ELSE RETURN, source_cat

END
