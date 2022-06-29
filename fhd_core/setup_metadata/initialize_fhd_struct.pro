PRO initialize_fhd_struct, struct_name, obs=obs, params=params
    ; Create a placeholder copy of the named structure.
    ; This defines the named structure, which ensures consistency when restoring previously generated structures.

    CASE struct_name OF
        'cal': struct_init = fhd_struct_init_cal(obs, params)
        ELSE: print, "Structure of type '" + struct_name + "' is not defined in initialize_fhd_struct.pro; skipping."
    END
    undefine_fhd, struct_init
END
