FUNCTION pyuvdata_beam_import, obs, antenna_str, pyuvdata_filepath,$
    za_arr=za_arr, az_arr=az_arr, psf_image_dim=psf_image_dim

lun = fxposit(pyuvdata_filepath, 0,/readonly)
MRD_HREAD, lun, primary_header, /silent

coordsys = strtrim(sxpar(primary_header,'coordsys'),2)
CASE coordsys OF
    "az_za": BEGIN
        Jones_matrix = import_az_el_beam(obs, antenna_str, pyuvdata_filepath,$
                                         za_arr=za_arr, az_arr=az_arr, psf_image_dim=psf_image_dim)
    END
    "orthoslant_zenith": BEGIN
        message,"COORDSYS type 'orthoslant_zenith' not yet supported!""
    END
    "healpix": BEGIN
        message,"COORDSYS type 'healpix' not yet supported!"
    END
    ELSE: message,"Unsupported COORDSYS found in pyuvdata file!"
ENDCASE

RETURN, Jones_matrix
END
