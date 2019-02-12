PRO pyuvdata_beam_import, pyuvdata_filepath

lun = fxposit(pyuvdata_filepath, 0,/readonly)
data_struct=mrdfits(lun,0,primary_header,/silent)

coordsys = sxpar(primary_header,'coordsys')
CASE coordsys OF
    "az_za": BEGIN
    END
    "orthoslant_zenith": BEGIN
    END
    "healpix": BEGIN
    END
    ELSE: message,"Unsupported COORDSYS found in pyuvdata file!"
ENDCASE

END
