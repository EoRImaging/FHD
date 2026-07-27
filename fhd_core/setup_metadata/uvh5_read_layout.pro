FUNCTION uvh5_read_dataset, fid, path
    did = H5D_OPEN(fid, path)
    data = H5D_READ(did)
    H5D_CLOSE, did
    RETURN, data
END

FUNCTION uvh5_read_layout, file_path_vis, _EXTRA=extra

    fid = H5F_OPEN(file_path_vis)

    ant_names     = uvh5_read_dataset(fid, '/Header/antenna_names')
    ant_numbers   = uvh5_read_dataset(fid, '/Header/antenna_numbers')
    ant_positions = uvh5_read_dataset(fid, '/Header/antenna_positions')

    latitude  = uvh5_read_dataset(fid, '/Header/latitude')
    longitude = uvh5_read_dataset(fid, '/Header/longitude')
    altitude  = uvh5_read_dataset(fid, '/Header/altitude')

    telescope_name = uvh5_read_dataset(fid, '/Header/telescope_name')

    H5F_CLOSE, fid

    nant = N_ELEMENTS(ant_numbers)

    ; IDL/HDF5 dimension order can be reversed, so make sure positions are [3, nant].
    pos_dims = SIZE(ant_positions, /DIMENSIONS)
    IF pos_dims[0] NE 3 THEN ant_positions = TRANSPOSE(ant_positions)

    ; Build a layout structure with the fields FHD usually needs.
    ; UVH5 antenna_positions are ECEF positions relative to the telescope location,
    ; not an AIPS AN binary table. pyuvdata documents that telescope.antenna_positions
    ; are ECEF positions relative to telescope.location. :contentReference[oaicite:2]{index=2}

    layout = { $
        n_antenna: nant, $
        antenna_names: ant_names, $
        antenna_numbers: ant_numbers, $
        antenna_positions: ant_positions, $
        latitude: latitude, $
        longitude: longitude, $
        altitude: altitude, $
        telescope_name: telescope_name $
    }

    RETURN, layout

END