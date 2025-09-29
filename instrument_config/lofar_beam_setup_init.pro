FUNCTION lofar_beam_setup_init, obs, antenna_str, antenna_size=antenna_size, dead_dipole_list=dead_dipole_list, $
    dipole_mutual_coupling_factor=dipole_mutual_coupling_factor, antenna_spacing=antenna_spacing, flag_dead_dipoles=flag_dead_dipoles

; LOFAR HBA tile: 4x4 dipoles, ~1.25m spacing
n_ant_pol = antenna_str.n_pol
n_tiles = obs.n_tile
n_dipoles = 16
nfreq_bin = antenna_str.nfreq_bin

IF N_Elements(dipole_mutual_coupling_factor) EQ 0 THEN dipole_mutual_coupling_factor = 1
IF N_Elements(antenna_size) EQ 0 THEN antenna_size = 5. ; meters (approximate tile size)
IF NOT Keyword_Set(antenna_spacing) THEN antenna_spacing = 1.25 ; meters (LOFAR HBA tile spacing)
antenna_height = 0.5 ; meters (typical HBA dipole height)
speed_light = 299792458.
base_delay_unit = 4.35E-10

; Dipole positions (centered)
xc_arr0 = Reform((meshgrid(4,4,1)) * antenna_spacing, 16)
xc_arr = xc_arr0 - Mean(xc_arr0)
yc_arr0 = Reform(Reverse(meshgrid(4,4,2),2) * antenna_spacing, 16)
yc_arr = yc_arr0 - Mean(yc_arr0)
zc_arr = Fltarr(16) + antenna_height

antenna_coords = Ptrarr(3)
antenna_coords[0] = Ptr_new(xc_arr)
antenna_coords[1] = Ptr_new(yc_arr)
antenna_coords[2] = Ptr_new(zc_arr)

; Read rotation angles from file
tile_names = (*obs.baseline_info).tile_names
file_path_rotations=filepath('lofar_hba_rotation_angles.txt',root=rootdir('FHD'),sub='instrument_config')
textfast,rotation_angles,/read,file_path=file_path_rotations

; Set up antenna structure
antenna_str.n_ant_elements = n_dipoles
antenna_str.size_meters = antenna_size
antenna_str.coords = antenna_coords
antenna_str.height = antenna_height

antenna = replicate(antenna_str, n_tiles)

for tile_i=0, n_tiles-1 do begin
    antenna[tile_i].rotation = rotation_angles[tile_i]
endfor

RETURN, antenna
END