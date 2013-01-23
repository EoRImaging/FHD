PRO beam_setup_init,gain_array_X,gain_array_Y,file_path_fhd,n_tiles=n_tiles,nfreq_bin=nfreq_bin
;IF not Keyword_Set(data_directory) THEN vis_path_default,data_directory,filename ;set default if not supplied
ext='.UVFITS'
tile_gain_x_filename='tile_gains_x'
tile_gain_y_filename='tile_gains_y'

;indices of antenna_gain_arr correspond to these antenna locations (add 1 for gain_array):
;12 13 14 15
;8  9  10 11
;4  5  6  7
;0  1  2  3 
IF N_Elements(n_tiles) EQ 0 THEN n_tiles=32.
IF N_Elements(nfreq_bin) EQ 0 THEN nfreq_bin=24. ;by coarse frequency channel
;gain_array=fltarr(17,nfreq_bin*n_tiles)+1. ;17 columns: first is tile number, 16 for each dipole in a tile
base_gain=fltarr(16)+1.
base_gain[[0,3,12,15]]=1.
base_gain[[1,2,4,7,8,11,13,14]]=1.
base_gain=[1.,base_gain] ;17 columns: first is tile number, 16 for each dipole in a tile
gain_array=base_gain#(fltarr(nfreq_bin*n_tiles)+1.)
gain_array[0,*]=Floor(indgen(nfreq_bin*n_tiles)/nfreq_bin)+1

tile_gain_x_filepath=file_path_fhd+tile_gain_x_filename
tile_gain_y_filepath=file_path_fhd+tile_gain_y_filename

;do not overwrite a gain_array if one already exists (it's either real data, or the same default data as this!)
IF file_test(tile_gain_x_filepath) EQ 0 THEN BEGIN
    gain_array_X=gain_array
    textfast,gain_array_X,tile_gain_x_filepath,/write
ENDIF ELSE textfast,gain_array_X,tile_gain_x_filepath,/read

IF file_test(tile_gain_y_filepath) EQ 0 THEN BEGIN
    gain_array_Y=gain_array_X
    textfast,gain_array_Y,tile_gain_y_filepath,/write
ENDIF ELSE textfast,gain_array_Y,tile_gain_y_filepath,/read

END