FUNCTION mwa_beam_setup_init,obs,file_path_fhd,dipole_mutual_coupling_factor=dipole_mutual_coupling_factor
;;IF not Keyword_Set(data_directory) THEN vis_path_default,data_directory,filename ;set default if not supplied
;ext='.UVFITS'
;tile_gain_x_filename='tile_gains_x'
;tile_gain_y_filename='tile_gains_y'

;indices of antenna_gain_arr correspond to these antenna locations (add 1 for gain_array):
;         N
;    0  1  2  3
;    
;    4  5  6  7  
;W                E
;    8  9  10 11   
;    
;    12 13 14 15 
;         S
;polarization 0: x, 1: y
n_tiles=obs.n_tile
n_freq=obs.n_freq

frequency_array=(*obs.baseline_info).freq
freq_bin_i=(*obs.baseline_info).fbin_i
nfreq_bin=Max(freq_bin_i)+1
freq_center=fltarr(nfreq_bin)
FOR fi=0L,nfreq_bin-1 DO BEGIN
    fi_i=where(freq_bin_i EQ fi,n_fi)
    IF n_fi EQ 0 THEN freq_center[fi]=Interpol(frequency_array,freq_bin_i,fi) $
        ELSE freq_center[fi]=Median(frequency_array[fi_i])
ENDFOR

base_gain=fltarr(16)+1.
IF N_Elements(file_path_fhd) EQ 0 THEN file_path_fhd=''
;gain_array=fltarr(17,nfreq_bin*n_tiles)+1. ;17 columns: first is tile number, 16 for each dipole in a tile
;IF N_Elements(base_gain) EQ 0 THEN BEGIN
;    base_gain=fltarr(16)+1.
;    base_gain[[0,3,12,15]]=1.
;    base_gain[[1,2,4,7,8,11,13,14]]=1.
;ENDIF

gain_str={freq:-1L,tile:-1L,gain:base_gain}
gain_arr=Ptrarr(2)
FOR pol_i=0,1 DO gain_arr[pol_i]=Ptr_new(replicate(gain_str,nfreq_bin,n_tiles))

;base_gain_use=[1.,base_gain] ;17 columns: first is tile number, 16 for each dipole in a tile
;gain_array=base_gain_use#(fltarr(nfreq_bin*n_tiles)+1.)
;gain_array[0,*]=Floor(indgen(nfreq_bin*n_tiles)/nfreq_bin)+1 ;tile number 
;
;tile_gain_x_filepath=file_path_fhd+tile_gain_x_filename
;tile_gain_y_filepath=file_path_fhd+tile_gain_y_filename
IF file_test(file_path_fhd+'_dipole_gains.sav') THEN restore,file_path_fhd+'_dipole_gains.sav'

;;do not overwrite a gain_array if one already exists (it's either real data, or the same default data as this!)
;IF file_test(tile_gain_x_filepath) EQ 0 THEN BEGIN
;    gain_array_X=gain_array
;    IF ~Keyword_Set(no_save) THEN textfast,gain_array_X,file_path=tile_gain_x_filepath,/write
;ENDIF ELSE textfast,gain_array_X,file_path=tile_gain_x_filepath,/read
;
;IF file_test(tile_gain_y_filepath) EQ 0 THEN BEGIN
;    gain_array_Y=gain_array_X
;    IF ~Keyword_Set(no_save) THEN textfast,gain_array_Y,file_path=tile_gain_y_filepath,/write
;ENDIF ELSE textfast,gain_array_Y,file_path=tile_gain_y_filepath,/read

;Account for mutual coupling
IF Keyword_Set(dipole_mutual_coupling_factor) THEN BEGIN
    max_iter=3
    gain_tile_i=reform(gain_array_X[0,*])
    gain_freq_bin_i=findgen(N_Elements(gain_tile_i)) mod nfreq_bin
    ;X polarization couples between dipoles adjacent to the North or South
    ;Y polarization couples between dipoles adjacent to the East or West
    FOR freq_i=0L,nfreq_bin-1 DO BEGIN
        FOR tile_i=0L,n_tiles-1 DO BEGIN
            gain_old_X=reform((*gain_arr[0])[freq_i,tile_i].gain,4,4)
            gain_old_Y=reform((*gain_arr[1])[freq_i,tile_i].gain,4,4)
            mask_X=fltarr(4,4)+1
            mask_X_i=where(gain_old_X EQ 0, n_zero_X,complement=mask_X_use,ncomplement=n_x_use)
            IF n_zero_X GT 0 THEN mask_X[mask_X_i]=0
            mask_Y=fltarr(4,4)+1
            mask_Y_i=where(gain_old_Y EQ 0, n_zero_Y,complement=mask_y_use,ncomplement=n_y_use)
            IF n_zero_Y GT 0 THEN mask_Y[mask_Y_i]=0
;            FOR iter=0,max_iter-1 DO BEGIN
                gain_new_X=gain_old_X
                gain_new_Y=gain_old_Y
                gain_new_X[*,0]+=dipole_mutual_coupling_factor*gain_old_X[*,1]
                gain_new_X[*,1]+=dipole_mutual_coupling_factor*(gain_old_X[*,0]+gain_old_X[*,2])
                gain_new_X[*,2]+=dipole_mutual_coupling_factor*(gain_old_X[*,1]+gain_old_X[*,3])
                gain_new_X[*,3]+=dipole_mutual_coupling_factor*gain_old_X[*,2]
                gain_new_Y[0,*]+=dipole_mutual_coupling_factor*gain_old_Y[1,*]
                gain_new_Y[1,*]+=dipole_mutual_coupling_factor*(gain_old_Y[0,*]+gain_old_Y[2,*])
                gain_new_Y[2,*]+=dipole_mutual_coupling_factor*(gain_old_Y[1,*]+gain_old_Y[3,*])
                gain_new_Y[3,*]+=dipole_mutual_coupling_factor*gain_old_Y[2,*]
                gain_new_X*=mask_X
                IF n_x_use GT 0 THEN gain_new_X/=Mean(gain_new_X[mask_X_use])
                gain_new_Y*=mask_Y
                IF n_y_use GT 0 THEN gain_new_Y/=Mean(gain_new_Y[mask_Y_use])
                gain_old_X=gain_new_X
                gain_old_Y=gain_new_Y
;            ENDFOR
;            ;SWAP X AND Y coupling for testing
;            gain_new_X=gain_old_Y
;            gain_new_Y=gain_old_X
            
            (*gain_arr[0])[freq_i,tile_i].gain=Reform(gain_new_X,16)
            (*gain_arr[1])[freq_i,tile_i].gain=Reform(gain_new_Y,16)
        ENDFOR        
    ENDFOR
ENDIF
RETURN,gain_arr
END