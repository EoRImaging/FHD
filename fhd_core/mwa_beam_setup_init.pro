FUNCTION mwa_beam_setup_init,obs,file_path_fhd,mutual_coupling=mutual_coupling

;indices of gain_arr correspond to these antenna locations
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
n_dipoles=16

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

gain_arr=Ptrarr(2)
FOR pol_i=0,1 DO gain_arr[pol_i]=Ptr_new(Rebin(reform(base_gain,1,1,n_dipoles),nfreq_bin,n_tiles,n_dipoles,/sample))

IF file_test(file_path_fhd+'_dipole_gains.sav') THEN restore,file_path_fhd+'_dipole_gains.sav'
mutual_coupling=mwa_dipole_mutual_coupling(freq_center)
;;Account for mutual coupling
;IF Keyword_Set(dipole_mutual_coupling_factor) THEN BEGIN
;    ;X polarization couples between dipoles adjacent to the North or South
;    ;Y polarization couples between dipoles adjacent to the East or West
;    FOR freq_i=0L,nfreq_bin-1 DO BEGIN
;        FOR tile_i=0L,n_tiles-1 DO BEGIN
;            gain_old_X=reform((*gain_arr[0])[freq_i,tile_i,*],4,4)
;            gain_old_Y=reform((*gain_arr[1])[freq_i,tile_i,*],4,4)
;            mask_X=fltarr(4,4)+1
;            mask_X_i=where(gain_old_X EQ 0, n_zero_X,complement=mask_X_use,ncomplement=n_x_use)
;            IF n_zero_X GT 0 THEN mask_X[mask_X_i]=0
;            mask_Y=fltarr(4,4)+1
;            mask_Y_i=where(gain_old_Y EQ 0, n_zero_Y,complement=mask_y_use,ncomplement=n_y_use)
;            IF n_zero_Y GT 0 THEN mask_Y[mask_Y_i]=0
;            
;            gain_new_X=gain_old_X
;            gain_new_Y=gain_old_Y
;            gain_new_X[*,0]+=dipole_mutual_coupling_factor*gain_old_X[*,1]
;            gain_new_X[*,1]+=dipole_mutual_coupling_factor*(gain_old_X[*,0]+gain_old_X[*,2])
;            gain_new_X[*,2]+=dipole_mutual_coupling_factor*(gain_old_X[*,1]+gain_old_X[*,3])
;            gain_new_X[*,3]+=dipole_mutual_coupling_factor*gain_old_X[*,2]
;            gain_new_Y[0,*]+=dipole_mutual_coupling_factor*gain_old_Y[1,*]
;            gain_new_Y[1,*]+=dipole_mutual_coupling_factor*(gain_old_Y[0,*]+gain_old_Y[2,*])
;            gain_new_Y[2,*]+=dipole_mutual_coupling_factor*(gain_old_Y[1,*]+gain_old_Y[3,*])
;            gain_new_Y[3,*]+=dipole_mutual_coupling_factor*gain_old_Y[2,*]
;            gain_new_X*=mask_X
;            IF n_x_use GT 0 THEN gain_new_X/=Mean(gain_new_X[mask_X_use])
;            gain_new_Y*=mask_Y
;            IF n_y_use GT 0 THEN gain_new_Y/=Mean(gain_new_Y[mask_Y_use])
;            gain_old_X=gain_new_X
;            gain_old_Y=gain_new_Y
;            
;;            ;SWAP X AND Y coupling for testing
;;            gain_new_X=gain_old_Y
;;            gain_new_Y=gain_old_X
;            
;            (*gain_arr[0])[freq_i,tile_i,*]=Reform(gain_new_X,16)
;            (*gain_arr[1])[freq_i,tile_i,*]=Reform(gain_new_Y,16)
;        ENDFOR        
;    ENDFOR
;ENDIF
RETURN,gain_arr
END