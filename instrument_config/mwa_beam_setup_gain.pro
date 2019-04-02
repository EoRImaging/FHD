FUNCTION mwa_beam_setup_gain,obs,antenna,file_path_fhd=file_path_fhd,$
    za_arr=za_arr,az_arr=az_arr,psf_image_dim=psf_image_dim,debug_flip=debug_flip,$
    _EXTRA = EXTRA 

n_ant_pol=Max(antenna.n_pol)
nfreq_bin=Max(antenna.nfreq_bin)
n_tile=obs.n_tile
beam_model_version=Max(antenna.model_version)
freq_center=antenna[0].freq ;all need to be identical, so just use the first
speed_light=299792458. ;speed of light, in meters/second

;calculate group identifications (used to set pointers to identical models)
FOR pol_i=0,n_ant_pol-1 DO BEGIN
    gi=0
    n_ungrouped=n_tile
    ungrouped_i=where(antenna.group_id[pol_i] EQ -1,n_ungrouped)
    WHILE n_ungrouped GT 0 DO BEGIN
        ref_i=ungrouped_i[0]
        antenna[ref_i].group_id[pol_i]=gi
        FOR ug_i=1L,n_ungrouped-1 DO $
            IF Total(Abs(*antenna[ungrouped_i[ug_i]].gain[pol_i] - *antenna[ref_i].gain[pol_i])) EQ 0 THEN $
                antenna[ungrouped_i[ug_i]].group_id[pol_i]=gi 
        ungrouped_i=where(antenna.group_id[pol_i] EQ -1,n_ungrouped)
        gi+=1
    ENDWHILE
ENDFOR

;build the instrumental pol Jones matrix
CASE beam_model_version OF
    2: BEGIN
        file_path_J_matrix=filepath('mwa_Jmatrix.fits',root=rootdir('FHD'),sub='instrument_config') ; NOTE STILL NEED THIS
        Jones_matrix = import_az_el_beam(obs, antenna, file_path_J_matrix,
                                         za_arr=za_arr,az_arr=az_arr,psf_image_dim=psf_image_dim)
    END
    0: BEGIN
        antenna_height=antenna[0].height
        wavelength=speed_light/freq_center
        Jones_matrix=antenna[0].jones
        FOR freq_i=0,nfreq_bin-1 DO BEGIN
            groundplane=2.*Sin(Cos(za_arr*!DtoR)*(2.*!Pi*(antenna_height)/wavelength[freq_i])) ;should technically have zc_arr, but until that is nonzero this is the same and faster
            groundplane0=2.*Sin(Cos(0.*!DtoR)*2.*!Pi*antenna_height/wavelength[freq_i]) ;normalization factor
            groundplane/=groundplane0
            Jones_matrix[0,0,freq_i]=Ptr_new(Sqrt(1.-(Sin(za_arr*!DtoR)*Sin(az_arr*!DtoR))^2.)*groundplane)
            Jones_matrix[1,0,freq_i]=Ptr_new(0.*groundplane)
            Jones_matrix[0,1,freq_i]=Ptr_new(0.*groundplane)
            Jones_matrix[1,1,freq_i]=Ptr_new(Sqrt(1.-(Sin(za_arr*!DtoR)*Cos(az_arr*!DtoR))^2.)*groundplane)
        ENDFOR
    
    END
    ELSE: BEGIN      
        print,"Using default beam model"
        antenna_height=antenna[0].height
        wavelength=speed_light/freq_center
        Jones_matrix=antenna[0].jones
        FOR freq_i=0,nfreq_bin-1 DO BEGIN
            groundplane=2.*Sin(Cos(za_arr*!DtoR)*(2.*!Pi*(antenna_height)/wavelength[freq_i])) ;should technically have zc_arr, but until that is nonzero this is the same and faster
            groundplane0=2.*Sin(Cos(0.*!DtoR)*2.*!Pi*antenna_height/wavelength[freq_i]) ;normalization factor
            groundplane/=groundplane0
            Jones_matrix[0,0,freq_i]=Ptr_new(Cos(za_arr*!DtoR)*Sin(az_arr*!DtoR)*groundplane)
            Jones_matrix[1,0,freq_i]=Ptr_new(Cos(az_arr*!DtoR)*groundplane)
            Jones_matrix[0,1,freq_i]=Ptr_new(Cos(za_arr*!DtoR)*Cos(az_arr*!DtoR)*groundplane)
            Jones_matrix[1,1,freq_i]=Ptr_new(-Sin(az_arr*!DtoR)*groundplane)
        ENDFOR
    ENDELSE
ENDCASE

;zenith_norm=fltarr(n_ant_pol,n_freq)
;FOR pol_i=0,n_ant_pol-1 DO FOR freq_i=0L,nfreq_bin-1 DO $
;    zenith_norm[pol_i,freq_i]=Sqrt(Max(abs((*Jones_matrix[0,pol_i,freq_i])*(Conj(*Jones_matrix[0,pol_i,freq_i]))+$
;         (*Jones_matrix[1,pol_i,freq_i])*(Conj(*Jones_matrix[1,pol_i,freq_i])))))
antenna.jones=Jones_matrix


RETURN,antenna
END
