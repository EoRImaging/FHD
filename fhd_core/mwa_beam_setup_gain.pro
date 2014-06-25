FUNCTION mwa_beam_setup_gain,obs,antenna,file_path_fhd=file_path_fhd,$
    za_arr=za_arr,az_arr=az_arr,dead_dipole_list=dead_dipole_list,psf_image_dim=psf_image_dim

n_ant_pol=Max(antenna.n_pol)
nfreq_bin=Max(antenna.nfreq_bin)
IF N_Elements(file_path_fhd) EQ 0 THEN file_path_fhd=''
n_tile=obs.n_tile
beam_model_version=Max(antenna.model_version)
xvals_instrument=za_arr*Sin(az_arr*!DtoR)
yvals_instrument=za_arr*Cos(az_arr*!DtoR)
freq_center=antenna[0].freq ;all need to be identical, so just use the first
speed_light=299792458. ;speed of light, in meters/second

IF Keyword_Set(dead_dipole_list) THEN BEGIN
    ;Format is 3xN array, column 0: Tile number (names, not index), 1: polarization (0:x, 1:y), 2: dipole number
    tile_id=Reform(dead_dipole_list[0,*])
    pol_id=Reform(dead_dipole_list[0,*])
    dipole_id=Reform(dead_dipole_list[0,*])
    n_dead_dipole=N_Elements(tile_id)
    names_ref=Fix((*obs.baseline_info).tile_names,type=Size(tile_id,/type))
    FOR d_i=0L,n_dead_dipole-1 DO BEGIN
        tile_i=where(names_ref EQ tile_id,n_match)
        IF n_match GT 0 THEN (*((antenna[tile_i].gain)[pol_id[d_i]]))[*,dipole_id[d_i]]=0.
    ENDFOR
ENDIF ELSE IF file_test(file_path_fhd+'_dipole_gains.sav') THEN restore,file_path_fhd+'_dipole_gains.sav'

;calculate group identifications (used to set pointers to identical models)
FOR pol_i=0,n_ant_pol-1 DO BEGIN
    gi=0
    n_ungrouped=n_tile
    ungrouped_i=where(antenna.group_id[pol_i] EQ -1,n_ungrouped)
    WHILE n_ungrouped GT 0 DO BEGIN
        ref_i=ungrouped_i[0]
        antenna[ref_i].group_id[pol_i]=gi
        FOR ug_i=1L,n_ungrouped-1 DO IF Total(*antenna[ungrouped_i[ug_i]].gain[pol_i] - *antenna[ref_i].gain[pol_i]) EQ 0 THEN antenna[ungrouped_i[ug_i]].group_id[pol_i]=gi 
        ungrouped_i=where(antenna.group_id[pol_i] EQ -1,n_ungrouped)
        gi+=1
    ENDWHILE
ENDFOR

;build the instrumental pol Jones matrix
CASE beam_model_version OF
    2: BEGIN
        file_path_J_matrix=filepath('mwa_Jmatrix.fits',root=rootdir('FHD'),sub='instrument_config')
        fits_info,file_path_J_matrix,/silent,n_ext=n_ext
        ;Cols of J matrix: theta phi  real(Jxt(t,p)) imag(Jxt(t,p)) real(Jxp(t,p)) imag(Jxp(t,p)) real(Jyt(t,p)) imag(Jyt(t,p)) real(Jyp(t,p)) imag(Jyp(t,p)))
        ;Where theta is the zenith angle, phi is angle measured clockwise from +east direction looking down
        ; Jxt is the Jones mapping unit vec in theta (t) direction to the x (east-west) dipole etc
        n_ext+=1 ;n_ext starts counting AFTER the 0th extension, which it considers to be the main data unit, but we use that one too
        freq_arr_Jmat=Fltarr(n_ext)
        
        FOR ext_i=0,n_ext-1 DO BEGIN
            Jmat1=mrdfits(file_path_J_matrix,ext_i,header,status=status,/silent)
            IF ext_i EQ 0 THEN BEGIN
                n_ang=Float(sxpar(header,'NAXIS2'))
                Jmat_arr=Complexarr(n_ext,n_ant_pol,n_ant_pol,n_ang)
                theta_arr=Fltarr(n_ext,n_ang)
                phi_arr=Fltarr(n_ext,n_ang)
            ENDIF
            theta_arr[ext_i,*]=Jmat1[0,*] ;zenith angle in degrees
            phi_arr[ext_i,*]=Jmat1[1,*] ;azimuth angle in degrees, clockwise from East
            FOR p_i=0,n_ant_pol-1 DO FOR p_j=0,n_ant_pol-1 DO $
                Jmat_arr[ext_i,p_i,p_j,*]=Jmat1[2+p_i*2+p_j*4,*]+icomp*Jmat1[2+p_i*2+p_j*4+1,*]
            freq_arr_Jmat[ext_i]=Float(sxpar(header,'FREQ')) ;in Hz
        ENDFOR
        theta_arr=median(theta_arr,dimension=1) ; all actually the same across freq, so reduce dimension
        phi_arr=median(phi_arr,dimension=1) ; all actually the same across freq, so reduce dimension
        phi_arr=270.-phi_arr ;change azimuth convention
        
    ;    Jmat_return=Ptrarr(n_ant_pol,n_ant_pol)
        Jmat_interp=Ptrarr(n_ant_pol,n_ant_pol)
        FOR p_i=0,n_ant_pol-1 DO FOR p_j=0,n_ant_pol-1 DO Jmat_interp[p_i,p_j]=Ptr_new(Complexarr(n_ang))
        FOR p_i=0,n_ant_pol-1 DO FOR p_j=0,n_ant_pol-1 DO FOR a_i=0L,n_ang-1 DO $
            (*Jmat_interp[p_i,p_j])[a_i]=Interpol(Jmat_arr[*,p_i,p_j,a_i],freq_arr_Jmat,frequency)
        
        xv_model=theta_arr*Sin(phi_arr*!DtoR)
        yv_model=theta_arr*Cos(phi_arr*!DtoR)
        
        horizon_test=where(abs(za_arr) GE 90.,n_horizon_test,complement=pix_use,ncomplement=n_pix)
        horizon_mask=fltarr(psf_image_dim,psf_image_dim)+1
        IF n_horizon_test GT 0 THEN horizon_mask[horizon_test]=0    
        Jones_matrix=Ptrarr(n_ant_pol,n_ant_pol)
        FOR p_i=0,n_ant_pol-1 DO FOR p_j=0,n_ant_pol-1 DO Jones_matrix[p_i,p_j]=Ptr_new(Complexarr(psf_image_dim,psf_image_dim))
        FOR i=0L,n_pix-1 DO BEGIN
            xv_instrument1=xvals_instrument[pix_use[i]]
            yv_instrument1=yvals_instrument[pix_use[i]]
            
            dist_test=Sqrt((xv_instrument1-xv_model)^2.+(yv_instrument1-yv_model)^2.)
            IF Min(dist_test,min_i) EQ 0 THEN BEGIN ;test if there is an exact match. Then just take that value and do no interpolation
                FOR p_i=0,n_ant_pol-1 DO FOR p_j=0,n_ant_pol-1 DO $
                    (*Jones_matrix[p_i,p_j])[pix_use[i]]=(*Jmat_interp[p_i,p_j])[min_i]
                CONTINUE ;move on to next iteration of pixel FOR loop
            ENDIF 
            i_use=(Sort(dist_test))[0:3] ;there had better be at least four points in the model!
            weight=1./dist_test[i_use]
            FOR p_i=0,n_ant_pol-1 DO FOR p_j=0,n_ant_pol-1 DO $
                (*Jones_matrix[p_i,p_j])[pix_use[i]]=Total((*Jmat_interp[p_i,p_j])[i_use]*weight)/Total(weight)
        ENDFOR
;        IF polarization EQ 0 THEN projection=Sqrt(*Jmat[0,0]*Conj(*Jmat[0,0])+*Jmat[1,0]*Conj(*Jmat[1,0])) $
;            ELSE projection=Sqrt(*Jmat[1,1]*Conj(*Jmat[1,1])+*Jmat[0,1]*Conj(*Jmat[0,1]))
;        projection/=Max(projection)
    END
    ELSE: BEGIN      
        print,"Using default beam model"
        antenna_height=antenna[0].height
        wavelength=speed_light/freq_center
        Jones_matrix=antenna.jones
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

antenna.jones=Jones_matrix

RETURN,antenna
END