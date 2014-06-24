FUNCTION mwa_beam_setup_gain,obs,antenna,file_path_fhd=file_path_fhd,dipole_mutual_coupling_factor=dipole_mutual_coupling_factor,$
    beam_model_version=beam_model_version,xvals_instrument=xvals_instrument,yvals_instrument=yvals_instrument,$
    za_arr=za_arr,az_arr=az_arr,dead_dipole_list=dead_dipole_list,antenna_size=antenna_size


IF N_Elements(file_path_fhd) EQ 0 THEN file_path_fhd=''
base_gain=fltarr(16)+1.

gain_arr=Ptrarr(n_ant_pol)
FOR pol_i=0,1 DO gain_arr[pol_i]=Ptr_new(Rebin(reform(base_gain,1,1,n_dipoles),nfreq_bin,n_tiles,n_dipoles,/sample))

IF Keyword_Set(dead_dipole_list) THEN BEGIN
    ;Format is 3xN array, column 0: Tile number, 1: polarization, 2: dipole number
    tile_id=Reform(dead_dipole_list[0,*])
    pol_id=Reform(dead_dipole_list[0,*])
    dipole_id=Reform(dead_dipole_list[0,*])
    n_dead_dipole=N_Elements(tile_id)
    names_ref=Fix((*obs.baseline_info).tile_names,type=Size(tile_id,/type))
    FOR d_i=0L,n_dead_dipole-1 DO BEGIN
        tile_i=where(names_ref EQ tile_id,n_match)
        IF n_match GT 0 THEN (*gain_arr[pol_id[d_i]])[*,tile_i,dipole_id[d_i]]=0.
    ENDFOR
ENDIF ELSE IF file_test(file_path_fhd+'_dipole_gains.sav') THEN restore,file_path_fhd+'_dipole_gains.sav'

IF Keyword_Set(dipole_mutual_coupling_factor) THEN mutual_coupling=mwa_dipole_mutual_coupling(freq_center) $
    ELSE BEGIN
        mutual_coupling=Ptrarr(n_ant_pol,nfreq_bin)
        FOR i=0L,N_Elements(mutual_coupling) DO mutual_coupling[i]=Ptr_new(Complex(Identity(n_dipoles)))
    ENDELSE
    
gain_arr_complex=Ptrarr(n_ant_pol)
FOR pol_i=0,n_ant_pol-1 DO BEGIN
    gain_arr_complex[pol_i]=Complexarr(nfreq_bin,n_tiles,n_dipoles)
    FOR freq_i=0L,nfreq_bin-1 DO BEGIN
        
    ENDFOR
ENDFOR

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
        horizon_mask=fltarr(psf_dim2,psf_dim2)+1
        IF n_horizon_test GT 0 THEN horizon_mask[horizon_test]=0    
        Jones_matrix=Ptrarr(n_ant_pol,n_ant_pol)
        FOR p_i=0,n_ant_pol-1 DO FOR p_j=0,n_ant_pol-1 DO Jones_matrix[p_i,p_j]=Ptr_new(Complexarr(psf_dim2,psf_dim2))
        FOR i=0L,n_pix-1 DO BEGIN
            xv_instrument1=xvals[pix_use[i]]
            yv_instrument1=yvals[pix_use[i]]
            
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
        IF polarization EQ 0 THEN projection=Sqrt(1.-proj_east^2.) ELSE projection=Sqrt(1.-proj_north^2.) 
        groundplane=2.*Sin(Cos(za_arr*!DtoR)*(2.*!Pi*(antenna_height)/wavelength)) ;should technically have zc_arr, but until that is nonzero this is the same and faster
        groundplane0=2.*Sin(Cos(0.*!DtoR)*2.*!Pi*antenna_height/wavelength) ;normalization factor
        groundplane/=groundplane0
    ENDELSE
ENDCASE

antenna_arr=Replicate(antenna,n_tiles)
FOR t_i=0L,n_tiles-1 DO BEGIN
    antenna.Jones=Ptr_new(Jones_matrix)
    antenna.coupling=mutual_coupling
    antenna.gain=gain_arr
ENDFOR

RETURN,antenna
END