FUNCTION mwa_tile_beam_generate,antenna_gain_arr,antenna_beam_arr,obsaz=obsaz,obsza=obsza,$
    frequency=frequency,polarization=polarization,$
    psf_dim=psf_dim,psf_resolution=psf_resolution,kbinsize=kbinsize,$
    normalization=normalization,xvals=xvals,yvals=yvals,$
    dimension=dimension,elements=elements,za_arr=za_arr,az_arr=az_arr,delay_settings=delay_settings,$
    mutual_coupling=mutual_coupling,beam_model_version=beam_model_version,_Extra=extra

compile_opt idl2,strictarrsubs  
;indices of antenna_gain_arr correspond to these antenna locations:
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
;angle offset is the rotation of the entire tile in current coordinates in DEGREES
; (this should be the rotation between E-W or N-S and Ra-Dec)

IF N_Elements(normalization) EQ 0 THEN normalization=1.
psf_dim=Float(psf_dim)
psf_dim2=dimension
IF N_Elements(beam_model_version) EQ 0 THEN beam_model_version=1

za=Float(obsza)
az=Float(obsaz)

antenna_spacing=1.1 ;meters (Same as M&C SetDelays script) ; Was 1.071 before? Verified in Tingay et al 2013
antenna_length=29.125*2.54/100. ;meters (measured) (NOT USED)
antenna_height=0.29 ;meters (June 2014 e-mail from Brian Crosse) ; Was 0.35 before
c_light_vacuum=299792458.
velocity_factor=0.673
c_light_cable=c_light_vacuum*velocity_factor ;not used
icomp=Complex(0,1)

Kconv=(2.*!Pi)*(frequency/c_light_vacuum) ;wavenumber (radians/meter)
wavelength=c_light_vacuum/frequency

;;REMOVE THIS SECTION FOR NOW
;IF Keyword_Set(antenna_beam_arr) THEN IF Keyword_Set(*antenna_beam_arr[0]) THEN BEGIN
;    tile_beam=fltarr(psf_dim2,psf_dim2)
;    FOR i=0,15 DO tile_beam+=*antenna_beam_arr[i]*antenna_gain_arr[i]
;    tile_beam*=normalization
;    tile_beam=tile_beam
;    RETURN,tile_beam
;ENDIF
xc_arr0=Reform((meshgrid(4,4,1))*antenna_spacing,16)
xc_arr=xc_arr0-Mean(xc_arr0) ;dipole east position (meters)
yc_arr0=Reform(Reverse(meshgrid(4,4,2),2)*antenna_spacing,16)
yc_arr=yc_arr0-Mean(yc_arr0) ;dipole north position (meters)
zc_arr=Fltarr(16)

;beamformer phase setting (meters) 
;Change units to match Sutinjo et al 2014 paper (units now in seconds)
IF not Ptr_valid(delay_settings) THEN BEGIN
    D0_d=xc_arr0*sin(za*!DtoR)*Sin(az*!DtoR)+yc_arr0*Sin(za*!DtoR)*Cos(az*!DtoR) 
    D0_d/=c_light_vacuum*4.35E-10 ;435 picoseconds is base delay length unit
    delay_settings=Ptr_new(Round(D0_d)) ;round to nearest real delay setting
ENDIF
D0_d=*delay_settings*4.35E-10;*c_light_vacuum
D0_d=Float(D0_d)
;D0_d-=Min(D0_d)
proj_east=Sin(za_arr*!DtoR)*Sin(az_arr*!DtoR) & proj_east_use=Reform(proj_east,(psf_dim2)^2.)
proj_north=Sin(za_arr*!DtoR)*Cos(az_arr*!DtoR) & proj_north_use=Reform(proj_north,(psf_dim2)^2.)
proj_z=Cos(za_arr*!DtoR) & proj_z_use=Reform(proj_z,(psf_dim2)^2.)

;phase of each dipole for the source (relative to the beamformer settings)
D_d=(proj_east_use#xc_arr+proj_north_use#yc_arr+proj_z_use#zc_arr)
D_d=Reform(D_d,psf_dim2,psf_dim2,16)
dipole_gain_arr=Exp(-icomp*Kconv*D_d)

CASE beam_model_version OF
    2: BEGIN
        file_path_J_matrix=filepath('Jmatrix.fits',root=rootdir('FHD'),sub='instrument_config')
        fits_info,file_path_J_matrix,/silent,n_ext=n_ext
        ;Cols of J matrix: theta phi  real(Jxt(t,p)) imag(Jxt(t,p)) real(Jxp(t,p)) imag(Jxp(t,p)) real(Jyt(t,p)) imag(Jyt(t,p)) real(Jyp(t,p)) imag(Jyp(t,p)))
        ;Where theta is the zenith angle, phi is angle measured clockwise from +east direction looking down
        ; Jxt is the Jones mapping unit vec in theta (t) direction to the x (east-west) dipole etc
        n_ext+=1 ;n_ext starts counting AFTER the 0th extension, which it considers to be the main data unit, but we use that one too
        freq_arr_Jmat=Fltarr(n_ext)
        
        FOR ext_i=0,n_ext-1 DO BEGIN
            Jmat1=mrdfits(file_path_J_matrix,ext_i,header,status=status,/silent)
            IF ext_i EQ 0 THEN BEGIN
                n_pol_param=Float(sxpar(header,'NAXIS1'))
                n_pol_param=8/4
                n_ang=Float(sxpar(header,'NAXIS2'))
                Jmat_arr=Complexarr(n_ext,n_pol_param,n_pol_param,n_ang)
                theta_arr=Fltarr(n_ext,n_ang)
                phi_arr=Fltarr(n_ext,n_ang)
            ENDIF
            theta_arr[ext_i,*]=Jmat1[0,*] ;zenith angle in degrees
            phi_arr[ext_i,*]=Jmat1[1,*] ;azimuth angle in degrees, clockwise from East
            FOR p_i=0,n_pol_param-1 DO FOR p_j=0,n_pol_param-1 DO Jmat_arr[ext_i,p_i,p_j,*]=Jmat1[2+p_i*2+p_j*4,*]+icomp*Jmat1[2+p_i*2+p_j*4+1,*]
            freq_arr_Jmat[ext_i]=Float(sxpar(header,'FREQ')) ;in Hz
        ENDFOR
        theta_arr=median(theta_arr,dimension=1) ; all actually the same across freq, so reduce dimension
        phi_arr=median(phi_arr,dimension=1) ; all actually the same across freq, so reduce dimension
        phi_arr=270.-phi_arr ;change azimuth convention
        
    ;    Jmat_return=Ptrarr(n_pol_param,n_pol_param)
        Jmat_interp=Ptrarr(n_pol_param,n_pol_param)
        FOR p_i=0,n_pol_param-1 DO FOR p_j=0,n_pol_param-1 DO Jmat_interp[p_i,p_j]=Ptr_new(Complexarr(n_ang))
        FOR p_i=0,n_pol_param-1 DO FOR p_j=0,n_pol_param-1 DO FOR a_i=0L,n_ang-1 DO $
            (*Jmat_interp[p_i,p_j])[a_i]=Interpol(Jmat_arr[*,p_i,p_j,a_i],freq_arr_Jmat,frequency)
        
        xv_model=theta_arr*Sin(phi_arr*!DtoR)
        yv_model=theta_arr*Cos(phi_arr*!DtoR)
        
        horizon_test=where(abs(za_arr) GE 90.,n_horizon_test,complement=pix_use,ncomplement=n_pix)
        horizon_mask=fltarr(psf_dim2,psf_dim2)+1
        IF n_horizon_test GT 0 THEN horizon_mask[horizon_test]=0    
        Jmat=Ptrarr(n_pol_param,n_pol_param)
        FOR p_i=0,n_pol_param-1 DO FOR p_j=0,n_pol_param-1 DO Jmat[p_i,p_j]=Ptr_new(Complexarr(psf_dim2,psf_dim2))
        FOR i=0L,n_pix-1 DO BEGIN
            xv_instrument1=xvals[pix_use[i]]
            yv_instrument1=yvals[pix_use[i]]
            
            dist_test=Sqrt((xv_instrument1-xv_model)^2.+(yv_instrument1-yv_model)^2.)
            IF Min(dist_test,min_i) EQ 0 THEN BEGIN ;test if there is an exact match. Then just take that value and do no interpolation
                FOR p_i=0,n_pol_param-1 DO FOR p_j=0,n_pol_param-1 DO (*Jmat[p_i,p_j])[pix_use[i]]=(*Jmat_interp[p_i,p_j])[min_i]
                CONTINUE ;move on to next iteration of pixel FOR loop
            ENDIF 
            i_use=(Sort(dist_test))[0:3] ;there had better be at least four points in the model!
            weight=1./dist_test[i_use]
            FOR p_i=0,n_pol_param-1 DO FOR p_j=0,n_pol_param-1 DO (*Jmat[p_i,p_j])[pix_use[i]]=Total((*Jmat_interp[p_i,p_j])[i_use]*weight)/Total(weight)
        ENDFOR
        IF polarization EQ 0 THEN projection=Sqrt(*Jmat[0,0]*Conj(*Jmat[0,0])+*Jmat[1,0]*Conj(*Jmat[1,0])) $
            ELSE projection=Sqrt(*Jmat[1,1]*Conj(*Jmat[1,1])+*Jmat[0,1]*Conj(*Jmat[0,1]))
        projection/=Max(projection)
    END
    ELSE: BEGIN                
        IF polarization EQ 0 THEN projection=Sqrt(1.-proj_east^2.) ELSE projection=Sqrt(1.-proj_north^2.) 
    ENDELSE
ENDCASE
;groundplane=2.*Sin(Cos(za_arr_use*!DtoR)#(Kconv*(antenna_height+zc_arr))) ;looks correct
;groundplane=Reform(groundplane,psf_dim2,psf_dim2,16)

groundplane=2.*Sin(Cos(za_arr*!DtoR)*(2.*!Pi*(antenna_height)/wavelength)) ;should technically have zc_arr, but until that is nonzero this is the same and faster
groundplane0=2.*Sin(Cos(0.*!DtoR)*2.*!Pi*antenna_height/wavelength) ;normalization factor

;horizon_test=where(abs(za_arr) GE 90.,n_horizon_test)
;horizon_mask=fltarr(psf_dim2,psf_dim2)+1
;IF n_horizon_test GT 0 THEN horizon_mask[horizon_test]=0    

voltage_delay=Exp(icomp*2.*!Pi*D0_d*frequency)*antenna_gain_arr ;I think this should account for missing dipoles correctly
IF Keyword_Set(mutual_coupling) THEN BEGIN
    port_current=mutual_coupling#voltage_delay
ENDIF ELSE BEGIN
    port_current=voltage_delay
ENDELSE

IF not Keyword_Set(antenna_beam_arr) THEN antenna_beam_arr=Ptrarr(16,/allocate)
FOR i=0,15 DO *antenna_beam_arr[i]=dipole_gain_arr[*,*,i]*projection/16.*groundplane/groundplane0;*pol

tile_beam=Complexarr(psf_dim2,psf_dim2)
FOR i=0,15 DO tile_beam+=*antenna_beam_arr[i]*port_current[i]

tile_beam*=normalization;*horizon_mask

RETURN,tile_beam

END