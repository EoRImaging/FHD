FUNCTION mwa_beam_setup_gain,obs,antenna,file_path_fhd=file_path_fhd,$
    za_arr=za_arr,az_arr=az_arr,psf_image_dim=psf_image_dim,debug_flip=debug_flip,$
    _EXTRA = EXTRA 

n_ant_pol=Max(antenna.n_pol)
nfreq_bin=Max(antenna.nfreq_bin)
IF N_Elements(file_path_fhd) EQ 0 THEN file_path_fhd=''
n_tile=obs.n_tile
beam_model_version=Max(antenna.model_version)
xvals_instrument=za_arr*Sin(az_arr*!DtoR)
yvals_instrument=za_arr*Cos(az_arr*!DtoR)
freq_center=antenna[0].freq ;all need to be identical, so just use the first
speed_light=299792458. ;speed of light, in meters/second
icomp=Complex(0,1)

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
            FOR p_i=0,n_ant_pol-1 DO FOR p_j=0,n_ant_pol-1 DO BEGIN
                Jmat_arr[ext_i,p_i,p_j,*]=Jmat1[2+p_i*2+p_j*4,*]+icomp*Jmat1[2+p_i*2+p_j*4+1,*]
                if keyword_set(debug_flip) then Jmat_arr[ext_i,p_j,p_i,*]=Jmat1[2+p_i*2+p_j*4,*]+icomp*Jmat1[2+p_i*2+p_j*4+1,*]
            ENDFOR
            freq_arr_Jmat[ext_i]=Float(sxpar(header,'FREQ')) ;in Hz
        ENDFOR
        theta_arr=median(theta_arr,dimension=1) ; all actually the same across freq, so reduce dimension
        phi_arr=median(phi_arr,dimension=1) ; all actually the same across freq, so reduce dimension
        phi_arr=270.-phi_arr ;change azimuth convention
        
;        ;normalize to zenith
;        norm_factor=fltarr(nfreq_bin,n_ant_pol)
;        FOR p_i=0,n_ant_pol-1 DO norm_factor[*,p_i]=interpol(Max(abs(Jmat_arr[*,p_i,p_i,*]),dimension=4),freq_arr_Jmat,freq_center)
;        norm_factor=1./Median(norm_factor,dimension=2,/even)
        
;        zenith_i=where(theta_arr EQ 0,n_zen)
;        max_zen=Max(abs(Jmat_arr[*,*,*,zenith_i]),max_zen_i,dimension=4)
;        FOR z_i=0L,n_zen-1 DO FOR p_i=0,n_ant_pol-1 DO FOR p_j=0,n_ant_pol-1 DO FOR ext_i=0,n_ext-1 DO $
;            Jmat_arr[ext_i,p_i,p_j,zenith_i[z_i]]=(Jmat_arr[*,*,*,zenith_i])[max_zen_i[ext_i,p_i,p_j]]
        
    ;    Jmat_return=Ptrarr(n_ant_pol,n_ant_pol)
        Jmat_interp=Ptrarr(n_ant_pol,n_ant_pol,nfreq_bin)
        FOR p_i=0,n_ant_pol-1 DO FOR p_j=0,n_ant_pol-1 DO FOR freq_i=0L,nfreq_bin-1 DO Jmat_interp[p_i,p_j,freq_i]=Ptr_new(Complexarr(n_ang))
        FOR p_i=0,n_ant_pol-1 DO FOR p_j=0,n_ant_pol-1 DO FOR a_i=0L,n_ang-1 DO BEGIN
            Jmat_single_ang=Interpol(Jmat_arr[*,p_i,p_j,a_i],freq_arr_Jmat,freq_center);*norm_factor
            FOR freq_i=0L,nfreq_bin-1 DO (*Jmat_interp[p_i,p_j,freq_i])[a_i]=Jmat_single_ang[freq_i]
;            (*Jmat_interp[p_i,p_j])[a_i]=Interpol(Jmat_arr[*,p_i,p_j,a_i],freq_arr_Jmat,freq_center)
        ENDFOR
        
        xv_model=theta_arr*Sin(phi_arr*!DtoR)
        yv_model=theta_arr*Cos(phi_arr*!DtoR)
        
        zenith_i=where(theta_arr EQ 0,n_zenith)
;        IF n_zenith GT 0 THEN $
;            FOR p_i=0,n_ant_pol-1 DO FOR p_j=0,n_ant_pol-1 DO FOR freq_i=0L,nfreq_bin-1 DO (*Jmat_interp[p_i,p_j,freq_i])[zenith_i]=Sqrt(Mean(Abs((*Jmat_interp[p_i,p_j,freq_i])[zenith_i])^2.))
        
        horizon_test=where(abs(za_arr) GE 90.,n_horizon_test,complement=pix_use,ncomplement=n_pix)
        horizon_mask=fltarr(psf_image_dim,psf_image_dim)+1
        IF n_horizon_test GT 0 THEN horizon_mask[horizon_test]=0  
        Jones_matrix=Ptrarr(n_ant_pol,n_ant_pol,nfreq_bin)
        FOR p_i=0,n_ant_pol-1 DO FOR p_j=0,n_ant_pol-1 DO FOR freq_i=0L,nfreq_bin-1 DO $
            Jones_matrix[p_i,p_j,freq_i]=Ptr_new(Complexarr(psf_image_dim,psf_image_dim))
            
        interp_res=obs.degpix
        angle_slice_i0=Uniq(phi_arr)
        n_ang_slice=N_Elements(angle_slice_i0)
        n_zen_slice=angle_slice_i0[0]+1
        az_ang_in=phi_arr[angle_slice_i0]
        zen_ang_in=theta_arr[0:angle_slice_i0[0]]
        zen_ang_out=Findgen(Ceil(90./interp_res)+1)*interp_res
        az_ang_out=Findgen(Ceil(360./interp_res)+1)*interp_res+Round(Min(az_ang_in))
        n_zen_ang=N_Elements(zen_ang_out)
        n_az_ang=N_Elements(az_ang_out)
        
        xv_model=zen_ang_out*Sin(az_ang_out*!DtoR)
        yv_model=zen_ang_out*Cos(az_ang_out*!DtoR)
        
        zen_ang_inst=Sqrt(xvals_instrument[pix_use]^2+yvals_instrument[pix_use]^2)
        az_ang_inst=Atan(yvals_instrument[pix_use],xvals_instrument[pix_use])*!Radeg+180.
        
        FOR p_i=0,n_ant_pol-1 DO FOR p_j=0,n_ant_pol-1 DO FOR freq_i=0L,nfreq_bin-1 DO BEGIN
            Jmat_use=Reform(*Jmat_interp[p_i,p_j,freq_i],n_zen_slice,n_ang_slice)
            Expand,Jmat_use,n_zen_ang,n_az_ang,Jmat_single
            (*Jones_matrix[p_i,p_j,freq_i])[pix_use]=Interpolate(Jmat_single,zen_ang_inst/interp_res,az_ang_inst/interp_res,cubic=-0.5)
            debug_point=1
        ENDFOR
        debug_point=1
;        FOR i=0L,n_pix-1 DO BEGIN
;            xv_instrument1=xvals_instrument[pix_use[i]]
;            yv_instrument1=yvals_instrument[pix_use[i]]
;            
;            IF Abs(xv_instrument1) LT 1 AND Abs(yv_instrument1) LT 1 THEN BEGIN
;                debug_point=1
;            ENDIF
;            
;            dist_test=Sqrt((xv_instrument1-xv_model)^2.+(yv_instrument1-yv_model)^2.)
;;            IF Min(dist_test,min_i) EQ 0 THEN BEGIN ;test if there is an exact match. Then just take that value and do no interpolation
;;                FOR p_i=0,n_ant_pol-1 DO FOR p_j=0,n_ant_pol-1 DO FOR freq_i=0L,nfreq_bin-1 DO $
;;                    (*Jones_matrix[p_i,p_j,freq_i])[pix_use[i]]=(*Jmat_interp[p_i,p_j,freq_i])[min_i]
;;                CONTINUE ;move on to next iteration of pixel FOR loop
;;            ENDIF ;SKIP THIS TEST, BECAUSE THERE ARE DUPLICATE ENTRIES WITH DIFFERENT VALUES
;            dist_ref=Min(dist_test)
;            i_use=where(dist_test LE dist_ref*Sqrt(2.),n_use)
;;            IF n_use LE 4 THEN BEGIN
;                i_use=(Sort(dist_test))[0:3] ;there had better be at least four points in the model!
;                weight=1./dist_test[i_use]
;                FOR p_i=0,n_ant_pol-1 DO FOR p_j=0,n_ant_pol-1 DO FOR freq_i=0L,nfreq_bin-1 DO BEGIN
;                    (*Jones_matrix[p_i,p_j,freq_i])[pix_use[i]]=Total((*Jmat_interp[p_i,p_j,freq_i])[i_use]*weight)/Total(weight)
;                ENDFOR
;;            ENDIF ELSE BEGIN
;;                weight=1./dist_test[i_use]
;;                FOR p_i=0,n_ant_pol-1 DO FOR p_j=0,n_ant_pol-1 DO FOR freq_i=0L,nfreq_bin-1 DO BEGIN
;;                    amp=Sqrt(Total(Abs((*Jmat_interp[p_i,p_j,freq_i])[i_use])^2.*weight)/Total(weight))
;;                    phase=Total(Atan((*Jmat_interp[p_i,p_j,freq_i])[i_use],/phase)*weight)/Total(weight)
;;                    (*Jones_matrix[p_i,p_j,freq_i])[pix_use[i]]=amp*Exp(icomp*phase)
;;                ENDFOR
;;            ENDELSE
;        ENDFOR
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