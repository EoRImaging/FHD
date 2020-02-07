FUNCTION import_az_el_beam, obs, antenna, file_path_J_matrix,$
    za_arr=za_arr,az_arr=az_arr,psf_image_dim=psf_image_dim
; Read in an e-field antenna beam stored in evenly-spaced azimuth-elevation (and frequency)

n_ant_pol=Max(antenna.n_pol)
nfreq_bin=Max(antenna.nfreq_bin)
n_tile=obs.n_tile
xvals_instrument=za_arr*Sin(az_arr*!DtoR)
yvals_instrument=za_arr*Cos(az_arr*!DtoR)
freq_center=antenna[0].freq ;all need to be identical, so just use the first
icomp=Complex(0,1)

fits_info,file_path_J_matrix,/silent,n_ext=n_ext
;Cols of J matrix: theta phi  real(Jxt(t,p)) imag(Jxt(t,p)) real(Jxp(t,p)) imag(Jxp(t,p)) real(Jyt(t,p)) imag(Jyt(t,p)) real(Jyp(t,p)) imag(Jyp(t,p)))
;Where theta is the zenith angle, phi is angle measured clockwise from +east direction looking down
; Jxt is the Jones mapping unit vec in theta (t) direction to the x (east-west) dipole etc
n_ext+=1 ;n_ext starts counting AFTER the 0th extension, which it considers to be the main data unit, but we use that one too
freq_arr_Jmat=Fltarr(n_ext)

;FOR ext_i=0,n_ext-1 DO BEGIN
;    Jmat1=mrdfits(file_path_J_matrix,ext_i,header,status=status,/silent)
;    IF ext_i EQ 0 THEN BEGIN
;        n_ang=Float(sxpar(header,'NAXIS2'))
;        Jmat_arr=Dcomplexarr(n_ext,n_ant_pol,n_ant_pol,n_ang)
;        theta_arr=Dblarr(n_ext,n_ang)
;        phi_arr=Dblarr(n_ext,n_ang)
;    ENDIF
;    theta_arr[ext_i,*]=Jmat1[0,*] ;zenith angle in degrees
;    phi_arr[ext_i,*]=Jmat1[1,*] ;azimuth angle in degrees, clockwise from East
;    FOR p_i=0,n_ant_pol-1 DO FOR p_j=0,n_ant_pol-1 DO BEGIN
;        Jmat_arr[ext_i,p_i,p_j,*]=Jmat1[2+p_i*2+p_j*4,*]+icomp*Jmat1[2+p_i*2+p_j*4+1,*]
;        if keyword_set(debug_flip) then Jmat_arr[ext_i,p_j,p_i,*]=Jmat1[2+p_i*2+p_j*4,*]+icomp*Jmat1[2+p_i*2+p_j*4+1,*]
;    ENDFOR
;    freq_arr_Jmat[ext_i]=Float(sxpar(header,'FREQ')) ;in Hz
;ENDFOR
;theta_arr=median(theta_arr,dimension=1) ; all actually the same across freq, so reduce dimension
;phi_arr=median(phi_arr,dimension=1) ; all actually the same across freq, so reduce dimension

;Begin hacks for pyuvdata beam
; CTYPE1: Azimuth in degrees, from 0 and increment of 1. Don't know origin: guessing clockwise from East
; CTYPE2: Zenith angle in degrees, from 0 and increment of 1
; CTYPE3: Frequency in Hz, from 50,000,000 and increment of 1,000,000
; CTYPE4: "Feed index", X then Y
; CTYPE5: Spectral window number. There is only one.
; CTYPE6: Basis vector index. guessing these are theta then phi
; CTYPE7: Complex, real then imaginary component. 
Jmat0=mrdfits(file_path_J_matrix,0,header,status=status,/silent)
n_za_ang = sxpar(header,'naxis2')
n_az_ang = sxpar(header,'naxis1')
n_freq = sxpar(header, 'naxis3')
n_feed = sxpar(header, 'naxis4')
n_basis = sxpar(header, 'naxis6')
n_ang = n_za_ang*n_az_ang

theta_arr0 = findgen(n_za_ang)*sxpar(header,'cdelt2') + sxpar(header, 'crval2') ;in degrees
phi_arr0 = findgen(n_az_ang)*sxpar(header,'cdelt1') + sxpar(header, 'crval1') ;in degrees
freq_arr_Jmat = findgen(sxpar(header,'naxis3'))*sxpar(header,'cdelt3') + sxpar(header, 'crval3') ; in Hz

az_ind_arr = Reform(Fix(meshgrid(n_za_ang, n_az_ang, 2)), n_ang)
za_ind_arr = Reform(Fix(meshgrid(n_za_ang, n_az_ang, 1)), n_ang)
theta_arr = theta_arr0[za_ind_arr]
phi_arr = phi_arr0[az_ind_arr]

Jmat_arr=Dcomplexarr(n_freq,n_basis,n_feed,n_ang)
FOR freq_i=0,n_freq-1 DO BEGIN
    FOR f_i=0,n_feed-1 DO BEGIN
        FOR b_i=0,n_basis-1 DO BEGIN
            Jmat1 = Reform(Jmat0[*, *, freq_i, f_i, 0, b_i, 0] + icomp*Jmat0[*, *, freq_i, f_i, 0, b_i, 1])
            Jmat1 = Transpose(Jmat1)
            Jmat_arr[freq_i,f_i,b_i,*] = Reform(Jmat1, n_ang)
        ENDFOR
    ENDFOR
ENDFOR

phi_arr=270.-phi_arr ;change azimuth convention


Jmat_interp=Ptrarr(n_ant_pol,n_ant_pol,nfreq_bin)
FOR p_i=0,n_ant_pol-1 DO FOR p_j=0,n_ant_pol-1 DO FOR freq_i=0L,nfreq_bin-1 DO Jmat_interp[p_i,p_j,freq_i]=Ptr_new(Dcomplexarr(n_ang))
FOR p_i=0,n_ant_pol-1 DO FOR p_j=0,n_ant_pol-1 DO FOR a_i=0L,n_ang-1 DO BEGIN
    Jmat_single_ang=Interpol(Jmat_arr[*,p_i,p_j,a_i],freq_arr_Jmat,freq_center);*norm_factor
    FOR freq_i=0L,nfreq_bin-1 DO (*Jmat_interp[p_i,p_j,freq_i])[a_i]=Jmat_single_ang[freq_i]
ENDFOR

xv_model=theta_arr*Sin(phi_arr*!DtoR)
yv_model=theta_arr*Cos(phi_arr*!DtoR)

zenith_i=where(theta_arr EQ 0,n_zenith)

horizon_test=where(abs(za_arr) GE 90.,n_horizon_test,complement=pix_use,ncomplement=n_pix)
horizon_mask=fltarr(psf_image_dim,psf_image_dim)+1
IF n_horizon_test GT 0 THEN horizon_mask[horizon_test]=0  
Jones_matrix=Ptrarr(n_ant_pol,n_ant_pol,nfreq_bin)
FOR p_i=0,n_ant_pol-1 DO FOR p_j=0,n_ant_pol-1 DO FOR freq_i=0L,nfreq_bin-1 DO $
    Jones_matrix[p_i,p_j,freq_i]=Ptr_new(Dcomplexarr(psf_image_dim,psf_image_dim))
    
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

RETURN, Jones_matrix
END
