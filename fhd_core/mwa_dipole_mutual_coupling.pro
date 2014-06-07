FUNCTION mwa_dipole_mutual_coupling,freq_arr

file_path_Z_matrix=filepath('Zmatrix.fits',root=rootdir('FHD'),sub='instrument_config')
file_path_Z_LNA=filepath('LNA_impedance_mwa.sav',root=rootdir('FHD'),sub='instrument_config')
n_dipole=16
n_ant_pol=2
icomp=Complex(0,1)

Zmat_arr=Ptrarr(1)
freq_arr_Zin=Fltarr(1)
status=0
ext_i=0
fits_info,file_path_Z_matrix,/silent,n_ext=n_ext
n_ext+=1
FOR ext_i=0,n_ext-1 DO BEGIN
    Zmat1=mrdfits(file_path_Z_matrix,ext_i,header,status=status,/silent)
    IF ext_i EQ 0 THEN BEGIN
        Zmat_arr[0]=Ptr_new(Zmat1)
        freq_arr_Zin[0]=Float(sxpar(header,'FREQ'))
    ENDIF ELSE IF status GE 0 THEN BEGIN
        Zmat_arr=[Zmat_arr,Ptr_new(Zmat1)]
        freq_arr_Zin=[freq_arr_Zin,Float(sxpar(header,'FREQ'))]
    ENDIF
ENDFOR

lna_impedance=getvar_savefile(file_path_Z_LNA,'lna_impedance')
freq_arr_lna=lna_impedance.frequency

n_freq=N_Elements(freq_arr)
Zmat_return=Ptrarr(n_ant_pol,n_freq)
FOR fi=0L,n_freq-1 DO BEGIN
    freq_Zin=Min(Abs(freq_arr[fi]-freq_arr_Zin),fi_Zin)
    freq_lna=Min(Abs(freq_arr[fi]-freq_arr_lna),fi_lna)
    Zmat=*Zmat_arr[fi_Zin]
    Zmat_mag_x=Reform((*Zmat_arr[fi_Zin])[n_dipole:*,n_dipole:*,0]) ;ordering in Z matrix is 0-15:Y, 16-31:X
    Zmat_phase_x=Reform((*Zmat_arr[fi_Zin])[n_dipole:*,n_dipole:*,0])*!DtoR
    Zmat_x=Zmat_mag_x*(Cos(Zmat_phase_x)+icomp*Sin(Zmat_phase_x))
    Zmat_mag_y=Reform((*Zmat_arr[fi_Zin])[0:n_dipole-1,0:n_dipole-1,0]) ;ordering in Z matrix is 0-15:Y, 16-31:X
    Zmat_phase_y=Reform((*Zmat_arr[fi_Zin])[0:n_dipole-1,0:n_dipole-1,0])*!DtoR
    Zmat_y=Zmat_mag_y*(Cos(Zmat_phase_y)+icomp*Sin(Zmat_phase_y))
    Zlna=lna_impedance.z[fi_lna]*Identity(n_dipole)
    
    Zmat_x+=Zlna
    Zmat_y+=Zlna
    Zinv_x=LA_Invert(Zmat_x)
    Zinv_y=LA_Invert(Zmat_y)
    
    ;for now, also normalize:
    Zinv_x=Zinv_x/Total(Abs(Zinv_x)/n_dipole)
    Zinv_y=Zinv_y/Total(Abs(Zinv_y)/n_dipole)
    
;    Zinv_x_phase=diag_matrix(Atan(Zinv_x,/phase))/2.
;    Zinv_y_phase=diag_matrix(Atan(Zinv_y,/phase))/2.
;    
;    FOR i=0,n_dipole-1 DO BEGIN
;        FOR j=0,n_dipole-1 DO BEGIN
;            Zinv_x[i,j]*=Exp(-icomp*(Zinv_x_phase[i]+Zinv_x_phase[j]))
;            Zinv_y[i,j]*=Exp(-icomp*(Zinv_y_phase[i]+Zinv_y_phase[j]))
;        ENDFOR
;    ENDFOR
    
    Zmat_return[0,fi]=Ptr_new(Zinv_x)
    Zmat_return[1,fi]=Ptr_new(Zinv_y)
ENDFOR
Ptr_free,Zmat_arr


RETURN,Zmat_return
END