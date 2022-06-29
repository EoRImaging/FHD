FUNCTION mwa_dipole_mutual_coupling,freq_arr

file_path_Z_matrix=filepath('mwa_ZMatrix.fits',root=rootdir('FHD'),sub='instrument_config')
file_path_Z_LNA=filepath('mwa_LNA_impedance.sav',root=rootdir('FHD'),sub='instrument_config')
n_dipole=16
n_ant_pol=2
icomp=DComplex(0,1)

status=0
fits_info,file_path_Z_matrix,/silent,n_ext=n_ext
n_ext+=1 ;n_ext starts counting AFTER the 0th extension, which it considers to be the main data unit, but we use that one too
Zmat_arr=DComplexarr(n_ext,n_ant_pol,n_dipole,n_dipole)
freq_arr_Zmat=Dblarr(n_ext)
FOR ext_i=0,n_ext-1 DO BEGIN
    Zmat1=mrdfits(file_path_Z_matrix,ext_i,header,status=status,/silent)
    Zmat=Zmat1[*,*,0]*(Cos(Zmat1[*,*,1])+icomp*Sin(Zmat1[*,*,1]))
    freq_arr_Zmat[ext_i]=sxpar(header,'FREQ')
    Zmat_arr[ext_i,0,*,*]=Zmat[n_dipole:*,n_dipole:*] ;ordering in Z matrix is 0-15:Y, 16-31:X
    Zmat_arr[ext_i,1,*,*]=Zmat[0:n_dipole-1,0:n_dipole-1] ;ordering in Z matrix is 0-15:Y, 16-31:X
ENDFOR

lna_impedance=getvar_savefile(file_path_Z_LNA,'lna_impedance')
freq_arr_lna=lna_impedance.frequency

n_freq=N_Elements(freq_arr)
Zmat_return=Ptrarr(n_ant_pol,n_freq)
Zmat_interp=DComplexarr(n_freq,n_ant_pol,n_dipole,n_dipole)
FOR pol_i=0,n_ant_pol-1 DO FOR di1=0,n_dipole-1 DO FOR di2=0,n_dipole-1 DO $
    Zmat_interp[*,pol_i,di1,di2]=Interpol(Zmat_arr[*,pol_i,di1,di2],freq_arr_Zmat,freq_arr)

Zlna_arr=interpol(lna_impedance.z,freq_arr_lna,freq_arr)

FOR fi=0L,n_freq-1 DO BEGIN
    
    Zmat_X=Reform(Zmat_interp[fi,0,*,*])
    Zmat_Y=Reform(Zmat_interp[fi,1,*,*])
    Zlna=Zlna_arr[fi]*Identity(n_dipole)
    
    Zinv_x=LA_Invert(Zlna+Zmat_x)
    Zinv_y=LA_Invert(Zlna+Zmat_y)
    
    ;normalize to a zenith pointing, where voltage=Exp(icomp*2.*!Pi*Delay*frequency) and delay=0 so voltage=1.    
    
    norm_test_x=n_dipole/abs(total(Zinv_x)) ;effectively the same as 1./Mean(Zinv_x#replicate(1.,n_dipole))
    norm_test_y=n_dipole/abs(total(Zinv_y))
    Zinv_x*=norm_test_x
    Zinv_y*=norm_test_y
        
    Zmat_return[0,fi]=Ptr_new(Zinv_x)
    Zmat_return[1,fi]=Ptr_new(Zinv_y)
ENDFOR

RETURN,Zmat_return
END
