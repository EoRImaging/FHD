PRO mwa_dipole_mutual_coupling,freq_arr

file_path_Z_matrix=filepath('Zmatrix.fits',root=rootdir('FHD'),sub='instrument_config')
file_path_Z_LNA=filepath('LNA_impedance_mwa.sav',root=rootdir('FHD'),sub='instrument_config')


Zmat_arr=Ptrarr(1)
freq_arr1=Fltarr(1)
status=0
ext_i=0
fits_info,file_path_Z_matrix,/silent,n_ext=n_ext
n_ext+=1
FOR ext_i=0,n_ext-1 DO BEGIN
    Zmat1=mrdfits(file_path_Z_matrix,ext_i,header,status=status,/silent)
    IF ext_i EQ 0 THEN BEGIN
        Zmat_arr[0]=Ptr_new(Zmat1)
        freq_arr1[0]=Float(sxpar(header,'FREQ'))
    ENDIF ELSE IF status GE 0 THEN BEGIN
        Zmat_arr=[Zmat_arr,Ptr_new(Zmat1)]
        freq_arr1=[freq_arr1,Float(sxpar(header,'FREQ'))]
    ENDIF
ENDFOR

lna_impedance=getvar_savefile(file_path_Z_LNA,'lna_impedance')
freq_arr2=lna_impedance.frequency

n_freq=N_Elements(freq_arr)
FOR fi=0L,n_freq-1 DO BEGIN
    freq1=Min(Abs(freq_arr[fi]-freq_arr1),fi1)
    freq2=Min(Abs(freq_arr[fi]-freq_arr2),fi2)
    Zmat=*Zmat_arr[fi1]
    Zlna=lna_impedance.z[fi2]
    debug_line=1
ENDFOR

;RETURN,matrix_arr
END