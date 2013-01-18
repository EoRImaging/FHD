;+
; :Description:
;    Construct the Stokes to instrumental polarization transformation matrix.
;    
;    dimensions are 4x4 (each a pointer referencing a dimension X elements array)
;    
;    columns are I, Q, U, V
;    
;    rows are XX, YY, XY, YX
;    
;    use polarization_map to get FROM Stokes TO instrumental, and polarization_correction to get FROM instrumental TO Stokes

;
; :Params:
;    azimuth_arr - array of azimuth values (degrees) of the same dimensions as the image
;    
;    elevation_arr - array of elevation values (degrees) of the same dimensions as the image
;    
;    polarization_correction - returns the inverse transformation matrix
;
; :Keywords:
;    stokes_zenith
;
; :Author: Ian
;-
FUNCTION polarization_map_create,azimuth_arr, elevation_arr, polarization_correction,$
    stokes_zenith=stokes_zenith,pflag=pflag

dimension=(size(azimuth_arr,/dimension))[0]
elements=(size(azimuth_arr,/dimension))[1]

polarization_map=Ptrarr(4,4,/allocate)
polarization_correction=Ptrarr(4,4,/allocate)
FOR i=0,3 DO FOR j=0,3 DO *polarization_map[i,j]=fltarr(dimension,elements)
FOR i=0,3 DO FOR j=0,3 DO *polarization_correction[i,j]=fltarr(dimension,elements)

IF N_Elements(stokes_zenith) EQ 0 THEN stokes_zenith=[1,0,0,0]
IF Keyword_Set(pflag) THEN BEGIN
    azimuth_cen=azimuth_arr[dimension/2.,elements/2.]
    elevation_cen=elevation_arr[dimension/2.,elements/2.]
    intensity_use=stokes_off_zenith(azimuth_cen, elevation_cen, stokes_zenith, $
        Ex_mag, Ey_mag,Cross_xy=Cross_xy,Cross_yx=Cross_yx,/intensity)
    matrix=fltarr(4,4)
    matrix[0,*]=[Ex_mag^2.,Ey_mag^2.,0,0]
    matrix[1,*]=[Ex_mag^2.,-Ey_mag^2.,0.,0.]
    matrix[2,*]=[0,0.,Cross_xy,Cross_yx]
    matrix[3,*]=[0,0.,Cross_xy,-Cross_yx]
    matrix_inv=Invert(matrix)
    FOR ii=0,3 DO BEGIN
        FOR jj=0,3 DO BEGIN
            (*polarization_map[ii,jj])+=matrix[ii,jj]
            (*polarization_correction[jj,ii])+=matrix_inv[ii,jj]
        ENDFOR
    ENDFOR
    
ENDIF ELSE BEGIN

    i_use=where((azimuth_arr NE 0) AND (elevation_arr NE 0),n_use)
    azimuth_use=azimuth_arr[i_use]
    elevation_use=elevation_arr[i_use]
    stokes_zenith = [1, 0, 0, 0]
    
    intensity_use=stokes_off_zenith(azimuth_use, elevation_use, stokes_zenith, $
        Ex_mag, Ey_mag,Cross_xy=Cross_xy,Cross_yx=Cross_yx,/intensity)
    
;    intensity_map=fltarr(dimension,elements)
;    intensity_map[i_use]=Float(intensity_use)
    
    FOR i=0.,n_use-1 DO BEGIN
        matrix=fltarr(4,4)
        matrix[0,*]=[Ex_mag[i]^2.,Ey_mag[i]^2.,0,0]
        matrix[1,*]=[Ex_mag[i]^2.,-Ey_mag[i]^2.,0.,0.]
        matrix[2,*]=[0,0.,Cross_xy[i],Cross_yx[i]]
        matrix[3,*]=[0,0.,Cross_xy[i],-Cross_yx[i]]
;        matrix/=intensity_use[i]
        matrix_inv=Invert(matrix)
        FOR ii=0,3 DO BEGIN
            FOR jj=0,3 DO BEGIN
                (*polarization_map[ii,jj])[i_use[i]]=matrix[ii,jj]
                (*polarization_correction[jj,ii])[i_use[i]]=matrix_inv[ii,jj]
            ENDFOR
        ENDFOR
    ENDFOR
ENDELSE
RETURN,polarization_map
END