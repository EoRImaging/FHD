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
FUNCTION polarization_map_create,obs,azimuth_arr=azimuth_arr, elevation_arr=elevation_arr,use_pointing_center=use_pointing_center,$
    polarization_correction=polarization_correction,stokes_zenith=stokes_zenith,pflag=pflag,trace_return=trace_return

IF N_Elements(azimuth_arr) EQ 0 OR N_Elements(elevation_arr) EQ 0 THEN BEGIN
    dimension=obs.dimension
    elements=obs.elements
    xvals=meshgrid(dimension,elements,1)
    yvals=meshgrid(dimension,elements,2)
    xy2ad,xvals,yvals,obs.astr,ra_arr,dec_arr
    valid_i=where(Finite(ra_arr),n_valid)
    ra_use=ra_arr[valid_i]
    dec_use=dec_arr[valid_i]
    
    IF Keyword_Set(use_pointing_center) THEN BEGIN 
;        zen_arr=angle_difference(obs.obsdec,obs.obsra,dec_use,ra_use,/degree,/nearest)
        hour_angle=obs.obsra - ra_use
        h_neg = where(hour_angle LT 0, N_neg)
        IF N_neg GT 0 THEN hour_angle[h_neg] = hour_angle[h_neg] + 360.
        hour_angle = hour_angle mod 360.
        hadec2altaz, hour_angle, dec_use, obs.obsdec, elevation_use, azimuth_use
    ENDIF ELSE BEGIN
        Eq2Hor,ra_use,dec_use,obs.Jd0,elevation_use,azimuth_use,lat=obs.lat,lon=obs.lon,alt=obs.alt,precess=1
        
    ENDELSE
    azimuth_arr=Fltarr(dimension,elements)
    elevation_arr=Fltarr(dimension,elements)
    azimuth_arr[valid_i]=azimuth_use
    elevation_arr[valid_i]=elevation_use
ENDIF ELSE BEGIN
    dimension=(size(azimuth_arr,/dimension))[0]
    elements=(size(azimuth_arr,/dimension))[1]
ENDELSE

polarization_map=Ptrarr(4,4,/allocate)
polarization_correction=Ptrarr(4,4,/allocate)
FOR i=0,3 DO FOR j=0,3 DO *polarization_map[i,j]=fltarr(dimension,elements)
FOR i=0,3 DO FOR j=0,3 DO *polarization_correction[i,j]=fltarr(dimension,elements)

IF N_Elements(stokes_zenith) EQ 0 THEN stokes_zenith=[1.,0.,0.,0.]
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
IF Keyword_Set(trace_return) THEN BEGIN
    test_ind=intarr(4,4)+1
    test_ind[[0,0,2,2],[0,1,2,3]]=0
    free_ind=where(test_ind,n_free)
    FOR ind=0,n_free-1 DO Ptr_free,polarization_map[free_ind[ind]],polarization_correction[free_ind[ind]]
    polarization_map=[polarization_map[0,0],polarization_map[0,1],polarization_map[2,2],polarization_map[2,3]]
    polarization_correction=[polarization_correction[0,0],polarization_correction[0,1],polarization_correction[2,2],polarization_correction[2,3]]
ENDIF
RETURN,polarization_map
END