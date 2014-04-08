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
FUNCTION polarization_map_create,obs,polarization_correction=polarization_correction,$
    mask=mask,trace_return=trace_return

dimension=obs.dimension
elements=obs.elements
IF N_Elements(mask) EQ 0 THEN mask=Replicate(1.,dimension,elements)

xvals=meshgrid(dimension,elements,1)
yvals=meshgrid(dimension,elements,2)
xy2ad,xvals,yvals,obs.astr,ra_arr,dec_arr
valid_i=where(Finite(ra_arr) AND mask,n_valid)
ra_use=ra_arr[valid_i]
dec_use=dec_arr[valid_i]
xv=xvals[valid_i]
yv=yvals[valid_i]

hour_angle=obs.obsra - ra_use
h_neg = where(hour_angle LT 0, N_neg)
IF N_neg GT 0 THEN hour_angle[h_neg] = hour_angle[h_neg] + 360.
hour_angle = hour_angle mod 360.

lat=obs.lat*!DtoR
dec=dec_use*!DtoR
ha=hour_angle*!DtoR
;calculate the elements of the dipole projection matrix J ((J11, J12), (J21,J22))
J11=Cos(lat)*Cos(dec)+Sin(lat)*Sin(dec)*Cos(ha)
J12=-Sin(lat)*Sin(ha)
J21=Sin(dec)*Sin(ha)
J22=Cos(ha)

p_map=Ptrarr(4,4,/allocate)
p_corr=Ptrarr(4,4,/allocate)
FOR i=0,3 DO FOR j=0,3 DO *p_map[i,j]=fltarr(dimension,elements)
FOR i=0,3 DO FOR j=0,3 DO *p_corr[i,j]=fltarr(dimension,elements)
FOR pix=0L,n_valid-1 DO BEGIN
    ;calculate tensor product J(X)J* 
    ;Jmat converts [pp,qq,pq,qp] -> [xx,yy,xy,yx]
    ;Jinv converts [xx, yy, xy, yx] -> [pp, qq, pq, qp]
    ;Note: Stokes [I, Q, U, V] = (1./2.)*[(pp+qq), (pp-qq), (pq+qp), (pq-qp)]
    
    Jmat=[[J11[pix]^2.,J12[pix]^2.,J11[pix]*J12[pix],J12[pix]*J11[pix]],[J21[pix]^2.,J22[pix]^2.,J21[pix]*J22[pix],J22[pix]*J21[pix]],$
        [J11[pix]*J21[pix],J12[pix]*J22[pix],J11[pix]*J22[pix],J12[pix]*J21[pix]],[J11[pix]*J21[pix],J12[pix]*J22[pix],J12[pix]*J21[pix],J11[pix]*J22[pix]]]
    Jinv=Invert(Jmat)
    
    FOR i=0,3 DO FOR j=0,3 DO (*p_map[i,j])[valid_i[pix]]=Jmat[i,j]
    FOR i=0,3 DO FOR j=0,3 DO (*p_corr[i,j])[valid_i[pix]]=Jinv[i,j]
ENDFOR

polarization_map=p_map
polarization_correction=p_corr
IF Keyword_Set(trace_return) THEN BEGIN
    test_ind=intarr(4,4)+1
    test_ind[[0,1,2,3],[0,1,2,3]]=0
    free_ind=where(test_ind,n_free)
    FOR ind=0,n_free-1 DO Ptr_free,polarization_map[free_ind[ind]],polarization_correction[free_ind[ind]]
    polarization_map=[polarization_map[0,0],polarization_map[1,1],polarization_map[2,2],polarization_map[3,3]]
    polarization_correction=[polarization_correction[0,0],polarization_correction[1,1],polarization_correction[2,2],polarization_correction[3,3]]
ENDIF
RETURN,polarization_map
END