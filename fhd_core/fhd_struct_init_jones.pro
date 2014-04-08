FUNCTION fhd_struct_init_jones,obs,file_path_fhd=file_path_fhd,mask=mask

IF Keyword_Set(file_path_fhd) THEN proj_filename=file_path_fhd+'_jones.sav'
dimension=obs.dimension
elements=obs.elements
IF N_Elements(mask) EQ 0 THEN mask=Replicate(1.,dimension,elements)

xvals=meshgrid(dimension,elements,1)
yvals=meshgrid(dimension,elements,2)
xy2ad,xvals,yvals,obs.astr,ra_arr,dec_arr
inds_use=where(Finite(ra_arr) AND mask,n_pix)
ra_use=ra_arr[inds_use]
dec_use=dec_arr[inds_use]
xv=xvals[inds_use]
yv=yvals[inds_use]

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
FOR i=0,3 DO FOR j=0,3 DO *p_map[i,j]=fltarr(n_pix)
FOR i=0,3 DO FOR j=0,3 DO *p_corr[i,j]=fltarr(n_pix)
Smat=(-0.5)*[[1,1,0,0],[1,-1,0,0],[0,0,1,1],[0,0,1,-1]]
Smat=Reverse(Smat,2)
FOR pix=0L,n_pix-1 DO BEGIN
    ;calculate tensor product J(X)J* 
    ;Jmat converts [pp,qq,pq,qp] -> [xx,yy,xy,yx]
    ;Jinv converts [xx, yy, xy, yx] -> [pp, qq, pq, qp]
    ;Note: Stokes [I, Q, U, V] = (1./2.)*[(pp+qq), (pp-qq), (pq+qp), (pq-qp)]
    
    Jmat=[[J11[pix]^2.,J12[pix]^2.,J11[pix]*J12[pix],J12[pix]*J11[pix]],[J21[pix]^2.,J22[pix]^2.,J21[pix]*J22[pix],J22[pix]*J21[pix]],$
        [J11[pix]*J21[pix],J12[pix]*J22[pix],J11[pix]*J22[pix],J12[pix]*J21[pix]],[J11[pix]*J21[pix],J12[pix]*J22[pix],J12[pix]*J21[pix],J11[pix]*J22[pix]]]
;    Jmat=Matrix_multiply(Jmat,Smat,/bt)
    Jinv=Invert(Jmat)
    
    FOR i=0,3 DO FOR j=0,3 DO (*p_map[i,j])[pix]=Jmat[i,j]
    FOR i=0,3 DO FOR j=0,3 DO (*p_corr[i,j])[pix]=Jinv[i,j]
ENDFOR

jones={inds:inds_use,dimension:dimension,elements:elements,Jmat:p_map,Jinv:p_corr}

;polarization_map=p_map
;polarization_correction=p_corr
;IF Keyword_Set(trace_return) THEN BEGIN
;    test_ind=intarr(4,4)+1
;    test_ind[[0,1,2,3],[0,1,2,3]]=0
;    free_ind=where(test_ind,n_free)
;    FOR ind=0,n_free-1 DO Ptr_free,polarization_map[free_ind[ind]],polarization_correction[free_ind[ind]]
;    polarization_map=[polarization_map[0,0],polarization_map[1,1],polarization_map[2,2],polarization_map[3,3]]
;    polarization_correction=[polarization_correction[0,0],polarization_correction[1,1],polarization_correction[2,2],polarization_correction[3,3]]
;ENDIF

IF Keyword_Set(file_path_fhd) THEN SAVE,jones,filename=proj_filename,/compress
RETURN,jones
END