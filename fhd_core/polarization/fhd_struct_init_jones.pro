FUNCTION fhd_struct_init_jones,obs,status_str,jones_in,file_path_fhd=file_path_fhd,mask=mask,$
    restore_last=restore_last,update_last=update_last,debug_jones_obs=debug_jones_obs,_Extra=extra

IF Keyword_Set(restore_last) THEN BEGIN
    fhd_save_io,status_str,jones,var='jones',file_path_fhd=file_path_fhd,/restore,_Extra=extra
    IF Keyword_Set(jones) THEN RETURN,jones
ENDIF
dimension=obs.dimension
elements=obs.elements

IF Keyword_Set(update_last) THEN BEGIN
    IF N_Elements(jones_in) EQ 0 THEN $
        jones_in=fhd_struct_init_jones(obs,status_str,file_path_fhd=file_path_fhd,mask=mask,/restore_last)
    dimension_in=jones_in.dimension
    elements_in=jones_in.elements
    mask_use=intarr(dimension_in,elements_in)
    mask_use[jones_in.inds]=1
    mask_use=Rebin(mask_use,dimension,elements)
    inds_use=where(mask_use,n_pix)
    p_map=Ptrarr(4,4,/allocate)
    p_corr=Ptrarr(4,4,/allocate)
    FOR pol_i2=0,3 DO FOR pol_i1=0,3 DO BEGIN
        temp=fltarr(dimension_in,elements_in)
        temp[jones_in.inds]=*jones_in.Jmat[pol_i1,pol_i2]
        temp=Rebin(temp,dimension,elements)
        *p_map[pol_i1,pol_i2]=temp[inds_use]
        
        temp=fltarr(dimension_in,elements_in)
        temp[jones_in.inds]=*jones_in.Jinv[pol_i1,pol_i2]
        temp=Rebin(temp,dimension,elements)
        *p_corr[pol_i1,pol_i2]=temp[inds_use]
    ENDFOR
    jones={inds:inds_use,dimension:dimension,elements:elements,Jmat:p_map,Jinv:p_corr}
    RETURN,jones
ENDIF
IF N_Elements(mask) EQ 0 THEN mask=Replicate(1.,dimension,elements)

xvals=meshgrid(dimension,elements,1)
yvals=meshgrid(dimension,elements,2)
;ignore effect of refraction, since we are only determining which pixels to include
apply_astrometry, obs, ra_arr=ra_arr, dec_arr=dec_arr, x_arr=xvals, y_arr=yvals, /xy2ad, /ignore_refraction
inds_use=where(Finite(ra_arr) AND mask,n_pix)
ra_use=ra_arr[inds_use]
dec_use=dec_arr[inds_use]
xv=xvals[inds_use]
yv=yvals[inds_use]

;IF Keyword_Set(debug_jones_obs) THEN hour_angle=obs.obsra - ra_use ELSE hour_angle=obs.zenra - ra_use
hour_angle=obs.obsra - ra_use
h_neg = where(hour_angle LT 0, N_neg)
IF N_neg GT 0 THEN hour_angle[h_neg] = hour_angle[h_neg] + 360.
hour_angle = hour_angle mod 360.

;IF Keyword_Set(debug_jones_obs) THEN phi=obs.obsdec*!DtoR ELSE phi=obs.zendec*!DtoR
;Convert observatory latitude to same coordinate system 
apply_astrometry, obs, ra_arr=obs.obsra, dec_arr=obs.lat, x_arr=x_t, y_arr=y_t, /ad2xy
apply_astrometry, obs, dec_arr=lat, x_arr=x_t, y_arr=y_t, /xy2ad, /ignore_refraction

phi=obs.obsdec*!DtoR
lat=lat*!DtoR
dec=dec_use*!DtoR
ha=hour_angle*!DtoR
;calculate the elements of the dipole projection matrix J ((J11, J12), (J21,J22))
;From Ord S.M. et al "Interferometric Imaging with the 32 Element Murchison Wide-Field Array" PASP 122 (2010)
;J11=Cos(phi)*Cos(dec)+Sin(phi)*Sin(dec)*Cos(ha)
;J12=-Sin(phi)*Sin(ha)
;J21=Sin(dec)*Sin(ha)
;J22=Cos(ha)

;NOTE!! RTS convention uses X for N-S dipoles, and Y for E-W, which is opposite everything else. So, we have to re-order the Jones matrix elements
;J21=Cos(phi)*Cos(dec)+Sin(phi)*Sin(dec)*Cos(ha)
;J22=-Sin(phi)*Sin(ha)
;J11=Sin(dec)*Sin(ha)
;J12=Cos(ha)
J21=Cos(lat)*Cos(dec)+Sin(lat)*Sin(dec)*Cos(ha)
J22=-Sin(lat)*Sin(ha)
J11=Sin(phi)*Sin(ha)
J12=Cos(ha)

p_map=Ptrarr(4,4,/allocate)
p_corr=Ptrarr(4,4,/allocate)
FOR i=0,3 DO FOR j=0,3 DO *p_map[i,j]=fltarr(n_pix)
FOR i=0,3 DO FOR j=0,3 DO *p_corr[i,j]=fltarr(n_pix)

FOR pix=0L,n_pix-1 DO BEGIN
    ;calculate tensor product J(X)J* 
    ;Jmat converts [pp,qq,pq,qp] -> [xx,yy,xy,yx]
    ;Jinv converts [xx, yy, xy, yx] -> [pp, qq, pq, qp]
    ;Note: Stokes [I, Q, U, V] = (1./2.)*[(pp+qq), (qq-pp), ?(pq+qp)?, ?(qp-pq)?]

    Jmat=[[J11[pix]^2.,J12[pix]^2.,J11[pix]*J12[pix],J12[pix]*J11[pix]],$
             [J21[pix]^2.,J22[pix]^2.,J21[pix]*J22[pix],J22[pix]*J21[pix]],$
             [J11[pix]*J21[pix],J12[pix]*J22[pix],J11[pix]*J22[pix],J12[pix]*J21[pix]],$
             [J21[pix]*J11[pix],J22[pix]*J12[pix],J21[pix]*J12[pix],J22[pix]*J11[pix]]]
    Jinv=Invert(Jmat)
    
    FOR i=0,3 DO FOR j=0,3 DO (*p_map[i,j])[pix]=Jmat[i,j]
    FOR i=0,3 DO FOR j=0,3 DO (*p_corr[i,j])[pix]=Jinv[i,j]
ENDFOR

jones={inds:inds_use,dimension:dimension,elements:elements,Jmat:p_map,Jinv:p_corr}
;jones={inds:inds_use,dimension:dimension,elements:elements,Jmat:p_corr,Jinv:p_map}
fhd_save_io,status_str,jones,var='jones',/compress,file_path_fhd=file_path_fhd,_Extra=extra
RETURN,jones
END