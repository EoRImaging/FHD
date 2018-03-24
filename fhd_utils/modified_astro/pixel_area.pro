FUNCTION pixel_area,obs,relative=relative

dimension=obs.dimension
elements=obs.elements
xvals=meshgrid(dimension,elements,1)
yvals=meshgrid(dimension,elements,2)
;refraction has negligible effect on the size of pixels, so ignore to save time
apply_astrometry, obs, x_arr=xvals, y_arr=yvals, ra_arr=ra_arr, dec_arr=dec_arr, /xy2ad, /ignore_refraction
ra_i_nan=where(Finite(ra_arr,/nan),n_nan,complement=ra_i_use)
ra_vals=ra_arr[ra_i_use]
dec_vals=dec_arr[ra_i_use]
IF n_nan GT 0 THEN BEGIN
    ra_arr[ra_i_nan]=0
    dec_arr[ra_i_nan]=0
ENDIF
ang2vec,dec_vals,ra_vals,cen_coords,/astro
;x_vec=cen_coords[*,0]
;y_vec=cen_coords[*,1]
;z_vec=cen_coords[*,2]
;x_arr=Fltarr(dimension,elements) & x_arr[ra_i_use]=x_vec
;y_arr=Fltarr(dimension,elements) & y_arr[ra_i_use]=y_vec
;z_arr=Fltarr(dimension,elements) & z_arr[ra_i_use]=z_vec

coord_cube=Fltarr(dimension,elements,3)
coord_slice=Fltarr(dimension,elements)
FOR i=0,2 DO BEGIN
    coord_slice[ra_i_use]=cen_coords[*,i]
    coord_cube[*,*,i]=coord_slice
ENDFOR

mask_test=intarr(dimension,elements)
mask_test[ra_i_use]=1
mask_test=morph_distance(mask_test)
i_use=where(mask_test GT 1,n_use)
mask=Fltarr(dimension,elements) & mask[i_use]=1

;calculate the pixel area from the cross product of two vectors. 
;This works because the coordinates of each pixel are calculated on the surface of a sphere, so the vectors along the pixel edges are already in the correct plane 
pix_vec_A=coord_cube-Shift(coord_cube,1,0,0)
pix_vec_B=coord_cube-Shift(coord_cube,0,1,0)

area_map=Fltarr(dimension,elements)
FOR i=0,2 DO BEGIN
    A_i=(i+1) mod 3
    B_i=(i+2) mod 3
    area_map+=(pix_vec_A[*,*,A_i]*pix_vec_B[*,*,B_i]-pix_vec_A[*,*,B_i]*pix_vec_B[*,*,A_i])^2.
ENDFOR
area_map=Sqrt(area_map)
if Keyword_Set(nside) THEN BEGIN
    print, "Using nside="+String(nside)
    area_map *= 0.0
    hpx_area = 4*!Pi / (12.0 * nside*nside)    ; nside^2 evaluates to 0 if nside is integer
    area_map += hpx_area
IF Keyword_Set(relative) THEN area_map/=(obs.degpix*!DtoR)^2.

;astr=obs.astr
;area0=Abs(Product(astr.cdelt))
;
;i_use=where(Finite(ra_vals),n_use)
;IF n_use EQ 0 THEN RETURN,-1
;ra_vals=ra_vals[i_use]
;dec_vals=dec_vals[i_use]
;ra0=astr.crval[0]
;dec0=astr.crval[1]
;ang_dist=angle_difference(dec0,ra0,dec_vals,ra_vals,/degree)
;area_vals=1./Cos(ang_dist*!DtoR)
;area_map=Fltarr(dimension,elements)
;area_map[i_use]=area_vals*area0
;
;
;;Turn off for now!
;area_map[*]=1.

RETURN,area_map
END
