FUNCTION pixel_area,astr,dimension=dimension,elements=elements

IF N_Elements(dimension) EQ 0 THEN dimension=(astr.crpix)*2.
IF N_Elements(elements) EQ 0 THEN elements=dimension
xvals=meshgrid(dimension,elements,1)
yvals=meshgrid(dimension,elements,2)
xy2ad,xvals,yvals,astr,ra_vals,dec_vals

area0=Abs(Product(astr.cdelt))

ang2vec,dec_vals,ra_vals,cen_coords,/astro
i_use=where(Finite(ra_vals),n_use)
IF n_use EQ 0 THEN RETURN,-1
ra_vals=ra_vals[i_use]
dec_vals=dec_vals[i_use]
ra0=astr.crval[0]
dec0=astr.crval[1]
ang_dist=angle_difference(dec0,ra0,dec_vals,ra_vals,/degree)
area_vals=1./Cos(ang_dist*!DtoR)
area_map=Fltarr(dimension,elements)
area_map[i_use]=area_vals*area0


;Turn off for now!
area_map[*]=1.

RETURN,area_map
END