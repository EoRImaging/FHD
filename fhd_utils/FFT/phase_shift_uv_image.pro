FUNCTION phase_shift_uv_image,obs,to_zenith=to_zenith,to_observed=to_observed,$
    to_orig_phase=to_orig_phase,uv_mask=uv_mask

dimension=obs.dimension
elements=obs.elements
astr=obs.astr
phase_x=dimension/2.
phase_y=elements/2.
phase_ra=obs.phasera
phase_dec=obs.phasedec

IF Keyword_Set(to_zenith) THEN BEGIN
    ra_use=obs.zenra
    dec_use=obs.zendec
ENDIF
IF Keyword_Set(to_observed) THEN BEGIN
    ra_use=obs.zenra
    dec_use=obs.zendec
ENDIF
IF Keyword_Set(to_orig_phase) THEN BEGIN
    ra_use=obs.orig_phasera
    dec_use=obs.orig_phasedec
ENDIF

;skip calculations if already phased correctly
IF (ra_use EQ phase_ra) AND (dec_use EQ phase_dec) THEN RETURN,Complexarr(dimension,elements)+1.

apply_astrometry,obs,ra_arr=ra_use, dec_arr=dec_use, x_arr=x_use, y_arr=y_use, /ad2xy

rephase_calc=Complexarr(dimension,elements)

IF N_Elements(uv_mask) EQ N_Elements(rephase_calc) THEN BEGIN
    uv_mask_use=uv_mask 
    mask_flag=1
ENDIF ELSE BEGIN
    uv_mask_use=Fltarr(dimension,elements)+1.
    mask_flag=0
ENDELSE

dx=x_use-phase_x
dy=y_use-phase_y

xvals=meshgrid(dimension,elements,1)-dimension/2
yvals=meshgrid(dimension,elements,2)-elements/2
IF mask_flag THEN BEGIN
    i_use=where(uv_mask_use,n_use)
    IF n_use GT 0 THEN BEGIN
        xvals=xvals[i_use]
        yvals=yvals[i_use]
    ENDIF ELSE mask_flag=0
ENDIF
dx*=(2.*!Pi/dimension)
dy*=(2.*!Pi/dimension)
phase=xvals*dx+yvals*dy
rephase_vals=Complex(Cos(phase),Sin(phase))

IF mask_flag THEN rephase_calc[i_use]=rephase_vals ELSE rephase_calc=rephase_vals

RETURN,rephase_calc
END