FUNCTION healpix_cnv_generate,obs,status_str,file_path_fhd=file_path_fhd,nside=nside,mask=mask,hpx_radius=hpx_radius,$
    restore_last=restore_last,silent=silent,pointer_return=pointer_return,no_save=no_save,$
    restrict_hpx_inds=restrict_hpx_inds,divide_pixel_area=divide_pixel_area,_Extra=extra

IF N_Elements(file_path_fhd) EQ 0 THEN file_path_fhd=''
IF Keyword_Set(restore_last) AND (file_test(file_path_fhd+'_hpxcnv'+'.sav') EQ 0) THEN BEGIN 
    IF ~Keyword_Set(silent) THEN print,file_path_fhd+'_hpxcnv'+'.sav' +' Not found. Recalculating.' 
    restore_last=0
ENDIF
IF Keyword_Set(restore_last) THEN BEGIN
    IF ~Keyword_Set(silent) THEN print,'Saved Healpix grid map restored'
    fhd_save_io,status_str,hpx_cnv,var='hpx_cnv',/restore,file_path_fhd=file_path_fhd,_Extra=extra
    nside=hpx_cnv.nside
    RETURN,hpx_cnv
ENDIF ELSE IF N_Elements(obs) EQ 0 THEN fhd_save_io,0,obs,var='obs',/restore,file_path_fhd=file_path_fhd,_Extra=extra

t00=Systime(1)
dimension=obs.dimension
elements=obs.elements

IF N_Elements(hpx_radius) EQ 0 THEN BEGIN
    IF Keyword_Set(mask) THEN BEGIN
        xv_arr=meshgrid(dimension,elements,1)
        yv_arr=meshgrid(dimension,elements,2)
        ;set /ignore_refraction for speed, since we don't need to be exact
        apply_astrometry, obs, x_arr=xv_arr, y_arr=yv_arr, ra_arr=ra_arr, dec_arr=dec_arr, /xy2ad, /ignore_refraction 
        ang_arr=angle_difference(dec_arr,ra_arr,obs.obsdec,obs.obsra,/degree)
        ang_i=where((mask GT 0) AND Finite(ang_arr),n_ang_use)
        IF n_ang_use GT 0 THEN radius=Max(ang_arr[ang_i])
    ENDIF ELSE radius=obs.degpix*(dimension>elements)/4. 
ENDIF ELSE radius=hpx_radius

;all angles in DEGREES
;uses RING index scheme

;check if a string, if it is assume it is a filepath to a save file with the desired indices 
; (will NOT be over-written with the indices)
IF Keyword_Set(restrict_hpx_inds) AND (size(restrict_hpx_inds,/type) NE 7) THEN restrict_hpx_inds=observation_healpix_inds_select(obs)
IF size(restrict_hpx_inds,/type) EQ 7 THEN BEGIN 
    file_path_use=restrict_hpx_inds
    IF file_test(file_path_use) EQ 0 THEN file_path_use=filepath(file_path_use,root=Rootdir('fhd'),subdir='Observations')
    
    IF  file_test(file_path_use) THEN BEGIN
        hpx_inds=getvar_savefile(file_path_use,'hpx_inds')
        nside_test=getvar_savefile(file_path_use,names=sav_contents)
        IF Max(strmatch(StrLowCase(sav_contents),'nside')) EQ 1 THEN nside=getvar_savefile(file_path_use,'nside') ELSE BEGIN
            max_ind=Max(hpx_inds)
            IF Keyword_Set(nside) THEN nside=(2.^(Ceil(ALOG(Sqrt(max_ind/12.))/ALOG(2))))>nside ELSE nside=2.^(Ceil(ALOG(Sqrt(max_ind/12.))/ALOG(2))) 
        ENDELSE
    ENDIF ELSE restrict_hpx_inds=file_path_use+"-- FILE NOT FOUND"
ENDIF
IF ~Keyword_Set(nside) THEN BEGIN
    pix_sky=4.*!Pi*!RaDeg^2./Product(Abs(obs.astr.cdelt))
    Nside=2.^(Ceil(ALOG(Sqrt(pix_sky/12.))/ALOG(2))) ;=1024. for 0.1119 degrees/pixel
ENDIF
npix=nside2npix(nside)

IF Keyword_Set(divide_pixel_area) THEN BEGIN
    pixel_area_cnv=(4.*!Pi / npix) * weight_invert(pixel_area(obs))
ENDIF ELSE pixel_area_cnv=1. ;turn this off for now

IF N_Elements(hpx_inds) GT 1 THEN BEGIN
    pix2vec_ring,nside,hpx_inds,pix_coords
    vec2ang,pix_coords,pix_dec,pix_ra,/astro
    apply_astrometry, obs, ra_arr=pix_ra, dec_arr=pix_dec, x_arr=xv_hpx, y_arr=yv_hpx, /ad2xy
ENDIF ELSE BEGIN
    ang2vec,obs.obsdec,obs.obsra,cen_coords,/astro
    Query_disc,nside,cen_coords,radius,hpx_inds0,ninds,/deg
    pix2vec_ring,nside,hpx_inds0,pix_coords
    vec2ang,pix_coords,pix_dec,pix_ra,/astro
    apply_astrometry, obs, ra_arr=pix_ra, dec_arr=pix_dec, x_arr=xv_hpx, y_arr=yv_hpx, /ad2xy
    pix_coords=0
    
    ;NOTE: slightly more restrictive boundary here ('LT' and 'GT' instead of 'LE' and 'GE') 
    pix_i_use=where((xv_hpx GT 0) AND (xv_hpx LT dimension-1) AND (yv_hpx GT 0) AND (yv_hpx LT elements-1),n_hpx_use)
    xv_hpx=xv_hpx[pix_i_use]
    yv_hpx=yv_hpx[pix_i_use]
    IF Keyword_Set(mask) THEN BEGIN
        hpx_mask00=mask[Floor(xv_hpx),Floor(yv_hpx)]
        hpx_mask01=mask[Floor(xv_hpx),Ceil(yv_hpx)]
        hpx_mask10=mask[Ceil(xv_hpx),Floor(yv_hpx)]
        hpx_mask11=mask[Ceil(xv_hpx),Ceil(yv_hpx)]
        hpx_mask=Temporary(hpx_mask00)*Temporary(hpx_mask01)*Temporary(hpx_mask10)*Temporary(hpx_mask11)
        pix_i_use2=where(Temporary(hpx_mask),n_hpx_use)
        xv_hpx=xv_hpx[pix_i_use2]
        yv_hpx=yv_hpx[pix_i_use2]
        pix_i_use=pix_i_use[pix_i_use2]
    ENDIF 
    hpx_inds=hpx_inds0[pix_i_use]
ENDELSE

; Test for pixels past the horizon. We don't need to be precise with this, so turn off precession, etc..
Eq2Hor,pix_ra, pix_dec, obs.JD0, alt_arr, az_arr, nutate=0,precess=0,aberration=0, refract=0, lon=obs.lon, alt=obs.alt, lat=obs.lat
horizon_i = where(alt_arr LE 0, n_horizon, complement=h_use)
IF n_horizon GT 0 THEN BEGIN
    print,String(format='("Cutting ",A, " HEALPix pixels that were below the horizon.")',Strn(n_horizon))
    xv_hpx = xv_hpx[h_use]
    yv_hpx = yv_hpx[h_use]
    hpx_inds = hpx_inds[h_use]
ENDIF

x_frac=1.-(xv_hpx-Floor(xv_hpx))
y_frac=1.-(yv_hpx-Floor(yv_hpx))

min_bin=Min(Floor(xv_hpx)+dimension*Floor(yv_hpx))>0L
max_bin=Max(Ceil(xv_hpx)+dimension*Ceil(yv_hpx))<(dimension*elements-1L)
h00=histogram(Floor(xv_hpx)+dimension*Floor(yv_hpx),min=min_bin,max=max_bin,/binsize,reverse_ind=ri00)
h01=histogram(Floor(xv_hpx)+dimension*Ceil(yv_hpx),min=min_bin,max=max_bin,/binsize,reverse_ind=ri01)
h10=histogram(Ceil(xv_hpx)+dimension*Floor(yv_hpx),min=min_bin,max=max_bin,/binsize,reverse_ind=ri10)
h11=histogram(Ceil(xv_hpx)+dimension*Ceil(yv_hpx),min=min_bin,max=max_bin,/binsize,reverse_ind=ri11)
htot=h00+h01+h10+h11
inds=where(htot,n_img_use)

n_arr=htot[inds]

i_use=inds+min_bin
sa=Ptrarr(n_img_use,/allocate)
ija=Ptrarr(n_img_use,/allocate)

FOR i=0L,n_img_use-1L DO BEGIN
    ind0=inds[i]
    sa0=fltarr(n_arr[i])
    ija0=Lonarr(n_arr[i])
    bin_i=Total([0L,h00[ind0],h01[ind0],h10[ind0],h11[ind0]],/cumulative)
    IF h00[ind0] GT 0 THEN BEGIN
        bi=0
        inds1=ri00[ri00[ind0]:ri00[ind0+1]-1]
        sa0[bin_i[bi]:bin_i[bi+1]-1]=x_frac[inds1]*y_frac[inds1]
        ija0[bin_i[bi]:bin_i[bi+1]-1]=inds1
    ENDIF
    IF h01[ind0] GT 0 THEN BEGIN
        bi=1
        inds1=ri01[ri01[ind0]:ri01[ind0+1]-1]
        sa0[bin_i[bi]:bin_i[bi+1]-1]=x_frac[inds1]*(1.-y_frac[inds1])
        ija0[bin_i[bi]:bin_i[bi+1]-1]=inds1
    ENDIF
    IF h10[ind0] GT 0 THEN BEGIN
        bi=2
        inds1=ri10[ri10[ind0]:ri10[ind0+1]-1]
        sa0[bin_i[bi]:bin_i[bi+1]-1]=(1.-x_frac[inds1])*y_frac[inds1]
        ija0[bin_i[bi]:bin_i[bi+1]-1]=inds1
    ENDIF
    IF h11[ind0] GT 0 THEN BEGIN
        bi=3
        inds1=ri11[ri11[ind0]:ri11[ind0+1]-1]
        sa0[bin_i[bi]:bin_i[bi+1]-1]=(1.-x_frac[inds1])*(1.-y_frac[inds1])
        ija0[bin_i[bi]:bin_i[bi+1]-1]=inds1
    ENDIF
    IF N_Elements(pixel_area_cnv) GT 1 THEN *sa[i]=sa0*pixel_area_cnv[i_use[i]] ELSE *sa[i]=sa0*pixel_area_cnv
;    *sa[i]=sa0
    *ija[i]=ija0
        
ENDFOR

hpx_cnv={nside:nside,ija:ija,sa:sa,i_use:i_use,inds:hpx_inds}
IF tag_exist(obs,'healpix') THEN BEGIN
    IF N_Elements(restrict_hpx_inds) NE 1 THEN ind_list="UNSPECIFIED" ELSE ind_list=restrict_hpx_inds
    n_hpx=N_Elements(hpx_inds)
    IF Keyword_Set(mask) THEN BEGIN
        mask_test=healpix_cnv_apply(mask,hpx_cnv)
        mask_test_i0=where(mask_test EQ 0,n_zero_hpx)
    ENDIF ELSE n_zero_hpx=-1
    obs.healpix.nside=Long(nside)
    obs.healpix.ind_list=String(ind_list)
    obs.healpix.n_pix=Long(n_hpx)
    obs.healpix.n_zero=Long(n_zero_hpx)
ENDIF

fhd_save_io,status_str,hpx_cnv,var='hpx_cnv',/compress,no_save=no_save,file_path_fhd=file_path_fhd,_Extra=extra
IF Keyword_Set(pointer_return) THEN RETURN,Ptr_new(hpx_cnv) ELSE RETURN,hpx_cnv
END
