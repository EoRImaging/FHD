PRO healpix_cnv,image,hpx_inds,hpx_vals,hpx_weights,mask=mask,astr=astr,weights=weights,nside=nside,$
    file_path=file_path,image_list=image_list,keep_inds=keep_inds
;NOTE: does not yet support double precision!    

dimension=(size(image,/dimension))[0]
elements=(size(image,/dimension))[1]
xvals=meshgrid(dimension,elements,1)
yvals=meshgrid(dimension,elements,2)
IF not Keyword_Set(mask) THEN mask=intarr(dimension,elements)+1
i_use=where(mask,n_use)

img_vec=image[i_use] 
x_vec=xvals[i_use]
y_vec=yvals[i_use]

xy2ad,x_vec,y_vec,astr,ra_vec,dec_vec

;all angles in DEGREES
;uses RING index scheme
IF not Keyword_Set(nside) THEN BEGIN
    pix_sky=4.*!Pi*!RaDeg^2./Product(astr.cdelt)
    ;Npix = 12* Nside^2
    Nside=2.^(Ceil(ALOG(Sqrt(pix_sky/12.))/ALOG(2))) ;=1024. for 0.1119 degrees/pixel
;    nside/=2.
;    npix=nside2npix(nside)
    ;ang2pix_ring,
ENDIF
npix=nside2npix(nside)
branch_test=Max(ra_vec)-Min(ra_vec)
branch_i=where(branch_test GT 180.,n_branch)
ra_use=ra_vec
IF n_branch GT 0 THEN ra_use[branch_i]+=360.

dec_cen=Median(dec_vec)
ra_cen_use=Median(ra_use) 
ra_cen=ra_cen_use mod 360.

dist_vec=angle_difference(dec_cen,ra_cen_use,dec_vec,ra_use,/degree,/nearest)
radius=Max(dist_vec,/nan)

ang2vec,dec_cen,ra_cen,cen_coords,/astro
;ang2vec,dec_vec,ra_vec,vec_coords,/astro
;vec2pix_ring, nside, vec_coords, ipring_vec

Query_disc,nside,cen_coords,radius,hpx_inds0,ninds,/deg

pix2vec_ring,nside,hpx_inds0,pix_coords,vertex_coords
vec2ang,pix_coords,pix_dec,pix_ra,/astro

vertex_ra=fltarr(5,ninds)
vertex_dec=fltarr(5,ninds)
FOR vi=0,3 DO BEGIN
    vec2ang,vertex_coords[*,*,vi],single_dec,single_ra,/astro
    vertex_ra[vi,*]=single_ra
    vertex_dec[vi,*]=single_dec
ENDFOR
vertex_coords=0
single_dec=(single_ra=0)
vertex_ra[4,*]=pix_ra
vertex_dec[4,*]=pix_dec

ad2xy,vertex_ra,vertex_dec,astr,vertex_x0,vertex_y0
vertex_x0=Round(vertex_x0)
vertex_y0=Round(vertex_y0)

vertex_i1_test=Min((vertex_x0 GE 0) AND (vertex_x0 LT dimension) AND (vertex_y0 GE 0) AND (vertex_y0 LT elements),dimension=1)
vertex_i1=where(vertex_i1_test,n1)

vertex_x1=vertex_x0[*,vertex_i1]
vertex_y1=vertex_y0[*,vertex_i1]
hpx_inds1=hpx_inds0[vertex_i1]

vertex_i2_test=Max(mask[vertex_x1,vertex_y1],dimension=1)
vertex_i2=where(vertex_i2_test,n2)

vertex_x2=vertex_x1[*,vertex_i2]
vertex_y2=vertex_y1[*,vertex_i2]

hpx_vals2=Total((image*mask)[vertex_x2,vertex_y2],1)/Total(mask[vertex_x2,vertex_y2],1)
hpx_inds2=hpx_inds1[vertex_i2]

IF Keyword_Set(weights) THEN hpx_weights2=Total((weights*mask)[vertex_x2,vertex_y2],1)/Total(mask[vertex_x2,vertex_y2],1)

IF not Keyword_Set(hpx_inds) THEN keep_inds=0
IF Keyword_Set(keep_inds) THEN BEGIN
    n_hpx=N_Elements(hpx_inds)
    h_test=histogram(hpx_inds,omin=hmin,omax=hmax,/binsize,/L64,reverse=ri)
    h_test2=histogram(hpx_inds2,min=hmin,max=hmax,/binsize,/L64,reverse=ri2)
    i_h_use=where(h_test AND h_test2,n_h_use)
    i_h_src=ri2[ri2[i_h_use]]
    i_h_in=ri[ri[i_h_use]]
    hpx_vals=Fltarr(n_hpx) & hpx_vals[i_h_in]=Temporary(hpx_vals2[i_h_src])
    IF Keyword_Set(weights) THEN BEGIN
        hpx_weights=Fltarr(n_hpx)
        hpx_weights[i_h_in]=Temporary(hpx_weights2[i_h_src])
    ENDIF
ENDIF ELSE BEGIN
    hpx_vals=Temporary(hpx_vals2)
    IF Keyword_Set(weights) THEN hpx_weights=Temporary(hpx_weights2)
    hpx_inds=Temporary(hpx_inds2)
ENDELSE

IF Keyword_Set(image_list) THEN BEGIN
    IF (size(image_list,/type)) EQ 10 THEN BEGIN
        FOR i=0,N_Elements(image_list)-1 DO BEGIN
            image_tmp=*image_list[i]
            hpx_tmp=Total((image_tmp*mask)[vertex_x2,vertex_y2],1)/Total(mask[vertex_x2,vertex_y2],1)
            IF Keyword_Set(keep_inds) THEN BEGIN
                IF size(*image_list[i],/type) EQ 6 THEN *image_list[i]=Complexarr(n_hpx) ELSE *image_list[i]=Fltarr(n_hpx)
                
                (*image_list[i])[i_h_in]=Temporary(hpx_tmp[i_h_src])
            ENDIF ELSE BEGIN
                *image_list[i]=Temporary(hpx_tmp)
            ENDELSE
        ENDFOR
        heap_gc
    ENDIF
ENDIF

IF Keyword_Set(file_path) THEN BEGIN
    write_fits_cut4,file_path+'_hpx.fits',hpx_inds,hpx_vals,/ring,Coords='C',nside=nside
    IF Keyword_Set(weights) THEN write_fits_cut4,file_path+'_hpx_weights.fits',hpx_inds,hpx_weights,/ring,Coords='C',nside=nside
ENDIF

END