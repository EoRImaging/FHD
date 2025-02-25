FUNCTION source_image_generate,source_array,obs,pol_i=pol_i,resolution=resolution,threshold=threshold,conserve_flux=conserve_flux,frequency=frequency,$
    dimension=dimension,elements=elements,restored_beam_width=restored_beam_width,ring_radius=ring_radius,n_sources=n_sources,_Extra=extra
IF Keyword_Set(obs) THEN BEGIN
    dimension=obs.dimension
    elements=obs.elements
    IF N_Elements(restored_beam_width) EQ 0 THEN restored_beam_width=beam_width_calculate(obs,/fwhm)
ENDIF ELSE IF N_Elements(elements) EQ 0 THEN elements=dimension
IF N_Elements(restored_beam_width) EQ 0 THEN restored_beam_width=1. ;width of gaussian to use to restore sources
IF N_Elements(ring_radius) EQ 0 THEN ring_radius=0 ;set to restore sources with a ring with a gaussian radial profile
IF N_Elements(pol_i) EQ 0 THEN pol_i=4 ;pol_i corresponds to 0-3: xx, yy, xy, yx in apparent brightness; 4-7: I, Q, U, V in sky brightness
IF Keyword_Set(n_sources) THEN ns=n_sources>(size(source_array,/dimension))[0] ELSE ns=(size(source_array,/dimension))[0]
IF N_Elements(resolution) EQ 0 THEN resolution=8. ELSE resolution=Float(resolution)
IF N_Elements(threshold) EQ 0 THEN threshold=1E-4
IF Keyword_Set(frequency) THEN BEGIN
    freq_ref=Median(source_array.freq)
    freq_ratio=Abs(Alog10(freq_ref/frequency)) ;it often happens that one is in Hz and the other in MHz. Assuming no one will ever want to extrapolate more than two orders of magnitude, correct any huge mismatch
    IF freq_ratio GT 2 THEN freq_scale=10.^(Round(Alog10(freq_ref/frequency)/3.)*3.) ELSE freq_scale=1.
    frequency_use=frequency*freq_scale
ENDIF

icomp=Complex(0,1)
x_vals=meshgrid(dimension,elements,1)-dimension/2.
y_vals=meshgrid(dimension,elements,2)-elements/2.

IF Keyword_Set(ring_radius) THEN $
    beam_use=Exp(-Abs(((x_vals)/restored_beam_width)^2.+((y_vals)/restored_beam_width)^2.-2.*(ring_radius+1)/restored_beam_width)/2.) $
ELSE beam_use=Exp(-Abs(((x_vals)/restored_beam_width)^2.+((y_vals)/restored_beam_width)^2.)/2.)
beam_use/=Max(beam_use) ;make sure it is normalized to 1
cut=reform(beam_use[*,elements/2.])
cut_endpoints=minmax(where(cut GE threshold))
box_dim=Ceil((cut_endpoints[1]-cut_endpoints[0])/2.)*2.+2
box_dimR=box_dim*resolution

x_valsR=meshgrid(box_dimR,box_dimR,1)-box_dimR/2.
y_valsR=meshgrid(box_dimR,box_dimR,2)-box_dimR/2.
xvals_i=meshgrid(box_dim,box_dim,1)*resolution
yvals_i=meshgrid(box_dim,box_dim,2)*resolution

IF Keyword_Set(ring_radius) THEN $
    beam_useR=Exp(-Abs(((x_valsR)/(restored_beam_width*resolution))^2.+((y_valsR)/(restored_beam_width*resolution))^2.-2.*(ring_radius+1)/restored_beam_width)/2.) $
ELSE beam_useR=Exp(-Abs(((x_valsR)/(restored_beam_width*resolution))^2.+((y_valsR)/(restored_beam_width*resolution))^2.)/2.)
;beam_useR/=Total(beam_useR)/resolution^2. ;make sure it is normalized
;beam_useR/=Max(beam_useR) ;make sure it is normalized to 1
IF Keyword_Set(ring_radius) THEN beam_useR*=ring_radius
beam_useR_arr=Ptrarr(resolution,resolution,/allocate)
IF Keyword_Set(conserve_flux) THEN BEGIN
    FOR i=0,resolution-1 DO FOR j=0,resolution-1 DO $
        *beam_useR_arr[i,j]=beam_useR[xvals_i+i,yvals_i+j]/Total(beam_useR[xvals_i+i,yvals_i+j])
ENDIF ELSE BEGIN
    FOR i=0,resolution-1 DO FOR j=0,resolution-1 DO $
        *beam_useR_arr[i,j]=beam_useR[xvals_i+i,yvals_i+j]/Max(beam_useR[xvals_i+i,yvals_i+j])
ENDELSE
sx=source_array[0:ns-1].x
sy=source_array[0:ns-1].y
flux=source_array[0:ns-1].flux.(pol_i)
IF Keyword_Set(frequency_use) THEN flux*=(frequency_use/source_array.freq)^source_array.alpha
x_offset=Round((Ceil(sx)-sx)*resolution) mod resolution    
y_offset=Round((Ceil(sy)-sy)*resolution) mod resolution
xcen0=Round(sx+x_offset/resolution) ;do this after offset, in case it has rounded to the next grid point
ycen0=Round(sy+y_offset/resolution)
xmin=Floor(xcen0-box_dim/2.) & xmax=xmin+box_dim-1
ymin=Floor(ycen0-box_dim/2.) & ymax=ymin+box_dim-1

si1=where((xmin GE 0) AND (ymin GE 0) AND (xmax LE dimension-1) AND (ymax LE elements-1),ns)
source_image=fltarr(dimension,elements)
FOR si=0L,ns-1L DO BEGIN
    IF Ptr_valid(source_array[si].extend) THEN BEGIN
        comp_arr=*(source_array[si].extend)
        cx=comp_arr.x
        cy=comp_arr.y
        x_offset1=Round((Ceil(cx)-cx)*resolution) mod resolution    
        y_offset1=Round((Ceil(cy)-cy)*resolution) mod resolution
        xcen1=Round(cx+x_offset1/resolution) ;do this after offset, in case it has rounded to the next grid point
        ycen1=Round(cy+y_offset1/resolution)
        xmin1=Floor(xcen1-box_dim/2.) & xmax1=xmin1+box_dim-1
        ymin1=Floor(ycen1-box_dim/2.) & ymax1=ymin1+box_dim-1
        ci1=where((xmin1 GE 0) AND (ymin1 GE 0) AND (xmax1 LE dimension-1) AND (ymax1 LE elements-1),nc)
        flux_comp=comp_arr.flux.(pol_i)
        IF Keyword_Set(frequency_use) THEN flux_comp*=(frequency_use/comp_arr.freq)^comp_arr.alpha
        FOR ci=0L,nc-1 DO BEGIN
            source_image[xmin1[ci1[ci]]:xmax1[ci1[ci]],ymin1[ci1[ci]]:ymax1[ci1[ci]]]+=$
                flux_comp[ci1[ci]]*(*beam_useR_arr[x_offset1[ci1[ci]],y_offset1[ci1[ci]]])
        ENDFOR
    ENDIF ELSE BEGIN
        source_image[xmin[si1[si]]:xmax[si1[si]],ymin[si1[si]]:ymax[si1[si]]]+=$
            flux[si1[si]]*(*beam_useR_arr[x_offset[si1[si]],y_offset[si1[si]]])
    ENDELSE
ENDFOR
;source_image=fltarr(dimension,elements)
;IF Keyword_Set(ring_radius) THEN FOR si=0,ns-1 DO $
;    source_image+=sf[si]*Exp(-Abs(((x_vals-sx[si])/restored_beam_width)^2.+((y_vals-sy[si])/restored_beam_width)^2.-2.*ring_radius)/2.) $
;ELSE FOR si=0.,ns-1 DO $
;    source_image+=sf[si]*Exp(-(((x_vals-sx[si])/restored_beam_width)^2.+((y_vals-sy[si])/restored_beam_width)^2.)/2.) 
Ptr_free,beam_useR_arr
RETURN,source_image
END