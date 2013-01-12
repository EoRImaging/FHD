FUNCTION source_image_generate,source_array,obs,pol_i=pol_i,resolution=resolution,threshold=threshold,$
    dimension=dimension,elements=elements,width=width,ring_radius=ring_radius,n_sources=n_sources
IF Keyword_Set(obs) THEN BEGIN
    dimension=obs.dimension
    elements=obs.elements
ENDIF ELSE IF N_Elements(elements) EQ 0 THEN elements=dimension
IF N_Elements(width) EQ 0 THEN width=1. ;width of gaussian to use to restore sources
IF N_Elements(ring_radius) EQ 0 THEN ring_radius=0 ;set to restore sources with a ring with a gaussian radial profile
IF N_Elements(pol_i) EQ 0 THEN pol_i=4 ;pol_i corresponds to 0-3: xx, yy, xy, yx in apparent brightness; 4-7: I, Q, U, V in sky brightness
IF Keyword_Set(n_sources) THEN ns=n_sources>(size(source_array,/dimension))[0] ELSE ns=(size(source_array,/dimension))[0]
IF N_Elements(resolution) EQ 0 THEN resolution=8.
IF N_Elements(threshold) EQ 0 THEN threshold=1E-4

icomp=Complex(0,1)
x_vals=meshgrid(dimension,elements,1)-dimension/2.
y_vals=meshgrid(dimension,elements,2)-elements/2.

IF Keyword_Set(ring_radius) THEN $
    beam_use=Exp(-Abs(((x_vals)/width)^2.+((y_vals)/width)^2.-2.*(ring_radius+1)/width)/2.) $
ELSE beam_use=Exp(-Abs(((x_vals)/width)^2.+((y_vals)/width)^2.)/2.)
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
    beam_useR=Exp(-Abs(((x_valsR)/(width*resolution))^2.+((y_valsR)/(width*resolution))^2.-2.*(ring_radius+1)/width)/2.) $
ELSE beam_useR=Exp(-Abs(((x_valsR)/(width*resolution))^2.+((y_valsR)/(width*resolution))^2.)/2.)
;beam_useR/=Total(beam_useR)/resolution^2. ;make sure it is normalized to 1
beam_useR/=Max(beam_useR) ;make sure it is normalized to 1
IF Keyword_Set(ring_radius) THEN beam_useR*=ring_radius
beam_useR_arr=Ptrarr(resolution,resolution,/allocate)
FOR i=0,resolution-1 DO FOR j=0,resolution-1 DO $
    *beam_useR_arr[i,j]=beam_useR[xvals_i+i,yvals_i+j]

sx=source_array[0:ns-1].x
sy=source_array[0:ns-1].y
flux=source_array[0:ns-1].flux.(pol_i)
x_offset=Round((Ceil(sx)-sx)*resolution) mod resolution    
y_offset=Round((Ceil(sy)-sy)*resolution) mod resolution
xcen0=Round(sx+x_offset/resolution) ;do this after offset, in case it has rounded to the next grid point
ycen0=Round(sy+y_offset/resolution)
xmin=Floor(xcen0-box_dim/2.) & xmax=xmin+box_dim-1
ymin=Floor(ycen0-box_dim/2.) & ymax=ymin+box_dim-1

si1=where((xmin GE 0) AND (ymin GE 0) AND (xmax LE dimension-1) AND (ymax LE elements-1),ns)
source_image=fltarr(dimension,elements)
FOR si=0L,ns-1L DO source_image[xmin[si1[si]]:xmax[si1[si]],ymin[si1[si]]:ymax[si1[si]]]+=$
    flux[si1[si]]*(*beam_useR_arr[x_offset[si1[si]],y_offset[si1[si]]])

;source_image=fltarr(dimension,elements)
;IF Keyword_Set(ring_radius) THEN FOR si=0,ns-1 DO $
;    source_image+=sf[si]*Exp(-Abs(((x_vals-sx[si])/width)^2.+((y_vals-sy[si])/width)^2.-2.*ring_radius)/2.) $
;ELSE FOR si=0.,ns-1 DO $
;    source_image+=sf[si]*Exp(-(((x_vals-sx[si])/width)^2.+((y_vals-sy[si])/width)^2.)/2.) 
Ptr_free,beam_useR_arr
RETURN,source_image
END