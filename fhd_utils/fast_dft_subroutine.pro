FUNCTION fast_dft_subroutine,x_vec,y_vec,amp_vec,dft_kernel=dft_kernel,kernel_size=kernel_size,$
    dimension=dimension,elements=elements,over_resolution=over_resolution,resolution=resolution,conserve_flux=conserve_flux

IF N_Elements(dft_kernel) EQ 0 THEN dft_kernel='sinc' ELSE dft_kernel=StrLowCase(dft_kernel)
IF N_Elements(elements) EQ 0 THEN elements=dimension
IF N_Elements(resolution) EQ 0 THEN resolution=32. ELSE resolution=Float(resolution)
over_resolution=1. ;also prevents case of over_resolution=0 which will error
IF ~Keyword_Set(kernel_size) THEN kernel_size=16. ELSE kernel_size=Ceil(kernel_size/2.)*2.

ns=N_Elements(x_vec)

res_total=resolution*over_resolution
box_dim=kernel_size*over_resolution
box_dimR=box_dim*resolution

x_valsR=meshgrid(box_dimR,box_dimR,1)-box_dimR/2.
y_valsR=meshgrid(box_dimR,box_dimR,2)-box_dimR/2.
xvals_i=meshgrid(box_dim,box_dim,1)*resolution
yvals_i=meshgrid(box_dim,box_dim,2)*resolution

kernel_over_x=Sin(!Pi*x_valsR/res_total)*weight_invert(!Pi*x_valsR/res_total)
zero_test=where(x_valsR EQ 0,n_zero) & IF n_zero GT 0 THEN kernel_over_x[zero_test]=1. 
kernel_over_y=Sin(!Pi*y_valsR/res_total)*weight_invert(!Pi*y_valsR/res_total)
zero_test=where(y_valsR EQ 0,n_zero) & IF n_zero GT 0 THEN kernel_over_y[zero_test]=1. 

kernel_over=kernel_over_x*kernel_over_y
;kernel_over*=Sqrt(Abs(kernel_over)) ;semi-questionable attempt to make the function fall off faster, but preserve negatives and zero crossing locations

kernel_arr=Ptrarr(resolution,resolution,/allocate)
FOR i=0,resolution-1 DO FOR j=0,resolution-1 DO *kernel_arr[i,j]=kernel_over[xvals_i+i,yvals_i+j]/Total(kernel_over[xvals_i+i,yvals_i+j])

;IF Keyword_Set(conserve_flux) THEN BEGIN
;    FOR i=0,resolution-1 DO FOR j=0,resolution-1 DO $
;        *kernel_arr[i,j]=kernel_over[xvals_i+i,yvals_i+j];/Total(kernel_over[xvals_i+i,yvals_i+j])
;ENDIF ELSE BEGIN
;    FOR i=0,resolution-1 DO FOR j=0,resolution-1 DO $
;        *kernel_arr[i,j]=kernel_over[xvals_i+i,yvals_i+j];/Max(kernel_over[xvals_i+i,yvals_i+j])
;ENDELSE

x_offset=Round((Ceil(x_vec)-x_vec)*resolution) mod resolution    
y_offset=Round((Ceil(y_vec)-y_vec)*resolution) mod resolution
xcen0=Round(x_vec+x_offset/resolution) ;do this after offset, in case it has rounded to the next grid point
ycen0=Round(y_vec+y_offset/resolution)
xmin=Floor(xcen0-box_dim/2.) & xmax=xmin+box_dim-1
ymin=Floor(ycen0-box_dim/2.) & ymax=ymin+box_dim-1

si1=where((xmin GE 0) AND (ymin GE 0) AND (xmax LE dimension-1) AND (ymax LE elements-1),ns)
model_img=fltarr(dimension,elements)
FOR si=0L,ns-1L DO BEGIN
    model_img[xmin[si1[si]]:xmax[si1[si]],ymin[si1[si]]:ymax[si1[si]]]+=amp_vec[si1[si]]*(*kernel_arr[x_offset[si1[si]],y_offset[si1[si]]])
ENDFOR
Ptr_free,kernel_arr

RETURN,model_img
END