FUNCTION fast_dft_subroutine,x_vec,y_vec,amp_vec,dft_kernel=dft_kernel,kernel_size=kernel_size,$
    dimension=dimension,elements=elements,resolution=resolution,conserve_flux=conserve_flux

IF N_Elements(dft_kernel) EQ 0 THEN dft_kernel='sinc' ELSE dft_kernel=StrLowCase(dft_kernel)
IF N_Elements(elements) EQ 0 THEN elements=dimension
IF N_Elements(resolution) EQ 0 THEN resolution=32. ELSE resolution=Float(resolution)
;over_resolution=1. ;also prevents case of over_resolution=0 which will error
IF ~Keyword_Set(kernel_size) THEN kernel_size=16. ELSE kernel_size=Ceil(kernel_size/2.)*2.
kernel_extend_length=(kernel_size^2.)<dimension<elements
kernel_extend_width=6.<kernel_size

ns=N_Elements(x_vec)

res_total=resolution
box_dim=kernel_size
box_dimR=box_dim*resolution
box_ext_lenR=kernel_extend_length*resolution
box_ext_widthR=kernel_extend_width*resolution

;calculate central NxN box 
x_valsR=meshgrid(box_dimR,box_dimR,1)-box_dimR/2.
y_valsR=meshgrid(box_dimR,box_dimR,2)-box_dimR/2.
xvals_i=meshgrid(box_dim,box_dim,1)*resolution
yvals_i=meshgrid(box_dim,box_dim,2)*resolution

kernel_over_x=Sin(!Pi*x_valsR/res_total)*weight_invert(!Pi*x_valsR/res_total)
zero_test=where(x_valsR EQ 0,n_zero) & IF n_zero GT 0 THEN kernel_over_x[zero_test]=1. 
kernel_over_y=Sin(!Pi*y_valsR/res_total)*weight_invert(!Pi*y_valsR/res_total)
zero_test=where(y_valsR EQ 0,n_zero) & IF n_zero GT 0 THEN kernel_over_y[zero_test]=1. 

kernel_over=kernel_over_x*kernel_over_y
;NOTE: slice up kernel later, to allow properly calculating the normalization

;calculate N^2 x width box (cross arm in the x-direction)
x_vals_extR=meshgrid(box_ext_lenR,box_ext_widthR,1)-box_ext_lenR/2.
y_vals_extR=meshgrid(box_ext_lenR,box_ext_widthR,2)-box_ext_widthR/2.
xvals_ext_i=meshgrid(kernel_extend_length,kernel_extend_width,1)*resolution
yvals_ext_i=meshgrid(kernel_extend_length,kernel_extend_width,2)*resolution

kernel_ext_over_x=Sin(!Pi*x_vals_extR/res_total)*weight_invert(!Pi*x_vals_extR/res_total)
zero_ext_test=where(x_vals_extR EQ 0,n_ext_zero) & IF n_ext_zero GT 0 THEN kernel_ext_over_x[zero_ext_test]=1. 
kernel_ext_over_y=Sin(!Pi*y_vals_extR/res_total)*weight_invert(!Pi*y_vals_extR/res_total)
zero_ext_test=where(y_vals_extR EQ 0,n_ext_zero) & IF n_ext_zero GT 0 THEN kernel_ext_over_y[zero_ext_test]=1. 

kernel_ext_over=kernel_ext_over_x*kernel_ext_over_y
kernel_ext_over[where(Abs(x_vals_extR) LE box_dimR/2.)]=0.

;Now calculate normalization 
kernel_norm=(resolution^2.)/(Total(kernel_over)+2.*Total(kernel_ext_over))

kernel_arr=Ptrarr(resolution,resolution,/allocate)
FOR i=0,resolution-1 DO FOR j=0,resolution-1 DO *kernel_arr[i,j]=kernel_over[xvals_i+i,yvals_i+j]*kernel_norm


kernel_ext_X_arr=Ptrarr(resolution,resolution,/allocate)
FOR i=0,resolution-1 DO FOR j=0,resolution-1 DO *kernel_ext_X_arr[i,j]=kernel_ext_over[xvals_ext_i+i,yvals_ext_i+j]*kernel_norm

;calculate width x N^2 box (cross arm in the y-direction)
x_vals_extR=meshgrid(box_ext_widthR,box_ext_lenR,1)-box_ext_widthR/2.
y_vals_extR=meshgrid(box_ext_widthR,box_ext_lenR,2)-box_ext_lenR/2.
xvals_ext_i=meshgrid(kernel_extend_width,kernel_extend_length,1)*resolution
yvals_ext_i=meshgrid(kernel_extend_width,kernel_extend_length,2)*resolution

kernel_ext_over_x=Sin(!Pi*x_vals_extR/res_total)*weight_invert(!Pi*x_vals_extR/res_total)
zero_ext_test=where(x_vals_extR EQ 0,n_ext_zero) & IF n_ext_zero GT 0 THEN kernel_ext_over_x[zero_ext_test]=1. 
kernel_ext_over_y=Sin(!Pi*y_vals_extR/res_total)*weight_invert(!Pi*y_vals_extR/res_total)
zero_ext_test=where(y_vals_extR EQ 0,n_ext_zero) & IF n_ext_zero GT 0 THEN kernel_ext_over_y[zero_ext_test]=1. 

kernel_ext_over=kernel_ext_over_x*kernel_ext_over_y
kernel_ext_over[where(Abs(y_vals_extR) LE box_dimR/2.)]=0.

kernel_ext_Y_arr=Ptrarr(resolution,resolution,/allocate)
FOR i=0,resolution-1 DO FOR j=0,resolution-1 DO *kernel_ext_Y_arr[i,j]=kernel_ext_over[xvals_ext_i+i,yvals_ext_i+j]*kernel_norm

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

xmin_X_ext=Floor(xcen0-kernel_extend_length/2.) & xmax_X_ext=xmin_X_ext+kernel_extend_length-1
ymin_X_ext=Floor(ycen0-kernel_extend_width/2.) & ymax_X_ext=ymin_X_ext+kernel_extend_width-1
xmin_Y_ext=Floor(xcen0-kernel_extend_width/2.) & xmax_Y_ext=xmin_Y_ext+kernel_extend_width-1
ymin_Y_ext=Floor(ycen0-kernel_extend_length/2.) & ymax_Y_ext=ymin_Y_ext+kernel_extend_length-1

si1=where((xmin GE 0) AND (ymin GE 0) AND (xmax LE dimension-1) AND (ymax LE elements-1),ns)

si1_unmod=where((xmin_X_ext[si1]<ymin_Y_ext[si1] GE 0) AND (xmax_X_ext[si1] LT dimension) AND (ymax_Y_ext[si1] LT elements),n_unmod)
mod_flag=intarr(ns)+1 & IF n_unmod GT 0 THEN mod_flag[si1_unmod]=0

model_img=fltarr(dimension,elements)
FOR si=0L,ns-1L DO BEGIN
    model_img[xmin[si1[si]]:xmax[si1[si]],ymin[si1[si]]:ymax[si1[si]]]+=amp_vec[si1[si]]*(*kernel_arr[x_offset[si1[si]],y_offset[si1[si]]])
    IF mod_flag[si] THEN BEGIN
        
    ENDIF ELSE BEGIN
        model_img[xmin_X_ext[si1[si]]:xmax_X_ext[si1[si]],ymin_X_ext[si1[si]]:ymax_X_ext[si1[si]]]+=amp_vec[si1[si]]*(*kernel_ext_X_arr[x_offset[si1[si]],y_offset[si1[si]]])
        model_img[xmin_Y_ext[si1[si]]:xmax_Y_ext[si1[si]],ymin_Y_ext[si1[si]]:ymax_Y_ext[si1[si]]]+=amp_vec[si1[si]]*(*kernel_ext_Y_arr[x_offset[si1[si]],y_offset[si1[si]]])
    ENDELSE
    
ENDFOR
Ptr_free,kernel_arr,kernel_ext_X_arr,kernel_ext_Y_arr

RETURN,model_img
END