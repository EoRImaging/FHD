FUNCTION interpolate_kernel, kernel_arr, x_offset=x_offset, y_offset=y_offset, $
    dx0dy0=dx0dy0, dx1dy0=dx1dy0, dx0dy1=dx0dy1, dx1dy1=dx1dy1

kernel = *kernel_arr[x_offset, y_offset]*dx0dy0
kernel += *kernel_arr[x_offset+1, y_offset]*dx1dy0
kernel += *kernel_arr[x_offset, y_offset+1]*dx0dy1
kernel += *kernel_arr[x_offset+1, y_offset+1]*dx1dy1
;kernel = Temporary(kernel_0_0) + Temporary(kernel_1_0) + Temporary(kernel_0_1) + Temporary(kernel_1_1)

;initial code below (before adding 1 element in each dimension to the beam kernel)
;kernel_0_0 = *kernel_arr[x_offset, y_offset]
;kernel_1_0 = *kernel_arr[(x_offset+1) mod resolution, y_offset]
;kernel_0_1 = *kernel_arr[x_offset, (y_offset+1) mod resolution]
;kernel_1_1 = *kernel_arr[(x_offset+1) mod resolution, (y_offset+1) mod resolution]
;
;IF (x_offset+1 EQ resolution) AND (y_offset+1 EQ resolution) THEN BEGIN
;    kernel_1_0=reform(shift(reform(kernel_1_0,dim,dim),1,0),dim^2.)
;    kernel_0_1=reform(shift(reform(kernel_0_1,dim,dim),0,1),dim^2.)
;    kernel_1_1=reform(shift(reform(kernel_1_1,dim,dim),1,1),dim^2.)
;ENDIF ELSE IF (x_offset+1 EQ resolution) THEN BEGIN
;    kernel_1_0=reform(shift(reform(kernel_1_0,dim,dim),1,0),dim^2.)
;    kernel_1_1=reform(shift(reform(kernel_1_1,dim,dim),1,0),dim^2.)
;ENDIF ELSE IF (y_offset+1 EQ resolution) THEN BEGIN
;    kernel_0_1=reform(shift(reform(kernel_0_1,dim,dim),0,1),dim^2.)
;    kernel_1_1=reform(shift(reform(kernel_1_1,dim,dim),0,1),dim^2.)
;ENDIF
;
;kernel = kernel_0_0*(1-dx)*(1-dy) + kernel_1_0*(dx)*(1-dy) + kernel_0_1*(1-dx)*dy + kernel_1_1*dx*dy
RETURN, kernel
END