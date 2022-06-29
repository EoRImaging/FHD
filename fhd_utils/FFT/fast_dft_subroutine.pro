FUNCTION fast_dft_subroutine,x_vec,y_vec,amp_vec,dimension=dimension,elements=elements,silent=silent,$
    dft_threshold=dft_threshold,return_kernel=return_kernel,inds_use=inds_use,double_precision=double_precision

t0_a=Systime(1)
IF N_Elements(elements) EQ 0 THEN elements=dimension
IF size(amp_vec,/type) EQ 10 THEN  ptr_flag=1 ELSE ptr_flag=0 ;check if pointer type. This allows the same locations to be used for multiple sets of fluxes

Pi=!DPi
dimension_kernel=Long(dimension*2)
elements_kernel=Long(elements*2)
IF N_Elements(dft_threshold) EQ 0 THEN dft_threshold=1./((2.*Pi)^2.*dimension)  ;1/2 value of kernel_test along either axis and one bin to either side at the edge of the image. 

t1_a=Systime(1)
xv_test=meshgrid(dimension_kernel,elements_kernel,1)-dimension_kernel/2.
yv_test=meshgrid(dimension_kernel,elements_kernel,2)-elements_kernel/2.

; Calculate the fractional contribution to the FFT 
; Create a mask to include only pixels with significant contribution
kernel_test=1D/((Abs(Pi*xv_test)>1.)*(Abs(Pi*yv_test)>1.))$
           +1D/((Abs(Pi*(xv_test+dimension_kernel)))*(Abs(Pi*yv_test)>1.))$
           +1D/((Abs(Pi*(xv_test-dimension_kernel)))*(Abs(Pi*yv_test)>1.))$
           +1D/((Abs(Pi*xv_test)>1.)*(Abs(Pi*(yv_test+dimension_kernel))))$
           +1D/((Abs(Pi*xv_test)>1.)*(Abs(Pi*(yv_test-dimension_kernel))))
kernel_test_shift=Shift(kernel_test,-1,-1) ;the peak of the kernel may be offset by up to one pixel
kernel_i=where((kernel_test>kernel_test_shift) GE dft_threshold,n_k)
kernel_mask=intarr(dimension_kernel,elements_kernel) & kernel_mask[kernel_i]=1

; Create x and y vectors for the extended, masked kernel 
xv_k=Long((kernel_i mod dimension_kernel)-dimension_kernel/2)
yv_k=Long(Floor(kernel_i/dimension_kernel)-elements_kernel/2)

; Create an array for pixel center shifts for the kernel depending on each sources' location
xcen0=Long(Floor(x_vec))
ycen0=Long(Floor(y_vec))
dx_arr=x_vec-xcen0
dy_arr=y_vec-ycen0

; Calculate which sources fall in the image plane
si_use=where((xcen0 GT 0) AND (ycen0 GT 0) AND (xcen0 LT dimension-1) AND (ycen0 LT elements-1),ns)
IF Keyword_Set(inds_use) THEN BEGIN
    ind_flag=intarr(N_Elements(xcen0))
    ind_flag2=ind_flag
    ind_flag[si_use]=1
    ind_flag2[inds_use]=1
    ind_flag*=ind_flag2
    si_use = where(ind_flag,ns)
ENDIF

; Test if any gridding kernels would extend beyond image boundaries
xv_test=Minmax(xcen0[si_use])+Minmax(xv_k)
yv_test=Minmax(ycen0[si_use])+Minmax(yv_k)

; Extend the calculated image grid to include full kernel extents
IF xv_test[0] LT 0 OR xv_test[1] GT dimension-1 OR yv_test[0] LT 0 OR yv_test[1] GT elements-1 THEN BEGIN
    mod_flag=1
    x0=Long(xv_test[0])
    y0=Long(yv_test[0])
    dimension_use=Long(xv_test[1]-x0)
    elements_use=Long(yv_test[1]-y0)
    xcen0-=x0
    ycen0-=y0
ENDIF ELSE BEGIN
    mod_flag=0 
    x0=0L
    y0=0L
    dimension_use=Long(dimension)
    elements_use=Long(elements)
ENDELSE

t1=Systime(1)-t1_a
t2=0
t3=0

; Initialize a model image that matches the format of the input amplitude vector (including the extended plane if necessary)
IF ptr_flag THEN BEGIN
    n_ptr0=N_Elements(amp_vec)
    ptr_i=where(Ptr_valid(amp_vec),n_ptr)
    amp_ptr=amp_vec[ptr_i]
    IF size(*amp_ptr[0], /type) GE 6 THEN init_arr=DComplexarr(dimension_use, elements_use) ELSE init_arr=Dblarr(dimension_use,elements_use)
    model_img_use=Ptrarr(n_ptr)
    FOR p_i=0,n_ptr-1 DO model_img_use[p_i]=Ptr_new(init_arr)
ENDIF ELSE BEGIN
    IF size(amp_vec, /type) GE 6 THEN init_arr=DComplexarr(dimension_use, elements_use) ELSE init_arr=Dblarr(dimension_use,elements_use)
    model_img_use=init_arr
ENDELSE
init_arr_ret=init_arr[0:dimension-1, 0:elements-1]

; Common operations calculated outside of the source loop for speed
xv0=Dindgen(dimension_kernel)-dimension_kernel/2.
yv0=Dindgen(elements_kernel)-elements_kernel/2.
x_sign=(-1D)^xv0
y_sign=(-1D)^yv0
xv_k_i=xv_k+dimension_kernel/2.
yv_k_i=yv_k+elements_kernel/2.
sin_x=Sin(Pi*(-dx_arr))
sin_y=Sin(Pi*(-dy_arr))

; Calculate each sources' most significant FFT contribution and add to the model image plane
FOR si_iter=0L,ns-1L DO BEGIN
    si = si_use[si_iter]
    t2_a=Systime(1)

    ; Calculate the sinc function (inverse of the FT of delta function on discrete plane) for only pixels which contribute
    ; significantly by shifting the kernel to the location of each source for x and y separately. For sources which line up 
    ; perfectly with the discrete plane, no correction is necessary
    IF dx_arr[si] EQ 0 THEN BEGIN
        kernel_x=Dblarr(dimension_kernel)
        kernel_x[dimension_kernel/2]=1D
    ENDIF ELSE BEGIN
        kernel_x=sin_x[si]/(Pi*(xv0-dx_arr[si]))$
                +sin_x[si]/(Pi*(dimension_kernel+xv0-dx_arr[si]+x0))$
                +sin_x[si]/(Pi*(-dimension_kernel+xv0-dx_arr[si]-x0))
        kernel_x*=x_sign
    ENDELSE
    IF dy_arr[si] EQ 0 THEN BEGIN
        kernel_y=Dblarr(elements_kernel)
        kernel_y[elements_kernel/2]=1D
    ENDIF ELSE BEGIN
        kernel_y=sin_y[si]/(Pi*(yv0-dy_arr[si]))$
                +sin_y[si]/(Pi*(elements_kernel+yv0-dy_arr[si]+y0))$
                +sin_y[si]/(Pi*(-elements_kernel+yv0-dy_arr[si]-y0))
        kernel_y*=y_sign
    ENDELSE
    kernel_single=kernel_x[xv_k_i]*kernel_y[yv_k_i]
;    kernel_norm=Total(kernel_single,/double)
    kernel_norm=1D
    
    t3_a=Systime(1)
    t2+=t3_a-t2_a

    ; Get the indicies that the shifted kernel corresponds to and add the sources' sinc approx to the model image plane
    inds=xcen0[si]+xv_k+(ycen0[si]+yv_k)*dimension_use
    IF ptr_flag THEN FOR p_i=0,n_ptr-1 DO (*model_img_use[p_i])[inds]+=(*amp_ptr[p_i])[si]*kernel_single/kernel_norm $
        ELSE model_img_use[inds]+=amp_vec[si]*kernel_single/kernel_norm
    t3+=Systime(1)-t3_a
ENDFOR

t4_a=Systime(1)
IF Keyword_Set(mod_flag) THEN BEGIN
    x_low0=x0>0
    y_low0=y0>0
    x_high0=(x0+dimension_use-1)<(dimension-1)
    y_high0=(y0+elements_use-1)<(elements-1)
    x_low1=-x0>0
    y_low1=-y0>0
    x_high1=x_high0-x_low0+x_low1
    y_high1=y_high0-y_low0+y_low1
    
    IF ptr_flag THEN BEGIN
        model_img=Ptrarr(n_ptr0)
            FOR p_i=0,n_ptr0-1 DO model_img[p_i]=Ptr_new(init_arr_ret)
        FOR p_i=0,n_ptr-1 DO (*model_img[ptr_i[p_i]])[x_low0:x_high0,y_low0:y_high0]=(*model_img_use[p_i])[x_low1:x_high1,y_low1:y_high1]
    ENDIF ELSE BEGIN 
        model_img=init_arr_ret
        model_img[x_low0:x_high0,y_low0:y_high0]=model_img_use[x_low1:x_high1,y_low1:y_high1]
    ENDELSE
    
    ;add in aliasing!
    IF x_low1 GT 0 THEN BEGIN
        IF ptr_flag THEN FOR p_i=0,n_ptr-1 DO (*model_img[ptr_i[p_i]])[dimension-x_low1:dimension-1,y_low0:y_high0]+=(*model_img_use[p_i])[0:x_low1-1,y_low1:y_high1] $
            ELSE model_img[dimension-x_low1:dimension-1,y_low0:y_high0]+=model_img_use[0:x_low1-1,y_low1:y_high1]
    ENDIF
    IF y_low1 GT 0 THEN BEGIN
        IF ptr_flag THEN FOR p_i=0,n_ptr-1 DO (*model_img[ptr_i[p_i]])[x_low0:x_high0,elements-y_low1:elements-1]+=(*model_img_use[p_i])[x_low1:x_high1,0:y_low1-1] $
            ELSE model_img[x_low0:x_high0,elements-y_low1:elements-1]+=model_img_use[x_low1:x_high1,0:y_low1-1]
    ENDIF
    IF x_high1 LT dimension_use-1 THEN BEGIN
        IF ptr_flag THEN FOR p_i=0,n_ptr-1 DO (*model_img[ptr_i[p_i]])[0:dimension_use-x_high1-2,y_low0:y_high0]+=(*model_img_use[p_i])[x_high1+1:dimension_use-1,y_low1:y_high1] $
            ELSE model_img[0:dimension_use-x_high1-2,y_low0:y_high0]+=model_img_use[x_high1+1:dimension_use-1,y_low1:y_high1]
    ENDIF
    IF y_high1 LT elements_use-1 THEN BEGIN
        IF ptr_flag THEN FOR p_i=0,n_ptr-1 DO (*model_img[ptr_i[p_i]])[x_low0:x_high0,0:elements_use-y_high1-2]+=(*model_img_use[p_i])[x_low1:x_high1,y_high1+1:elements_use-1] $
            ELSE model_img[x_low0:x_high0,0:elements_use-y_high1-2]+=model_img_use[x_low1:x_high1,y_high1+1:elements_use-1]
    ENDIF
    undefine_fhd,model_img_use
ENDIF ELSE model_img=model_img_use

t4=Systime(1)-t4_a
t0=Systime(1)-t0_a

IF not Keyword_Set(silent) THEN print,t0,t1,t2,t3,t4
RETURN,model_img
END
