FUNCTION convolve2,image,kernel,absolute=absolute,window_function=window_function,pad=pad
;performs a fast convolution of image with kernel
;Either image or kernel may be supplied as their fourier transform
;any parameter supplied as a complex number will be assumed to be its FFT
IF N_Elements(pad) EQ 0 THEN pad=0
dimension0=(size(image,/dimension))[0]
elements0=(size(image,/dimension))[1]
dimension=dimension0+2.*pad
elements=elements0+2.*pad

k_dimension=(size(kernel,/dimension))[0]
k_elements=(size(kernel,/dimension))[1]

x_start=(dimension-k_dimension)/2
y_start=(elements-k_elements)/2

IF ((size(kernel,/type) EQ 6) OR (size(kernel,/type) EQ 9)) THEN BEGIN
    kernel_use=complexarr(dimension,elements)
    kernel_use[x_start:x_start+k_dimension-1,y_start:y_start+k_elements-1]=kernel
    f_kernel=kernel_use
ENDIF ELSE BEGIN
    kernel_use=fltarr(dimension,elements)
    kernel_use[x_start:x_start+k_dimension-1,y_start:y_start+k_elements-1]=kernel
    f_kernel=fft(kernel_use)
ENDELSE

;IF ((size(image,/type) EQ 6) OR (size(image,/type) EQ 9)) THEN BEGIN
;    f_image=image 
;ENDIF ELSE BEGIN
;    f_image=fft(image)
;ENDELSE
CASE size(image,/type) OF
    6: BEGIN
        f_image=complexarr(dimension,elements)
        f_image[pad:pad+dimension0-1,pad:pad+elements0-1]=fft_shift(image)
        f_image=fft_shift(f_image)
    END
    9: BEGIN
        f_image=Dcomplexarr(dimension,elements)
        f_image[pad:pad+dimension0-1,pad:pad+elements0-1]=fft_shift(image)
        f_image=fft_shift(f_image)    
    END
    ELSE: BEGIN
;        f_image=complexarr(dimension,elements)
;        f_image[pad:pad+dimension0-1,pad:pad+elements0-1]=fft_shift(fft(image))
;        f_image=fft_shift(f_image)

        f_image=fltarr(dimension,elements)
        f_image[pad:pad+dimension0-1,pad:pad+elements0-1]=image
        f_image=fft(f_image)
    ENDELSE
ENDCASE

f_image=fft_shift(f_image)
f_kernel=fft_shift(f_kernel)

f_result=f_image*f_kernel

IF Keyword_Set(window_function) THEN BEGIN
    window_function=StrLOWcase(window_function)
    dimension_use=(dimension<elements)
    xarr=meshgrid(dimension,elements,1)-dimension/2
    yarr=meshgrid(dimension,elements,2)-elements/2
    CASE window_function OF
        'circle': BEGIN
            window=fltarr(dimension,elements)
            window[where(Sqrt(xarr^2.+yarr^2.) LT Floor(dimension_use/2))]=1
        END
        'hanning':window=Hanning(dimension,elements)
        'hanning2':window=fft_shift(Hanning(dimension,elements))
        'gaussian':window=Exp(-((xarr/(dimension_use/4.))^2.+(yarr/(dimension_use/4.))^2.)/2.)
    ENDCASE
    window/=Mean(window) ;compensate for missing modes?    
ENDIF ELSE window=fltarr(dimension,elements)+1.
f_result*=window
;f_result=fft_shift(f_result)

f_result=fft_shift(f_result)
IF Keyword_Set(absolute) THEN result=Abs(fft(f_result,/inverse)) ELSE result=Real_part(fft(f_result,/inverse))
result*=dimension*elements
;result=Real_part(fft(f_result,/inverse))*dimension*elements;/total(kernel)
result=fft_shift(result)

IF Keyword_Set(pad) THEN result=result[pad:pad+dimension0-1,pad:pad+elements0-1]
RETURN,result
END