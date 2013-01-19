;+
; :Description:
;    Shifts an image in x and y by any amount
;
; :Params:
;    image
;
; :Keywords:
;    dx Shift in x (Float)
;    dy Shift in y(Float)
;
; :Author: Ian Sullivan
;-
FUNCTION Fshift,image,dx=dx,dy=dy
;

IF N_Elements(dx) EQ 0 THEN dx=0.
IF N_Elements(dy) EQ 0 THEN dy=0.

dimension=(size(image,/dimension))[0]
elements=(size(image,/dimension))[1]

xvals=meshgrid(dimension,elements,1)-dimension/2
yvals=meshgrid(dimension,elements,2)-elements/2

icomp=Complex(0,1)
phase=Exp((2.*!Pi*icomp/dimension)*(dx*xvals+dy*yvals))

fimage=fft_shift(FFT(fft_shift(image),/inverse))*dimension*elements
fimage2=fimage*phase
result=dirty_image_generate(fimage2)

RETURN,result
END