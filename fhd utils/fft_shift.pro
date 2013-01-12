FUNCTION fft_shift,image
dimension=(size(image,/dimension))[0]
elements=(size(image,/dimension))[1]
shift_image=shift(image,dimension/2,elements/2)

RETURN,shift_image
END