FUNCTION fft_shift_cube, image_cube
dimension=(size(image_cube,/dimension))[0]
elements=(size(image_cube,/dimension))[1]
n_slice=(size(image_cube,/dimension))[2]
shifted_image_cube = Fltarr(dimension, elements, n_slice)
FOR s=0,n_slice-1 DO $
  shifted_image_cube[*, *, s] = fft_shift(Reform(image_cube[*, *, s]))
RETURN, shifted_image_cube
END