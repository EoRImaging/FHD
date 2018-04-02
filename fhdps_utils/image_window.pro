function image_window, x_locs, y_locs, image_window_name = image_window_name, $
  fractional_size = fractional_size

  ;; This function makes a 2D spatial window for an irregularly gridded (e.g. HEALPix) pixel set

  ;; x_locs and y_locs give the x,y values for all the pixels
  ;; (number of image pixels = n_elements(x_locs) = n_elements(y_locs))
  if n_elements(x_locs) ne n_elements(y_locs) then begin
    message, 'Number of x pixel locations must match the number of y pixel locations'
  endif

  x_minmax = minmax(x_locs)
  y_minmax = minmax(y_locs)
  x_extent = x_minmax[1] - x_minmax[0]
  y_extent = y_minmax[1] - y_minmax[0]

  ;; Make a 1000 element mask given the filter type (should be fairly smooth with that many elements).
  window_1d = spectral_window(1000, periodic = periodic, type = image_window_name, fractional_size = fractional_size)

  ;; Make a 1000x1000 mask
  window_2d = window_1d # transpose(window_1d)

  ;; Find locations of pixel centers
  pix_center_x = (x_locs - x_minmax[0]) * N_elements(window_1d)/x_extent
  pix_center_y = (y_locs - y_minmax[0]) * N_elements(window_1d)/y_extent

  ;; Interpolate the 1000x1000 mask to the pixel centeres
  pix_window = interpolate(window_2d, pix_center_x, pix_center_y)

  ;; n_elements(pix_center_y) gives the number of pixels
  norm_factor = sqrt(n_elements(pix_center_y)/total(pix_window^2.))

  pix_window = pix_window * norm_factor

  return, pix_window
end
