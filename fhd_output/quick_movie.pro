PRO quick_movie, images_location, movie_name = movie_name, format = format, starting_image = starting_image, $
                ending_image = ending_image, image_type = image_type
;images_location is the path information to the images, where all images in path will be used for movie.
;movie_name gives the name of the movie. format is the format type of the movie, i.e. mpeg, avi, gif,
;and default is mpeg. starting_image and ending_image are ints that specify if some images at either end of the
;spectrum are to be excluded, and default is no exclusion. image_type is the format of the images in directory,
;and default is png.

  IF ~keyword_set(movie_name) THEN movie_name = 'quick_movie1'

;only works for at least IDL version 8.1, or for computers with personal licenses

  IF ~(!VERSION.RELEASE LT 8.1) THEN version_check = 1

  IF version_check NE 1 THEN BEGIN
    print, 'Must have at least IDL version 8.1."
    RETURN
  ENDIF

;define maximum resolution size

  dimensions = GET_SCREEN_SIZE()  
  x_screen_resolution = dimensions[0]
  y_screen_resolution = dimensions[1]

;check that images location and image format is of the proper format

  num_to_slash = STRPOS(images_location, '/', /REVERSE_SEARCH)
  string_length = STRLEN(images_location)
  IF string_length GT num_to_slash + 1 THEN images_location = images_location + '/'

  IF ~keyword_set(image_type) THEN image_type = '.png'
  IF ~STRMATCH(image_type, '.*') THEN image_type = '.' + image_type

;find the names of all the images and how many there are, error if there are none

  name_array = FILE_SEARCH(images_location + '*' + image_type)
  nframes = N_ELEMENTS(name_array)

  IF (nframes EQ 1) THEN BEGIN
    print, 'No files of type ' + image_type + ' found in ' + images_location
    RETURN
  ENDIF

;If some images at end or beginning are to be excluded, then include

  IF NOT keyword_set(starting_image) THEN starting_image = 1                   
  IF NOT keyword_set(ending_image) THEN ending_image = nframes                     

;Get the biggest sized image and make that the movie size to assure all images fit

  image_status = QUERY_IMAGE(name_array[0], info)
  xsize = info.dimensions[0]
  ysize = info.dimensions[1]

  FOR n = starting_image-1, ending_image-1 DO BEGIN
    image_status = QUERY_IMAGE(name_array[n], info)
    IF (info.dimensions[0] GT xsize) THEN xsize = info.dimensions[0]
    IF (info.dimensions[1] GT ysize) THEN ysize = info.dimensions[1]
  ENDFOR

;scale images to fit the screen

  set_image_scale = ROUND((FLOAT(ysize)/FLOAT(y_screen_resolution) + .4))  

;Create movie

  IF version_check EQ 1 THEN BEGIN

    IF ~keyword_set(format) THEN format = '.mp4'
    IF ~STRMATCH(format, '.*') THEN format = '.' + format

    video = idlffvideowrite(images_location + movie_name + format)
    print, 'Movie output: ', images_location + movie_name + format
 
    speed = 2                                                            ;animation speed in frames per second
    window_dimensions = [xsize/set_image_scale,ysize/set_image_scale]
    stream = video.addvideostream(window_dimensions[0], window_dimensions[1], speed)

    FOR n = starting_image-1, ending_image-1 DO BEGIN                          ;begin loop over all images
      image = READ_IMAGE(name_array[n]) 
      rescaled_image = CONGRID(image, 3, xsize/set_image_scale, ysize/set_image_scale)      ;rescale the images to fit in the screen and have the same size  
      !NULL = video.put(stream, rescaled_image)
    ENDFOR

    video.cleanup
  ENDIF

END
