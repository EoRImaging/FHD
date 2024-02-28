pro healpix_three_d_images, cube_path, savefile_directory, even = even, odd = odd, total = total, residual=residual, xx=xx, yy=yy, override = override
;cube_path is the $PATH to the cube's location. savefile_directory is where the image dump will be made and where the movie will be made. 
;even or odd indicates that only even or odd cubes will be used for images. total and residual indicate if even+odd or even-odd calculations
;will be used in images. xx and yy indicate the polarization to be used in images. override makes just images and no movie.

;check that movies can be made using machine before execution unless override is specified

  IF ~(!VERSION.RELEASE LT 8.1) THEN version_check = 1

  IF version_check NE 1 AND ~keyword_set(override) THEN BEGIN
    print, 'Must have at least IDL version 8.1 to run quick_movie. Can enter override = 1 to make just the images."
    RETURN
  ENDIF


;check that keywords needed exist, and that cube path and savefile directory exist

  IF (~keyword_set(even) AND ~keyword_set(odd) AND ~keyword_set(residual) AND ~keyword_set(total)) THEN BEGIN
    print, 'Please choose keywords even, odd, residual, or total'
    RETURN
  ENDIF
  
  IF (~keyword_set(xx) AND ~keyword_set(yy)) THEN BEGIN
    print, 'Please choose a polarization through the keywords xx or yy'
    RETURN
  ENDIF

  IF ~FILE_TEST(cube_path, /DIRECTORY) AND ~FILE_TEST(cube_path) THEN BEGIN
    print, "Cube path or cube does not exist."
    RETURN
  ENDIF

  IF ~FILE_TEST(savefile_directory, /DIRECTORY) THEN BEGIN
    print,"Savefile directory does not exist."
    RETURN
  ENDIF

;check formating of savefile input

  num_to_directory = STRPOS(cube_path, '/', /REVERSE_SEARCH)+1
  IF (STRPOS(savefile_directory, '/', /REVERSE_SEARCH)+1) LT STRLEN(savefile_directory) $
    THEN savefile_directory = savefile_directory + '/'

;If cube path doesn't specify a cube, then find and specify the first cube that is in the path.

  IF ~STRMATCH(STRMID(cube_path, num_to_directory), '*.sav') THEN BEGIN
    IF keyword_set(even) THEN name_array = FILE_SEARCH(cube_path + '*even_cube.sav')  
    IF keyword_set(odd) THEN name_array = FILE_SEARCH(cube_path + '*odd_cube.sav')
    IF keyword_set(residual) OR keyword_set(total) THEN name_array = FILE_SEARCH(cube_path + '*even_cube.sav')
    cube_path = name_array[0]
    IF N_ELEMENTS(name_array) GT 1 THEN BEGIN
      print, 'More than one possible choice, choosing first seen: ' + name_array[0]
      print, 'Please specify a cube save file in path input for others.'
    ENDIF
  ENDIF

;If two cubes needed, then find the other complementary cube from the cube specified or the first one found.
;15 IS AN ESTIMATE. FIND A HARDCODED ALTERNATIVE

  IF keyword_set(residual) OR keyword_set(total) THEN BEGIN
    cube_dir = STRMID(cube_path, 0, STRPOS(cube_path, '/', /REVERSE_SEARCH)+1)
    name_check_obs_id = STRMID(cube_path, STRPOS(cube_path, '/', /REVERSE_SEARCH)+1, STRLEN(cube_path)-15-num_to_directory)
    name_check_for_even = STRMID(cube_path, STRPOS(cube_path, '/', /REVERSE_SEARCH)+1, STRLEN(cube_path)-num_to_directory)
    IF STRMATCH(name_check_for_even, '*' + 'even' + '*') THEN BEGIN 
      name_array_odd = FILE_SEARCH(cube_dir + '*odd_cube.sav')
      name_match = WHERE(STRMATCH(name_array_odd, '*' + name_check_obs_id[0] + '*'))
      cube_path_odd = name_array_odd[name_match]
    ENDIF ELSE BEGIN
      cube_path_odd = cube_path
      name_array_even = FILE_SEARCH(cube_dir + '*even_cube.sav')
      name_match = WHERE(STRMATCH(name_array_even, '*' + name_check_obs_id[0] + '*')) 
      cube_path = name_array_even[name_match] 
    ENDELSE
  ENDIF

;get the savefiles
;old savefiles may have called obs differently, so the catch is there to account for that
;PERHAPS CHECK TO SEE IF SAVEFILES ALREADY LOADED

  hpx_inds = getvar_savefile(cube_path, 'hpx_inds')
  nside = getvar_savefile(cube_path, 'nside')

  obs_name='obs'
  CATCH, obs_arr_error
  IF obs_arr_error NE 0 THEN BEGIN
    obs_name='obs_arr'
    CATCH, /CANCEL
  ENDIF

  obs_arr = getvar_savefile(cube_path, obs_name)


  obs_arr = obs_arr[0]
  n_vis = obs_arr.n_vis
  n_freq = obs_arr.n_freq
  baseline_info = *(obs_arr.baseline_info)
  freq_array_big = baseline_info.freq
  freq_array_big_dim = SIZE(freq_array_big, /DIMENSIONS)

;Load the cube specified, and insert polarization label in the picture title

  IF keyword_set(xx) AND ~keyword_set(residual) AND ~keyword_set(total) THEN BEGIN
    input_cube = getvar_savefile(cube_path, 'res_xx_cube')
    size_info = SIZE(input_cube, /DIMENSIONS)
    IF keyword_set(even) THEN BEGIN 
      title_input = 'xx polarization even, '
      title_movie = 'xx_even'
    ENDIF
    IF keyword_set(odd) THEN BEGIN
      title_input = 'xx polarization odd, '
      title_movie = 'xx_odd'
    ENDIF
  ENDIF

  IF keyword_set(yy) AND ~keyword_set(residual) AND ~keyword_set(total) THEN BEGIN
    input_cube = getvar_savefile(cube_path, 'res_yy_cube')
    size_info = SIZE(input_cube, /DIMENSIONS)
    IF keyword_set(even) THEN BEGIN 
      title_input = 'yy polarization even, '
      title_movie = 'yy_even'
    ENDIF
    IF keyword_set(odd) THEN BEGIN
      title_input = 'yy polarization odd, '
      title_movie = 'yy_odd'
    ENDIF
  ENDIF

  IF keyword_set(residual) AND keyword_set(xx) THEN BEGIN
    res_xx_cube_even = getvar_savefile(cube_path, 'res_xx_cube')
    res_xx_cube_odd = getvar_savefile(cube_path_odd, 'res_xx_cube') 
    size_info = SIZE(res_xx_cube_even, /DIMENSIONS)
    input_cube = res_xx_cube_even - res_xx_cube_odd
    title_input = 'xx polarization residual, '
    title_movie = 'xx_residual'
  ENDIF

  IF keyword_set(residual) AND keyword_set(yy) THEN BEGIN
    res_yy_cube_even = getvar_savefile(cube_path, 'res_yy_cube')
    res_yy_cube_odd = getvar_savefile(cube_path_odd, 'res_yy_cube') 
    size_info = SIZE(res_yy_cube_even, /DIMENSIONS)
    input_cube = res_yy_cube_even - res_yy_cube_odd
    title_input = 'yy polarization residual, '
    title_movie = 'yy_residual'
  ENDIF

  IF keyword_set(total) AND keyword_set(xx) THEN BEGIN
    res_xx_cube_even = getvar_savefile(cube_path, 'res_xx_cube')
    res_xx_cube_odd = getvar_savefile(cube_path_odd, 'res_xx_cube') 
    size_info = SIZE(res_xx_cube_even, /DIMENSIONS)
    input_cube = res_xx_cube_even + res_xx_cube_odd
    title_input = 'xx polarization total, '
    title_movie = 'xx_total'
  ENDIF

  IF keyword_set(total) AND keyword_set(yy) THEN BEGIN
    res_yy_cube_even = getvar_savefile(cube_path, 'res_yy_cube')
    res_yy_cube_odd = getvar_savefile(cube_path_odd, 'res_yy_cube') 
    size_info = SIZE(res_yy_cube_even, /DIMENSIONS)
    input_cube = res_yy_cube_even + res_yy_cube_odd
    title_input = 'yy polarization total, '
    title_movie = 'yy_total'
  ENDIF

;Find the frequency per image in MHz by utilizing baseline_info.freq via averaging to the right channel number

  n_avg = freq_array_big_dim/size_info[1]
  h=0
  freq_array = DBLARR(size_info[1])

  FOR j=0, size_info[1]-1 DO BEGIN
    freq_array[j] = TOTAL(freq_array_big[(j * n_avg):((j+1) * n_avg - 1)])/n_avg
    ENDFOR
  freq_array = freq_array * .00000001

;create images (using a flag list) that divides cubes by an average number of visibilities if
;number of visibilities per frequency does not exist

  IF ~TAG_EXIST(obs_arr[0], 'nf_vis') THEN BEGIN

    flag_list=INTARR(size_info[1])                
    FOR ii=0, size_info[1]-1 DO BEGIN
      IF (ii EQ 0) OR (ii EQ 31) OR (ii EQ 32) OR (ii EQ 63) OR (ii EQ 64) OR (ii EQ 95) OR (ii EQ 96) OR (ii EQ 127) $
          OR (ii EQ 128) OR (ii EQ 159) OR (ii EQ 160) OR (ii EQ 191) THEN flag_list[ii]=1
      IF (ii EQ 15) OR (ii EQ 47) OR (ii EQ 79) OR (ii EQ 111) OR (ii EQ 143) OR (ii EQ 175) THEN flag_list[ii]=2
    ENDFOR      
  
    FOR ii=0, size_info[1]-1 DO BEGIN        
      IF flag_list[ii] EQ 1 THEN healpix_quickimage,input_cube/(n_vis/n_freq)/2,hpx_inds,nside, $
          title = title_input + STRTRIM(STRING(freq_array[ii]),1) + ' MHz, ' + STRTRIM(STRING(ii),1) + ' slice', $
          slice_ind = ii,savefile = savefile_directory + STRING(ii)
      IF flag_list[ii] EQ 2 THEN healpix_quickimage,input_cube/(n_vis/n_freq)/(4/3),hpx_inds,nside, $
          title = title_input + STRTRIM(STRING(freq_array[ii]),1) + ' MHz, ' + STRTRIM(STRING(ii),1) + ' slice', $
          slice_ind = ii,savefile = savefile_directory + STRING(ii)
      IF flag_list[ii] EQ 0 THEN healpix_quickimage,input_cube/(n_vis/n_freq),hpx_inds,nside, $
          title = title_input + STRTRIM(STRING(freq_array[ii]),1) + ' MHz, ' + STRTRIM(STRING(ii),1) + ' slice', $
          slice_ind = ii,savefile = savefile_directory + STRING(ii)
    ENDFOR

;If number of visibilities per frequency exists, use it to get the correct units.  Create images
      
  ENDIF ELSE BEGIN
    IF keyword_set(xx) THEN pol_i=0
    IF keyword_set(yy) THEN pol_i=1
    nf_vis = obs_arr.nf_vis
    FOR ii=0, size_info[1]-1 DO BEGIN
      healpix_quickimage,input_cube/nf_vis[pol_i,ii],hpx_inds,nside, $
          title = title_input + STRTRIM(STRING(freq_array[ii]),1) + ' MHz, ' + STRTRIM(STRING(ii),1) + ' slice', $
          slice_ind = ii,savefile = savefile_directory + STRING(ii)
    ENDFOR
  ENDELSE


  IF ~keyword_set(override) THEN $
    quick_movie, savefile_directory, movie_name = title_movie, format = '.mp4', image_type = '.png'

END