FUNCTION rot_cubic_complex, img, angle_deg, x_interp, y_interp, center_ind

    theta = -angle_deg * !dpi / 180.0
    cos_t = COS(theta)
    sin_t = SIN(theta)

    x_rot = (x_interp-center_ind)*cos_t - (y_interp-center_ind)*sin_t + center_ind
    y_rot = (x_interp-center_ind)*sin_t + (y_interp-center_ind)*cos_t + center_ind

    ; Interpolate all at once
    out = INTERPOLATE(img, x_rot, y_rot, CUBIC=-0.5)
    RETURN, out
END

FUNCTION lofar_beam_setup_gain,obs,antenna,file_path_fhd=file_path_fhd,$
    za_arr=za_arr,az_arr=az_arr,psf_image_dim=psf_image_dim,debug_flip=debug_flip,$
    _EXTRA = EXTRA 

    n_ant_pol=Max(antenna.n_pol)
    nfreq_bin=Max(antenna.nfreq_bin)
    IF N_Elements(file_path_fhd) EQ 0 THEN file_path_fhd=''
    n_tile=obs.n_tile
    beam_model_version=Max(antenna.model_version)
    xvals_instrument=za_arr*Sin(az_arr*!DtoR)
    yvals_instrument=za_arr*Cos(az_arr*!DtoR)
    horizon_test=where(abs(za_arr) GE 90.,n_horizon_test,complement=pix_use,ncomplement=n_pix)
    freq_center=antenna[0].freq ;all need to be identical, so just use the first
    speed_light=299792458. ;speed of light, in meters/second
    icomp=DComplex(0,1)

    ; Read the instrumental pol Jones matrix in az za
    if (min((*obs.baseline_info).freq) GT 189.06250e6) OR (max((*obs.baseline_info).freq) LT 128.85550e6) then begin
        message, 'No LOFAR beam model found for this frequency range'
    endif else begin
        if (min((*obs.baseline_info).freq) GE 159.59950e6) then begin
            file_path_J_matrix=filepath('lofar_hamaker_jones_highband.h5',root=rootdir('FHD'),sub='instrument_config')
        endif else begin 
            if (max((*obs.baseline_info).freq) LE 159.59950e6) then begin
                file_path_J_matrix=filepath('lofar_hamaker_jones_lowband.h5',root=rootdir('FHD'),sub='instrument_config')
            endif else begin
                message, 'Frequency range spans both LOFAR high and low band. Please split the observation or develop the code.'
            endelse
        endelse 
    endelse

    file_id = H5F_OPEN(file_path_J_matrix) ;Get file ID

    ; Define the dataset names
    dataset_names = ['gx', 'gy', 'Dx', 'Dy', 'gx_element', 'gy_element', 'Dx_element', 'Dy_element', $
        'az', 'freqs', 'nside', 'radec_reso']
        ; also present in file but not required at this stage: 'za' (zenith angle), 'station' (station index)

    ; Read the LOFAR h5 file 
    FOR i = 0, N_ELEMENTS(dataset_names)-1 DO BEGIN
        dataset_id = H5D_OPEN(file_id, dataset_names[i])
        data = H5D_READ(dataset_id)
        H5D_CLOSE, dataset_id
        
        ; Assign to appropriately named variable
        ; Reconstruct complex values
        ; "_element" refers to a single element beam, used for phase flip information in D calculation
        CASE dataset_names[i] OF
            'gx': gx = (complex(data.R, data.I))
            'gy': gy = (complex(data.R, data.I))
            'Dx': Dx = (complex(data.R, data.I))
            'Dy': Dy = (complex(data.R, data.I))
            'gx_element': gx_element = (complex(data.R, data.I))
            'gy_element': gy_element = (complex(data.R, data.I))
            'Dx_element': Dx_element = (complex(data.R, data.I))
            'Dy_element': Dy_element = (complex(data.R, data.I))
            'az': az = data
            ;'za': za = data ;not used
            'freqs': freqs = data
            'nside': nside = data
            'radec_reso': radec_reso = data
        ENDCASE
    ENDFOR

    ; Get rotation angles of each LOFAR station compared to the first LOFAR station
    rotation_filepath=filepath('lofar_hba_rotation_angles.txt',root=rootdir('FHD'),subdir='instrument_config')
    textfast,rotation_angle,/read,file_path=rotation_filepath

    nside2 = ulong(nside)^2

    ; Find pixels used at a single frequency
    nan_inds = where(~finite(az),nan_count, complement=pix_use_input)
    ; Remove NaN values to make interpolation easier across all freqs
    nan_inds = where(~finite(gx),nan_count)
    if nan_count GT 0 then begin
        gx[nan_inds] = 0
        Dx[nan_inds] = 0
        Dy[nan_inds] = 0
        gy[nan_inds] = 0
    endif

    ; The inputs from the hd5 file were created with python, and thus have
    ; row-col major instead of IDL's column-row major.
    ; We need to transpose the dimensions of the image to match IDL's expectations.
    n_freq_input = N_Elements(gx[0,*])
    gx = reform(transpose(reform(gx,nside,nside,n_freq_input),[1, 0, 2]),nside2,n_freq_input)
    gy = reform(transpose(reform(gy,nside,nside,n_freq_input),[1, 0, 2]),nside2,n_freq_input)
    Dx = reform(transpose(reform(Dx,nside,nside,n_freq_input),[1, 0, 2]),nside2,n_freq_input)
    Dy = reform(transpose(reform(Dy,nside,nside,n_freq_input),[1, 0, 2]),nside2,n_freq_input)
    ; Transpose image for element beams, and leave in 2D since no freq interp required
    gx_element = transpose(reform(gx_element,nside,nside))
    gy_element = transpose(reform(gy_element,nside,nside))
    Dx_element = transpose(reform(Dx_element,nside,nside))
    Dy_element = transpose(reform(Dy_element,nside,nside))

    ; Get the astrometry structure for the beam pointed at the phase center
    hor2eq,obs.phasedec,obs.phasera,obs.jd0,beam_center_ra,beam_center_dec,ha_out,lat=obs.lat,lon=obs.lon,/precess,/nutate
    projection_slant_orthographic,astr=beam_astr,degpix=radec_reso,obsra=beam_center_ra,obsdec=beam_center_dec,zenra=beam_center_ra,zendec=beam_center_dec,$
        dimension=nside,elements=nside,obsx=obsx,obsy=obsy,zenx=beam_zenx,zeny=beam_zeny,phasera=beam_center_ra,phasedec=beam_center_dec,$
        epoch=2000.,JDate=obs.JD0

    ; Using this astrometry, find the x,y positions of the instrumental beam pixels
    Hor2Eq,90.-za_arr[pix_use],az_arr[pix_use],obs.jd0,ra_use,dec_use,lat=obs.lat,lon=obs.lon,alt=obs.alt,precess=1,/nutate, refract=0
    ad2xy,ra_use,dec_use,beam_astr,x_interp,y_interp

    ; Find indices where a flip in phase occurs, given a 0.1 rad tolerance.
    ; To define a positive delay from the dipole arm, these J matrix indices will be multiplied by -1.
    delay_element = atan(imaginary(float(gx_element[pix_use_input])), real_part(float(gx_element[pix_use_input])))
    flip_inds_J00 = where(delay_element GT (!pi-0.1))
    delay_element = atan(imaginary(float(Dx_element[pix_use_input])), real_part(float(Dx_element[pix_use_input])))
    flip_inds_J01 = where(delay_element GT (!pi-0.1))
    delay_element = atan(imaginary(float(Dy_element[pix_use_input])), real_part(float(Dy_element[pix_use_input])))
    flip_inds_J10 = where(delay_element GT (!pi-0.1))
    delay_element = atan(imaginary(float(gy_element[pix_use_input])), real_part(float(gy_element[pix_use_input])))
    flip_inds_J11 = where(delay_element GT (!pi-0.1))

    ; Calculate center index of input for rotation algorithm
    center_ind = (ulong(nside)) / 2

    ; initialize K matrix for instrumental pixels
    K_proj_inst = PTRARR(2, 2)

    ; Loop over instrumental frequencies
    FOR freq_i = 0, nfreq_bin-1 DO BEGIN

        J00 = complex(fltarr(N_elements(pix_use_input)))
        J11 = complex(fltarr(N_elements(pix_use_input)))
        J01 = complex(fltarr(N_elements(pix_use_input)))
        J10 = complex(fltarr(N_elements(pix_use_input)))

        for pix_i=0L, N_elements(pix_use_input)-1 do begin
            J00[pix_i] = interpol(gx[pix_use_input[pix_i],*],freqs, freq_center[freq_i],/quadratic)
            J11[pix_i] = interpol(gy[pix_use_input[pix_i],*],freqs, freq_center[freq_i],/quadratic)
            J01[pix_i] = interpol(Dx[pix_use_input[pix_i],*],freqs, freq_center[freq_i],/quadratic)
            J10[pix_i] = interpol(Dy[pix_use_input[pix_i],*],freqs, freq_center[freq_i],/quadratic)
        endfor

        ; Solve for the magnitude of F, the diagonal amplitude, hence only two elements defined
        F_mag = [[sqrt(abs(J00)^2 + abs(J01)^2)], $
            [sqrt(abs(J10)^2 + abs(J11)^2)]]

        ; Get the combination of F_phase and K (pseudo, or old K), 
        ; which is easily solvable because the inverse of F is the reciprocal
        pseudo_K = [[[J00 / F[*,0]], [J01 / F[*,0]]], $
                    [[J10 / F[*,1]], [J11 / F[*,1]]]]

        J00[flip_inds_J00] *= -1
        J01[flip_inds_J01] *= -1
        J10[flip_inds_J10] *= -1
        J11[flip_inds_J11] *= -1

        ; F_phase is the diagonal time delay matrix, the combined phase received by p and q components
        F_phase = [[exp(icomp * atan(imaginary(J00+J01), real_part(J00+J01)))], $
            [exp(icomp * atan(imaginary(J10+J11), real_part(J10+J11)))]]

        ; K_proj is the polarisation-dependent (projection) response which should be tile-independent
        K_proj = [[[ pseudo_K[*,0,0] / D[*, 0] ], [ pseudo_K[*,0,1] / D[*, 0] ]], $
            [[ pseudo_K[*,1,0] / D[*, 1] ], [ pseudo_K[*,1,1] / D[*, 1] ]]]

        ; Dummy matrix for intermediate steps
        input_matrix = Dcomplexarr(nside,nside)

        ;Interpolate K_proj to the instrumental pixel grid
        for i = 0,1 do begin
            for j = 0,1 do begin
                input_matrix[pix_use_input] = K_proj[*, i, j]
                K_proj_inst[i, j] = Ptr_new(INTERPOLATE(input_matrix, x_interp, y_interp))
            endfor
        endfor

        antenna[*].K_proj[*,*,freq_i] = K_proj_inst

        for tile_i=0, n_tile-1 do begin
            ; For each tile, rotate the F_mag and F_phase matrices.
            ; This is way around having a seperate beam model file for each LOFAR station.
            ; Generalised input_matrix and output_matrix used to reduce RAM overhead.

            input_matrix[pix_use_input] = F_mag[*,0]
            output_matrix = rot_cubic_complex(input_matrix, rotation_angle[tile_i], x_interp, y_interp, center_ind)
            (antenna[tile_i].F_mag[0, freq_i]) = ptr_new(output_matrix)

            input_matrix[pix_use_input] = F_phase[*,0]
            output_matrix = rot_cubic_complex(input_matrix, rotation_angle[tile_i], x_interp, y_interp, center_ind)
            (antenna[tile_i].F_phase[0, freq_i]) = ptr_new(output_matrix)
            
            input_matrix[pix_use_input] = F_mag[*,1]
            output_matrix = rot_cubic_complex(input_matrix, rotation_angle[tile_i], x_interp, y_interp, center_ind)
            (antenna[tile_i].F_mag[1, freq_i]) = ptr_new(output_matrix)

            input_matrix[pix_use_input] = F_phase[*,1]
            output_matrix = rot_cubic_complex(input_matrix, rotation_angle[tile_i], x_interp, y_interp, center_ind)
            (antenna[tile_i].F_phase[1, freq_i]) = ptr_new(output_matrix)

            ; Update combined Jones matrix ( F D K ) to reflect rotation of delays and amplitude
            (antenna[tile_i].jones[0, 0, freq_i]) = Ptr_new( (*antenna[tile_i].F_mag[0, freq_i]) * (*antenna[tile_i].F_phase[0, freq_i]) * (*antenna[tile_i].K_proj[0, 0, freq_i]) )
            (antenna[tile_i].jones[0, 1, freq_i]) = Ptr_new( (*antenna[tile_i].F_mag[0, freq_i]) * (*antenna[tile_i].F_phase[0, freq_i]) * (*antenna[tile_i].K_proj[0, 1, freq_i]) )
            (antenna[tile_i].jones[1, 0, freq_i]) = Ptr_new( (*antenna[tile_i].F_mag[1, freq_i]) * (*antenna[tile_i].F_phase[1, freq_i]) * (*antenna[tile_i].K_proj[1, 0, freq_i]) )
            (antenna[tile_i].jones[1, 1, freq_i]) = Ptr_new( (*antenna[tile_i].F_mag[1, freq_i]) * (*antenna[tile_i].F_phase[1, freq_i]) * (*antenna[tile_i].K_proj[1, 1, freq_i]) )

        endfor

    endfor

    ; Group_id identifies which tiles have the same F D. 
    ; By definition, all lofar stations have a unique F D.
    for tile_i=0, n_tile-1 do begin
        antenna[tile_i].group_id = [tile_i, tile_i]
    endfor

    RETURN,antenna
END
