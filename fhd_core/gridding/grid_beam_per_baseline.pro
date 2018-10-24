function grid_beam_per_baseline, psf, uu, vv, ww, l_mode, m_mode, n_tracked, frequency_array, x, y,$
    xmin_use, ymin_use, freq_i, bt_index, polarization, fbin, image_bot, image_top, psf_dim3,$
    box_matrix, vis_n, beam_int=beam_int, beam2_int=beam2_int, n_grp_use=n_grp_use,$
    degrid_flag=degrid_flag,beam_clip_floor=beam_clip_floor,_Extra=extra

;Make the beams on the fly with corrective phases given the baseline location. 
;Will need to be rerun for every baseline, so speed is key.
;For more information, see Jack Line's thesis

;Loop over all visibilities that fall within the chosen visibility box
FOR ii=0L,vis_n-1 DO begin
    ;Pixel center offset phases
    deltau_l = l_mode*(uu[bt_index[ii]]*frequency_array[freq_i[ii]]-x[xmin_use+psf.dim/2])
    deltav_m = m_mode*(vv[bt_index[ii]]*frequency_array[freq_i[ii]]-y[ymin_use+psf.dim/2])
    ;w term offset phase
    w_n_tracked = n_tracked*ww[bt_index[ii]]*frequency_array[freq_i[ii]]

    ;Generate a UV beam from the image space beam, offset by calculated phases
    psf_base_superres=dirty_image_generate((*(*psf.image_info).image_power_beam_arr[polarization,fbin[ii]])*$
      exp(2.*!pi*Complex(0,1)*(-w_n_tracked+deltau_l+deltav_m)),/no_real)
 
    psf_base_superres=psf_base_superres[image_bot:image_top,image_bot:image_top]

    ;A quick way to sum down the image by a factor of 2 in both dimensions.
    ;A 4x4 example where we sum down by a factor of 2
    ;
    ;1  2  3  4           1  2           1  2           1  2  5  6            14 46           14 22
    ;5  6  7  8    -->    3  4    -->    5  6    -->    9  10 13 14    -->    22 54    -->    56 54
    ;9  10 11 12                         9  10
    ;13 14 15 16          5  6           13 14          3  4  7  8
    ;                     7  8                          11 12 15 16
    ;                                    3  4
    ;                     9  10          7  8
    ;                     11 12          11 12
    ;                                    15 16
    ;                     13 14
    ;                     15 16    
    d = size(psf_base_superres,/DIMENSIONS) & nx = d[0]/psf.resolution & ny = d[1]/psf.resolution
    psf_base_superres = transpose(total(reform(transpose(reform(psf_base_superres,psf.resolution,$
      nx,psf.resolution*ny,/overwrite),[0,2,1]),psf.resolution^2,ny,nx,/overwrite),1))
 
    psf_base_superres = reform(psf_base_superres, psf.dim^2.)
    box_matrix[psf_dim3*ii]=psf_base_superres
endfor

;Subtract off a small clip, set negative indices to 0, and renomalize.
;  This is a modification of the look-up-table beam using a few assumptions
;  to make it faster/feasible to run.
;Modifications: done per group of baselines that fit within the current box, 
;  rather than individually. region_grow is not used to find a contiguous
;  edge around the beam to cut because it is too slow.
if keyword_set(beam_clip_floor) then begin
    psf_val_ref=Total(box_matrix,1)
    psf_amp = abs(box_matrix)
    psf_mask_threshold_use = Max(psf_amp)/psf.beam_mask_threshold
    psf_amp -= psf_mask_threshold_use
    psf_phase = Atan(box_matrix, /phase)
    psf_amp = psf_amp>0
    box_matrix = psf_amp*Cos(psf_phase) + Complex(0,1)*psf_amp*Sin(psf_phase)
    ref_temp = total(box_matrix,1)
    for ii=0, vis_n-1 do box_matrix[*,ii]*=psf_val_ref[ii]/ref_temp[ii]
endif

if keyword_set(degrid_flag) then begin
    ;Calculate the beam and beam^2 integral (degridding)
    psf_resolution = Long(psf.resolution)
    beam_int_temp = Total(box_matrix,1,/double)/psf_resolution^2.
    beam2_int_temp = Total(Abs(box_matrix)^2,1,/double)/psf_resolution^2.
    for ii=0, N_elements(freq_i)-1 do begin
        beam_int[freq_i[ii]]+=beam_int_temp[ii]
        beam2_int[freq_i[ii]]+=beam2_int_temp[ii]
        n_grp_use[freq_i[ii]]+=1
    endfor
endif

return, box_matrix

end
