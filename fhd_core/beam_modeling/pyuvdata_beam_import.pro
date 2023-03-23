FUNCTION pyuvdata_beam_import, obs, antenna_str, pyuvdata_filepath,$
    za_arr=za_arr, az_arr=az_arr, psf_image_dim=psf_image_dim, pix_use=pix_use

lun = fxposit(pyuvdata_filepath, 0,/readonly)
MRD_HREAD, lun, primary_header, /silent

coordsys = strtrim(sxpar(primary_header,'coordsys'),2)
CASE coordsys OF
    "az_za": BEGIN
        Jones_matrix = import_az_el_beam(obs, antenna_str, pyuvdata_filepath,$
                                         za_arr=za_arr, az_arr=az_arr, psf_image_dim=psf_image_dim)
    END
    "orthoslant_zenith": BEGIN
        message,"COORDSYS type 'orthoslant_zenith' not yet supported!""
    END
    "healpix": BEGIN
        message,"COORDSYS type 'healpix' not yet supported!"
    END
    ELSE: message,"Unsupported COORDSYS found in pyuvdata file!"
ENDCASE

for pol1=0,(size(Jones_matrix))[1]-1 do begin
  for pol2=0,(size(Jones_matrix))[2]-1 do begin
    if (size(Jones_matrix))[0] gt 2 then begin
      for ind=0,(size(Jones_matrix))[3]-1 do begin
        *Jones_matrix[pol1, pol2, ind] = (*Jones_matrix[pol1, pol2, ind])[pix_use]
      endfor
    endif else begin
      *Jones_matrix[pol1, pol2] = (*Jones_matrix[pol1, pol2])[pix_use]
    endelse
  endfor
endfor

RETURN, Jones_matrix
END
