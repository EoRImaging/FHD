FUNCTION paper_tile_beam_generate,antenna_gain_arr,antenna_beam_arr,$
    frequency=frequency,angle_offset=angle_offset,polarization=polarization,$
    psf_dim=psf_dim,psf_resolution=psf_resolution,kbinsize=kbinsize,$
    normalization=normalization,xvals=xvals,yvals=yvals,$
    dimension=dimension,elements=elements,za_arr=za_arr,az_arr=az_arr,_Extra=extra

paper_beam_filepath='C:\MWA\PAPER_DATA\PAPER_beam_xx.fits'
beam_cube=mrdfits(paper_beam_filepath,0,header,/silent)
naxis1=sxpar(header,'NAXIS1')
naxis2=sxpar(header,'NAXIS2')
n_freq=sxpar(header,'NAXIS3')

beam_RA_pix0=sxpar(header,'CRPIX1')-1
beam_RA0=sxpar(header,'CRVAL1')
beam_RA_delt=sxpar(header,'CDELT1')
beam_Dec_pix0=sxpar(header,'CRPIX2')-1
beam_Dec0=sxpar(header,'CRVAL2')
beam_Dec_delt=sxpar(header,'CDELT2')

freq_ref_i=sxpar(header,'CRPIX3')-1
freq_ref_val=sxpar(header,'CRVAL3')
freq_delt=sxpar(header,'CDELT3')
freq_arr=(findgen(n_freq)-freq_ref_i)*freq_delt+freq_ref_val

beam_slice=fltarr(naxis1,naxis2)
FOR i=0,naxis1-1 DO FOR j=0,naxis2-1 DO beam_slice[i,j]=Interpol(beam_cube[i,j,*],freq_arr,frequency)

;IF polarization EQ 0 THEN BEGIN
;    ra_arr=(meshgrid(naxis1,naxis2,1)-beam_RA_pix0)*beam_RA_delt+beam_RA0
;    dec_arr=(meshgrid(naxis1,naxis2,2)-beam_Dec_pix0)*beam_Dec_delt+beam_Dec0
;ENDIF ELSE BEGIN
;    ra_arr=(meshgrid(naxis1,naxis2,2)-beam_RA_pix0)*beam_RA_delt+beam_RA0
;    dec_arr=(meshgrid(naxis1,naxis2,1)-beam_Dec_pix0)*beam_Dec_delt+beam_Dec0
;ENDELSE


RETURN,tile_beam
END