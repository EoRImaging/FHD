FUNCTION paper_tile_beam_generate,antenna_gain_arr,antenna_beam_arr,$
    frequency=frequency,angle_offset=angle_offset,polarization=polarization,$
    psf_dim=psf_dim,psf_resolution=psf_resolution,kbinsize=kbinsize,$
    normalization=normalization,xvals=xvals,yvals=yvals,$
    dimension=dimension,elements=elements,za_arr=za_arr,az_arr=az_arr,$
    ra_arr=ra_arr,dec_arr=dec_arr,_Extra=extra

IF N_Elements(normalization) EQ 0 THEN normalization=1.
IF Keyword_Set(antenna_beam_arr) THEN IF Keyword_Set(*antenna_beam_arr[0]) THEN BEGIN
    tile_beam=*antenna_beam_arr[0]*antenna_gain_arr[0]
    tile_beam*=normalization
    tile_beam=tile_beam
    RETURN,tile_beam
ENDIF
xvals_use=xvals
yvals_use=yvals
dimension=(size(xvals,/dimension))[0]
elements=(size(yvals,/dimension))[1]

IF strlowcase(!version.os_family) EQ 'windows' THEN $ 
    paper_beam_filepath=rootdir()+'PAPER_DATA\PAPER_beam_xx.fits' ELSE $
    paper_beam_filepath='/data2/PAPER/PAPER_beam_xx.fits'
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

degpix=[beam_RA_delt,beam_Dec_delt]


freq_i_test=Min(abs(freq_arr-frequency),freq_i_use)
beam_slice=Reform(beam_cube[*,*,freq_i_use])

beam_slice=Sqrt(beam_slice>0.)

;IF polarization EQ 1 THEN beam_slice=shift(Rotate(beam_slice,1),1,0)

projection_slant_orthographic,astr=astr,degpix=degpix,obsra=beam_RA0,obsdec=beam_Dec0,zenra=beam_RA0,zendec=beam_Dec0,$
    dimension=naxis1,elements=naxis2,obsx=beam_RA_pix0+1,obsy=beam_Dec_pix0+1,zenx=beam_RA_pix0+1,zeny=beam_Dec_pix0+1

IF Keyword_Set(polarization) THEN astr.cd=[[0.,1.],[1.,0.]] ELSE astr.cd=[[1.,0.],[0.,1.]]
mask_i=where((xvals_use EQ 0) AND (yvals_use EQ 90),n_mask)
IF n_mask GT 0 THEN BEGIN
    xvals_use[mask_i]=!Values.F_NAN
    yvals_use[mask_i]=!Values.F_NAN
ENDIF
ad2xy,xvals_use,yvals_use,astr,x_int,y_int
tile_beam=Float(interpolate(beam_slice,x_int,y_int,missing=0))

IF not Keyword_Set(antenna_beam_arr) THEN antenna_beam_arr=Ptrarr(1,/allocate)
*antenna_beam_arr[0]=tile_beam

RETURN,tile_beam
END