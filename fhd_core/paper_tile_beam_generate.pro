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
;beam_slice=fltarr(naxis1,naxis2)
;FOR i=0,naxis1-1 DO FOR j=0,naxis2-1 DO beam_slice[i,j]=Interpol(beam_cube[i,j,*],freq_arr,frequency)

IF Keyword_Set(polarization) THEN beam_slice=Rotate(beam_slice,1)

;IF polarization EQ 0 THEN BEGIN
;    ra_arr=(meshgrid(naxis1,naxis2,1)-beam_RA_pix0)*beam_RA_delt+beam_RA0
;    dec_arr=(meshgrid(naxis1,naxis2,2)-beam_Dec_pix0)*beam_Dec_delt+beam_Dec0
;ENDIF ELSE BEGIN
;    ra_arr=(meshgrid(naxis1,naxis2,2)-beam_RA_pix0)*beam_RA_delt+beam_RA0
;    dec_arr=(meshgrid(naxis1,naxis2,1)-beam_Dec_pix0)*beam_Dec_delt+beam_Dec0
;ENDELSE
projection_slant_orthographic,astr=astr,degpix=degpix,obsra=beam_RA0,obsdec=beam_Dec0,zenra=beam_RA0,zendec=beam_Dec0,$
    dimension=naxis1,elements=naxis2,obsx=beam_RA_pix0+1,obsy=beam_Dec_pix0+1,zenx=beam_RA_pix0+1,zeny=beam_Dec_pix0+1

IF Keyword_Set(polarization) THEN ca=[2,1] ELSE ca=[1,2] ;gives default polarization of XX (polarization=0)
;xv=meshgrid(naxis1,naxis2,ca[0])
;yv=meshgrid(naxis1,naxis2,ca[1])
;xy2ad,xv,yv,astr,ra1,dec1
;valid_i=where(Finite(ra1) AND Finite(dec1))
;ra=fltarr(naxis1,naxis2) & ra[valid_i]=ra1[valid_i]
;dec=fltarr(naxis1,naxis2) & dec[valid_i]=dec1[valid_i]
;
;alt_arr=90.-za_arr
;jd=Julday(0,0,2000.)
;Hor2Eq,alt_arr,az_arr,jd,ra_arr,dec_arr,precess=0,nutate=0,aberration=0,refract=0,lat=beam_Dec0
;valid_i2=where(Finite(ra_arr) AND Finite(dec_arr))
;ra_use=ra_arr[valid_i2]
;dec_use=dec_arr[valid_i2]

valid_i2=where(Finite(ra_arr) AND Finite(dec_arr))
dimension=(size(ra_arr,/dimension))[0]
elements=(size(ra_arr,/dimension))[1]
ra_offset=ra_arr[dimension/2.,elements/2.]
dec_offset=dec_arr[dimension/2.,elements/2.]
ra_use=ra_arr[valid_i2]-ra_offset
dec_use=dec_arr[valid_i2]-dec_offset
ad2xy,ra_use,dec_use,astr,xv_use,yv_use
tile_beam_use=interpolate(beam_slice,xv_use,yv_use,missing=0)
tile_beam=Fltarr(size(za_arr,/dimension))
tile_beam[valid_i2]=tile_beam_use  ;gives default polarization of XX (polarization=0)

IF not Keyword_Set(antenna_beam_arr) THEN antenna_beam_arr=Ptrarr(1,/allocate)
*antenna_beam_arr[0]=tile_beam

RETURN,tile_beam
END