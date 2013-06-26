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

;;;BEGIN COPY FROM MWA AS A CHECK!!!

;kbinsize_use=kbinsize;/psf_resolution
;
;IF N_Elements(normalization) EQ 0 THEN normalization=1.
;psf_dim=Float(psf_dim)
;;psf_dim2=psf_dim*psf_resolution
;psf_dim2=dimension
;degpix_use=!RaDeg/(kbinsize_use*psf_dim2) 
;IF N_Elements(xvals) EQ 0 THEN xvals=(meshgrid(psf_dim2,psf_dim2,1)-psf_dim2/2.)*degpix_use
;IF N_Elements(yvals) EQ 0 THEN yvals=(meshgrid(psf_dim2,psf_dim2,2)-psf_dim2/2.)*degpix_use
;IF (N_Elements(obsaz)EQ 0) OR (N_Elements(obsza) EQ 0) THEN BEGIN
;    x1=reform(xvals[*,psf_dim2/2.]) & x1i=where(x1)
;    y1=reform(yvals[psf_dim2/2.,*]) & y1i=where(y1)
;    x0=interpol(x1i,x1[x1i],0.)
;    y0=interpol(y1i,y1[y1i],0.)
;    za=Interpolate(za_arr,x0,y0,cubic=-0.5)
;    az=Interpolate(az_arr,x0,y0,cubic=-0.5)
;ENDIF ELSE BEGIN
;    za=obsza
;    az=obsaz
;ENDELSE
;;za=za_arr[psf_dim2/2.,psf_dim2/2.]
;;az=az_arr[psf_dim2/2.,psf_dim2/2.]
;
;antenna_length=29.125*2.54/100. ;FROM MWA!!! meters (measured)
;antenna_height=0.60 ; meters (rumor from Danny J.)
;
;Kconv=(2.*!Pi)*(frequency/299792458.) ;wavenumber (radians/meter)
;;Kconv=(frequency/299792458.) ;wavenumber (radians/meter)
;wavelength=299792458./frequency
;
;xc_arr=[0.] & yc_arr=[0.] & zc_arr=[0.]
;term_A=Tan(az*!DtoR)
;term_B=za*!DtoR
;xc=Sqrt((term_B^2.)/(1+term_A^2.))
;yc=term_A*xc
;za_arr_use=Reform(za_arr,(psf_dim2)^2.)
;az_arr_use=Reform(az_arr,(psf_dim2)^2.)
;D0_d=[0.]
;
;proj_east=Reform(xvals,(psf_dim2)^2.)
;proj_north=Reform(yvals,(psf_dim2)^2.)
;proj_z=Cos(za_arr_use*!DtoR)
;
;;;phase of each dipole for the source (relative to the beamformer settings)
;;D_d=(proj_east#xc_arr+proj_north#yc_arr+proj_z#zc_arr-replicate(1,(psf_dim2)^2.)#D0_d*!Radeg);/Kconv
;;D_d=Reform(D_d,psf_dim2,psf_dim2,16)
;
;groundplane=2.*Sin(Cos(za_arr_use*!DtoR)*(Kconv*(antenna_height)))
;groundplane=Reform(groundplane,psf_dim2,psf_dim2)
;
;projection=1.
;
;ii=Complex(0,1)
;;dipole_gain_arr=Exp(-ii*Kconv*D_d*!DtoR)
;
;
;IF not Keyword_Set(antenna_beam_arr) THEN antenna_beam_arr=Ptrarr(1,/allocate)
;FOR i=0,0 DO BEGIN
;    *antenna_beam_arr[i]=groundplane;*dipole_gain_arr[*,*,i]
;ENDFOR
;
;;tile_beam=fltarr(psf_dim2,psf_dim2)
;tile_beam=*antenna_beam_arr[0]*antenna_gain_arr[0]
;
;tile_beam*=normalization
;
;;tile_beam=tile_beam>0.
;RETURN,tile_beam
;END

xvals_use=xvals
yvals_use=yvals
dimension=(size(xvals,/dimension))[0]
elements=(size(yvals,/dimension))[1]

IF strlowcase(!version.os_family) EQ 'windows' THEN $ 
    paper_beam_filepath=rootdir()+'PAPER_DATA\PAPER_x_beam_nside128.fits' ELSE $
    paper_beam_filepath='/data2/PAPER/psa32/PAPER_x_beam_nside128.fits'
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

;beam_slice=Sqrt(beam_slice>0.)

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
tile_beam=Float(interpolate(beam_slice,x_int,y_int,missing=0,cubic=-0.5))>0.

IF not Keyword_Set(antenna_beam_arr) THEN antenna_beam_arr=Ptrarr(1,/allocate)
*antenna_beam_arr[0]=tile_beam

RETURN,tile_beam
END