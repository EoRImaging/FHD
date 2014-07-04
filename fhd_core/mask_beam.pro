FUNCTION mask_beam,obs,antenna,tile_beam,psf_image_dim=psf_image_dim,psf_intermediate_res=psf_intermediate_res,frequency=frequency

kbinsize=obs.kpix
obs_ha=obs.obsra-obs.zenra
IF obs_ha LT 0 THEN obs_ha+=360.

xvals=meshgrid(psf_image_dim,psf_image_dim,1)-psf_image_dim/2
yvals=meshgrid(psf_image_dim,psf_image_dim,2)-psf_image_dim/2

lat=obs.lat *!DtoR
dec=obs.obsdec*!DtoR
ha=obs_ha*!DtoR

J11=Cos(lat)*Cos(dec)+Sin(lat)*Sin(dec)*Cos(ha)
J12=-Sin(lat)*Sin(ha)
J21=Sin(dec)*Sin(ha)
J22=Cos(ha)
xv_use=J11*xvals+J12*yvals+psf_image_dim/2.
yv_use=J21*xvals+J22*yvals+psf_image_dim/2.

antenna_size=antenna.size_meters
speed_light=299792458. ;speed of light, in meters/second
antenna_size_pix=psf_intermediate_res*Ceil((antenna_size*frequency/speed_light)/kbinsize)
antenna_size_pix=Ceil((antenna_size_pix-1)/2)*2.+1
beam_mask0=fltarr(psf_image_dim,psf_image_dim)
beam_mask0[((psf_image_dim-antenna_size_pix)/2)>0:((psf_image_dim+antenna_size_pix)/2)<(psf_image_dim-1),$
    ((psf_image_dim-antenna_size_pix)/2)>0:((psf_image_dim+antenna_size_pix)/2)<(psf_image_dim-1)]=1.
beam_mask=interpolate(beam_mask0,xv_use,yv_use)

tile_beam_ft=fft_shift(FFT(fft_shift(tile_beam)))

tile_beam_return=fft_shift(FFT(fft_shift(tile_beam_ft*beam_mask),/inverse))

tile_beam_return*=Max(Abs(tile_beam))/Max(Abs(tile_beam_return))
RETURN,tile_beam_return
END