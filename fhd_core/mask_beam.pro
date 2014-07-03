FUNCTION mask_beam,obs,antenna,tile_beam,psf_image_dim=psf_image_dim,psf_intermediate_res=psf_intermediate_res,frequency=frequency

kbinsize=obs.kpix
obs_ha=obs.zenra-obs.obsra
coord_rotation=Acos(Cos(obs.lat*!DtoR)*Cos(obs.obsdec*!DtoR)+Sin(obs.lat*!DtoR)*Sin(obs.obsdec*!DtoR)*Cos(obs_ha*!DtoR))*!Radeg

antenna_size=antenna.size_meters
speed_light=299792458. ;speed of light, in meters/second
antenna_size_pix=psf_intermediate_res*Ceil((antenna_size*frequency/speed_light)/kbinsize)
antenna_size_pix=Ceil(antenna_size_pix/2)*2.
beam_mask0=fltarr(psf_image_dim,psf_image_dim)
beam_mask0[((psf_image_dim-antenna_size_pix)/2)>0:((psf_image_dim+antenna_size_pix)/2-1)<(psf_image_dim-1),$
    ((psf_image_dim-antenna_size_pix)/2)>0:((psf_image_dim+antenna_size_pix)/2-1)<(psf_image_dim-1)]=1.
beam_mask=rot(beam_mask0,coord_rotation,pivot=[psf_image_dim/2,psf_image_dim/2],/interp)

tile_beam_ft=fft_shift(FFT(fft_shift(tile_beam)))

tile_beam_return=fft_shift(FFT(fft_shift(tile_beam_ft*beam_mask),/inverse))

tile_beam_return*=Max(Abs(tile_beam))/Max(Abs(tile_beam_return))
RETURN,tile_beam_return
END