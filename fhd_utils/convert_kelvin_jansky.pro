FUNCTION convert_kelvin_jansky,Temperature,nside=nside,degpix=degpix,to_kelvin=to_kelvin,frequency=frequency
;function that converts a map in Kelvin to Jy/pixel, or the reverse if /to_kelvin is set
;frequency can be an array

IF Keyword_Set(nside) THEN pix_area=4.*!Pi/nside2npix(nside) ELSE pix_area=(degpix*!DtoR)^2.
k_b=1.38E-23/1E-26 ;Boltzmann constant combined with 10^-26 Jansky conversion factor
c_light=299792458.
wavelength=c_light/Mean(frequency)
surface_brightness=2.*k_b*Temperature/wavelength^2.
Jy_per_pixel=surface_brightness*pix_area

RETURN,Jy_per_pixel
END