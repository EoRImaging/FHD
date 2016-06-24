FUNCTION refraction_air,wavelength,zenith_angle,_Extra=extra

n=refractive_index_air(wavelength,_Extra=extra)
R0=(n^2.-1.)/(2.*n^2.)
refraction=R0*Tan(zenith_angle*!DtoR)*!RaDeg

RETURN,refraction
END