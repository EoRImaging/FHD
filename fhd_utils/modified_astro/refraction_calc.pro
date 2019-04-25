PRO refraction_calc,obs,refract_amplitude,refract_angle

dimension=obs.dimension
elements=obs.elements
astr=obs.astr
freq=obs.freq_center
zenra=obs.zenra
zendec=obs.zendec
lat=obs.lat

xy2ad,meshgrid(dimension,elements,1),meshgrid(dimension,elements,2),astr,ra_arr,dec_arr

coord_i_use=where(Finite(ra_arr))
ra_vals=ra_arr[coord_i_use]
dec_vals=dec_arr[coord_i_use]
zenith_angle=angle_difference(zendec,zenra,dec_vals,ra_vals,/degree,/nearest)
hour_angle=ra_vals-zenra

refract_amplitude_vals=refraction_air(freq,zenith_angle,/unit_Hz)
Eq2Hor,ra_vals,dec_vals,obs.JD0,alt_arr1,az_arr1,lat=obs.lat,lon=obs.lon,alt=obs.alt,precess=1,/nutate

refract_amplitude=Fltarr(dimension,elements)
refract_angle=Fltarr(dimension,elements)
refract_amplitude[coord_i_use]=refract_amplitude_vals
refract_angle[coord_i_use]=az_arr1 ;(az_arr1+180.) mod 360


END
