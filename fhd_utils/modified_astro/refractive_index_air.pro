FUNCTION refractive_index_air,wavelength,unit_micron=unit_micron,unit_nm=unit_nm,unit_meter=unit_meter,$
    unit_Hz=unit_Hz,_Extra=extra

c_light=299792458.
CASE 1 OF
    Keyword_Set(unit_Hz): wavelength_use=(c_light/wavelength)*1E6
    Keyword_Set(unit_meter):wavelength_use=wavelength*1E6
    Keyword_Set(unit_nm):wavelength_use=wavelength*1E-3
    Keyword_Set(unit_micron):wavelength_use=wavelength
    ELSE:wavelength_use=wavelength
ENDCASE
wavelength_use=Double(wavelength_use)

term_A=64.328D
term_B=29498.1/(146.-(1/wavelength_use^2))
term_C=255.4/(41.-(1/wavelength_use^2))

n=1.+(term_A+term_B+term_C)*1E-6
RETURN,n
END