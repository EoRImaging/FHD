FUNCTION beam_width_calculate,obs,min_restored_beam_width=min_restored_beam_width,$
    fwhm=fwhm,area_return=area_return,degrees=degrees,radians=radians
;calculates the approximate FWHM of a PSF from the maximum baseline of an instrument
;result is in pixels unless /degree is set

;standard formula is wavelength/baseline length in meters. Here, max baseline is already in wavelengths
IF Keyword_Set(radians) THEN degrees=1
beam_width=1./(obs.MAX_BASELINE) 
IF ~Keyword_Set(degrees) THEN beam_width/=obs.degpix
IF ~Keyword_Set(radians) THEN beam_width*=!RaDeg
IF N_Elements(beam_width) GT 1 THEN beam_width=Median(beam_width,/even)
beam_area=!Pi*beam_width^2.;/(4.*Alog(2.)) ;area under a 2D gaussian with given FWHM 
;factor of (2.*Sqrt(2.*Alog(2.))) is to convert FWHM and sigma of gaussian
IF ~Keyword_Set(fwhm) THEN beam_width/=2.*Sqrt(2.*Alog(2.))
IF N_Elements(min_restored_beam_width) EQ 0 THEN min_restored_beam_width=0.75
IF Keyword_Set(min_restored_beam_width) THEN beam_width=beam_width>min_restored_beam_width

IF Keyword_Set(area_return) THEN RETURN,beam_area
RETURN,beam_width

END