FUNCTION beam_width_calculate,obs,min_restored_beam_width=min_restored_beam_width,fwhm=fwhm

;calculates the approximate FWHM of a PSF from the maximum baseline of an instrument

;factor of (2.*Sqrt(2.*Alog(2.))) is to convert FWHM and sigma of gaussian
beam_width=!RaDeg/(obs_out.MAX_BASELINE/obs_out.KPIX)/obs_out.degpix
IF ~Keyword_Set(fwhm) THEN beam_width/=2.*Sqrt(2.*Alog(2.))
IF N_Elements(min_restored_beam_width) EQ 0 THEN min_restored_beam_width=0.75
IF Keyword_Set(min_restored_beam_width) THEN beam_width=beam_width>min_restored_beam_width

RETURN,beam_width

END