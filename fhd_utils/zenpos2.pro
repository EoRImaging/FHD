PRO ZENPOS2, date, ra, dec,lat=lat,lng=lng,degree=degree,J2000=J2000
;+
; NAME:
;       ZENPOS
; PURPOSE:
;       Return the zenith RA and Dec in radians for a given Julian date.
;
; CALLING SEQUENCE:
;       ZENPOS, Date, Ra, Dec
;
; INPUT:
;       Date  The Julian date, in double precision, of the date and time
;               for which the zenith position is desired, scalar or vector.
;
; OUTPUTS:
;       Ra    The right ascension in RADIANS of the zenith.
;       Dec   The declination in RADIANS of the zenith.
;
; PROCEDURE:
;       The local sidereal time is computed; this is the RA of the zenith.
;       It and the observatories latitude (corresponding to the Dec.) are
;       converted to radians and returned as the zenith direction.
;
;
; PROCEDURE CALLS:
;       CT2LST - Convert to Local Mean Sidereal Time
; MODIFICATION HISTORY:
;       Written by Michael R. Greason, STX, 14 October 1988.
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Update documentation, longitude now *east* of Greenwich W.L. July 2000
;       Removed tzone since ignored when julian dates are supplied
;       Renamed to ZENPOS2, added keyword degree,  added keywords lat and lng, and removed COMMON block I. Sullivan April 2012
;       Added J2000 keyword isullivan 2012
;-

 if N_params() EQ 0 then begin
     print,'Syntax - zenpos, date, ra, dec'
     print,'         date = Julian Date, Ouput Ra and Dec in radians'
     return
 endif

 if N_elements(lat) eq 0 then read, $
       'Enter latitude and longitude (in degrees): ',lat,lng
;
;                            Define the needed conversion factors.
;
 d2rad = !PI / 180.0
 h2rad = !PI / 12.0
;
;                            Get the sideral time corresponding to the 
;                            supplied date.
;
 ct2lst, lst, lng, 0, date
;
;                            Compute the RA and Dec.
;
 ra = lst * h2rad
 dec = ra*0. + lat * d2rad
 
;
IF Keyword_Set(J2000) THEN BEGIN
    caldat,date,month,day,year
    epoch=year+ymd2dn(year,month,day)/365.25
    Precess,ra,dec,epoch,2000.,/radian
ENDIF

 IF Keyword_Set(degree) THEN BEGIN
    ra*=!Radeg
    dec*=!Radeg
 ENDIF
 RETURN
 END
