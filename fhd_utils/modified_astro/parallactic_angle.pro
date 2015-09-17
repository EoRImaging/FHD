FUNCTION parallactic_angle,latitude=latitude,hour_angle=hour_angle,zenith_angle=zenith_angle

sinP=Cos(latitude*!DtoR)*Sin(hour_angle*!DtoR)/Sin(zenith_angle*!DtoR)
parallactic_angle=Atan(sinP)*!RaDeg ;Asin(sinP) ?
RETURN,parallactic_angle
END