FUNCTION parallactic_angle,latitude=latitude,hour_angle=hour_angle,dec=dec

y_term = Sin(hour_angle*!DtoR)
x_term = Cos(dec*!DtoR)*Tan(latitude*!DtoR) - Sin(dec*!DtoR)*Cos(hour_angle*!DtoR)
par_angle=Atan(y_term, x_term)*!RaDeg

RETURN,par_angle
END
