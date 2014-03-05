FUNCTION angle_difference,theta1,phi1,theta2,phi2,degree=degree,nearest=nearest
;For ra_dec coordinates, theta=dec, phi=ra
; Input must be in radians. Output angle is also in radians (unless /degree is set, and then all are in degrees)
;set nearest to compute the smallest angle between the two vectors, otherwise the implied angle will be computed
IF Keyword_Set(degree) THEN conv=!DtoR ELSE conv=1.
x1=Cos(phi1*conv)*Cos(theta1*conv) & y1=Sin(phi1*conv)*Cos(theta1*conv) & z1=Sin(theta1*conv)
x2=Cos(phi2*conv)*Cos(theta2*conv) & y2=Sin(phi2*conv)*Cos(theta2*conv) & z2=Sin(theta2*conv)

Relative_angle=2*ASin((((x1-x2)^2.+(y1-y2)^2.+(z1-z2)^2.)^.5)/2)
IF Keyword_Set(nearest) THEN Relative_angle=Relative_angle<(2.*!Pi-Relative_angle)
RETURN,Relative_Angle/conv
END