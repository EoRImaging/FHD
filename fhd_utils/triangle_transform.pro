FUNCTION triangle_transform,x_center,y_center,xvals,yvals,fvals,max_number=max_number,degree=degree,direction=direction,perimeter=perimeter,binsize=binsize,acute=acute
;needs to be modified to return TRIPLETS of angles -> for the full triangle
N=N_Elements(xvals)
IF N_Elements(binsize) EQ 0 THEN binsize_use=1 ELSE binsize_use=binsize
IF not Keyword_Set(degree) THEN binsize_use*=!DtoR
IF Keyword_Set(max_number) THEN BEGIN
    IF N GT max_number THEN BEGIN
        keep_i=(Reverse(Sort(fvals)))[0:max_number-1]
        xvals=xvals[keep_i]
        yvals=yvals[keep_i]
        fvals=fvals[keep_i]
    ENDIF
ENDIF

;base_angle=Atan((yvals-y_center)/(xvals-x_center))
;base_angle=Atan((xvals-x_center),(yvals-y_center))
angles=fltarr(3,N,N)
perimeter=fltarr(N,N)
direction=fltarr(N,N)

neg_x_test=fltarr(N)
neg_x_i=where(x_center GT xvals,n_neg_x)
neg_y_test=fltarr(N)
neg_y_i=where(y_center GT yvals,n_neg_y)
IF n_neg_x GT 0 THEN neg_x_test[neg_x_i]=1.
IF n_neg_y GT 0 THEN neg_y_test[neg_y_i]=1.
angle_direction=Atan(Abs((y_center-yvals)/(x_center-xvals)))
angle_direction=Abs(!Pi*neg_x_test-angle_direction)
angle_direction=Abs(2*!Pi*neg_y_test-angle_direction)
FOR i=0,N-1 DO BEGIN
    FOR j=0,N-1 DO BEGIN
        IF i LE j THEN CONTINUE
        l1=SQRT((x_center-xvals[i])^2.+(y_center-yvals[i])^2.)
        l2=SQRT((x_center-xvals[j])^2.+(y_center-yvals[j])^2.)
        lx=SQRT((xvals[j]-xvals[i])^2.+(yvals[j]-yvals[i])^2.)
        IF Keyword_Set(Acute) THEN IF lx LT l1<l2 THEN CONTINUE ;don't include highly acute triangles
        perimeter[i,j]=l1+l2+lx

        ;compute the angle from the dot product of the two vectors
        angles[0,i,j]=ACos(((xvals[j]-x_center)*(xvals[i]-x_center)+(yvals[j]-y_center)*(yvals[i]-y_center))/(l1*l2))
        angles[1,i,j]=ACos(((xvals[i]-x_center)*(xvals[i]-xvals[j])+(yvals[i]-y_center)*(yvals[i]-yvals[j]))/(l1*lx))
        angles[2,i,j]=ACos(((xvals[j]-x_center)*(xvals[j]-xvals[i])+(yvals[j]-y_center)*(yvals[j]-yvals[i]))/(lx*l2))

;        x_av=((xvals[i]+xvals[j])/2.)-x_center
;        y_av=((yvals[i]+yvals[j])/2.)-y_center
;       ;direction is tricky because it needs to have the range 0 to 2 Pi
;        direction[i,j]=ACos(x_av/SQRT(x_av^2.+y_av^2.))

;        direction[i,j]=(Atan((y_center-yvals[i])/(x_center-xvals[i]))+Atan((y_center-yvals[j])/(x_center-xvals[j])))/2.
        x_av=((xvals[i]-x_center)/l1+(xvals[j]-x_center)/l2)/2.
        y_av=((yvals[i]-y_center)/l1+(yvals[j]-y_center)/l2)/2.
        angle_direction=Atan(Abs((y_av)/(x_av)))
        IF nan_test(angle_direction) GT 0 THEN angle_direction[where(Finite(angle_direction,/nan))]=0.
        IF x_av LT 0 THEN angle_direction=Abs(!Pi-angle_direction)
        IF y_av LT 0 THEN angle_direction=Abs(2*!Pi-angle_direction)
        direction[i,j]=angle_direction

;        IF Abs(angle_direction[i]-angle_direction[j]) GT !Pi THEN direction[i,j]=((angle_direction[i]+angle_direction[j]+2.*!Pi)/2.) mod (2.*!Pi) $
;        ELSE direction[i,j]=(angle_direction[i]+angle_direction[j])/2.
    ENDFOR
ENDFOR

;angles=abs(angles) mod 2.*!Pi
;angles=(2*!Pi-angles)<angles ;restrict to 0<angle<!Pi
IF Keyword_Set(degree) THEN angles*=180./!Pi
angles=(Round(angles/binsize_use)*binsize_use) > 0
angles0=angles[0,*,*]
;valid_angles=(Sort(angles0))[Uniq(angles0[Sort(angles0)])]
valid_angles=where(perimeter)
return_angles=fltarr(3,N_Elements(valid_angles))
return_angles[0,*]=(angles[0,*,*])[valid_angles]
return_angles[1,*]=(angles[1,*,*])[valid_angles]
return_angles[2,*]=(angles[2,*,*])[valid_angles]
direction=(Keyword_Set(degree)) ? (direction[valid_angles]*180./!Pi):(direction[valid_angles])
perimeter=perimeter[valid_angles]

RETURN,return_angles
END