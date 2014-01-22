PRO image_align,image1,image2,dx,dy,theta,scale,source_array1_base,source_array2_base,source_array1_out,source_array2_out,$
    low_threshold=low_threshold,high_threshold=high_threshold,max_number=max_number,radius=radius,sub_max_number=sub_max_number,$
    final_match=final_match,binsize=binsize,error=error,min_radius=min_radius,fix_scale=fix_scale,fix_theta=fix_theta
;attempts to align two images (now from their source_arrays, to allow for better accuracy in position)
;the images can have different overall scaling, but it will work better if they are at least close
;each must contain only single pixel stars
;computation time scales quickly with the number of stars in each image! N^3, I think
;Set max_number to restrict the total number of sources to consider. Excess sources are discarded from the faint (low) end
;Set radius to restrict star triads to within a fixed radius of the central star  -> reduces time to N* M^2
;includes sources with brightness between low_threshold and high_threshold
;final_match sets the number of points to use from both images (that have the highest confidence in matching) to use to determine the alignment
heap_gc ;clean up from any previous run that was aborted

;columns of source_array are: 0:x, 1:y, 2:RA, 3:Dec, 4:flux, 5:pixel index, 6:use may vary
;IF N_Elements(radius) EQ 0 THEN radius=256.
IF N_Elements(final_match) EQ 0 THEN final_match=16.
IF N_Elements(binsize) EQ 0 THEN binsize=1.0
IF N_Elements(min_radius) EQ 0 THEN min_radius=Round(!RaDeg/binsize) ;sources that are too close together have very large pixelization error!
IF N_Elements(radius) EQ 0 THEN radius=min_radius*4.
source_array1=source_array1_base
source_array2=source_array2_base

;IF N_Elements(max_number) EQ 0 THEN  max_number=128.
absolute_max_number=4096.
IF N_Elements(sub_max_number) EQ 0 THEN sub_max_number=20. ;with too many sources in each subset, duplicate angles become more common!
;match_threshold=Round(Sqrt(sub_max_number))

;IF N_Elements(low_threshold) EQ 0 THEN low_threshold=(Min(image1)<Min(image2))>0
;IF N_Elements(high_threshold) EQ 0 THEN high_threshold=Max(image1)>Max(image2)
;
;indices1=where((image1 GT low_threshold) AND (image1 LT high_threshold),n1)
;indices2=where((image2 GT low_threshold) AND (image2 LT high_threshold),n2)
indices1=Reform(source_array1[5,*])
indices2=Reform(source_array2[5,*])
flux1=Reform(source_array1[4,*])
flux2=Reform(source_array2[4,*])
xvals_base1=Reform(source_array1[0,*])
yvals_base1=Reform(source_array1[1,*])
xvals_base2=Reform(source_array2[0,*])
yvals_base2=Reform(source_array2[1,*])


dimension1=(size(image1,/dimension))[0]
elements1=(size(image1,/dimension))[1]
dimension2=(size(image2,/dimension))[0]
elements2=(size(image2,/dimension))[1]
Npix1=dimension1*elements1
Npix2=dimension2*elements2


max_number1=Round(Sqrt(2)*Npix1*sub_max_number/(!Pi*radius^2.-!Pi*min_radius^2.))<absolute_max_number
max_number2=Round(Sqrt(2)*Npix2*sub_max_number/(!Pi*radius^2.-!Pi*min_radius^2.))<absolute_max_number

i_use=where(indices1 GE 0,n1)
IF n1 GT max_number1 THEN BEGIN
    i_use=i_use[((Reverse(Sort(flux1[i_use])))[0:max_number1-1])]
    n1=max_number1
ENDIF
indices1=indices1[i_use]
flux1=flux1[i_use]
xvals_base1=xvals_base1[i_use]
yvals_base1=yvals_base1[i_use]

i_use=where(indices2 GE 0,n2)
IF n2 GT max_number2 THEN BEGIN
    i_use=i_use[((Reverse(Sort(flux2[i_use])))[0:max_number2-1])]
    n2=max_number2
ENDIF
indices2=indices2[i_use]
flux2=flux2[i_use]
xvals_base2=xvals_base2[i_use]
yvals_base2=yvals_base2[i_use]

angle_array1=Ptrarr(n1,/allocate)
angle_array2=Ptrarr(n2,/allocate)
direction_array1=Ptrarr(n1,/allocate)
direction_array2=Ptrarr(n2,/allocate)
Perimeter_array1=Ptrarr(n1,/allocate)
Perimeter_array2=Ptrarr(n2,/allocate)
match_array=Lonarr(n1,n2)
skip_array=Intarr(n1,n2)

kernel=intarr(2*radius,2*radius)
circle_template=(dist(2*radius+1,2*radius+1))
circle_template=Shift(circle_template,radius,radius)
circle_template=circle_template[1:*,1:*]
kernel[where((circle_template LE radius) AND (circle_template GE min_radius))]=1
kernel[radius,radius]=0 ;don't include the center, no matter what
xvals_kernel=(where(kernel) mod (2*radius))-radius
yvals_kernel=Floor(where(kernel)/(2*radius))-radius

t1=Systime(1)
FOR im=0,1 DO BEGIN ;use a loop to make sure the analysis is the same for both images
    IF im EQ 0 THEN BEGIN
        image=image1
        indices=indices1
        flux=flux1
        xvals_base=xvals_base1
        yvals_base=yvals_base1
    ENDIF
    IF im EQ 1 THEN BEGIN
        image=image2
        indices=indices2
        flux=flux2
        xvals_base=xvals_base2
        yvals_base=yvals_base2
    ENDIF
    dimension=(size(image,/dimension))[0]
    elements=(size(image,/dimension))[1]

    N_sources=N_Elements(indices)
    x_max=Max(xvals_base)
    y_max=Max(yvals_base)
    
    FOR i=0,N_sources-1 DO BEGIN
        x_center=Float(xvals_base[i])
        y_center=Float(yvals_base[i])
        yvals_kernel_use=yvals_kernel+Round(y_center)
        xvals_kernel_use=xvals_kernel+Round(x_center)
        i_use=where((yvals_kernel_use GE 0) AND (yvals_kernel_use LE y_max) AND (xvals_kernel_use GE 0) AND (xvals_kernel_use LE x_max),n_inds)
        IF n_inds LT 3 THEN BEGIN
            IF im EQ 0 THEN skip_array[i,*]=1
            IF im EQ 1 THEN skip_array[*,i]=1
            CONTINUE
        ENDIF   
        sub_indices0=xvals_kernel_use[i_use]+yvals_kernel_use[i_use]*dimension   
;        sub_indices0=((yvals_kernel+Round(y_center))*dimension))+((xvals_kernel+Round(x_center))) ;express the kernel in 1D indices of the full image centered at (x_center,y_center)
        sub_indices=where((histogram(sub_indices0,omin=om,/binsize) GT 0) AND (histogram(indices,min=om,/binsize,reverse_i=ri) GT 0),n_inds)+om ;determine which values of indices fall within the kernel
        IF n_inds LT 3 THEN BEGIN
            IF im EQ 0 THEN skip_array[i,*]=1
            IF im EQ 1 THEN skip_array[*,i]=1
            CONTINUE
        ENDIF
        
        i_use=ri[ri[sub_indices-om]]
        IF n_inds GT sub_max_number THEN BEGIN
            i_use=i_use[((Reverse(Sort(flux[i_use])))[0:sub_max_number-1])]
;            sub_indices=(Reverse(sub_indices[Sort(image[sub_indices])]))[0:sub_max_number-1]
            n_inds=sub_max_number
        ENDIF
        xvals=xvals_base[i_use]
        yvals=yvals_base[i_use]
;        xvals=Float(sub_indices mod dimension)
;        yvals=Float(Floor(sub_indices/dimension))
        angles=triangle_transform(x_center,y_center,xvals,yvals,/degree,binsize=binsize,direction=direction,perimeter=perimeter) ;returns a triplet of angles (the whole triangle)
        IF im EQ 0 THEN IF N_Elements(angles) LE 3 THEN BEGIN  skip_array[i,*]=1 & CONTINUE & ENDIF $
            ELSE BEGIN *angle_array1[i]=angles & *direction_array1[i]=direction & *perimeter_array1[i]=perimeter & ENDELSE
        IF im EQ 1 THEN IF N_Elements(angles) LE 3 THEN BEGIN  skip_array[*,i]=1 & CONTINUE & ENDIF $
            ELSE BEGIN *angle_array2[i]=angles & *direction_array2[i]=direction & *perimeter_array2[i]=perimeter & ENDELSE
;        angles=triangle_transform(x_center,y_center,xvals,yvals,/degree,binsize=binsize) ;returns a triplet of angles (the whole triangle)
;        IF im EQ 0 THEN IF N_Elements(angles) LE 3 THEN BEGIN  skip_array[i,*]=1 & CONTINUE & ENDIF ELSE  *angle_array1[i]=angles
;        IF im EQ 1 THEN IF N_Elements(angles) LE 3 THEN BEGIN  skip_array[*,i]=1 & CONTINUE & ENDIF ELSE *angle_array2[i]=angles

    ENDFOR
ENDFOR
t1=Systime(1)-t1

;Match_array is a little complicated:
; its dimension corresponds to the number of sources used from image1
; its elements corresponds to the number of sources used from image2
; a single pixel Match_array[i,j] contains the number of triangle angles that EXACTLY match between source[i] in image1 and source[j] in image2
;original indices for e.g. image1 would be image1[indices[i]]
t2=Systime(1)
;h1_array=Ptrarr(n1,/allocate)
;h2_array=Ptrarr(n2,/allocate)
max_angle=180.
max_angle_bin=Round(max_angle/binsize)
nbins=max_angle_bin+1
nbins2=max_angle_bin^2.+1
h1_array=intarr(n1,nbins2)
;dir1_array=fltarr(n1,nbins2)
;per1_array=fltarr(n1,nbins2)

h2_array=intarr(n2,nbins2)
;dir2_array=fltarr(n2,nbins2)
;per2_array=fltarr(n2,nbins2)

FOR i=0,n1-1 DO BEGIN
    ;needed for the slow but more accurate way:
   ;compute these here, because every bit of extra speed helps
   ;very little (though some!) extra information is in the third angle
   ;the loop will run ~ 100 times faster without it, so leave it out for now
   IF Min(skip_array[i,*]) EQ 1 THEN CONTINUE
    angles1=*angle_array1[i]
    direction1=*direction_array1[i]
    perimeter1=*perimeter_array1[i]
    angles1a=Round(angles1[0,*]/binsize)
    angles1b=Round(angles1[1,*]/binsize)
    angles1c=Round(angles1[2,*]/binsize)
;    angles1d=angles1a+max_angle_bin*(angles1b+max_angle_bin*angles1c)
;    angles1d=angles1a+max_angle_bin*angles1b
    angles1d=angles1a+max_angle_bin*abs(angles1c-angles1b)
    ;it is possible (but rare) to have multple identical triangles.
    ;This would mess up all matches after that point, so instead throw out any duplicates.
    ;That one triagle will be badly matched, but at least it won't affect the others
    order=Uniq(angles1d,Sort(angles1d))
    angles1d=angles1d[order] ;will be in monotonic order
    direction1=direction1[order]
    perimeter1=perimeter1[order]

    h1d=histogram(angles1d,min=0,max=nbins2-1,binsize=1);,rever=r_i)
    h1_array[i,*]=h1d
    inds_use=where(h1d)
;    dir1_array[i,inds_use]=direction1;[r_i[r_i[0]:r_i[nbins2]-1]] reverse indices are not needed, because everything has already been sorted
;    per1_array[i,inds_use]=perimeter1
ENDFOR
;Ptr_free,angle_array1
FOR j=0,n2-1 DO BEGIN
    IF min(skip_array[*,j]) EQ 1 THEN CONTINUE
    angles2=*angle_array2[j]
    direction2=*direction_array2[j]
    perimeter2=*perimeter_array2[j]
    angles2a=Round(angles2[0,*]/binsize)
    angles2b=Round(angles2[1,*]/binsize)
    angles2c=Round(angles2[2,*]/binsize)
;    angles2d=angles1a+max_angle_bin*(angles2b+max_angle_bin*angles2c)
;    angles2d=angles2a+max_angle_bin*angles2b
    angles2d=angles2a+max_angle_bin*abs(angles2c-angles2b)
    order=Uniq(angles2d,Sort(angles2d))
    angles2d=angles2d[order] ;will be in monotonic order
    direction2=direction2[order]
    perimeter2=perimeter2[order]

    h2d=histogram(angles2d,min=0,max=nbins2-1,binsize=1);,rever=r_i)
    h2_array[j,*]=h2d
    inds_use=where(h2d)
;    dir2_array[j,inds_use]=direction2;[r_i[r_i[0]:r_i[nbins2]-1]] reverse indices are not needed, because everything has already been sorted
;    per2_array[j,inds_use]=perimeter2
ENDFOR
;Ptr_free,angle_array2
t2=Systime(1)-t2
t3=Systime(1)

;IDENTICAL results to nested loop below, but much faster
;also identical results to the single line matrix multiplication below
;direction_match=fltarr(1)
;scale_match=fltarr(1)
;FOR i=0,nbins2-1 DO BEGIN
;    ind1=where(h1_array[*,i],nind1)
;    IF nind1 EQ 0 THEN CONTINUE
;    ind2=where(h2_array[*,i],nind2)
;    IF nind2 EQ 0 THEN CONTINUE
;    direction_single=fltarr(nind1,nind2)
;    scale_single=fltarr(nind1,nind2)
;    FOR j=0,nind1-1 DO BEGIN
;;      match_array[ind1[j],ind2]+=1
;;   direction_single[j,*]=(dir2_array[ind2,i]-dir1_array[ind1[j],i]+360.) mod 360.
;     direction_single=(dir2_array[ind2,i]-dir1_array[ind1[j],i]+360.) mod 360.
;     direction_match=[direction_match, direction_single]
;;   scale_single[j,*]=per2_array[ind2,i]/per1_array[ind1[j],i]
;     scale_single=per2_array[ind2,i]/per1_array[ind1[j],i]
;     scale_match=[scale_match,scale_single]
;   ENDFOR
;ENDFOR
;direction_match=direction_match[1:*]
;scale_match=scale_match[1:*]
;
;threshold=0.2
;
;dir_hist=histogram(direction_match,binsize=binsize,min=0,max=360.,location=dir_vals)
;dir_hist=dir_hist[0:max_angle_bin*2-1]
;dir_hist-=median(dir_hist,5)
;dir_peak=where(dir_hist GT 3.*Stddev(dir_hist),n_peak)
;
;scale_binsize=(8.)/nbins ;maximum scale to consider is 8.
;scale_hist=histogram(scale_match,min=0,max=8.,binsize=scale_binsize,location=scale_vals)
;
;;dir_hist=simple_histogram(direction_match,center=direction_est,low=0.,high=360.,/positive)
;;scale_hist=simple_histogram(scale_match,center=scale_est,low=0.125,high=8.,/positive)
;
;;repeat the above loop, this time only including those matches that also have consistent direction and scale
;;match_array=Lonarr(n1,n2)
;scale_low=scale_est*(1.-threshold)
;scale_high=scale_est*(1.+threshold)
;dir_low=direction_est*(1.-threshold)
;dir_high=direction_est*(1.+threshold)
;FOR i=0,nbins2-1 DO BEGIN
;    ind1=where(h1_array[*,i],nind1)
;    IF nind1 EQ 0 THEN CONTINUE
;    ind2=where(h2_array[*,i],nind2)
;    IF nind2 EQ 0 THEN CONTINUE
;    direction_single=fltarr(nind1,nind2)
;    scale_single=fltarr(nind1,nind2)
;    FOR j=0,nind1-1 DO BEGIN
;     direction_single=(dir2_array[ind2,i]-dir1_array[ind1[j],i]+360.) mod 360.
;     scale_single=per2_array[ind2,i]/per1_array[ind1[j],i]
;     i2=where((scale_single GE scale_low) AND (scale_single LE scale_high) AND (direction_single GE dir_low) AND (direction_single LE dir_high),n_i2)
;     IF n_i2 EQ 0 THEN CONTINUE
;       match_array[ind1[j],ind2[i2]]+=1
;   ENDFOR
;ENDFOR
;;match_array2=intarr(n1,n2)
;;FOR i=0,n1-1 DO BEGIN
;;    FOR j=0,n2-1 DO BEGIN
;;        match_array2[i,j]=Total(Reform(h1_array[i,*])*Reform(h2_array[j,*]))
;;    ENDFOR
;;ENDFOR
match_array=h1_array#Transpose(h2_array)
match_array2=match_array
FOR i=0,n1-1 DO match_array2[i,*]-=median(match_array[i,*])
FOR j=0,n2-1 DO match_array2[*,j]-=median(match_array[*,j])

t3=systime(1)-t3

final_match_i=where(match_array2 GT 0,n_matches)
IF n_matches LE 2 THEN BEGIN
    error=1
    RETURN
ENDIF

IF n_matches LE final_match THEN final_match=n_matches $
    ELSE final_match_i=(final_match_i[Reverse(Sort(match_array2[final_match_i]))])[0:final_match-1]

;match_test=intarr(n1,n2)

;IF n2 GT n1 THEN BEGIN
;    FOR i=0,n1-1 DO BEGIN
;        best_match=Max(match_array[i,*],max_i)
;        match_test[i,max_i]=best_match
;    ENDFOR
;ENDIF ELSE BEGIN
;    FOR j=0,n2-1 DO BEGIN
;        best_match=Max(match_array[*,j],max_j)
;        match_test[max_j,j]=best_match
;    ENDFOR
;ENDELSE

;final_match_i=(Reverse(Sort(match_test)))[0:final_match-1]
final_image1_ref=final_match_i mod n1
final_image1_indices=indices1[final_image1_ref]
final_image2_ref=Floor(final_match_i/n1)
final_image2_indices=indices2[final_image2_ref]

x1_vals=xvals_base1[final_image1_ref]
y1_vals=yvals_base1[final_image1_ref]
;x1_vals=final_image1_indices mod dimension1
;y1_vals=Floor(final_image1_indices/dimension1)

x2_vals=xvals_base2[final_image2_ref]
y2_vals=yvals_base2[final_image2_ref]
;x2_vals=final_image2_indices mod dimension2
;y2_vals=Floor(final_image2_indices/dimension2)

test1=fltarr(dimension1,elements1)
test2=fltarr(dimension2,elements2)
test1[Round(x1_vals),Round(y1_vals)]+=1
test2[Round(x2_vals),Round(y2_vals)]+=1
test1=smooth(test1,20,/edge_truncate)
test2=smooth(test2,20,/edge_truncate)

;calculate new triangles from the 'matched' points. These will be used to calculate relative rotation and scale!
;theta_array=fltarr(final_match)
theta_error=fltarr(final_match)
;scale_array=fltarr(final_match)
scale_error=fltarr(final_match)

t4=Systime(1)
    ;the approach using 'd' angles below is the most straightforward, but it quickly generates ENORMOUS arrays, which take a LOT of memory and time
    ;it works, though. So, for now restrict binsize to something that works
IF binsize LT 1.0 THEN binsize=1.0
max_angle=180
max_angle_bin=Round(max_angle/binsize)
nbins=max_angle_bin+1
nbins2=max_angle_bin^2+1
FOR i=0,final_match-1 DO BEGIN

    direction1=(direction2=1.)
    perimeter1=(perimeter2=1.)
    ri1=(ri2=1)
    other_i=where(indgen(final_match) NE i)
    angles1=triangle_transform(x1_vals[i],y1_vals[i],x1_vals[other_i],y1_vals[other_i],direction=direction1,perimeter=perimeter1,/degree,binsize=binsize)  ;returns a triplet of angles (the whole triangle)
    angles2=triangle_transform(x2_vals[i],y2_vals[i],x2_vals[other_i],y2_vals[other_i],direction=direction2,perimeter=perimeter2,/degree,binsize=binsize)  ;returns a triplet of angles (the whole triangle)

    angles1a=Round(angles1[0,*]/binsize)
    angles1b=Round(angles1[1,*]/binsize)
    angles1c=Round(angles1[2,*]/binsize)
;    h1a=histogram(angles1a,omin=om,binsize=1,rever=ri1a)
;    h1b=histogram(angles1b,min=om,binsize=1,rever=ri1b)
;    h1c=histogram(angles1c,min=om,binsize=1,rever=ri1c)
;    angles1d=angles1a+max_angle_bin*(angles1b+max_angle_bin*angles1c)
    angles1d=angles1a+max_angle_bin*abs(angles1c-angles1b)

    angles2a=Round(angles2[0,*]/binsize)
    angles2b=Round(angles2[1,*]/binsize)
    angles2c=Round(angles2[2,*]/binsize)
;    angles2d=angles2a+max_angle_bin*(angles2b+max_angle_bin*angles2c)
    angles2d=angles2a+max_angle_bin*abs(angles2c-angles2b)

    h1d=histogram(angles1d,/binsize,min=0,max=nbins2-1,rever=ri1)
    h2d=histogram(angles2d,/binsize,min=0,max=nbins2-1,rever=ri2)
    matches=where(h1d*h2d,n_matches)
    ;there is always a triangle with angle 0 at the head of each list, which is reported as a match. Discard it
    IF n_matches LE 1 THEN BEGIN
;        theta_array[i]=!Values.F_NAN
        theta_error[i]=!Values.F_NAN
;        scale_array[i]=!Values.F_NAN
        scale_error[i]=!Values.F_NAN
        CONTINUE
    ENDIF
    match_i1=ri1[ri1[matches]]
    match_i2=ri2[ri2[matches]]

;    h1d=histogram(angles1d,omin=om,binsize=1,rever=ri1)
;    h2d=histogram(angles2d,min=om,binsize=1,rever=ri2)
;    matches=where((h1d GT 0) AND (h2d GT 0),n_matches)+om
;    match_i1=ri1[ri1[matches]]
;    match_i2=ri2[ri2[matches]]
    IF matches[0] EQ 0 THEN BEGIN
        match_i1=match_i1[1:*]
        match_i2=match_i2[1:*]
        n_matches-=1
    ENDIF
;    matches*=binsize
;
;    test_theta1=direction1[match_i1]*!DtoR
;    test_theta2=direction2[match_i2]*!DtoR
;    theta_match_inv=(Cos(test_theta1)*Cos(test_theta2)+Sin(test_theta1)*Sin(test_theta2))<1 ;if the angles are matched EXACTLY, it is possible to get a number slightly greater than 1 due to floating-point precision
;    theta_match=ACos(theta_match_inv)*180./!Pi
;    theta_match=direction1[match_i1]-direction2[match_i2]
    theta_match=(direction2[match_i2]-direction1[match_i1]+360.) mod 360. ;convert to purely positive angles
    quadrant_test=Floor(theta_match/90)
    break_angle=((median(quadrant_test)+2) mod 4)*90
    IF break_angle GT 0 THEN BEGIN
        over_i=where(theta_match GT break_angle,n_over)
        IF n_over GT 0 THEN theta_match[over_i]-=360.
    ENDIF
;    theta_array[i]=Median(theta_match,/even)
;    scale_match=perimeter1[match_i1]/perimeter2[match_i2]
    scale_match=perimeter2[match_i2]/perimeter1[match_i1]
;    scale_array[i]=Median(scale_match,/even)
    IF N_matches LE 2 THEN BEGIN
        theta_error[i]=!Values.F_NAN
        scale_error[i]=!Values.F_NAN
        CONTINUE
    ENDIF ELSE BEGIN
        theta_error[i]=Stddev(theta_match)
        scale_error[i]=Stddev(scale_match)
    ENDELSE
    good_theta_i=where(ABS(theta_match-Median(theta_match)) LE 3.*theta_error[i])
    good_scale_i=where(ABS(scale_match-Median(scale_match)) LE 3.*scale_error[i])
    good_i=where((histogram(good_theta_i,omin=om,binsize=1) GT 0) AND (histogram(good_scale_i,min=om,binsize=1) GT 0),n_good) +om
    IF n_good EQ 0 THEN CONTINUE
    theta_match=theta_match[good_i]
    scale_match=scale_match[good_i]
    IF N_Elements(theta_array) EQ 0 THEN theta_array=theta_match ELSE theta_array=[theta_array,theta_match]
    IF N_Elements(scale_array) EQ 0 THEN scale_array=scale_match ELSE scale_array=[scale_array,scale_match]

ENDFOR
t4=Systime(1)-t4
valid_i=where(Finite(theta_error),n_valid)
;theta_use=theta_array[valid_i]
;theta_error_use=theta_error[valid_i]
;scale_use=scale_array[valid_i]
;scale_error_use=scale_error[valid_i]
;theta=Mean(theta_use/theta_error_use)*total(1./theta_error_use)
;scale=Mean(scale_use/scale_error_use)*total(1./scale_error_use)
;
;theta=Median(theta_array[valid_i],/even)
;Scale=Median(Scale_array[valid_i],/even)
IF N_Elements(theta_array) EQ 0 THEN BEGIN
    error=1
    RETURN
ENDIF
theta_array=(theta_array+360.) mod 360. ;convert to purely positive angles
quadrant_test=Floor(theta_array/90)
break_angle=((median(quadrant_test)+2) mod 4)*90
IF break_angle GT 0 THEN BEGIN
    over_i=where(theta_array GT break_angle,n_over)
    IF n_over GT 0 THEN theta_array[over_i]-=360.
ENDIF

n_cut=3
nsigma=3.
FOR cut_i=0,n_cut-1 DO BEGIN
    theta=Median(theta_array,/even)
    sigma_theta=Stddev(theta_array)
    good_ti=where(Abs(theta_array-theta) LE nsigma*sigma_theta,n_ti)
    scale=Median(scale_array,/even)
    sigma_scale=Stddev(scale_array)
    good_si=where(Abs(scale_array-scale) LE nsigma*sigma_scale,n_si)
    good_i=where((histogram(good_ti,min=0,binsize=1) GT 0) AND (histogram(good_si,min=0,binsize=1) GT 0),n_good)
    IF n_good LT 3 THEN CONTINUE
    theta_array=theta_array[good_i]
    scale_array=scale_array[good_i]

    theta=Median(theta_array,/even)
    scale=Median(scale_array,/even)
ENDFOR
;theta_hist=simple_histogram(theta_array,center=theta)
;scale_hist=simple_histogram(scale_array,center=scale)
;theta=Median(theta_array,/even)
;scale=Median(scale_array,/even)

IF Keyword_Set(fix_scale) THEN Scale=fix_scale
IF N_Elements(fix_theta) EQ 1 THEN Theta=fix_theta

x1a_vals=Scale*(x1_vals[valid_i]*Cos(theta*!DtoR)-y1_vals[valid_i]*Sin(theta*!DtoR))
y1a_vals=Scale*(y1_vals[valid_i]*Cos(theta*!DtoR)+x1_vals[valid_i]*Sin(theta*!DtoR))

;Now x2=dx+x1a and y2=dy+y1a

dx_array=x2_vals[valid_i]-x1a_vals
dy_array=y2_vals[valid_i]-y1a_vals
dx=Median(dx_array,/even)
dy=Median(dy_array,/even)

IF n_valid GE 2 THEN BEGIN
    sigma_dx=stddev(dx_array)<10.
    sigma_dy=stddev(dy_array)<10.

    valid_i_i=where((Abs(dx_array-dx) LE 3.*sigma_dx) AND(Abs(dy_array-dy) LE 3.*sigma_dy),n_valid)
    IF n_valid EQ 0 THEN BEGIN error=2 & n_valid=N_Elements(dx_array) & valid_i_i=indgen(n_valid) & ENDIF
    valid_i2=valid_i[valid_i_i]
    dx=Median(dx_array[valid_i_i],/even)
    dy=Median(dy_array[valid_i_i],/even)
ENDIF ELSE BEGIN
    valid_i_i=0
    valid_i2=0
    error=2
    dx=dx_array[0]
    dy=dy_array[0]
    n_valid=1
ENDELSE
;;x,y,theta,scale,flux
source_array1_out=(source_array2_out=fltarr(7,n_valid))
source_array1_out[0,*]=x1_vals[valid_i2]
source_array1_out[1,*]=y1_vals[valid_i2]
source_array1_out[2,*]=theta_array[valid_i2]
source_array1_out[3,*]=scale_array[valid_i2]
source_array1_out[4,*]=image1[final_image1_indices[valid_i2]]
source_array1_out[5,*]=final_image1_indices[valid_i2]
source_array1_out[6,*]=final_image2_indices[valid_i2]

source_array2_out[0,*]=x2_vals[valid_i2]
source_array2_out[1,*]=y2_vals[valid_i2]
source_array2_out[2,*]=theta_array[valid_i2]
source_array2_out[3,*]=scale_array[valid_i2]
source_array2_out[4,*]=image2[final_image2_indices[valid_i2]]
source_array2_out[5,*]=final_image2_indices[valid_i2]
source_array2_out[6,*]=final_image1_indices[valid_i2]
END
