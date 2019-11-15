FUNCTION rotate_jones_matrix, obs, Jones, in_place=in_place
;Rotate the 2x2 Jones matix from Az-ZA coordinates to RA-Dec, using the parallactic angle

;Generate a grid of RA and Dec coordinates matching the image
xvals=meshgrid(obs.dimension,obs.elements,1)
yvals=meshgrid(obs.dimension,obs.elements,2)
apply_astrometry, obs, ra_arr=ra_arr, dec_arr=dec_arr, x_arr=xvals, y_arr=yvals, /xy2ad
; Pixels beyond the horizon will have NAN RA and Dec values. Skip them.
inds_use=where(Finite(ra_arr),n_pix)
ra_use=ra_arr[inds_use]
dec_use=dec_arr[inds_use]

hour_angle=obs.zenra - ra_use
h_neg = where(hour_angle LT 0, N_neg)
IF N_neg GT 0 THEN hour_angle[h_neg] = hour_angle[h_neg] + 360.
hour_angle = hour_angle mod 360.

; Calculate the parallactic angle.
; Parallactic angle is the angle between the -ZA axis and the +Dec axis.
; A rotation angle of 0 degrees is defined with
; North along the -ZA axis and East along the -Az axis.
; A rotation angle of 90 degrees is defined with
; North along the +Az axis and East along the -ZA axis.
; See parallactic_angle_memo.pdf for a derivation of the parallactic angle
par_ang = parallactic_angle(latitude=obs.zendec,hour_angle=hour_angle,dec=dec_use)

; Now define the elements of the rotation matrix
;       | ZA |   |sin(par_ang)   -cos(par_ang)|     | ZA |   | RA |
; R (x) |    | = |                            | (x) |    | = |    |
;       | Az |   |-cos(par_ang)  -sin(par_ang)|     | Az |   | Dec|
R00 = Sin(par_ang*!DtoR)
R10 = -Cos(par_ang*!DtoR)
R01 = -Cos(par_ang*!DtoR)
R11 = -Sin(par_ang*!DtoR)
IF Keyword_Set(in_place) THEN Jones_rot = Jones ELSE Jones_rot = Pointer_copy(Jones)

FOR pix=0L,n_pix-1 DO BEGIN

    J00 = (*Jones[0, 0])[inds_use[pix]]
    J10 = (*Jones[1, 0])[inds_use[pix]]
    J01 = (*Jones[0, 1])[inds_use[pix]]
    J11 = (*Jones[1, 1])[inds_use[pix]]
    J_matrix = [[J00, J10], [J01, J11]]
    R_matrix = [[R00[pix], R10[pix]], [R01[pix], R11[pix]]]

    ; ; Perform the matrix multiplication
    ; ; |R00  R10|       |J00  J10|     |R00 J00 + R10 J01  R00 J10 + R10 J11|
    ; ; |        |  (x)  |        |  =  |                                    |
    ; ; |R01  R11|       |J01  J11|     |R01 J00 + R11 J01  R01 J10 + R11 J11|
    ; J00r = R00[pix]*J00 + R10[pix]*J01
    ; J10r = R00[pix]*J10 + R10[pix]*J11
    ; J01r = R01[pix]*J00 + R11[pix]*J01
    ; J11r = R01[pix]*J10 + R11[pix]*J11
    Jr_matrix = matrix_multiply(R_matrix, J_matrix)

    ;Insert the rotated elements into the new Jones matrix for this pixel
    (*Jones_rot[0, 0])[inds_use[pix]] = Jr_matrix[0, 0]
    (*Jones_rot[1, 0])[inds_use[pix]] = Jr_matrix[1, 0]
    (*Jones_rot[0, 1])[inds_use[pix]] = Jr_matrix[0, 1]
    (*Jones_rot[1, 1])[inds_use[pix]] = Jr_matrix[1, 1]
ENDFOR
RETURN, Jones_rot
end
