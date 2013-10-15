PRO edge_match,image,mask

IF N_Elements(mask) EQ 0 THEN BEGIN
    mask=Fltarr(size(image,/dimension))
    mask[where(image)]=1
ENDIF
IF Total(abs(mask)) EQ 0 THEN RETURN

dist_test=morph_distance(mask,neighbor=2)
pix_fit=where((dist_test GT 0) AND (dist_test LE 3),n_fit)
IF n_fit GT 0 THEN meanval=Mean(image[pix_fit]) ELSE meanval=0.

image[where(mask)]-=meanval

END