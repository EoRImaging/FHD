FUNCTION polarization_fit,image_arr,beam_arr,threshold=threshold

IF not Keyword_Set(threshold) THEN threshold=1E-2

n_pol=2.
IF N_Elements(beam_arr) LT n_pol THEN RETURN,1.

beam_mask=fltarr(size(*beam_arr[0],/dimension))+1
FOR pol_i=0,n_pol-1 DO BEGIN
    cut_i=where(*beam_arr[pol_i] LT threshold,n_cut,complement=beam_i,ncomp=n_use)
    IF n_use EQ 0 THEN CONTINUE
    beam_mask[beam_i]*=((*beam_arr[pol_i])[beam_i])^2.
    IF n_cut EQ 0 THEN CONTINUE
    beam_mask[cut_i]=0
ENDFOR
beam_mask=Sqrt(beam_mask)

i_use=where(beam_mask,n_use)
IF n_use EQ 0 THEN RETURN,1.

pol0_vals=(*image_arr[0])[i_use]
pol1_vals=(*image_arr[1])[i_use]
weights=beam_mask[i_use]

scale_fit=(linfit(pol0_vals,pol1_vals,measure_error=1./weights))[1]

RETURN,scale_fit
END