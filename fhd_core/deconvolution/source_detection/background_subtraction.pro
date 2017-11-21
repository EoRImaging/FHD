FUNCTION background_subtraction,image_unfiltered, dimension=dimension, elements=elements,$
    smooth_width=smooth_width, beam_mask=beam_mask

IF smooth_width LE 11 THEN smooth_factor = Ceil(dimension/smooth_width) ELSE smooth_factor = Round(dimension/smooth_width)
IF dimension EQ elements THEN smooth_factor2 = smooth_factor ELSE BEGIN
    IF smooth_width LE 11 THEN smooth_factor2 = Ceil(elements/smooth_width) ELSE smooth_factor2 = Round(elements/smooth_width)
ENDELSE
image_rebin=Rebin(image_unfiltered*beam_mask,smooth_factor,smooth_factor2)
mask_rebin=Rebin(beam_mask,smooth_factor,smooth_factor2)
mask_i_use=where(mask_rebin)
filter_i=where(Abs(image_rebin[mask_i_use]-Mean(image_rebin[mask_i_use])) GT 5.*Stddev(image_rebin[mask_i_use]),n_filter)

IF n_filter GT 0 THEN BEGIN
    image_rebin2=Median(image_rebin,4,/even)
    image_rebin[mask_i_use[filter_i]]=image_rebin2[mask_i_use[filter_i]]
ENDIF
image_smooth=Rebin(image_rebin,dimension,elements)
image_filtered=(image_unfiltered-image_smooth)*beam_mask

RETURN, image_filtered
END