FUNCTION primary_beam_radius, obs, psf, beam_threshold=beam_threshold, select_radius_multiplier=select_radius_multiplier,_Extra=extra
  ;Approximate width of circular primary beam, out to some threshold

  if not keyword_set(beam_threshold) then beam_threshold = 0.05

  dimension=obs.dimension
  elements=obs.elements
  degpix=obs.degpix
  n_pol= obs.n_pol

  IF N_Elements(beam_arr) LT (n_pol<2) THEN BEGIN 
      beam_arr=Ptrarr(n_pol<2)
      FOR pol_i=0,(n_pol<2)-1 DO beam_arr[pol_i]=Ptr_new(beam_image(psf,obs,pol_i=pol_i,square=0)>0.)
  ENDIF
  beam=fltarr(dimension,elements)
  FOR pol_i=0,(n_pol<2)-1 DO beam+=*beam_arr[pol_i]^2.
  beam=Sqrt(beam/(n_pol<2))
  obs_i = Round(obs.obsx) + dimension*Round(obs.obsy)
  beam_primary_i=region_grow(beam,obs_i,threshold=[Max(beam)/2.<beam_threshold,Max(beam)>1.])
  boundary=find_boundary(beam_primary_i,xsize=dimension, ysize=elements,perim_area=perim_area)
  select_radius = degpix*sqrt(perim_area)/2   ;Primary beam radius
  if keyword_set(select_radius_multiplier) THEN select_radius *= select_radius_multiplier
return, select_radius

END
