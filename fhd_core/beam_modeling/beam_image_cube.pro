FUNCTION beam_image_cube,obs,psf,freq_i_arr=freq_i_arr,pol_i_arr=pol_i_arr,$
    n_freq_bin=n_freq_bin,beam_mask=beam_mask,square=square,beam_threshold=beam_threshold

n_pol=obs.n_pol
n_freq=obs.n_freq
freq_arr=(*obs.baseline_info).freq
freq_bin_i=(*obs.baseline_info).fbin_i
dimension=obs.dimension
elements=obs.elements
IF N_Elements(square) EQ 0 THEN square=1
IF N_Elements(beam_threshold) EQ 0 THEN beam_threshold=0.05

IF N_Elements(pol_i_arr) EQ 0 THEN pol_i_arr=indgen(n_pol)
n_pol=N_Elements(pol_i_arr)

IF Keyword_Set(n_freq_bin) THEN freq_i_arr=Floor(indgen(n_freq_bin)*n_freq/n_freq_bin)
IF N_Elements(freq_i_arr) EQ 0 THEN BEGIN
    print,"Either freq_i_arr or n_freq_bin must be supplied!"
    RETURN,Ptrarr(n_pol,1) ;null pointer
ENDIF

n_freq_bin=N_Elements(freq_i_arr)
IF Median(freq_i_arr) GT n_freq THEN BEGIN
    ;allow frequencies to be specified, instead of bin numbers
    freq_i_use=Interpol(Lindgen(n_freq),freq_arr,freq_i_arr)
ENDIF ELSE freq_i_use=freq_i_arr

beam_arr=Ptrarr(n_pol,n_freq_bin,/allocate)

bin_arr=freq_bin_i[freq_i_use]
bin_hist=histogram(bin_arr,/binsize,min=0,reverse=bri)
bin_use=where(bin_hist,n_freq_use)
IF n_freq_use EQ 0 THEN RETURN, beam_arr 
bin_n=bin_hist[bin_use]

beam_mask=fltarr(dimension,elements)+1.
FOR p_i=0,n_pol-1 DO FOR fb_i=0L,n_freq_use-1 DO BEGIN
    pol_i=pol_i_arr[p_i]
    f_i_i=bri[bri[bin_use[fb_i]]:bri[bin_use[fb_i]+1]-1]
    f_i=freq_i_use[f_i_i[0]]
    beam_single=beam_image(psf,obs,pol_i=pol_i,freq_i=f_i,square=square)
    FOR b_i=0,bin_n[fb_i]-1 DO beam_arr[p_i,f_i_i[b_i]]=Ptr_new(beam_single)
    b_i=obs.obsx+obs.obsy*dimension
    beam_i=region_grow(beam_single,b_i,thresh=[beam_threshold^(Keyword_Set(square)+1.),max(beam_single)])
    beam_mask1=fltarr(dimension,elements)
    beam_mask1[beam_i]=1.
    beam_mask*=beam_mask1
ENDFOR

RETURN,beam_arr
END
