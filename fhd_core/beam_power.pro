FUNCTION beam_power,antenna1,antenna2,ant_pol1=ant_pol1,ant_pol2=ant_pol2,freq_i=freq_i,$
    psf_image_dim=psf_image_dim,psf_intermediate_res=psf_intermediate_res,$
    beam_mask_electric_field=beam_mask_electric_field,beam_mask_threshold=beam_mask_threshold,$
    xvals_uv_superres=xvals_uv_superres,yvals_uv_superres=yvals_uv_superres
    
freq_center=antenna1.freq[freq_i]

beam_ant1=*(antenna1.response[ant_pol1,freq_i])
beam_ant2=*(antenna2.response[ant_pol2,freq_i])
Jones1=antenna1.Jones[*,*,freq_i]
Jones2=antenna2.Jones[*,*,freq_i]
;Jones_inst_response=*Jones1[ant_pol1,0]*Conj(*Jones2[ant_pol2,0])+$
;                    *Jones1[ant_pol1,1]*Conj(*Jones2[ant_pol2,1])

beam_ant1=*(antenna1.response[ant_pol1,freq_i])
beam_ant2=Conj(*(antenna2.response[ant_pol2,freq_i]))


;beam_norm=Max(Abs(beam_ant1*beam_ant2))
beam_norm=1.
beam_test=dirty_image_generate(beam_ant1*beam_ant2)

dimension_super=(size(xvals_uv_superres,/dimension))[0]
psf_base_test_superres=Interpolate(beam_test,xvals_uv_superres,yvals_uv_superres,cubic=-0.5)
beam_i=region_grow(psf_base_test_superres,dimension_super*(1.+dimension_super)/2.,thresh=[Max(psf_base_test_superres)/beam_mask_threshold,Max(psf_base_test_superres)])
uv_mask_superres=fltarr(dimension_super,dimension_super)
uv_mask_superres[beam_i]=1.
;uv_mask=fltarr(psf_image_dim,psf_image_dim)
;beam_i=region_grow(beam_test,psf_image_dim*(1.+psf_image_dim)/2.,$
;    thresh=[Max(beam_test)/beam_mask_threshold,Max(beam_test)])
;uv_mask[beam_i]=1.
;uv_mask_superres=Interpolate(uv_mask,xvals_uv_superres,yvals_uv_superres)

power_beam=(*Jones1[ant_pol1,0]*beam_ant1)*(Conj(*Jones2[ant_pol2,0])*beam_ant2)+$
           (*Jones1[ant_pol1,1]*beam_ant1)*(Conj(*Jones2[ant_pol2,1])*beam_ant2)
;power_beam=(*Jones1[0,ant_pol1]*beam_ant1)*(Conj(*Jones2[0,ant_pol2])*beam_ant2)+$
;           (*Jones1[1,ant_pol1]*beam_ant1)*(Conj(*Jones2[1,ant_pol2])*beam_ant2)           
psf_base_single=dirty_image_generate(power_beam,/no_real)

psf_base_superres=Interpolate(psf_base_single,xvals_uv_superres,yvals_uv_superres,cubic=-0.5)
psf_base_superres*=psf_intermediate_res^2. ;FFT normalization correction in case this changes the total number of pixels
psf_base_superres/=beam_norm
;psf_base_superres*=uv_mask_superres

RETURN,psf_base_superres
END