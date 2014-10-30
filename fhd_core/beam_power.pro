FUNCTION beam_power,antenna1,antenna2,ant_pol1=ant_pol1,ant_pol2=ant_pol2,freq_i=freq_i,$
    psf_image_dim=psf_image_dim,psf_intermediate_res=psf_intermediate_res,$
    beam_mask_electric_field=beam_mask_electric_field,beam_mask_threshold=beam_mask_threshold,$
    xvals_uv_superres=xvals_uv_superres,yvals_uv_superres=yvals_uv_superres,zen_int_x=zen_int_x,zen_int_y=zen_int_y
    
freq_center=antenna1.freq[freq_i]
dimension_super=(size(xvals_uv_superres,/dimension))[0]

beam_ant1=*(antenna1.response[ant_pol1,freq_i])
beam_ant2=*(antenna2.response[ant_pol2,freq_i])
Jones1=antenna1.Jones[*,*,freq_i]
Jones2=antenna2.Jones[*,*,freq_i]

beam_ant1=*(antenna1.response[ant_pol1,freq_i])
beam_ant2=Conj(*(antenna2.response[ant_pol2,freq_i]))
beam_norm=1.

power_zenith=abs((*Jones1[0,ant_pol1])[zen_int_x,zen_int_y]*(Conj(*Jones2[0,ant_pol2]))[zen_int_x,zen_int_y]+$
             (*Jones1[1,ant_pol1])[zen_int_x,zen_int_y]*(Conj(*Jones2[1,ant_pol2]))[zen_int_x,zen_int_y])
power_beam=(*Jones1[0,ant_pol1]*beam_ant1)*(Conj(*Jones2[0,ant_pol2])*beam_ant2)+$
           (*Jones1[1,ant_pol1]*beam_ant1)*(Conj(*Jones2[1,ant_pol2])*beam_ant2)           
;power_beam_zenith=Abs(power_beam[zen_int_x,zen_int_y])
;power_zenith_norm=power_zenith/power_beam_zenith/Max(Abs(power_beam))
psf_base_single=dirty_image_generate(power_beam/power_zenith,/no_real)

psf_base_superres=Interpolate(psf_base_single,xvals_uv_superres,yvals_uv_superres,cubic=-0.5)
uv_mask_superres=Fltarr(dimension_super,dimension_super)
beam_i=region_grow(Abs(psf_base_superres),dimension_super*(1.+dimension_super)/2.,$
    thresh=[Max(Abs(psf_base_superres))/beam_mask_threshold,Max(Abs(psf_base_superres))])
uv_mask_superres[beam_i]=1

psf_base_superres*=psf_intermediate_res^2. ;FFT normalization correction in case this changes the total number of pixels
psf_base_superres/=beam_norm
psf_val_ref=Total(psf_base_superres)
psf_base_superres*=uv_mask_superres
psf_base_superres*=psf_val_ref/Total(psf_base_superres)
RETURN,psf_base_superres
END