;; 
;; Create a hyperresolved, normalized image of the beam for a specified baseline type,
;; frequency, and polarization.
;;
;; antenna1, antenna2: Required antenna structures
;; ant_pol1, ant_pol2: Required antenna polarizations
;; freq_i: Required frequency to create the beam image
;; zen_int_x,zen_int_y: Required zenith location in pixels 

function beam_image_hyperresolved,antenna1,antenna2,ant_pol1,ant_pol2,freq_i,$
  zen_int_x,zen_int_y

  ;; Defined only at non-zero pixels for memory reduction
  pix_use=*antenna1[0].pix_use
  psf_image_dim = antenna1.psf_image_dim

  ;; Jones pointers and antenna beams
  Jones1=antenna1.Jones[*,*,freq_i]
  Jones2=antenna2.Jones[*,*,freq_i]
  beam_ant1=DComplex(*(antenna1.response[ant_pol1,freq_i]))
  beam_ant2=DComplex(Conj(*(antenna2.response[ant_pol2,freq_i])))

  ;; Amplitude of the response from ant1 is Sqrt(|J1[0,pol1]|^2 + |J1[1,pol1]|^2)
  ;; Amplitude of the response from ant2 is Sqrt(|J2[0,pol2]|^2 + |J2[1,pol2]|^2)
  ;; Amplitude of the baseline response is the product of the antenna responses
  power_zenith_beam=Sqrt((abs(*Jones1[0,ant_pol1])^2.+abs(*Jones1[1,ant_pol1])^2.)*$
    (abs(*Jones2[0,ant_pol2])^2.+abs(*Jones2[1,ant_pol2])^2.))
 
  ;; Create only one full-scale array for memory reduction
  image_power_beam = Dcomplexarr(psf_image_dim,psf_image_dim)

  ;; Co-opt this array to calculate the power at zenith
  image_power_beam[pix_use] = power_zenith_beam
  power_zenith=Interpolate(image_power_beam,zen_int_x,zen_int_y,cubic=-0.5)
  
  ;; Redefine the array to be the image power beam, normalized to zenith
  image_power_beam[pix_use] = (power_zenith_beam*beam_ant1*beam_ant2)/power_zenith

  return, image_power_beam

end
