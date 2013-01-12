


function stokes_off_zenith, azimuth_deg, elevation_deg, stokes_zenith, Ex_mag, Ey_mag, Ez_mag, $
    Efield_zenith = Efield_zenith, norm = norm, frac_unpolarized = frac_unpolarized, intensity_only = intensity_only, $
    Cross_xy=Cross_xy,Cross_yx=Cross_yx

  n_az = n_elements(azimuth_deg)
  n_el = n_elements(elevation_deg)

  if n_az ne n_el or n_az lt 1 then message, $
     'Azimuth(s) and elevation(s) of source(s) must be provided and have the same number of elements'

  if n_elements(Efield_zenith) ne 0 and n_elements(stokes_zenith) ne 0 then $
     message, 'Specify either the E field at zenith or the Stokes parameters at zenith, not both.'

  if n_elements(Efield_zenith) ne 0 then begin
    if n_elements(Efield_zenith) ne 2 then message, 'E field at zenith must be a 2-element array in the form [Ex, Ey]'
    if keyword_set(norm) then norm_factor = 1d/sqrt(total(abs(Efield_zenith)^2)) else norm_factor = 1d

    Ex = Efield_zenith[0] * norm_factor
    Ey = Efield_zenith[1] * norm_factor

    if n_elements(frac_unpolarized) eq 0 then frac_unpolarized = 0
  endif else begin 
     if n_elements(frac_unpolarized) then print, 'Unpolarized fraction can only be set with E field at zenith. Ignoring.'
     if keyword_set(norm) then print, 'Norm keyword can only be set with E field at zenith. Ignoring.'
    
     if n_elements(stokes_zenith) eq 0 then begin
        print, 'No Stokes at zenith provided, assuming 1 Jy unpolarized.'
        stokes_zenith = [1, 0, 0, 0]
     endif

     if n_elements(stokes_zenith) ne 4 then message, 'Stokes_zenith must be a 4-element array in the form [I, Q, U, V]'

     if stokes_zenith[0]^2d - total(stokes_zenith[1:3]^2d) lt -1*10^(-8d) then message, $
        'Invalid stokes_zenith: I^2 must be >= Q^2+U^2+V^2'

     ;; To calculate Ex & Ey, assume any unpolarized light is actually
     ;; linearly polarized, then multiply U by (1 - unpolarized fraction) to
     ;; get the stokes parameters right.
     frac_unpolarized = 0

     if stokes_zenith[3] eq 0 then begin
        ;; no circular polarization component -- Ex & Ey have no phase offset
        if total(stokes_zenith[1:3]^2) eq 0 then begin 
           ;; fully unpolarized.
           frac_unpolarized = 1
        endif
        Ex = sqrt((stokes_zenith[0] + stokes_zenith[1]) / 2d)
        Ey = sqrt((stokes_zenith[0] - stokes_zenith[1]) / 2d)
     endif else if stokes_zenith[0]^2d - total(stokes_zenith[1:3]^2d) lt 10^(-8d) then begin
        ;; this is valid for monochromatic light, can just assume phase of Ex is zero
        Ex = sqrt((stokes_zenith[0] + stokes_zenith[1]) / 2d)
        Ey_mag = sqrt((stokes_zenith[0] - stokes_zenith[1]) / 2d)
        Ey_phase = atan(stokes_zenith[3], stokes_zenith[2])
        Ey = Ey_mag * exp(complex(0,1) * Ey_phase)
     endif else begin
        ;; this is not valid for monochromatic light -- it has an unpolarized component.
        Ex = sqrt((stokes_zenith[0] + stokes_zenith[1]) / 2d)
        Ey_mag = sqrt((stokes_zenith[0] - stokes_zenith[1]) / 2d)
        Ey_phase = atan(stokes_zenith[3], stokes_zenith[2])

        frac_unpolarized = (stokes_zenith[0]^2d - total(stokes_zenith[1:3]^2d)) / stokes_zenith[0]^2d
        
        ;; treat unpolarized part as linearly polarized -- no phase offset between Ex & Ey
        Ey = Ey_mag * (frac_unpolarized + (1-frac_unpolarized)*exp(complex(0,1) * Ey_phase))

     endelse
  endelse
;  print, 'unpolarized fraction: ', frac_unpolarized

  ;; theta_src_deg = elevation_deg - 90d
  ;; phi_src_deg = azimuth_deg - 90d 
  theta_src_deg = 90d - elevation_deg
  phi_src_deg = 90d - azimuth_deg 

  theta_src_rad = !pi * theta_src_deg / 180d
  phi_src_rad = !pi * phi_src_deg / 180d

  ;; need to keep the contributions from each input term separate
  ;; because we have to deal with unpolarized fractions
  ;; (terms shouldn't cancel if they have random phase)
  Ex_ground_x = Ex * (cos(theta_src_rad) * cos(phi_src_rad)^2d + sin(phi_src_rad)^2)
  Ex_ground_y = -1.* Ey * (cos(theta_src_rad) -1.) * sin(phi_src_rad) * cos(phi_src_rad)
  Ey_ground_x = -1.*Ex * (cos(theta_src_rad) -1.) * sin(phi_src_rad) * cos(phi_src_rad)
  Ey_ground_y = Ey * (cos(theta_src_rad) * sin(phi_src_rad)^2d + cos(phi_src_rad)^2)
  Ez_ground_x = Ex * sin(theta_src_rad) * cos(phi_src_rad) 
  Ez_ground_y = -1.*Ey * sin(theta_src_rad) * sin(phi_src_rad)
  
  ;; calculate phases
  Ex_phase_diff = atan(dcomplex(Ex_ground_y),/phase) - atan(dcomplex(Ex_ground_x),/phase)
  Ey_phase_diff = atan(dcomplex(Ey_ground_y),/phase) - atan(dcomplex(Ey_ground_x),/phase)
  Ez_phase_diff = atan(dcomplex(Ez_ground_y),/phase) - atan(dcomplex(Ez_ground_x),/phase)

  ;; now calculate magnitudes. Subtract off cross term for unpolarized portion 
  Ex_mag =sqrt(abs(Ex_ground_x + Ex_ground_y)^2 - frac_unpolarized * 2 * abs(Ex_ground_x) * abs(Ex_ground_y) * cos(Ex_phase_diff))
  Ey_mag =sqrt(abs(Ey_ground_x + Ey_ground_y)^2 - frac_unpolarized * 2 * abs(Ey_ground_x) * abs(Ey_ground_y) * cos(Ey_phase_diff))
  Ez_mag =sqrt(abs(Ez_ground_x + Ez_ground_y)^2 - frac_unpolarized * 2 * abs(Ez_ground_x) * abs(Ez_ground_y) * cos(Ez_phase_diff))
  Cross_xy=(Conj(Ex_ground_x)*Ey_ground_x+Conj(Ex_ground_y)*Ey_ground_y+(Conj(Ex_ground_x)*Conj(Ex_ground_y)+Ey_ground_y*Ey_ground_x)*(1.-frac_unpolarized))/2.
  Cross_yx=Conj(Cross_xy)

  Emag_ground = sqrt(Ex_mag^2d + Ey_mag^2d + Ez_mag^2d)

  phase_diff = abs(atan(Ey_ground_x + Ey_ground_y,/phase)) - abs(atan(Ex_ground_x + Ex_ground_y,/phase))

  ;; Now construct stokes. U & V need to be corrected for unpolarized fraction
  IF Keyword_Set(intensity_only) THEN BEGIN
    stokes_az_el=Ex_mag^2d + Ey_mag^2d ;fltarr(size(azimuth_deg,/degrees))
  ENDIF ELSE BEGIN
    
    stokes_az_el = dblarr(4, n_az)
    stokes_az_el[0, *] = Ex_mag^2d + Ey_mag^2d
    stokes_az_el[1, *] = Ex_mag^2d - Ey_mag^2d
    stokes_az_el[2, *] = 2 * Ex_mag * Ey_mag * cos(phase_diff) * (1-frac_unpolarized)
    stokes_az_el[3, *] = 2 * Ex_mag * Ey_mag * sin(phase_diff) * (1-frac_unpolarized)
  ENDELSE
  
  phase_diff2=atan(Ey_ground_x + Ey_ground_y,/phase) - atan(Ex_ground_x + Ex_ground_y,/phase)
  Cross_xy=2 * Ex_mag * Ey_mag * (cos(phase_diff2)+sin(phase_diff2))/2.
  Cross_yx=2 * Ex_mag * Ey_mag * (cos(phase_diff2)-sin(phase_diff2))/2.

;  print, 'Ex_mag, Ey_mag, Ez_mag, Emag: '
;  for i=0, n_az -1 do print, Ex_mag[i], Ey_mag[i], Ez_mag[i], Emag_ground[i]
;  print, 'Stokes I, Q, U, V'
;  print, stokes_az_el

  return, stokes_az_el

end
