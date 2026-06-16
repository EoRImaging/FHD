; ---------- Stable Dirichlet-kernel amplitude: D_N(f)=sin(pi*N*f)/sin(pi*f)
FUNCTION dirichlet_kernel_amp, f, N
  f = DOUBLE(f) & N = DOUBLE(N)
  x = !DPI * f
  res   = DComplex(DBLARR(N_ELEMENTS(f)))
  denom = SIN(x)
  nz = WHERE(ABS(denom) GT 1D-14, n_nz)
  IF n_nz GT 0 THEN res[nz] = SIN(N * x[nz]) / denom[nz]
  z = WHERE(ABS(denom) LE 1D-14, n_z)
  IF n_z GT 0 THEN res[z] = N  ; lim f->0 is N
  RETURN, res
END

; ---------- 1D 7-term BH DTFT amplitude (no phase), discrete-time closed form
FUNCTION bh_dtft_1d_amp, f, N, sigma, eps
  f = DOUBLE(f) & N = DOUBLE(N)
  ; Harris 7-term coefficients (Table II, 1978)
  a   = [ 0.2712203606D, 0.4334446123D, 0.21800412D, 0.0657853433D, $
          0.0107618673D, 0.0007700121D, 0.00001368088D ]
  df  = 1D / sigma

  ; a0 term
  res = a[0] * dirichlet_kernel_amp(f, N)

  ; ±m shifted terms
  FOR m=1,6 DO BEGIN
    shift = m * df
    w = EXP( 2D*!DPI*DComplex(0,1) * m * eps / sigma )  ; e^{+i 2π m eps/sigma}
    res += (a[m] / 2D) * ( $
      w * dirichlet_kernel_amp(f - shift, N) + conj(w) * dirichlet_kernel_amp(f + shift, N) )
  ENDFOR
  RETURN, res
END

;===============================================================
; MAIN: Blackman–Harris decomposition (separable) with analytic FT
;   - Real-space branch: builds the 2-D separable 7-term BH window
;                        on the finite support you define via x_norm,y_norm.
;   - Fourier-space branch: analytic DTFT using 1-D BH spectra and
;                           a placement phase (no FFT of images).

FUNCTION blackmanharris_decomp, x, y, p, $
  ftransform=ftransform, model_npix=model_npix, model_res=model_res, over_res=over_res,$
  volume_beam=volume_beam, sq_volume_beam=sq_volume_beam, max_amp=max_amp

  nx = N_ELEMENTS(x) & ny = N_ELEMENTS(y)
  decomp_beam = DCOMPLEX(DBLARR(nx, ny))
  xmat = meshgrid(nx, ny, 1)
  ymat = meshgrid(nx, ny, 2)
  i    = DCOMPLEX(0D, 1D)

  ; Expand the p vector to readable names
  var      = REFORM(p, 4, N_ELEMENTS(p)/4)
  amp      = var[0,*]
  width_x  = var[1,*]
  width_y  = var[1,*]
  offset_x = var[2,*]
  offset_y = var[3,*]
  n_lobes  = (SIZE(var))[2]

  IF ~KEYWORD_SET(over_res) then over_res = 1.0d

  ; Adjust for model resolution differences
  IF KEYWORD_SET(model_res) THEN BEGIN
    width_x *= over_res / model_res
    width_y *= over_res / model_res 
    offset_x *= over_res / model_res 
    offset_y *= over_res / model_res 
  ENDIF
    ; Adjust for model grid differences
  IF KEYWORD_SET(model_npix) THEN BEGIN
    offset = (model_npix * over_res / model_res - nx) / 2.0
    offset_x -= offset
    offset_y -= offset
  ENDIF

  ; It appears as though the fft of the bh is better than any analytic form...
  analytic_ftransform = 0

  IF KEYWORD_SET(analytic_ftransform) THEN BEGIN
    ; === Fourier-space (analytic DTFT) ===
    ; Builds separable Blackman-Harris function in u and v directions,
    ; Floor is at FFT level in a cross-pattern at u and v directions from lobe. 
    ; Only single lobe currently implemented

    IF n_lobes NE 1 THEN MESSAGE, 'BH analytic FT with multiple lobes not implemented'

    ; Discrete support box to reduce computations
    x_norm = (xmat - offset_x[0]) / width_x[0] + 0.5D
    y_norm = (ymat - offset_y[0]) / width_y[0] + 0.5D
    inds = WHERE((x_norm GE 0D) AND (x_norm LT 1D) AND (y_norm GE 0D) AND (y_norm LT 1D), n_inds)

    mask = BYTARR(nx, ny) & mask[inds] = 1B
    supp_x = WHERE(TOTAL(mask, 2) GT 0, ncol)
    supp_y = WHERE(TOTAL(mask, 1) GT 0, nrow)
    Nsx = LONG(MAX(supp_x) - MIN(supp_x) + 1)
    Nsy = LONG(MAX(supp_y) - MIN(supp_y) + 1)
    x0  = LONG(MIN(supp_x))
    y0  = LONG(MIN(supp_y))

    ; centers (zero-phase at discrete center of support)
    cx = DOUBLE(x0) + (DOUBLE(Nsx) - 1D)/2D
    cy = DOUBLE(y0) + (DOUBLE(Nsy) - 1D)/2D

    ; Frequency axes (cycles/pixel), centered DC
    kx = (DINDGEN(nx) - nx/2D) / DOUBLE(nx)
    ky = (DINDGEN(ny) - ny/2D) / DOUBLE(ny)
    kxmat = REBIN(kx, nx, ny)
    kymat = TRANSPOSE(REBIN(ky, nx, ny))

    ; Placement phase
    phase = EXP(-2D*!DPI*i * ( kxmat*cx + kymat*cy ))

    ; eps = (start-index of discrete support) - (continuous start location)
    eps_x = DOUBLE(x0) - (offset_x[0] - width_x[0]/2D)
    eps_y = DOUBLE(y0) - (offset_y[0] - width_y[0]/2D)

    ; 1D analytic spectra (no phase) along x and y
    Fx = bh_dtft_1d_amp(kx, Nsx, width_x[0], eps_x)
    Fy = bh_dtft_1d_amp(ky, Nsy, width_y[0], eps_y)

    decomp_beam = amp[0] * (Fx # Fy) * phase

  ENDIF ELSE BEGIN
    ; === real-space ===

    ; Discrete support box to reduce computations
    x_norm = (xmat - offset_x[0]) / width_x[0] + 0.5D
    y_norm = (ymat - offset_y[0]) / width_y[0] + 0.5D
    inds = WHERE((x_norm GE 0D) AND (x_norm LT 1D) AND (y_norm GE 0D) AND (y_norm LT 1D), n_inds)

    ; 7-term Blackman-Harris window coefficients
    a0 = 0.2712203606d & a1 = 0.4334446123d & a2 = 0.21800412d
    a3 = 0.0657853433d & a4 = 0.0107618673d & a5 = 0.0007700121d & a6 = 0.00001368088d

    ; Build separable 2D Blackman-Harris window on defined support
    decomp_beam[inds] += amp[0] * ( (a0 $
      - a1*COS(2*!DPI*x_norm[inds]) $
      + a2*COS(4*!DPI*x_norm[inds]) $
      - a3*COS(6*!DPI*x_norm[inds]) $
      + a4*COS(8*!DPI*x_norm[inds]) $
      - a5*COS(10*!DPI*x_norm[inds]) $
      + a6*COS(12*!DPI*x_norm[inds])) * $
      (a0 $
      - a1*COS(2*!DPI*y_norm[inds]) $
      + a2*COS(4*!DPI*y_norm[inds]) $
      - a3*COS(6*!DPI*y_norm[inds]) $
      + a4*COS(8*!DPI*y_norm[inds]) $
      - a5*COS(10*!DPI*y_norm[inds]) $
      + a6*COS(12*!DPI*y_norm[inds])) )

    max_amp = amp[0]

    IF KEYWORD_SET(ftransform) THEN BEGIN
      decomp_beam = fft_shift(fft(fft_shift(decomp_beam)))
    endif

  ENDELSE

  sq_volume_beam = 0
  volume_beam = 0

  RETURN, decomp_beam
END
;===============================================================