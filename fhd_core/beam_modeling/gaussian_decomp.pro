;;
;; Using input parameters p, build a gaussian mixture model of the image beam on 
;; the x,y grid. 
;;
;; x: Required x-axis vector
;; y: Required y-axis vector
;; p: Required gaussian parameter vector, 
;;  ordered as amp, offset x, sigma x, offset y, sigma y per lobe
;; ftransform: Optionally return the analytic Fourier Transform of the input gaussians
;; model_npix: Optionally provide the number of pixels on an axis used to derive
;;  the input parameters to convert to the current x,y grid 
;; model_res: Optionally provide the grid resolution used to derive the input
;;  parameters to convert to the current grid resolution
;; volume_beam: Returns the analytical calculation of the beam volume
;; sq_volume_beam: Returns the analytical calculation of the squared beam volume
;; conserve_memory: Optionally supply the max number of bytes to split the fourier transform 
;;  into loops to reduce the memory required to calculate hyperresolved beam kernels

function gaussian_decomp, x, y, p, ftransform=ftransform, model_npix=model_npix, model_res=model_res,$
  volume_beam=volume_beam,sq_volume_beam=sq_volume_beam,conserve_memory=conserve_memory
 
  nx = N_elements(x)
  ny = N_elements(y)
  decomp_beam=FLTARR(nx,ny)
  i = DComplex(0,1)

  ;Expand the p vector to readable names
  var = reform(p,5,N_elements(p)/5)
  amp = var[0,*]
  offset_x = var[1,*]
  sigma_x = var[2,*]
  offset_y = var[3,*]
  sigma_y = var[4,*]
  n_lobes = (size(var))[2]

  ;If the parameters were built on a different grid, then put on new grid
  ;Npix only affects the offset params
  if keyword_set(model_npix) then begin
    if model_npix lt nx then begin
      offset = abs(nx/2. - model_npix/2.)
      offset_x += offset 
      offset_y += offset
    endif
    if model_npix gt nx then begin
      offset = abs(model_npix/2. - nx/2.)
      offset_x -= offset
      offset_y -= offset
    endif 
  endif
  ;Resolution affects gaussian sigma and offsets
  if keyword_set(model_res) then begin
    sigma_x *=  model_res
    sigma_y *= model_res
    offset_x = ((offset_x-nx/2.) * model_res) + nx/2.
    offset_y = ((offset_y-ny/2.) * model_res) + ny/2.
  endif
  
  if ~keyword_set(ftransform) then begin
    ;
    ; Full image model with all the gaussian components
    for lobe_i=0, n_lobes - 1 do begin
      decomp_beam += amp[lobe_i] * $
        (exp(-(x-offset_x[lobe_i])^2/(2*sigma_x[lobe_i]^2))#exp(-(y-offset_y[lobe_i])^2/(2*sigma_y[lobe_i]^2)))  
    endfor
  endif else begin
    ;
    ; Full uv model with all the gaussian components
    decomp_beam=complex(decomp_beam)
    volume_beam = total(amp)
    sq_volume_beam = !Dpi*total((sigma_x)*(sigma_y)*amp^2)/(nx*ny)

    offset_x -= nx/2
    offset_y -= ny/2

    ;if constraining memory usage, then est number of loops needed
    if keyword_set(conserve_memory) then begin
      IF conserve_memory GT 1E6 THEN mem_thresh=conserve_memory ELSE mem_thresh=1E9 ;in bytes
      required_bytes = 8.*ny*nx
      split = ceil(required_bytes/mem_thresh)
      if split GT 1 then begin
        if (split mod 2) EQ 1 then split += 1
      endif
    endif else split=1

    for split_i=0,split-1 do begin
      for split_j=0,split-1 do begin
       range_x = [split_i*nx/split, split_i*nx/split+nx/split-1]
       range_y = [split_j*ny/split, split_j*ny/split+ny/split-1]

        kx = (DINDGEN(nx)+nx/2*split_i-nx/2.)#(DBLARR(ny)+1.)
        ky = (DBLARR(nx)+1.)#(DINDGEN(ny)+ny/2*split_j-ny/2.)
        for lobe_i=0, n_lobes - 1 do begin
          ;
          ; A*2*pi/(N^2)*sigmax*sigmay * exp(-2*pi^2/(N^2)*(sigmax^2*kx^2+sigmay^2*ky^2)) 
          ;  * exp(-2*i*pi/N*(offsetx*kx+offsety*ky)) 
          decomp_beam[range_x[0]:range_x[1], range_y[0]:range_y[1]] += $
            amp[lobe_i]*2.*!dpi/(nx*ny)*sigma_x[lobe_i]*sigma_y[lobe_i]* $
            exp(-2*!dpi^2/(nx*ny)*(sigma_x[lobe_i]^2*kx^2+sigma_y[lobe_i]^2*ky^2)-$
            2*!dpi/(nx)*i*(offset_x[lobe_i]*kx+offset_y[lobe_i]*ky))
        endfor

      endfor
    endfor
  
  endelse

  return, decomp_beam

end

