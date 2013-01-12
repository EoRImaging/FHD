;+
; :Description:
;    Generates the average beam image for one polarization
;
; :Params:
;    psf_base_ptr - equal to psf.base standard structure.
;
; :Keywords:
;    pol_i - polarization index. 0:XX, 1:YY, 2:XY, 3:YX
;    
;    freq_bin_i - If set, returns the beam of a specific frequency bin instead of the average beam.
;    
;    dimension - size of image in pixels. If elements is also set, this refers to the size of the first dimension
;    
;    elements - defaults to elements=dimension
;
; :Author: isullivan May 4, 2012
;-
FUNCTION beam_image,psf_base_ptr,pol_i=pol_i,freq_bin_i=freq_bin_i,dimension=dimension,elements=elements
compile_opt idl2,strictarrsubs  

IF N_Elements(elements) EQ 0 THEN elements=dimension
dims=Size(psf_base_ptr,/dimension)
n_freq_bin=dims[1]
;IF N_Elements(freq_bin_i) EQ 0 THEN freq_bin_i=n_freq_bin/2

IF N_Elements(freq_bin_i) GT 0 THEN psf_base=*psf_base_ptr[pol_i,freq_bin_i,0,0] $
;ELSE BEGIN
;    psf_base=*psf_base_ptr[pol_i,0,0,0]
;    FOR freq_i=1,n_freq_bin-1 DO psf_base+=*psf_base_ptr[pol_i,freq_i,0,0]
;    psf_base/=n_freq_bin
;ENDELSE
ELSE BEGIN
    psf_base=weight_invert(*psf_base_ptr[pol_i,0,0,0],/abs)
    FOR freq_i=1,n_freq_bin-1 DO psf_base+=weight_invert(*psf_base_ptr[pol_i,freq_i,0,0],/abs)
    psf_base/=n_freq_bin
    psf_base=weight_invert(psf_base,/abs)
ENDELSE
psf_dim=(size(psf_base,/dimension))[0]

beam_base_uv=fltarr(dimension,elements)
beam_base_uv[dimension/2.-Floor(psf_dim/2.):dimension/2.-Floor(psf_dim/2.)+psf_dim-1,elements/2.-Floor(psf_dim/2.):elements/2.-Floor(psf_dim/2.)+psf_dim-1]=psf_base
beam_base=fft_shift(real_part(FFT(fft_shift(beam_base_uv),/inverse)))>0.
;beam_base/=max(beam_base)

RETURN,beam_base
END