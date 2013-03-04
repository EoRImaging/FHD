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
FUNCTION beam_image,psf,obs,pol_i=pol_i,freq_i=freq_i,dimension=dimension,elements=elements,abs=abs
compile_opt idl2,strictarrsubs  

psf_base_ptr=psf.base
IF N_Elements(dimension) EQ 0 THEN dimension=obs.dimension
IF N_Elements(elements) EQ 0 THEN elements=dimension
beam_base=fltarr(dimension,elements)
psf_dim=Sqrt((size(*psf_base_ptr[0,0,0,0],/dimension))[0])
xl=dimension/2.-Floor(psf_dim/2.)
xh=dimension/2.-Floor(psf_dim/2.)+psf_dim-1
yl=elements/2.-Floor(psf_dim/2.)
yh=elements/2.-Floor(psf_dim/2.)+psf_dim-1

IF tag_exist(psf,'fbin_i') THEN freq_bin_i=psf.fbin_i

IF Keyword_Set(obs) THEN BEGIN
    freq_bin_i=obs.fbin_i
    n_freq=obs.n_freq
    IF tag_exist((*obs.baseline_info),'freq_use') THEN freq_i_use=where((*obs.baseline_info).freq_use GT 0) $
        ELSE freq_i_use=findgen(n_freq)
ENDIF

n_bin_use=0.
IF N_Elements(freq_bin_i) EQ 0 THEN BEGIN
    dims=Size(psf_base_ptr,/dimension)
    n_freq_bin=dims[1]
    FOR fi=0,n_freq_bin-1 DO BEGIN
;        IF N_Elements(freq_bin_i) GT 0 THEN IF Total(freq_bin_i EQ fi) EQ 0 THEN CONTINUE
        beam_base_uv=Complexarr(dimension,elements)
        beam_base_uv[xl:xh,yl:yh]=Reform(Keyword_Set(abs) ? Abs(*psf_base_ptr[pol_i,fi,0,0]):*psf_base_ptr[pol_i,fi,0,0],psf_dim,psf_dim)
        beam_base_uv+=Shift(Reverse(reverse(Conj(beam_base_uv),1),2),1,1)
        beam_base1=fft_shift(FFT(fft_shift(beam_base_uv),/inverse))/2.
        beam_base+=beam_base1
        n_bin_use+=1.
    ENDFOR
ENDIF ELSE BEGIN
    IF N_Elements(n_freq) EQ 0 THEN n_freq=N_Elements(freq_bin_i)
    IF N_Elements(freq_i_use) EQ 0 THEN freq_i_use=findgen(n_freq)
    nf_use=N_Elements(freq_i_use)
    FOR fi0=0L,nf_use-1 DO BEGIN
        fi=freq_i_use[fi0]
        IF N_Elements(freq_i) GT 0 THEN IF Total(freq_i EQ fi) EQ 0 THEN CONTINUE
        fbin=freq_bin_i[fi]
        beam_base_uv=Complexarr(dimension,elements)
        beam_base_uv[xl:xh,yl:yh]=Reform(Keyword_Set(abs) ? Abs(*psf_base_ptr[pol_i,fbin,0,0]):*psf_base_ptr[pol_i,fbin,0,0],psf_dim,psf_dim)
        beam_base_uv+=Shift(Reverse(reverse(Conj(beam_base_uv),1),2),1,1)
        beam_base1=fft_shift(FFT(fft_shift(beam_base_uv),/inverse))/2.
        beam_base+=beam_base1
        n_bin_use+=1.
    ENDFOR
ENDELSE

beam_base/=n_bin_use
beam_base=real_part(beam_base)

CASE pol_i OF 
    0:BEGIN pol_i1=0 & pol_i2=0 & END
    1:BEGIN pol_i1=1 & pol_i2=1 & END
    2:BEGIN pol_i1=0 & pol_i2=1 & END
    3:BEGIN pol_i1=1 & pol_i2=0 & END
ENDCASE

;since this form of the beam is only an approximation (should be individually applied to each frequency), ensure that the normalization is preserved
IF tag_exist(psf,'norm') THEN beam_base*=psf.norm[pol_i1]*psf.norm[pol_i2]/Max(beam_base);[dimension/2.,elements/2.]

RETURN,beam_base
END