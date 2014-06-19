;+
; :Description:
;    Initializes the structure containing gridded PSF data for a visibility. 
;
; :Keywords:
;    base - Beam average over all baselines. will have dimensions (npol,nfreq,resolution,resolution)
;
; :Author: isullivan May 6, 2012
;-
FUNCTION fhd_struct_init_psf,base=base,res_i=res_i,res_val=res_val,res_N=res_N,complex_flag=complex_flag,$
    xvals=xvals,yvals=yvals,fbin_i=fbin_i,psf_resolution=psf_resolution,psf_dim=psf_dim,$
    n_pol=n_pol,n_freq=n_freq,freq_cen=freq_cen,pol_norm=pol_norm,freq_norm=freq_norm,gain_arr=gain_arr,mutual_coupling=mutual_coupling

IF N_Elements(n_pol) EQ 0 THEN n_pol=1 ELSE n_pol=Fix(n_pol)
IF N_Elements(n_freq) EQ 0 THEN n_freq=1 ELSE n_freq=Fix(n_freq)
IF N_Elements(freq_cen) EQ 0 THEN freq_cen=Fltarr(n_freq) ELSE freq_cen=Float(freq_cen)
IF N_Elements(base) EQ 0 THEN base=Ptrarr(1,1,1,1) ;will actually have dimensions (npol,nfreq,resolution,resolution)
IF N_Elements(psf_resolution) EQ 0 THEN psf_resolution=1. ELSE psf_resolution=Float(psf_resolution);over-resolution
IF N_Elements(psf_dim) EQ 0 THEN psf_dim=1. ELSE psf_dim=Float(psf_dim)
IF N_Elements(res_i) EQ 0 THEN res_i=Ptrarr(1) ;will have the same dimensions as base. Contains indices for pixels with values different from base
IF N_Elements(res_val) EQ 0 THEN res_val=Ptrarr(1);will have the same dimensions as base. Contains values for pixels with values different from base
IF N_Elements(res_n) EQ 0 THEN res_n=lon64arr(1) ELSE res_n=Long(res_n) ;will have the same dimensions as base. Contains the number of pixels with values different from base
IF N_Elements(xvals) EQ 0 THEN xvals=Ptrarr(1) ;will have dimensions of (resolution,resolution)
IF N_Elements(pol_norm) EQ 0 THEN pol_norm=replicate(1.,n_pol) ELSE pol_norm=Float(pol_norm)
IF N_Elements(freq_norm) EQ 0 THEN freq_norm=replicate(1.,n_freq) ELSE freq_norm=Float(freq_norm)
IF N_Elements(fbin_i) EQ 0 THEN fbin_i=Lonarr(1) ELSE fbin_i=Long(fbin_i)
IF N_Elements(complex_flag) EQ 0 THEN complex_flag=1
IF N_Elements(gain_arr) EQ 0 THEN gain_arr=Ptrarr(2)
IF N_Elements(mutual_coupling) EQ 0 THEN mutual_coupling=Ptrarr(2,n_freq)

struct={base:base,res_i:res_i,res_val:res_val,res_n:res_n,xvals:xvals,yvals:yvals,pnorm:pol_norm,fnorm:freq_norm,$
    fbin_i:fbin_i,resolution:psf_resolution,dim:psf_dim,complex_flag:complex_flag,n_pol:n_pol,n_freq:n_freq,freq:freq_cen,$
    gain_arr:gain_arr,mutual_coupling:mutual_coupling}
RETURN,struct
END