;+
; :Description:
;    Initializes the structure containing gridded PSF data for a visibility. 
;
; :Keywords:
;    base - Beam average over all baselines. will have dimensions (npol,nfreq,resolution,resolution)
;    
;    res_i 
;    
;    res_val
;    
;    res_N
;    
;    xvals
;    
;    yvals
;
; :Author: isullivan May 6, 2012
;-
FUNCTION vis_struct_init_psf,base=base,res_i=res_i,res_val=res_val,res_N=res_N,$
    xvals=xvals,yvals=yvals,norm=norm,fbin_i=fbin_i,psf_resolution=psf_resolution,psf_dim=psf_dim
;

IF N_Elements(base) EQ 0 THEN base=Ptrarr(1) ;will actually have dimensions (npol,nfreq,resolution,resolution)
IF N_Elements(psf_resolution) EQ 0 THEN psf_resolution=1. ELSE psf_resolution=Float(psf_resolution);over-resolution
IF N_Elements(psf_dim) EQ 0 THEN psf_dim=1. ELSE psf_dim=Float(psf_dim)
IF N_Elements(res_i) EQ 0 THEN res_i=Ptrarr(1) ;will have the same dimensions as base. Contains indices for pixels with values different from base
IF N_Elements(res_val) EQ 0 THEN res_val=Ptrarr(1);will have the same dimensions as base. Contains values for pixels with values different from base
IF N_Elements(res_n) EQ 0 THEN res_n=lon64arr(1) ;will have the same dimensions as base. Contains the number of pixels with values different from base
IF N_Elements(xvals) EQ 0 THEN xvals=Ptrarr(1) ;will have dimensions of (resolution,resolution)
IF N_Elements(norm) EQ 0 THEN norm=fltarr(2)+1.
IF N_Elements(fbin_i) EQ 0 THEN fbin_i=Lonarr(1)

struct={base:base,res_i:res_i,res_val:res_val,res_n:res_n,xvals:xvals,yvals:yvals,norm:norm,fbin_i:fbin_i,resolution:psf_resolution,dim:psf_dim}
RETURN,struct
END