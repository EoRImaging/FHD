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
FUNCTION vis_struct_init_psf,base=base,res_i=res_i,res_val=res_val,res_N=res_N,xvals=xvals,yvals=yvals
;

IF N_Elements(base) EQ 0 THEN base=Ptrarr(1) ;will actually have dimensions (npol,nfreq,resolution,resolution)
IF N_Elements(resolution) EQ 0 THEN resolution=1. ;over-resolution
IF N_Elements(res_i) EQ 0 THEN res_i=Ptrarr(1) ;will have the same dimensions as base. Contains indices for pixels with values different from base
IF N_Elements(res_val) EQ 0 THEN res_val=Ptrarr(1);will have the same dimensions as base. Contains values for pixels with values different from base
IF N_Elements(res_n) EQ 0 THEN res_n=lon64arr(1) ;will have the same dimensions as base. Contains the number of pixels with values different from base
IF N_Elements(xvals) EQ 0 THEN xvals=Ptrarr(1) ;will have dimensions of (resolution,resolution)
IF N_Elements(yvals) EQ 0 THEN yvals=Ptrarr(1) ;will have dimensions of (resolution,resolution)

struct={base:base,res_i:res_i,res_val:res_val,res_n:res_n,xvals:xvals,yvals:yvals}
RETURN,struct
END