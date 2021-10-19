FUNCTION visibility_count,obs,psf,params,vis_weight_ptr=vis_weight_ptr,file_path_fhd=file_path_fhd,$
    preserve_visibilities=preserve_visibilities,no_conjugate=no_conjugate,fill_model_vis=fill_model_vis,$
    double_precision=double_precision,bi_use=bi_use,fi_use=fi_use,n_freq_use=n_freq_use,xmin=xmin,ymin=ymin,$
    mask_mirror_indices=mask_mirror_indices,_Extra=extra

IF N_Elements(obs) EQ 0 THEN fhd_save_io,status_str,obs,var='obs',/restore,file_path_fhd=file_path_fhd,_Extra=extra
IF N_Elements(psf) EQ 0 THEN fhd_save_io,status_str,psf,var='psf',/restore,file_path_fhd=file_path_fhd,_Extra=extra
IF N_Elements(params) EQ 0 THEN fhd_save_io,status_str,params,var='params',/restore,file_path_fhd=file_path_fhd,_Extra=extra

weight_type = size(vis_weight_ptr,/type)
IF (weight_type EQ 10) OR (weight_type EQ 0) THEN BEGIN
  ; If the weights are pointers or undefined
  IF Min(ptr_valid(vis_weight_ptr)) EQ 0 THEN $
    fhd_save_io,status_str,vis_weight_ptr,var='vis_weights',/restore,file_path_fhd=file_path_fhd,_Extra=extra
ENDIF

;extract information from the structures
dimension=Long(obs.dimension)
elements=Long(obs.elements)

; For each unflagged baseline, get the minimum contributing pixel number for gridding 
bin_n=baseline_grid_locations(obs,psf,params,n_bin_use=n_bin_use,bin_i=bin_i,ri=ri,$
    xmin=xmin,ymin=ymin,vis_weight_ptr=vis_weight_ptr,fill_model_vis=fill_model_vis,$
    bi_use=bi_use,fi_use=fi_use,mask_mirror_indices=mask_mirror_indices)

double_precision=obs.double_precision
IF Keyword_Set(double_precision) THEN uniform_filter=Dblarr(dimension,elements) ELSE uniform_filter=Fltarr(dimension,elements)
FOR bi=0L,n_bin_use-1 DO BEGIN
    inds=ri[ri[bin_i[bi]]:ri[bin_i[bi]+1]-1]
    ind0=inds[0]
    
    xmin_use=xmin[ind0] ;should all be the same, but don't want an array
    ymin_use=ymin[ind0]
    uniform_filter[xmin_use:xmin_use+psf_dim-1,ymin_use:ymin_use+psf_dim-1]+=bin_n[bin_i[bi]]
ENDFOR
    
IF ~Keyword_Set(no_conjugate) THEN uniform_filter=(uniform_filter+conjugate_mirror(uniform_filter))/2.

fhd_save_io,status_str,uniform_filter,var='vis_count',file_path_fhd=file_path_fhd,_Extra=extra

RETURN,uniform_filter
END
