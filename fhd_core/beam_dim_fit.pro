PRO beam_dim_fit,beam_arr,psf_dim=psf_dim,psf_resolution=psf_resolution,beam_mask_threshold=beam_mask_threshold,beam_residual_threshold=beam_residual_threshold
IF N_Elements(beam_residual_threshold) EQ 0 THEN beam_residual_threshold=0.
edge_test=fltarr(psf_dim*psf_dim)
res_i=psf_resolution/2
 ;add together ALL beams, because we just want to find out if EVERY border pixel is zero
FOR i=0L,N_Elements(beam_arr)-1 DO edge_test+=(Abs(*(*beam_arr[i])[res_i,res_i]) GT Max(Abs(*(*beam_arr[i])[res_i,res_i]))/beam_mask_threshold)
edge_test=reform(edge_test,psf_dim,psf_dim)
edge_test_cut=Total(edge_test,1)+Total(edge_test,2) 
edge_test_cut+=Reverse(edge_test_cut)
edge_test_cut/=Max(edge_test_cut)
edge_zeroes=(where(edge_test_cut GT beam_residual_threshold))[0]
IF edge_zeroes GT 0 THEN BEGIN
    psf_dim-=2.*edge_zeroes
    FOR pol_i=0,n_pol-1 DO FOR freq_i=0,nfreq_bin-1 DO FOR i=0,psf_resolution-1 DO FOR j=0,psf_resolution-1 DO $
        *psf_base[pol_i,freq_i,psf_resolution-1-i,psf_resolution-1-j]=$
            Reform((*psf_base[pol_i,freq_i,psf_resolution-1-i,psf_resolution-1-j])[edge_zeroes:edge_zeroes+psf_dim-1,edge_zeroes:edge_zeroes+psf_dim-1],psf_dim*psf_dim)
ENDIF 
END