PRO beam_dim_fit,beam_arr,psf_dim=psf_dim,psf_resolution=psf_resolution,beam_mask_threshold=beam_mask_threshold,$
    beam_residual_threshold=beam_residual_threshold,psf_xvals=psf_xvals,psf_yvals=psf_yvals
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
    psf_dim2=psf_dim-2.*edge_zeroes
    n_pol=(size(beam_arr,/dimension))[0]
    nfreq_bin=(size(beam_arr,/dimension))[1]
    n_baselines=(size(beam_arr,/dimension))[2]
    FOR pol_i=0,n_pol-1 DO FOR freq_i=0,nfreq_bin-1 DO BEGIN
        bi=0L
        WHILE bi LT n_baselines DO BEGIN
            bi_test_i=where(beam_arr[pol_i,freq_i,bi] EQ beam_arr[pol_i,freq_i,*],n_match)
            IF n_match GT 0 THEN BEGIN
                FOR p_i=0L,psf_resolution-1 DO FOR p_j=0L,psf_resolution-1 DO BEGIN
                    psf_single=Reform(*(*beam_arr[pol_i,freq_i,bi])[p_i,p_j],psf_dim,psf_dim)
                    psf_single=psf_single[edge_zeroes:psf_dim-edge_zeroes-1,edge_zeroes:psf_dim-edge_zeroes-1]
                    *(*beam_arr[pol_i,freq_i,bi])[p_i,p_j]=Reform(psf_single,psf_dim2*psf_dim2)
                ENDFOR
                bi+=n_match
            ENDIF ELSE bi+=1
        ENDWHILE
    ENDFOR
    psf_dim=psf_dim2
    
    FOR i=0,psf_resolution-1 DO FOR j=0,psf_resolution-1 DO BEGIN 
        *psf_xvals[i,j]=meshgrid(psf_dim,psf_dim,1)-psf_dim/2.+Float(i)/psf_resolution
        *psf_yvals[i,j]=meshgrid(psf_dim,psf_dim,2)-psf_dim/2.+Float(j)/psf_resolution
    ENDFOR
ENDIF 
END