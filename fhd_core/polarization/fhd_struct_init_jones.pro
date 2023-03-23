FUNCTION fhd_struct_init_jones,obs,status_str,jones_in,file_path_fhd=file_path_fhd,mask=mask,$
    restore_last=restore_last,update_last=update_last,debug_jones_obs=debug_jones_obs,$
    beam_model_version=beam_model_version,save_beam_metadata_only=save_beam_metadata_only,_Extra=extra

IF Keyword_Set(restore_last) THEN BEGIN
    fhd_save_io,status_str,jones,var='jones',file_path_fhd=file_path_fhd,/restore,_Extra=extra
    IF Keyword_Set(jones) THEN RETURN,jones
ENDIF
dimension=obs.dimension
elements=obs.elements

IF Keyword_Set(update_last) THEN BEGIN
    IF N_Elements(jones_in) EQ 0 THEN $
        jones_in=fhd_struct_init_jones(obs,status_str,file_path_fhd=file_path_fhd,mask=mask,/restore_last)
    dimension_in=jones_in.dimension
    elements_in=jones_in.elements
    mask_use=intarr(dimension_in,elements_in)
    mask_use[jones_in.inds]=1
    mask_use=Rebin(mask_use,dimension,elements)
    inds_use=where(mask_use,n_pix)
    p_map=Ptrarr(4,4,/allocate)
    p_corr=Ptrarr(4,4,/allocate)
    FOR pol_i2=0,3 DO FOR pol_i1=0,3 DO BEGIN
        temp=Dcomplexarr(dimension_in,elements_in)
        temp[jones_in.inds]=*jones_in.Jmat[pol_i1,pol_i2]
        temp=Dcomplex(Rebin(real_part(temp),dimension,elements), Rebin(Imaginary(temp),dimension,elements))
        *p_map[pol_i1,pol_i2]=temp[inds_use]
        
        temp=Dcomplexarr(dimension_in,elements_in)
        temp[jones_in.inds]=*jones_in.Jinv[pol_i1,pol_i2]
        temp=Dcomplex(Rebin(real_part(temp),dimension,elements), Rebin(Imaginary(temp),dimension,elements))
        *p_corr[pol_i1,pol_i2]=temp[inds_use]
    ENDFOR
    jones={inds:inds_use,dimension:dimension,elements:elements,Jmat:p_map,Jinv:p_corr}
    RETURN,jones
ENDIF
IF N_Elements(mask) EQ 0 THEN mask=Replicate(1.,dimension,elements)

xvals=meshgrid(dimension,elements,1)
yvals=meshgrid(dimension,elements,2)
;ignore effect of refraction, since we are only determining which pixels to include
apply_astrometry, obs, ra_arr=ra_arr, dec_arr=dec_arr, x_arr=xvals, y_arr=yvals, /xy2ad, /ignore_refraction
inds_use=where(Finite(ra_arr) AND mask,n_pix)
ra_use=ra_arr[inds_use]
dec_use=dec_arr[inds_use]
xv=xvals[inds_use]
yv=yvals[inds_use]

;Convert observatory latitude to same coordinate system 
apply_astrometry, obs, ra_arr=obs.obsra, dec_arr=obs.lat, x_arr=x_t, y_arr=y_t, /ad2xy
apply_astrometry, obs, dec_arr=lat, x_arr=x_t, y_arr=y_t, /xy2ad, /ignore_refraction

obs_temp = fhd_struct_update_obs(obs, beam_nfreq_avg=obs.n_freq) ; Use only one average Jones matrix, not one per frequency
antenna=fhd_struct_init_antenna(obs_temp,beam_model_version=beam_model_version,use_psf_resolution=1.,$
    psf_dim=obs.dimension,psf_intermediate_res=1.,psf_image_resolution=1.,timing=t_ant,_Extra=extra)
Jones=rotate_jones_matrix(obs, antenna[0])

; Calculate the normalization
ant_pol1 = 0
ant_pol2 = 1
power_zenith_beam = dblarr(dimension, elements)
FOR ant_pol=0,1 DO FOR sky_pol=0,1 DO $
    power_zenith_beam += Real_part(*Jones[sky_pol,ant_pol]*Conj(*Jones[sky_pol, ant_pol]))
power_zenith_beam /= 2.
power_zenith=Interpolate(power_zenith_beam,obs_temp.zenx,obs_temp.zeny,cubic=-0.5)
FOR ptr_i=0,3 DO *Jones[ptr_i] = (*Jones[ptr_i])[inds_use]/Sqrt(power_zenith)

p_map=Ptrarr(4,4,/allocate)
p_corr=Ptrarr(4,4,/allocate)
FOR i=0,3 DO FOR j=0,3 DO *p_map[i,j]=Dcomplexarr(n_pix)
FOR i=0,3 DO FOR j=0,3 DO *p_corr[i,j]=Dcomplexarr(n_pix)
order_1 = [0,1,0,1]
order_2 = [0,1,1,0]

FOR pix=0L,n_pix-1 DO BEGIN
    ;calculate tensor product J(X)J*
    Jmat = Dcomplexarr(4,4)
    FOR ii=0,3 DO BEGIN
        FOR jj = 0,3 DO BEGIN
            Jmat[ii, jj] = (*Jones[order_1[ii], order_1[jj]])[pix]*Conj((*Jones[order_2[ii], order_2[jj]])[pix])
        ENDFOR
    ENDFOR
;    Note that we re-order the matrix elements after the Kronecker product
;    Leave the following code in as a reference for the final matrix element ordering
;    Jmat=[[J11[pix]*Conj(J11[pix]),J12[pix]*Conj(J12[pix]),J11[pix]*Conj(J12[pix]),J12[pix]*Conj(J11[pix])],$
;          [J21[pix]*Conj(J21[pix]),J22[pix]*Conj(J22[pix]),J21[pix]*Conj(J22[pix]),J22[pix]*Conj(J21[pix])],$
;          [J11[pix]*Conj(J21[pix]),J12[pix]*Conj(J22[pix]),J11[pix]*Conj(J22[pix]),J12[pix]*Conj(J21[pix])],$
;          [J21[pix]*Conj(J11[pix]),J22[pix]*Conj(J12[pix]),J21[pix]*Conj(J12[pix]),J22[pix]*Conj(J11[pix])]]
    Jinv=Invert(Jmat)
    
    FOR i=0,3 DO FOR j=0,3 DO (*p_map[i,j])[pix]=Jmat[i,j]
    FOR i=0,3 DO FOR j=0,3 DO (*p_corr[i,j])[pix]=Jinv[i,j]
ENDFOR

jones_str={inds:inds_use,dimension:dimension,elements:elements,Jmat:p_map,Jinv:p_corr}
if ~keyword_set(save_beam_metadata_only) then fhd_save_io,status_str,jones_str,var='jones',/compress,file_path_fhd=file_path_fhd,_Extra=extra
RETURN,jones_str
END
