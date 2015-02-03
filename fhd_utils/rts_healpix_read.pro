FUNCTION rts_healpix_read,obs,jones,file_path_fhd=file_path_fhd,rts_file_path=rts_file_path


IF N_Elements(rts_file_path) EQ 0 THEN rts_file_path='D:\MWA\DATA3\corall.fits'
data=mrdfits(rts_file_path,1,header)


hpx_i=data.healpix_pixnum
n_pol=4
nside=2048
ind_type='ring'
hpx_inds=data.(0)
image_hpx_arr=Ptrarr(n_pol)
weights_hpx_arr=Ptrarr(n_pol)
pol_order=[0,3,1,2]
FOR pol_i=0,n_pol-1 DO BEGIN
    image_hpx_arr[pol_i]=Ptr_new(data.(pol_order[pol_i]*2+1))
    weights_hpx_arr[pol_i]=Ptr_new(data.(pol_order[pol_i]*2+2))
ENDFOR


IF Keyword_Set(obs) THEN BEGIN
    image_arr=healpix_interpolate(image_hpx_arr,obs,nside=nside,hpx_inds=hpx_inds,hpx_ordering=ind_type)
    weights_arr=healpix_interpolate(weights_hpx_arr,obs,nside=nside,hpx_inds=hpx_inds,hpx_ordering=ind_type)
ENDIF

RETURN,rts_model
END