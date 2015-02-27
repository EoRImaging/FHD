FUNCTION planck_map_read,obs,jones

planck_directory='D:\MWA\DATA\Planck_maps\'
IF N_Elements(obs) EQ 0 THEN obs=getvar_savefile('D:\MWA\DATA3\128T\testcal3\fhd_firstpass_standard\metadata\1061316296_obs.sav','obs')

dustfilename='COM_CompMap_ThermalDust-commander_2048_R2.00.fits'
sync_filename='COM_CompMap_Synchrotron-commander_0256_R2.00.fits'
sync_pol_filename='COM_CompMap_SynchrotronPol-commander_0256_R2.00.fits'

sync_I=mrdfits(filepath(sync_filename,root=planck_directory),1,sync_header)
sync_pol=mrdfits(filepath(sync_pol_filename,root=planck_directory),1,syncpol_header)

nside=sxpar(sync_header,'nside')
n_hpx=nside2npix(nside)
ordering=Strtrim(sxpar(sync_header,'ordering'),2)
Tunit=sxpar(sync_header,'tunit1') ;micro Kelvin (uK_RJ) <- what does the RJ stand for?
coord_sys=sxpar(sync_header,'coordsys')
bad_data=sxpar(sync_header,'bad_data')

sync_hpx=Ptrarr(2)
sync_hpx[0]=Ptr_new(sync_I.(0)/1e6)
sync_hpx[1]=Ptr_new(sync_pol.(0)/1e6)
;sync_hpx[2]=Ptr_new(sync_pol.(1)/1e6)
;sync_hpx[3]=Ptr_new(Fltarr(n_hpx))
undefine_fhd,sync_I,sync_pol


sync_map=healpix_interpolate(sync_hpx,obs,nside=nside,hpx_ordering=ordering,/from_kelvin,coord_sys=coord_sys)

RETURN,sync_map
END