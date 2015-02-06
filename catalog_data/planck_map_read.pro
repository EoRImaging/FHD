FUNCTION planck_map_read,obs,plank_dust_map=planck_dust_map,planck_snychrotron_map=planck_snychrotron_map,$
    planck_pol_synchrotron_map=planck_pol_synchrotron_map

planck_directory='D:\MWA\DATA\Planck_maps\'
IF N_Elements(obs) EQ 0 THEN obs=getvar_savefile('D:\MWA\DATA3\128T\testcal3\fhd_firstpass_standard\metadata\1061316296_obs.sav','obs')

CASE 1 OF 
    Keyword_Set(plank_dust_map):planck_filename='COM_CompMap_ThermalDust-commander_2048_R2.00.fits'
    Keyword_Set(planck_snychrotron_map):planck_filename='COM_CompMap_Synchrotron-commander_0256_R2.00.fits'
    Keyword_Set(planck_pol_synchrotron_map):planck_filename='COM_CompMap_SynchrotronPol-commander_0256_R2.00.fits'
    ELSE:
ENDCASE

data=mrdfits(filepath(planck_filename,root=planck_directory),1,header)

nside=sxpar(header,'nside')
ordering=Strtrim(sxpar(header,'ordering'),2)
Tunit=sxpar(header,'tunit1') ;micro Kelvin (uK_RJ) <- what does the RJ stand for?
coord_sys=sxpar(header,'coordsys')
bad_data=sxpar(header,'bad_data')

map=healpix_interpolate(data.(0),obs,nside=nside,hpx_ordering=ordering,/from_kelvin,coord_sys=coord_sys)/1e6

RETURN,map
END