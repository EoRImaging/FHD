# FHD Keyword Dictionary
FHD uses keywords to create unique run-specific settings. This dictionary describes the purpose of each keyword, as well as their logic or applicable ranges. Some keywords can override others, which is also documentated. The FHD default is listed when applicable, which can be overriden by EoR_firstpass settings in the top-level script eor_firstpass_version.pro.

This is a work in progress; please add keywords as you find them in alphabetical order with their corresponding definition. If there's a question about a definition or keyword, label it with !Q.

## Beam

**beam_model_version**: a number that indicates the tile beam model calculation. This is dependent on the instrument, and specific calculations are carried out in `<instrument>_beam_setup_gain.pro`. For the MWA, there are currently three options: 0) !Q, 1) a Hertzian dipole as prescribed by Cheng 1992 and Balanis 1989 (!Q Ian, dipole looks flipped from what Sutinjo has in paper?), 2) the average embedded element model from Sutinjo 2015. For PAPER, there are currently two options: 1) !Q, 2) !Q. For HERA, there is currently one option: !Q. <br />
  -*EoR_firstpass settings*: 2 <br />
  -*Default*: 1 <br />
  -*MWA range*: 0, 1 (or anything else captured in the `else` statement), 2 <br />
  -*PAPER range*: 1 (or anything else captured in the `else` statement), 2 <br />
  -*HERA range*: automatically defaults <br />

**beam_offset_time**: calculate the beam at a specific time within the observation. 0 seconds indicates the start of the observation, and the # of seconds in an observation indicates the end of the observation. <br />
  -*EoR_firstpass settings*: 56 <br />
  -*Default*: 0 <br />
  -*Range*: 0-# of seconds in an observation <br />

**complex_beam**: set if the beam is complex. Affects how gridding is handled. <br />
  -*Turn off/on*: 0/1 <br />
  -*Default*: 1 <br />

**dipole_mutual_coupling_factor**: allows a modification to the beam as a result of mutual coupling between dipoles calculated in `mwa_dipole_mutual_coupling.pro` (See Sutinjo 2015 for more details). <br />
  -*Needs updating*: calculation is done for the MWA setup, even if a different instrument is being used if `dipole_mutual_coupling_factor` is set. Needs to have an extra check !Q<br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: 1 <br />

**nfreq_avg**: the number of fine frequency channels to calculate a beam for, using the average of the frequencies. The beam is a function of frequency, and a calculation on the finest level is most correct (nfreq_avg=1). However, this is computationally difficult for most machines. <br />
  -*EoR_firstpass settings*: 16 <br />
  -*Default*: 1 <br />
  -*Range*: 1-# of frequency channels, as long as it evenly divides the # of frequency channels <br />
  
**psf_resolution** : !Q


## Calibration

**allow_sidelobe_cal_sources**: allows FHD to calibrate on sources in the sidelobes. Forces the beam_threshold to 0.01 in order to go down to 1% of the beam to capture sidelobe sources during the generation of a calibration source catalog for the particular observation. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: 1 !Q <br />
  
**cable_bandpass_fit**: average the calibration solutions across tiles within a cable grouping for the particular instrument. <br />
  -*Dependency*: instrument_config/<instrument>_cable_length.txt <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: 1 !Q <br />
  
**cal_amp_degree_fit**: the order of the polynomial fit over the whole band to create calibration solutions for the amplitude of the gain. Setting it to 0 gives a 0th order polynomial fit (one number for the whole band), 1 gives a 1st order polynomial fit (linear fit), 2 gives a 2nd order polynomial fit (quadratic), etc etc. <br />
  -*Dependency*: calibration_polyfit must be on for the polynomial fitting to occur. <br />
  -*Turn off/on*: undefined/defined <br />
  -*EoR_firstpass settings*: 2 <br />
  -*Default*: 2 !Q <br />
  
**cal_mode_fit**: Determines whether calibration will fit for reflection in cables (see following three entries). This will be set if
	`cal_cable_reflection_correct` or `cal_cable_reflection_fit` is set. <br />
  -*Obsolete* - use `cal_cable_reflection_correct` or `cal_cable_reflection_mode_fit` instead. <br />
  
**cal_phase_degree_fit**: the order of the polynomial fit over the whole band to create calibration solutions for the phase of the gain. Setting it to 0 gives a 0th order polynomial fit (one number for the whole band), 1 gives a 1st order polynomial fit (linear fit), 2 gives a 2nd order polynomial fit (quadratic), etc etc. <br />
  -*Dependency*: calibration_polyfit must be on for the polynomial fitting to occur. <br />
  -*Turn off/on*: undefined/defined <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: 1 !Q <br />
  
**cal_cable_reflection_correct**: Use predetermined cable reflection parameters in calibration solutions. <br />
  -*Needs updating*: all cable keywords need a major overhaul <br />
  -Set to 1 to correct all antennas using default tile for instrument (e.g. `FHD/instrument_config/mwa_cable_reflection_coefficients.txt`) <br />
  -OR set to cable length or array of cable lengths to only correct those lengths (e.g. [90,150]), again using default file. <br />
  -OR set to file path with reflection coefficients. <br />
  -*EoR_firstpass settings*: unset (off) <br />
  -*Default*: unset (off) !Q <br />
  
**cal_cable_reflection_fit**: calculate theoretical cable reflection modes given the velocity and length data stored in a config file. <br />
  -*Needs updating*: all cable keywords need a major overhaul <br />
  -Set to length of cable to fit, or negative length of cable to omit. <br />
  -*Must be used in conjunction with `cal_cable_reflection_mode_fit`* <br />
  -*EoR_firstpass settings*: 150 <br />
  -*Default*: 150 !Q <br />
  
**cal_cable_reflection_mode_fit**: Fits residual gains to reflection mode and coefficient. <br />
  -*Needs updating*: all cable keywords need a major overhaul <br />
  -Takes precidence over `cal_cable_reflection_correct`. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 150 <br />
  -*Default*: 150 !Q <br />
  
**calibrate_visibilities**: turn on or turn off calibration of the visibilities. If turned on, calibration of the dirty, modelling, and subtraction to make a residual occurs. Otherwise, none of these occur and an uncalibrated dirty cube is output. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: 1 !Q <br />
  
**calibration_subtract_sidelobe_catalog**: set to subtract a catalog in the sidelobes that is different from that used in the primary beam <br />
  
**calibration_flux_threshold**: this sets an lower exclusion threshold in flux (Jy) for the calibration sources. If the flux threshold is negative, then it is treated as a upper exlusion threshold in flux (Jy). <br />
  -*EoR_firstpass settings*: not set <br />
  -*Default*: not set <br />
  
**diffuse_calibrate**: a map/model of the diffuse in which to calibrate on. The map/model undergoes a DFT for every pixel, and the contribution from every pixel is added to the model visibilities from which to calibrate on. If no diffuse_model is specified, then this map/model is used for the subtraction model as well. <br />
  -*EoR_firstpass settings*: filepath('EoR0_diffuse_model_94.sav',root=rootdir('FHD'),subdir='catalog_data') <br />
  -*Default*: undefined (off) <br />
  
**max_calibration_sources**: limits the number of sources used in the calibration. Sources are weighted by apparent brightness before applying the cut. Note that extended sources with many associated source components count as only a single source. <br />
  -*Dependency*: The sources are also included in the model if `return_cal_visibilities` is set.
  -*EoR_firstpass settings*: 20000 <br />
  -*Default*: All valid sources in the catalog are used. <br />
  
**saved_run_bp**: use a saved bandpass for bandpass calibration. Reads in a text file saved in instrument config which is dependent on pointing number at the moment. Needs updating. <br />
  -*Needs updating*: File name needs more information to descriminate between instruments and bands. Need to have capability to read in saved bandpasses not dependent on cable type.<br />
  -*Dependency*: instrument_config/<pointing number>_bandpass.txt <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: undefined (off) <br />

**max_cal_baseline**: the maximum baseline length in wavelengths to be used in calibration. If max_baseline is smaller, it will be used instead. <br />
  -*EoR_firstpass settings*: not set <br />
  -*Default*: equal to max_baseline <br />
  
**min_cal_baseline**: the minimum baseline length in wavelengths to be used in calibration. <br />
  -*EoR_firstpass settings*: 50 <br />
  -*Default*: 50 !Q <br />
  
**return_cal_visibilities**: saves the visibilities created for calibration for use in the model. If `model_visibilities` is set to 0, then the calibration model visibilities and the model visibilities will be the same if `return_cal_visibilities` is set. If `model_visibilities` is set to 1, then any new modelling (of more sources, diffuse, etc.) will take place and the visibilities created for the calibration model will be added. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: 1 !Q <br />

**transfer_calibration**: the file path of a calibration to be read-in. The string can be: a directory where a <obsid>_cal.sav is located, the full file path with the obsid (file/path/<obsid>), the full file path to a sav file, the full file path to txt file, the full file path to a npz file, or the full file path to a npy file. (Which formats is the gain array expected in for these file types? !Q). Note that this will calibrate, but not generate a model. <br />
  -*Needs updating*: will not generate a model for subtraction in the current setup. <br />
  -*EoR_firstpass settings*: not set <br />
  -*Default*: not set <br />

catalog_file_path=filepath('MRC_full_radio_catalog.fits',root=rootdir('FHD'),subdir='catalog_data') <br />
calibration_catalog_file_path=filepath('mwa_calibration_source_list.sav',root=rootdir('FHD'),subdir='catalog_data') <br />
bandpass_calibrate=1 <br />
calibration_polyfit=2 <br />
no_restrict_cal_sources=1 <br /> 

**include_catalog_sources**: !Q <br />
   -*Default* : 0 <br />

## Deconvolution

**deconvolve**: run fast holgraphic deconvolution. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 0 <br />
  -*Default*: 0 <br />
  
**deconvolution_filter**:  !Q <br />
  -*EoR_firstpass settings*: filter_uv_uniform <br />
  -*Default*: filter_uv_uniform !Q <br />
  
**gain_factor**: a percent amount to add to the flux of a given source to compensate for not capturing all flux in deconvolution components. <br />
  -*EoR_firstpass settings*: <br />
  -*Default*: !Q <br />
  
**max_sources**: the number of source components allowed to be found in fast holographic deconvolution. Not used outside of deconvolution. <br />
  -*EoR_firstpass settings*: 20000 <br />
  -*Default*: 20000 !Q <br />
  
**return_decon_visibilities**: <br />

**return_sidelobe_catalog**: include sidelobes sources from the `subtract_sidelobe_catalog` in the source list. This will include the sidelobe sources in foreground subtraction. <br />
  -*Dependency*: `deconvolve` must be set to 1 in order for the keyword to take effect. `subtract_sidelobe_catalog` must also be set. <br />
  -*EoR_firstpass settings*: not set <br />
  -*Default*: not set <br />

**subtract_sidelobe_catalog**: a catalog to subtract sources from the sidelobes before deconvolution. <br />
  -*Dependency*: `deconvolve` must be set to 1 in order for the keyword to take effect. <br />
  -*EoR_firstpass settings*: not set <br />
  -*Default*: not set <br />


## Diffuse

## In situ simulation

**enhance_eor**: input a multiplicative factor to boost the signal of the EoR in the dirty input visibilities. <br />
  -*Dependency*: `eor_savefile` must be set to an EoR sav file path in order for the keyword to take effect. <br />
  -*EoR_firstpass settings*: not set <br />
  -*Default*: not set<br />

**eor_savefile**: input a path to a savefile of EoR visibilities to include the EoR in the dirty input visibilities. <br />
  -*Dependency*: `in_situ_input` must be set to 1 or a sav file path in order for the keyword to take effect. <br />
  -*EoR_firstpass settings*: not set <br />
  -*Default*: not set<br />

**in_situ_sim_input**: run an in situ simulation, where model visibilities are made and input as the dirty visibilities (see Barry et. al. 2016 for more information on use-cases). Setting to 1 forces the visibilities to be made within the current run. Setting to a sav file path inputs model visibilities from a previous run, which is the preferred method since that run is independently documented.<br />
  -*EoR_firstpass settings*: not set <br />
  -*Default*: not set<br />
  
**sim_noise**: add a uncorrelated thermal noise to the input dirty visibilities from a specified sav file, or create them for the run. <br />
  -*Dependency*: `in_situ_input` must be set to 1 or a sav file path in order for the keyword to take effect. <br />
  -*EoR_firstpass settings*: not set <br />
  -*Default*: not set<br />
  

## Model

**allow_sidelobe_model_sources**: allows FHD to model sources in the sidelobes for subtraction. Forces the beam_threshold to 0.01 in order to go down to 1% of the beam to capture sidelobe sources during the generation of amodel alibration source catalog for the particular observation. <br />
  -*Dependency*: `model_visibilities` must be set to 1 in order for the keyword to take effect. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: Additional sidelobe sources are not included. If `return_cal_visibilities` and `allow_sidelobe_cal_sources` are both set, then the model will include those sources in the sidelobes that were used in calibration. <br />
  -*Default*: Additional sidelobe sources are not included. If `return_cal_visibilities` and `allow_sidelobe_cal_sources` are both set, then the model will include those sources in the sidelobes that were used in calibration. !Q <br />

**max_model_sources**: limits the number of sources used in the model. Sources are weighted by apparent brightness before applying the cut. Note that extended sources with many associated source components count as only a single source. <br />
  -*Dependency*: `model_visibilities` must be set to 1 in order for the keyword to take effect. If `return_cal_visibilities` is set, then the final model will include all calibration sources and all model sources (duplicates are caught and included only once). <br />
  -*EoR_firstpass settings*: not set <br />
  -*Default*: All valid sources are used. !Q <br />

**model_catalog_file_path**: a catalog of sources to be used to make model visibilities for subtraction. <br />
  -*Dependency*: `model_visibilities` must be set to 1 in order for the keyword to take effect.  <br />
  -*EoR_firstpass settings*: filepath('mwa_calibration_source_list.sav',root=rootdir('FHD'),subdir='catalog_data') <br />
  -*Default*: not set <br />
  
**model_flux_threshold**: this sets an lower exclusion threshold in flux (Jy) for the model sources. If the flux threshold is negative, then it is treated as a upper exlusion threshold in flux (Jy). <br />
  -*Dependency*: `model_visibilities` must be set to 1 in order for the keyword to take effect.  <br />
  -*EoR_firstpass settings*: not set <br />
  -*Default*: not set <br />

**model_visibilities**: make visibilities for the subtraction model separately from the model used in calibration. This is useful if the user sets keywords to make the subtraction model different from the model used in calibration. If not set, the model used for calibration is the same as the subtraction model. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 0 <br />
  -*Default*: 0 <br />

## Export
  
**export_images**: export fits files and images of the sky. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: 1 <br /> 
  
**snapshot_healpix_export**: appears to be preserving visibilities !Q<br />
  -*EoR_firstpass settings*: 1 <br />
  
**no_fits**: do not export fits files of the sky. This typically saves ~20Mb of memory for every fits file, which by default there are 16 for two polarizations. <br />
  -*Needs updating*: might be better to change the logic (avoid the double negative) !Q. <br />
  -*Dependency*: `export_images` must be set to 1 in order for the keyword to take effect.  <br />
  -*Turn off/on*: 0/1 <br />
  -*Default*: 0 <br /> 

**pad_uv_image**: pad the UV image by this factor with 0's along the outside so that output images are at a higher resolution. <br />
  -*EoR_firstpass settings*: 1. <br />
  -*Default*: 1. <br /> 
  
**ps_export**: not used !Q<br />
  -*EoR_firstpass settings*: 0 <br />
  -*Default*:  <br /> 
  
**restrict_hpx_inds**: only allow gridding of the output healpix cubes to include the healpix pixels specified in a save file. This is useful for restricting many observations to have consistent healpix pixels during integration, and saves on memory and walltime. Set to a string to specify the name of the save file in the Observations subdirectory. <br />
  -*EoR_firstpass settings*: 1, which defaults to `EoR0_high_healpix_inds.idlsave`, `EoR0_low_healpix_inds.idlsave`, `EoR1_high_healpix_inds.idlsave`, or `EoR1_low_healpix_inds.idlsave` depending on obs parameters.<br />
  -*Default*: not set <br /> 

**save_visibililties**: save the calibrated data visibilities, the model visibilities, and the visibility flags. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: 1 probably <br /> 

**silent**: do not print messages. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 0 <br />
  -*Default*: 0 probably <br /> 
  
**split_ps_export**: split up the Healpix outputs into even and odd time samples. This is essential to propogating errors in &epsilon;ppsilon. <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: not set <br /> 

## Flagging

**dead_dipole_list**: an array of 3 x # of dead dipoles, where column 0 is the tile name, column 1 is the polarization (0:x, 1:y), and column 2 is the dipole number. These dipoles are flagged, which greatly increases memory usage due to the creation of many separate tile beams. <br />
  -*Default*: not set <br />  

**flag_calibration**: flags antennas based on calculations in `vis_calibration_flag.pro`.<br />
  -*Needs update*: keyword check both in general_obs and vis_calibrate !Q <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: not set <br />
  -*Default*: 1 <br />  

**flag_dead_dipoles**: flag the dead dipoles listed in `<instrument>_dead_dipole_list.txt` for the golden set of Aug 23, 2013. This greatly increases memory usage due to the creation of many separate tile beams. <br />
  -*Default*: not set <br />  

**flag_visibilities**: flag visibilities based on calculations in `vis_flag.pro`. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 0 <br />
  -*Default*: 0 <br />  

**no_calibration_frequency_flagging**: do not flag frequencies based off of zeroed calibration gains. <br />
  -*Needs updating*: might be better if changed to calibration_frequency_flagging and change the logic (avoid the double negative) !Q.
  -*Turn off/on*: 0/1 (flag/don't flag) <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: not set <br />

**tile_flag_list**: a string array of tile names to manually flag tiles. Note that this is an array of tile names, not tile indices! <br />
  -*Default*: not set <br />

**time_cut**: seconds to cut (rounded up to next time integration step) from the beginning of the observation. Can also specify a negative time to cut off the end of the observation. Specify a vector to cut at both the start and end. <br />
  -*Default*: not set <br />

**unflag_all**: unflag all tiles/antennas and frequencies. While not practical for real data, this is useful for creating unflagged model visibilities for the input of an in-situ simulations. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 0 <br />
  -*Default*: 0 <br />

## Instrument Parameters

**alt**: altitude of the instrument, in meters.  <br />
  -*Default*: 377.827 (MWA, from Tingay et al. 2013)<br />
  
**lat**: latitude of the instrument, in decimal degrees. <br />
  -*Default*: -26.7033194 (MWA, from Tingay et al. 2013)<br /> 
  
**lon**: longitude of the instrument, in decimal degrees.  <br />
  -*Default*: 116.67081524 (MWA, from Tingay et al. 2013)<br />

**no_rephase**: set to use the observation phase center rather than the predefined phase center in the metafits. This forces the phase center to be determined from the header keywords RA and Dec, rather than RAPHASE and DECPHASE. <br />
  -*needs updating*: double logical negative !Q  <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: 0 <br />

**override_target_phasera**: RA of the target phase center, which overrides the value supplied in the metafits under the header keyword RAPHASE. If the metafits doesn't exist, it ovverides the value supplied in the uvfits under the header keyword RA.<br />
  -*Default*: not set<br />

**override_target_phasedec**: dec of the target phase center, which overrides the value supplied in the metafits under the header keyword DECPHASE. If the metafits doesn't exist, it ovverides the value supplied in the uvfits under the header keyword Dec.<br />
  -*Default*: not set<br />
  
**rephase_weights**: if turned off, target phase center is the pointing center (as defined by Cotter). Setting rephase_weights=0 overrides override_target_phasera and override_target_phasedec. <br />
  -*Turn off/on*: 0/1 <br />
  -*Default*: 1 <br />


## Import

**uvfits_version**: the version number of the uvfits. `eor_firstpass_versions.pro` will use this keyword in conjuction with `uvfits_subversion` to query the mwa_qc database to find the location of the uvfits file on the MIT cluster. See `uvfits_subversion` for the current uvfits versions available. <br />
  -*Range*: currently 3 to 5 <br />
  -*EoR_firstpass settings*: 4 <br />

**uvfits_subversion**: the subversion number of the uvfits. `eor_firstpass_versions.pro` will use this keyword in conjuction with `uvfits_version` to query the mwa_qc database to find the location of the uvfits file on the MIT cluster. Here are the available uvfits versions, ordered by version number and subversion number: 3,3 was used to test compressed fits; 3,4 was a rerun of 3,1 with a newer version of cotter before that version was recorded; 4,0 went back to old settings for an industrial run; 4,1 was the same as 4,0 but for running on compressed gpubox files; 5,0 was a test to phase all obs to zenith (phasing needs to be added per obs currently); 5,1 incorperates flag files and runs cotter without the bandpass applied, with all the other default settings. Many of these uvfits versions were removed for space reasons, and so only 4,1 and 5,0 are reliable.<br />
  -*Range*: currently 0 to 4 <br />
  -*EoR_firstpass settings*: 1 <br />


## Recalculation

**recalculate_all**: forces FHD to recalculate all values instead of reading in save files. This is the same as setting `mapfn_recalculate`, `healpix_recalculate` , ... to 1. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 0 <br />
  -*Default*: 0 <br />

mapfn_recalculate=0
healpix_recalculate=0

## Resolution

**dimension**: the number of pixels in the UV plane along one axis. <br />
  -*EoR_firstpass settings*: 2048 <br />
  -*Default*: 2 to the power of the rounded result of log&#8322;(k_span/k_binsize).<br />

**kbinsize**: size of UV pixels in wavelengths. Given a defined number of pixels in `dimension`, this sets the UV space extent. This will supersede `degpix` if `dimension` is also set. <br />
  -*Dependency*: will only go into effect if `FoV` is not set. <br />
  -*EoR_firstpass settings*: 0.5 <br />
  -*Default*: 0.5 if `FoV` not set !Q <br />

**FoV**: the field of view in degrees, which determines the UV resolution given a defined number of pixels in `dimension`. If set to 0, then `kbinsize` determines the UV resolution. <br />
  -*EoR_firstpass settings*: 0 <br />
  -*Default*: not set <br />

**max_baseline**: the maximum baseline length in wavelengths to include in the analysis. <br />
  -*EoR_firstpass settings*: net set <br />
  -*Default*: the maximum baseline length in wavelengths of the instrument, specifically calculated from the params structure  <br />

**min_baseline**: the minimum baseline length in wavelengths to include in the analysis. <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: the minimum baseline length in wavelengths of the instrument, specifically calculated from the params structure. This includes autocorrelations (!Q is that right Ian?) <br />
  



ps_kbinsize=0.5
ps_kspan=600.


cleanup=0
combine_healpix=0
deconvolve=0

vis_baseline_hist=1
calibration_visibilities_subtract=0

n_avg=2
ps_kbinsize=0.5
ps_kspan=600.
image_filter_fn='filter_uv_uniform'
deconvolution_filter='filter_uv_uniform'

max_sources=20000
no_ps=1

ring_radius=10.*pad_uv_image
combine_obs=0
smooth_width=32.

psf_resolution=100
calibration_flag_iterate = 0


