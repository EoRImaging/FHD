# FHD Keyword Dictionary
FHD uses keywords to create unique run-specific settings. This dictionary describes the purpose of each keyword, as well as their logic or applicable ranges. Some keywords can override others, which is also documentated. The FHD default is listed when applicable, which can be overriden by EoR_firstpass settings in the top-level script eor_firstpass_version.pro.

This is a work in progress; please add keywords as you find them in alphabetical order with their corresponding definition.

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

**complex_beam**: !Q <br />
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
  
**min_cal_baseline**: the minimum baseline length in wavelengths to be used in calibration. <br />
  -*EoR_firstpass settings*: 50 <br />
  -*Default*: 50 !Q <br />
  
**return_cal_visibilities**: saves the visibilities created for calibration for use in the model. If `model_visibilities` is set to 0, then the calibration model visibilities and the model visibilities will be the same if `return_cal_visibilities` is set. If `model_visibilities` is set to 1, then any new modelling (of more sources, diffuse, etc.) will take place and the visibilities created for the calibration model will be added. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: 1 !Q <br />


catalog_file_path=filepath('MRC_full_radio_catalog.fits',root=rootdir('FHD'),subdir='catalog_data') <br />
calibration_catalog_file_path=filepath('mwa_calibration_source_list.sav',root=rootdir('FHD'),subdir='catalog_data') <br />
bandpass_calibrate=1 <br />
calibration_polyfit=2 <br />
no_restrict_cal_sources=1 <br /> 

**include_catalog_sources**: !Q <br />
   -*Default* : 0

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

**subtract_sidelobe_catalog**: a catalog to subtract sources from the sidelobes before deconvolution. <br />
  -*Dependency*: `deconvolve` must be set to 1 in order for the keyword to take effect. <br />
  -*EoR_firstpass settings*: not set <br />
  -*Default*: not set <br />


## Diffuse

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

**model_visibilities**: make visibilities for the subtraction model separately from the model used in calibration. This is useful if the user sets keywords to make the subtraction model different from the model used in calibration. If not set, the model used for calibration is the same as the subtraction model. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 0 <br />
  -*Default*: 0 <br />

## Export
  
**snapshot_healpix_export**: <br />
  -*EoR_firstpass settings*: 1 <br />

**pad_uv_image**: pad the UV image by this factor with 0's along the outside so that output images are at a higher resolution. <br />
  -*EoR_firstpass settings*: 1. <br />
  -*Default*: 1. <br /> 
  
**ps_export**: not used !Q<br />
  -*EoR_firstpass settings*: 0 <br />
  -*Default*:  <br /> 
  
**split_ps_export**: split up the Healpix outputs into even and odd time samples. This is essential to propogating errors in &epsilon;ppsilon. <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: not set <br /> 

## Flagging

**dead_dipole_list**: an array of 3 x # of dead dipoles, where column 0 is the tile name, column 1 is the polarization (0:x, 1:y), and column 2 is the dipole number. These dipoles are flagged, which greatly increases memory usage due to the creation of many separate tile beams. <br />
  -*Default*: not set <br />  

**flag_dead_dipoles**: flag the dead dipoles listed in `<instrument>_dead_dipole_list.txt` for the golden set of Aug 23, 2013. This greatly increases memory usage due to the creation of many separate tile beams. <br />
  -*Default*: not set <br />  

**no_calibration_frequency_flagging**: do not flag frequencies based off of zeroed calibration gains. <br />
  -*Needs updating*: might be better if changed to calibration_frequency_flagging and change the logic (avoid the double negative) !Q.
  -*Turn off/on*: 0/1 (flag/don't flag) <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: not set <br />

**tile_flag_list**: a string array of tile names to manually flag tiles. Note that this is an array of tile names, not tile indices! <br />
  -*Default*: not set <br />

**time_cut**: seconds to cut (rounded up to next time integration step) from the beginning of the observation. Can also specify a negative time to cut off the end of the observation. Specify a vector to cut at both the start and end. <br />
  -*Default*: not set <br />

## Instrument Parameters

**alt**: altitude of the instrument, in meters.  <br />
  -*Default*: 377.827 (MWA, from Tingay et al. 2013)<br />
  
**lat**: latitude of the instrument, in decimal degrees. <br />
  -*Default*: -26.7033194 (MWA, from Tingay et al. 2013)<br /> 
  
**lon**: longitude of the instrument, in decimal degrees.  <br />
  -*Default*: 116.67081524 (MWA, from Tingay et al. 2013)<br />

**override_target_phasera**: RA of the target phase center, which overrides the value supplied in the metafits under the header keyword RAPHASE. If the metafits doesn't exist, it ovverides the value supplied in the uvfits under the header keyword RA.<br />
  -*Default*: not set<br />

**override_target_phasedec**: dec of the target phase center, which overrides the value supplied in the metafits under the header keyword DECPHASE. If the metafits doesn't exist, it ovverides the value supplied in the uvfits under the header keyword Dec.<br />
  -*Default*: not set<br />

## Import

uvfits_version=4
uvfits_subversion=1

## Recalculation

**recalculate_all**: forces FHD to recalculate all values instead of reading in save files. This is the same as setting `mapfn_recalculate`, `healpix_recalculate` , ... to 1. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 0 <br />
  -*Default*: 0 <br />

mapfn_recalculate=0
healpix_recalculate=0


cleanup=0
combine_healpix=0
deconvolve=0

flag_visibilities=0
vis_baseline_hist=1
silent=0
save_visibilities=1
calibration_visibilities_subtract=0

n_avg=2
ps_kbinsize=0.5
ps_kspan=600.
image_filter_fn='filter_uv_uniform'
deconvolution_filter='filter_uv_uniform'

dimension=2048
max_sources=20000
FoV=0
no_ps=1
min_baseline=1.

ring_radius=10.*pad_uv_image
no_rephase=1
combine_obs=0
smooth_width=32.


restrict_hpx_inds=1

kbinsize=0.5
psf_resolution=100
calibration_flag_iterate = 0

; even newer defaults
export_images=1


