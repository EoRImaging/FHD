# FHD Keyword Dictionary
FHD uses keywords to create unique run-specific settings. This dictionary describes the purpose of each keyword, as well as their logic or applicable ranges. Some keywords can override others, which is also documentated. The FHD default is listed when applicable, which can be overriden by EoR_firstpass settings in the top-level script eor_firstpass_version.pro.

This is a work in progress; please add keywords as you find them in alphabetical order with their corresponding definition. If there's a question about a definition or keyword, label it with !Q.

## Beam

**beam_cal_threshold**: the fractional power response relative to the peak from which to include sources for calibration.<br />
  -*Default*: 0.05, or 0.01 if `allow_sidelobe_sources` set <br />

**beam_mask_threshold**: the factor at which to clip the beam model. For example, a factor of 100 would clip the beam model at 100x down from the maximum value. This removes extraneous and uncertain modelling at low levels.  <br />
  -*Default*: 100 <br />

**beam_model_threshold**: the fractional power response relative to the peak from which to include sources for the model.<br />
  -*Dependency*: `model_visibilities` must be set <br />
  -*Default*: 0.05, or 0.01 if `allow_sidelobe_sources` set <br />

**beam_model_version**: a number that indicates the tile beam model calculation. This is dependent on the instrument, and specific calculations are carried out in `<instrument>_beam_setup_gain.pro`. For the MWA, there are currently three options: 0) !Q, 1) a Hertzian dipole as prescribed by Cheng 1992 and Balanis 1989, 2) the average embedded element model from Sutinjo 2015. For PAPER, there are currently two options: 1) !Q, 2) !Q. For HERA, there are currently two options: 2) 2016 version by Dave Deboer saved as cross and co-pol, 0) an earlier version !Q saved as X and Y pols. <br />
  -*EoR_firstpass settings*: 2 <br />
  -*Default*: 1 <br />
  -*MWA range*: 0, 1 (or anything else captured in the `else` statement), 2 <br />
  -*PAPER range*: 1 (or anything else captured in the `else` statement), 2 <br />
  -*HERA range*: 2 (or anything else captured in the `else` statement) <br />

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

**inst_tile_ptr**: a pointer array to designate which tile indices belong to which instrument. The order of the pointer array is assumed to match the order of instruments specified in the keyword string array `instrument`. Only used if there is more than one instrument supplied. Tiles are numbered from 0. <br />
  -*Example*: `inst_tile_ptr = PTRARR(2,/allocate)`<br />
  `*inst_tile_ptr[0] = [0,1,2,3]`<br />
  `*inst_tile_ptr[1] = [4,5,6]`<br />
  -*Default*: not set <br />

**interpolate_beam_threshold**: set to smoothly interpolate the UV beam model to zero (in amplitude) beyond the clip set by `beam_mask_threshold`. <br />
  -*Turn off/on*: 0/1 <br />
  -*Default*: Not set (same as 0) <br />

**nfreq_avg**: the number of fine frequency channels to calculate a beam for, using the average of the frequencies. The beam is a function of frequency, and a calculation on the finest level is most correct (nfreq_avg=1). However, this is computationally difficult for most machines. <br />
  -*EoR_firstpass settings*: 16 <br />
  -*Default*: 1 <br />
  -*Range*: 1-# of frequency channels, as long as it evenly divides the # of frequency channels <br />
  
**psf_resolution**: super-resolution factor of the psf. The psf will be interpolated to a grid of dimension (psf_superres_dim,psf_superres_dim). !Q <br />
  -*Default*: 16 <br />

**transfer_psf**: filepath to the FHD beams directory with the same obsid's psf structure (i.e. `/path/to/FHD/dir/fhd_nb_test/beams`). That psf structure is used instead of calculating a new one. The obs structure from that FHD directory is also used to provide the beam_integral. <br />
  -*Default*: not set <br />


## Calibration

**allow_sidelobe_cal_sources**: allows FHD to calibrate on sources in the sidelobes. Forces the beam_threshold to 0.01 in order to go down to 1% of the beam to capture sidelobe sources during the generation of a calibration source catalog for the particular observation. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: 1 !Q <br />
  
**bandpass_calibrate**: calculates a bandpass. This is an average of tiles by frequency by polarization (default), beamformer-to-LNA cable types by frequency by polarization (see `cable_bandpass_fit`), or over the whole season by pointing by by cable type by frequency by polarization via a read-in file (see `saved_run_bp`). If unset, no by-frequency bandpass is used. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: 1 <br />  
  
**cable_bandpass_fit**: average the calibration solutions across tiles within a cable grouping for the particular instrument. <br />
  -*Dependency*: instrument_config/\<instrument\>_cable_length.txt <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: 1 !Q <br />
  
**cal_amp_degree_fit**: the order of the polynomial fit over the whole band to create calibration solutions for the amplitude of the gain. Setting it to 0 gives a 0th order polynomial fit (one number for the whole band), 1 gives a 1st order polynomial fit (linear fit), 2 gives a 2nd order polynomial fit (quadratic), etc etc. <br />
  -*Dependency*: calibration_polyfit must be on for the polynomial fitting to occur. <br />
  -*Turn off/on*: undefined/defined <br />
  -*EoR_firstpass settings*: 2 <br />
  -*Default*: 2 !Q <br />
  
**cal_bp_transfer**: use a saved bandpass for bandpass calibration. Read in the specified file (.sav, .txt, or .fits), with calfits format greatly preferred. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: if set to 1, becomes `mwa_eor0_highband_season1_cable_bandpass.fits` <br />
  
**cal_convergence_threshold**: threshold at which calibration ends. <br />
  -*Default*: !Q

**cal_mode_fit**: determines whether calibration will fit for reflection in cables. This will be set if
	`cal_reflection_mode_file`, `cal_reflection_mode_theory`, `cal_reflection_mode_delay`, or `cal_reflection_hyperresolve` is set. Setting it to a positive cable length (scalar or array) will specifically include the associated tiles for fitting. Setting it to a negative cable length (scalar or array) will specifically exclude the associated tiles from fitting. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: undefined <br />
  -*Default*: 1 <br />
  
**cal_phase_degree_fit**: the order of the polynomial fit over the whole band to create calibration solutions for the phase of the gain. Setting it to 0 gives a 0th order polynomial fit (one number for the whole band), 1 gives a 1st order polynomial fit (linear fit), 2 gives a 2nd order polynomial fit (quadratic), etc etc. <br />
  -*Dependency*: calibration_polyfit must be on for the polynomial fitting to occur. <br />
  -*Turn off/on*: undefined/defined <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: 1 !Q <br />

  
**cal_reflection_hyperresolve**: hyperresolve and fit residual gains using nominal reflection modes (calculated from `cal_reflection_mode_delay` or `cal_reflection_mode_theory`) , producing a finetuned mode fit, amplitude, and phase. Will be ignored if `cal_reflection_mode_file` is set because it is assumed that a file read-in contains mode/amp/phase to use. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: not set <br />
  
**cal_reflection_mode_delay**: calculate cable reflection modes by Fourier transforming the residual gains, removing modes contaminated by frequency flagging, and choosing the maximum mode. <br />
  -*EoR_firstpass settings*: not set <br />
  -*Default*: not set <br />

**cal_reflection_mode_file**: use predetermined cable reflection parameters (mode, amplitude, and phase) in the calibration solutions. Set to 1 to correct antennas using default tile for instrument (e.g. `FHD/instrument_config/mwa_cable_reflection_coefficients.txt`) or set to a filepath with the reflection coefficients. The specified format of the text file must have one header line and eleven columns (tile index, tile name, cable length, cable velocity factor, logic on whether to fit (1) or not (0), mode for X, amplitude for X, phase for X, mode for Y, amplitude for Y, phase for Y). See `vis_cal_polyfit.pro` for units. <br />
  -*EoR_firstpass settings*: undefined <br />
  -*Default*: undefined <br />
  
**cal_reflection_mode_theory**: calculate theoretical cable reflection modes given the velocity and length data stored in a config file named `<instrument>_cable_length.txt`. File must have a header line and at least five columns (tile index, tile name, cable length, cable velocity factor, logic on whether to fit (1) or not (0)). Can set it to positive/negative cable lengths (see `cal_mode_fit`) to include/exclude certain cable types. <br />
  -*EoR_firstpass settings*: 150 <br />
  -*Default*: not set <br />
  
**cal_stop**: stops the code right after calibration, and saves unflagged model visibilities along with the obs structure in a folder called cal_prerun in the FHD file structure. This allows for post-processing calibration steps like multi-day averaging, but still has all of the needed information for minimal reprocessing to get to the calibration step. To run a post-processing run, see keywords `model_transfer` and `transfer_psf`.<br />
  -*Turn off/on*: 0/1 <br />
  -*Default*: 0 <br />

**cal_time_average**: performs a time average of the model/data visibilities over the time steps in the observation to reduce the number of equations that are used in the linear-least squares solver. This improves computation time, but will downweight longer baseline visibilities due to their faster phase variation. <br />
  -*Turn off/on*: 0/1 <br />
  -*Default*: 1 <br />
  
**calibrate_visibilities**: turn on or turn off calibration of the visibilities. If turned on, calibration of the dirty, modelling, and subtraction to make a residual occurs. Otherwise, none of these occur and an uncalibrated dirty cube is output. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: 1 !Q <br />
  
**calibration_auto_fit**: use the autocorrelations to calibrate. This will suffer from increased, correlated noise and bit statistic errors. However, this will save the autos as the gain in the cal structure, which can be a useful diagnostic. <br />
  -*Turn off/on*: 0/1 <br />
  -*Default*: 0 <br />  
  
**calibration_subtract_sidelobe_catalog**: set to subtract a catalog in the sidelobes that is different from that used in the primary beam <br />
  
**calibration_flux_threshold**: this sets an lower exclusion threshold in flux (Jy) for the calibration sources. If the flux threshold is negative, then it is treated as a upper exlusion threshold in flux (Jy). <br />
  -*EoR_firstpass settings*: not set <br />
  -*Default*: not set <br />
  
**calibration_polyfit**: calculates a polynomial fit across the frequency band for the gain, and allows a cable reflection to be fit. The orders of the polynomial fit are determined by `cal_phase_degree_fit` and `cal_amp_degree_fit`. If unset, no polynomial fit or cable reflection fit are used. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: 1 <br />  
  
**diffuse_calibrate**: a map/model of the diffuse in which to calibrate on. The map/model undergoes a DFT for every pixel, and the contribution from every pixel is added to the model visibilities from which to calibrate on. If no diffuse_model is specified, then this map/model is used for the subtraction model as well. <br />
  -*EoR_firstpass settings*: filepath('EoR0_diffuse_model_94.sav',root=rootdir('FHD'),subdir='catalog_data') <br />
  -*Default*: undefined (off) <br />
  
**digital_gain_jump_polyfit**:  perform polynomial fitting for the amplitude separately before and after the highband digital gain jump at 187.515E6. <br />
  -*Turn off/on*: 0/1 <br />
  -*Default*: undefined (off) <br />
  
**max_calibration_sources**: limits the number of sources used in the calibration. Sources are weighted by apparent brightness before applying the cut. Note that extended sources with many associated source components count as only a single source. <br />
  -*Dependency*: The sources are also included in the model if `return_cal_visibilities` is set. <br />
  -*EoR_firstpass settings*: 20000 <br />
  -*Default*: All valid sources in the catalog are used. <br />

**max_cal_baseline**: the maximum baseline length in wavelengths to be used in calibration. If max_baseline is smaller, it will be used instead. <br />
  -*Default*: equal to max_baseline <br />
  
**min_cal_baseline**: the minimum baseline length in wavelengths to be used in calibration. <br />
  -*EoR_firstpass settings*: 50 <br />
  -*Default*: 50 !Q <br />
  
**model_transfer**: filepath to the FHD directory with model visbilities of the same obsid to be used instead of recalculating (i.e. `/path/to/the/FHD/dir/fhd_nb_test/cal_prerun/vis_data`). This is currently only an option for when the calibration model visibilities are the same as the subtraction model visibilities. The model visibilities can't have been flagged (see `cal_stop` on how to generate unflagged model visbilities). <br />
  -*Default*: 50 <br />
  
**n_avg**: number of frequencies to average over to smooth the frequency band. <br />
  -*Default*: !Q <br />
  
**return_cal_visibilities**: saves the visibilities created for calibration for use in the model. If `model_visibilities` is set to 0, then the calibration model visibilities and the model visibilities will be the same if `return_cal_visibilities` is set. If `model_visibilities` is set to 1, then any new modelling (of more sources, diffuse, etc.) will take place and the visibilities created for the calibration model will be added. If n_pol = 4 (full pol mode), return_cal_visibilites must be set because the visibilites are required for calculating the mixing angle between Q and U. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: 1 <br />

**transfer_calibration**: the file path of a calibration to be read-in. The string can be: a directory where a \<obsid\>_cal.sav is located, the full file path with the obsid (file/path/\<obsid\>), the full file path to a sav file, the full file path to txt file, the full file path to a npz file, the full file path to a npy file, or the full file path to a fits file that adheres to calfits format. Note that this will calibrate, but not generate a model. Please set model visibility keywords separately to generate a subtraction model. <br />
  -*EoR_firstpass settings*: not set <br />
  -*Default*: not set <br />

catalog_file_path=filepath('MRC_full_radio_catalog.fits',root=rootdir('FHD'),subdir='catalog_data') <br />
calibration_catalog_file_path=filepath('mwa_calibration_source_list.sav',root=rootdir('FHD'),subdir='catalog_data') <br />
bandpass_calibrate=1 <br />
calibration_polyfit=2 <br />
no_restrict_cal_sources=1 <br /> 

**include_catalog_sources**: Adds sources in the file specified by catalog_file_path to the source_list for simulations (used in vis_simulate.pro) <br />
   -*Default* : 0 <br />
   -*If set to zero, and no source_array is passed to vis_simulate, then no point sources will be included in the simulation. Originally, sim_versions_wrapper would load the source array directly and pass it along to vis_simulate, then additional sources could be included via catalog_file_path. This feature has been turned off, so this parameter alone specified whether or not point sources will be included in simulations.* !Q

## Debug
WARNING! Options in this section may change without notice, and should never be turned on by default. <br />

### Beam debugging options

**debug_beam_clip_grow**: Set to grow the UV beam mask by one full-resolution pixel in all directions, after applying the clip set by `beam_mask_threshold` <br />
  -*Turn on*:: 1

**debug_clip_beam_mask**: Set to mask pixels in the UV beam model if the pixel would be masked for any super-resolution offset. <br />
  -*Turn on*: 1

**debug_beam_clip_floor**: Set to subtract the minimum non-zero value of the beam model from all pixels, and rescale to preserve normalization. <br />
  -*Turn on*: 1

## Deconvolution

**deconvolve**: run fast holgraphic deconvolution. <br />
  -*Turn off/on*: 0/1 <br />
  -*Default*: 0 <br />
  
**deconvolution_filter**: filter applied to images from deconvolution. <br />
  -*EoR_firstpass settings*: filter_uv_uniform <br />
  -*Default*: filter_uv_uniform <br />

**deconvolution_horizon_threshold**: degrees above the horizon to exclude from deconvolution. <br />
  -*Default*: 10 <br />

**filter_background**: filters out large-scale background fluctuations before deconvolving point sources. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: 1 <br />
  
**gain_factor**: a fractional amount to add to the flux of a given source to compensate for not capturing all flux in deconvolution components. <br />
  -*Default*: 0.15 <br />
  
**independent_fit**: fit source components in 4 Stokes parameters I, Q, U, V instead of only Stokes I. <br />
  -*Turn off/on*: 0/1 <br />
  -*Default*: 0 <br />
  
**max_sources**: the number of source components allowed to be found in fast holographic deconvolution. Not used outside of deconvolution. <br />
  -*EoR_firstpass settings*: 20000 <br />
  -*Default*: 20000 !Q <br />
  
**reject_pol_sources**: rejects source candidates that have a high Stokes Q to Stokes I ratio.<br />
  -*Needs updating*: not used in code! <br />
  -*Turn off/on*: 0/1 <br />
  -*Default*: 0 <br />
  
**return_decon_visibilities**: <br />

**return_sidelobe_catalog**: include sidelobes sources from the `subtract_sidelobe_catalog` in the component/source list. Source finding will be performed on the input sidelobe sources. This will also include the sidelobe sources in foreground subtraction. <br />
  -*Dependency*: `deconvolve` must be set to 1 in order for the keyword to take effect. `subtract_sidelobe_catalog` must also be set. <br />
  -*EoR_firstpass settings*: not set <br />
  -*Default*: not set <br />

**sigma_cut**: only include source components detected with signal to noise greater than the specified standard deviation. Also used when condensing components to sources after deconvolution. <br />
  -*Dependency*: `deconvolve` must be set to 1 in order for the keyword to take effect. <br />
  -*Default*: 2 <br />

**smooth_width**: integer equal to the size of the region to smooth when filtering out large-scale background fluctuations. <br />
  -*Dependency*: `filter_background` must be set to 1 in order for the keyword to take effect. <br />

**subtract_sidelobe_catalog**: a catalog to subtract sources from the sidelobes before deconvolution. <br />
  -*Dependency*: `deconvolve` must be set to 1 in order for the keyword to take effect. <br />
  -*EoR_firstpass settings*: not set <br />
  -*Default*: not set <br />

## EoR Bubble Simulations

**bubble_fname**: Specify the path to an HDF5 file containing at least "spectral_info/spectrum" and "spectral_info/freq" of shapes (Npix, Nchan) and (Nchan,), respectively. The "spectrum" object is a set of healpix maps vs frequency representing a full sky EoR signal. <br />
**select_radius_multiplier**: A circular region is selected from the input healpix maps, corresponding with the primary beam radius. This sets the selection radius to (primary_beam_radius)x(select_radius_multiplier) <br />

## Diffuse

**diffuse_model**: File path to the diffuse model sav file. <br />
  -*Currently only on diffuse_simulations branch* <br />
  -*Default*: filepath('gsm_150MHz.sav',root=rootdir('FHD'),subdir='catalog_data')<br />
  -Replace gsm_150MHz.sav with any other diffuse .sav file. This file should contain the following:<br />
  - MODEL_ARR = A healpix map with the diffuse model<br />
  - NSIDE = The corresponding NSIDE parameter<br />
  - HPX_INDS = The corresponding healpix indices of the model_arr<br />
  - coord_sys = (Optional) 'galactic' or 'celestial'. Specifies the coordinate system of the healpix map. GSM is in galactic coordinates, for instance. If missing, defaults to equatorial.<br />

## In situ simulation

**enhance_eor**: input a multiplicative factor to boost the signal of the EoR in the dirty input visibilities. <br />
  -*Dependency*: `eor_savefile` must be set to an EoR sav file path in order for the keyword to take effect. <br />
  -*Default*: not set<br />

**eor_savefile**: input a path to a savefile of EoR visibilities to include the EoR in the dirty input visibilities. <br />
  -*Dependency*: `in_situ_input` must be set to 1 or a sav file path in order for the keyword to take effect. <br />
  -*Default*: not set<br />

**in_situ_sim_input**: run an in situ simulation, where model visibilities are made and input as the dirty visibilities (see Barry et. al. 2016 for more information on use-cases). Setting to 1 forces the visibilities to be made within the current run. Setting to a sav file path inputs model visibilities from a previous run, which is the preferred method since that run is independently documented.<br />
  -*Default*: not set<br />
  
**sim_noise**: add a uncorrelated thermal noise to the input dirty visibilities from a specified sav file, or create them for the run. <br />
  -*Dependency*: `in_situ_input` must be set to 1 or a sav file path in order for the keyword to take effect. <br />
  -*Default*: not set<br />
  

## Model

**allow_sidelobe_model_sources**: allows FHD to model sources in the sidelobes for subtraction. Forces the beam_threshold to 0.01 in order to go down to 1% of the beam to capture sidelobe sources during the generation of a model calibration source catalog for the particular observation. <br />
  -*Dependency*: `model_visibilities` must be set to 1 in order for the keyword to take effect. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: Additional sidelobe sources are not included. If `return_cal_visibilities` and `allow_sidelobe_cal_sources` are both set, then the model will include those sources in the sidelobes that were used in calibration. <br />
  -*Default*: Additional sidelobe sources are not included. If `return_cal_visibilities` and `allow_sidelobe_cal_sources` are both set, then the model will include those sources in the sidelobes that were used in calibration. !Q <br />

**max_model_sources**: limits the number of sources used in the model. Sources are weighted by apparent brightness before applying the cut. Note that extended sources with many associated source components count as only a single source. <br />
  -*Dependency*: `model_visibilities` must be set to 1 in order for the keyword to take effect. If `return_cal_visibilities` is set, then the final model will include all calibration sources and all model sources (duplicates are caught and included only once). <br />
  -*Default*: All valid sources are used. !Q <br />

**model_catalog_file_path**: a catalog of sources to be used to make model visibilities for subtraction. <br />
  -*Dependency*: `model_visibilities` must be set to 1 in order for the keyword to take effect.  <br />
  -*EoR_firstpass settings*: filepath('mwa_calibration_source_list.sav',root=rootdir('FHD'),subdir='catalog_data') <br />
  -*Default*: not set <br />
  
**model_flux_threshold**: this sets an lower exclusion threshold in flux (Jy) for the model sources. If the flux threshold is negative, then it is treated as a upper exlusion threshold in flux (Jy). <br />
  -*Dependency*: `model_visibilities` must be set to 1 in order for the keyword to take effect.  <br />
  -*Default*: not set <br />

**model_visibilities**: make visibilities for the subtraction model separately from the model used in calibration. This is useful if the user sets keywords to make the subtraction model different from the model used in calibration. If not set, the model used for calibration is the same as the subtraction model. <br />
  -*Turn off/on*: 0/1 <br />
  -*Default*: 0 <br />

## Export
  
**export_images**: export fits files and images of the sky. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: 1 <br /> 

**image_filter_fn**: weighting filter to be applyed to resulting snapshot images and fits files. Options include  filter_uv_hanning, filter_uv_natural, filter_uv_radial, filter_uv_tapered_uniform, filter_uv_uniform, and filter_uv_weighted. Specifics on these filters can be found in `FHD/fhd_output/fft_filters`.<br />
  -*EoR_firstpass settings*: filter_uv_uniform <br />
  -*Default*: filter_uv_uniform <br />

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

**ring_radius**: sets the size of the rings around sources in the restored images. To generate restored images without rings, set ring_radius = 0. <br />
  -*EoR_firstpass settings*: not 0 !Q<br />
  -*Default*: not 0 !Q<br />

**save_uvf**: saves the gridded uv plane as a function of frequency for dirty, model, weights, and variance cubes. <br />
  -*Turn off/on*: 0/1 <br />
  -*Default*: 0 <br /> 

**save_visibililties**: save the calibrated data visibilities, the model visibilities, and the visibility flags. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: 1 probably <br /> 
  
**snapshot_healpix_export**: appears to be preserving visibilities. Save model/dirty/residual/weights/variance cubes as healpix arrays, split into even and odd time samples, in preparation for eppsilon.  !Q<br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: 0 <br />

**silent**: do not print messages. <br />
  -*Turn off/on*: 0/1 <br />
  -*Default*: 0 probably <br /> 
  
**split_ps_export**: split up the Healpix outputs into even and odd time samples. This is essential to propogating errors in &epsilon;ppsilon. <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: not set <br /> 
  
**write_healpix_fits**: create Healpix fits files. <br />
  -*Turn off/on*: 0/1 <br />
  -*EoR_firstpass settings*: 0 <br />
  -*Default*: 0 <br />

## Flagging

**dead_dipole_list**: an array of 3 x # of dead dipoles, where column 0 is the tile name, column 1 is the polarization (0:x, 1:y), and column 2 is the dipole number. These dipoles are flagged, which greatly increases memory usage due to the creation of many separate tile beams. <br />
  -*Default*: not set <br />  

**flag_calibration**: flags antennas based on calculations in `vis_calibration_flag.pro`.<br />
  -*Needs update*: keyword check both in general_obs and vis_calibrate !Q <br />
  -*Turn off/on*: 0/1 <br />
  -*Default*: 1 <br />  

**flag_dead_dipoles**: flag the dead dipoles listed in `<instrument>_dead_dipole_list.txt` for the golden set of Aug 23, 2013. This greatly increases memory usage due to the creation of many separate tile beams. <br />
  -*Default*: not set <br />  

**flag_visibilities**: flag visibilities based on calculations in `vis_flag.pro`. <br />
  -*Turn off/on*: 0/1 <br />
  -*Default*: 0 <br />  

**freq_end**: Frequency in MHz to end the observation. Flags frequencies greater than it.  <br />
  -*Default*: not set<br /> 

**freq_start**: Frequency in MHz to begin the observation. Flags frequencies less than it.  <br />
  -*Default*: not set<br />  

**no_calibration_frequency_flagging**: do not flag frequencies based off of zeroed calibration gains. <br />
  -*Needs updating*: might be better if changed to calibration_frequency_flagging and change the logic (avoid the double negative) !Q.
  -*Turn off/on*: 0/1 (flag/don't flag) <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: not set <br />

**tile_flag_list**: a string array of tile names to manually flag tiles. Note that this is an array of tile names, not tile indices! <br />
  -*Default*: not set <br />

**time_cut**: seconds to cut (rounded up to next time integration step) from the beginning of the observation. Can also specify a negative time to cut off the end of the observation. Specify a vector to cut at both the start and end. <br />
  -*Default*: not set <br />
  
**transfer_weights**: transfers weights information from another run. Set to a filename of the flags save file in the vis_data directory.  <br />
  -*Default*: not set <br />

**unflag_all**: unflag all tiles/antennas and frequencies. While not practical for real data, this is useful for creating unflagged model visibilities for the input of an in-situ simulations. <br />
  -*Turn off/on*: 0/1 <br />
  -*Default*: 0 <br />

**no_ps** : Do not save output images in postscript format. Only png and fits.<br /> 
  -*Default*: 1 <br />

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
  
**time_offset**: time offset of phase center in seconds from start time of the observation. <br />
  -*Default*: 0 second offset <br />
  
**rephase_weights**: if turned off, target phase center is the pointing center (as defined by Cotter). Setting rephase_weights=0 overrides override_target_phasera and override_target_phasedec. <br />
  -*Turn off/on*: 0/1 <br />
  -*Default*: 1 <br />


## Import

**uvfits_version**: the version number of the uvfits. `eor_firstpass_versions.pro` will use this keyword in conjuction with `uvfits_subversion` to query the mwa_qc database to find the location of the uvfits file on the MIT cluster. See `uvfits_subversion` for the current uvfits versions available. <br />
  -*Range*: currently 3 to 5 <br />
  -*EoR_firstpass settings*: 5 <br />

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

**baseline_threshold**: Positive numbers cut baselines shorter than the given number in wavelengths. Negative numbers cut baselines longer than the given number in wavelengths. <br />
  -*EoR_firstpass settings*: 0 <br />
  -*Default*: 0 <br />

**dft_threshold**: set equal to 1 to use the DFT approximation. When set equal to 0 the true DFT is calculated for each source. It can also be explicitly set to a value that determines the accuracy of the approximation. <br />
  -*Default*: 1 <br />

**dimension**: the number of pixels in the UV plane along one axis. <br />
  -*EoR_firstpass settings*: 2048 <br />
  -*Default*: 2 to the power of the rounded result of log&#8322;(k_span/k_binsize).<br />

**kbinsize**: size of UV pixels in wavelengths. Given a defined number of pixels in `dimension`, this sets the UV space extent. This will supersede `degpix` if `dimension` is also set. <br />
  -*Dependency*: will only go into effect if `FoV` is not set. <br />
  -*EoR_firstpass settings*: 0.5 <br />
  -*Default*: 0.5 if `FoV` not set !Q <br />

**FoV**: A proxy for the field of view in degrees. `FoV` is actually used to determine `kbinsize`, which will be set to `!RaDeg/FoV`. This means that the pixel size at phase center times `dimension` is approximately equal to `FoV`, which is not equal to the actual field of view owing to larger pixel sizes away from phase center. If set to 0, then `kbinsize` determines the UV resolution. <br />
  -*EoR_firstpass settings*: 0 <br />
  -*Default*: not set <br />

**max_baseline**: the maximum baseline length in wavelengths to include in the analysis. <br />
  -*EoR_firstpass settings*: net set <br />
  -*Default*: the maximum baseline length in wavelengths of the instrument, specifically calculated from the params structure  <br />

**min_baseline**: the minimum baseline length in wavelengths to include in the analysis. <br />
  -*EoR_firstpass settings*: 1 <br />
  -*Default*: the minimum baseline length in wavelengths of the instrument, specifically calculated from the params structure. This includes autocorrelations (!Q is that right Ian?) <br />
  
**n_pol**: number of polarizations to use (XX, YY versus XX, YY, XY, YX). <br />
  -*Set*: 2/4 <br />
  -*Default*: 2 <br />
  
**ps_beam_threshold** : Minimum value to which to calculate the beam out to in image space. The beam in UV space is pre-calculated and may have its own `beam_threshold` (see that keyword for more information), and this is only an additional cut in image space. <br />
  -*Default*: not set<br />     
  
**ps_dimension** : UV plane dimension in pixel number for Healpix cube generation. Overrides `ps_degpix` if set. If `ps_kspan`, `ps_dimension`, or `ps_degpix` are not set, the UV plane dimension is calculated from the FoV and the `degpix` from the obs structure.<br />
  -*Dependency*: `ps_kspan` must not be set in order for the keyword to take effect. <br />
  -*Default*: not set<br />   
  
**ps_degpix** : Degrees per pixel for Healpix cube generation. If `ps_kspan`, `ps_dimension`, or `ps_degpix` are not set, the UV plane dimension is calculated from the FoV and the `degpix` from the obs structure.<br />
  -*Dependency*: `ps_kspan` and `ps_dimension` must not be set in order for the keyword to take effect. <br />
  -*Default*: not set<br />   
  
**ps_fov** : Field of view in degrees for Healpix cube generation. Overrides `kpix` in the obs structure if set.<br />
  -*Dependency*: `ps_kbinsize` must not be set in order for the keyword to take effect. <br />
  -*Default*: not set<br />  
  
**ps_kbinsize** : UV pixel size in wavelengths to grid for Healpix cube generation. Overrides `ps_fov` and the `kpix` in the obs structure if set. <br />
  -*EoR_firstpass settings*: 0.5<br />
  -*Default*: not set<br />

**ps_kspan** :  UV plane dimension in wavelengths for Healpix cube generation. Overrides `ps_dimension` and `ps_degpix` if set. If `ps_kspan`, `ps_dimension`, or `ps_degpix` are not set, the UV plane dimension is calculated from the FoV and the `degpix` from the obs structure.<br />
  -*EoR_firstpass settings*: 600. <br />
  -*Default*: not set<br />
  
**ps_nfreq_avg** :  <br />
  -*Default*: not set<br />
  
**ps_psf_resolution** :  <br />
  -*Default*: not set<br />
  
**ps_tile_flag_list** :  <br />
  -*Default*: not set<br />


cleanup=0
combine_healpix=0
deconvolve=0

vis_baseline_hist=1
calibration_visibilities_subtract=0

n_avg=2
deconvolution_filter='filter_uv_uniform'

max_sources=20000   (Cut this -- Only max_calibration_sources or max_model_sources is used)


ring_radius=10.*pad_uv_image
combine_obs=0
smooth_width=32.

calibration_flag_iterate = 0


