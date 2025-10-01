# Outputs <br />
FHD outputs various data products, outlined and described below. Items marked with (!Q are currently not defined -- ask Ian if needed and update please!)<br />

## Beam <br />

### \<obsid\>\_beams.sav <br />

**beam_ptr**: a pointer to a pointer array of dimensions N<sub>polarizations</sub> x N<sub>frequencies</sub> x N<sub>baselines</sub>, which themselves are pointer arrays of dimension N<sub>UVresolution</sub> x N<sub>UVresolution</sub> to the appropriate hyperresolution uv complex beam model for that particular frequency, polarization, and baseline sampled at the uv resolution pixel. There is a one-pixel offset in the UV resolution for interpolation purposes. The final result is a vectorized image, where each slice has been concatenated into a vector that can be efficiently used in matrix multiplication algorithms.

### \<obsid\>\_jones.sav <br />

**jones**: structure containing the Mueller matrix information. Jones matrices are 2x2 matrices that relate the antenna responses to polarization directions on the sky. The Mueller matrix is the Kroeneker product of the Jones matrices. Mueller matrices are 4x4 matrices that relate the polarized visibility responses to the coherencies on the sky. They are direction-dependent: different directions on the sky have different associated Mueller matrices.
  * **inds**: long array; pixel indices relating jmat and jinv to directions on the sky
  * **dimension**: float; the number of pixels in the x-coordinate direction
  * **elements**: float; the number of pixels in the y-coordinate direction
  * **jmat**: 4x4 array of pointers. Each element is a pointer to a dcomplex array of length equal to the length of inds. Value (\*jones.jmat[i,j])[k] is the (i,j) element of the Mueller matrix at the direction on the sky given by pixel inds[k]. Each 4x4 Mueller matrix converts the vector [RR*, DD*, RD*, DR*] to instrumental polarization apparent sky [xx*, yy*, xy*, yx*] where R refers to the Right Ascension direction and D refers to the Declination direction.
  * **jinv**: 4x4 array of pointers. Each element is a pointer to a dcomplex array of length equal to the length of inds. Value (\*jones.jinv[i,j])[k] is the (i,j) element of the inverse Mueller matrix at the direction on the sky given by pixel inds[k]. Each 4x4 inverse Mueller matrix converts the instrumental polarization apparent sky vector [xx*, yy*, xy*, yx*] to [RR*, DD*, RD*, DR*] where R refers to the Right Ascension direction and D refers to the Declination direction.


## Calibration <br />

### \<obsids\>\_cal.sav <br />

**cal**: structure of the various calibration parameters. Pointers and large arrays in this structure are defined below. Other variables in the structure include:
  * **n_pol**: long number of polarizations
  * **n_freq**: long number of frequency channels
  * **n_tile**: long number of tiles
  * **n_time**: long number of time steps
  * **auto_initialize**: integer 1/0 value of whether window scaled autocorrelations were used/not used to set the initial calibration solution before iteration
  * **max_iter**: long number of maximum iterations in the X<sup>2</sup> calibration subroutine
  * **phase_iter**: long number of maximum iterations in the X<sup>2</sup> calibration subroutine where only the phase was updated
  * **auto_scale**: average amplitude of the calibration solutions fit from the auto-correlations
  * **tile_names**: string array of the tile names specified in the metafits and if unspecified then indices of tiles starting at 1
  * **freq**: float array of frequency values in Hz
  * **min_cal_baseline**: float of the minimum baseline length included in calibration
  * **max_cal_baseline**: float of the maximum baseline included in calibration
  * **n_vis_cal**: long number of visibilities used in calibration
  * **time_avg**: integer 1/0 value of whether or not visibilities were averaged in time during the X<sup>2</sup> calibration subroutine
  * **min_sols**: integer minimum number of equations used in the X<sup>2</sup> calibration subroutine per frequency channel
  * **ref_antenna**: long number index of tile to reference phases from
  * **ref_antenna_name**: name of the antenna used as the phase reference
  * **conv_thresh**: float number of the required convergence threshold
  * **polyfit**: integer 1/0 value of whether or not polynomials were fit
  * **amp_degree**: integer number of degree of coefficients used in fitting the amplitude of the cal solutions i.e. 2 = 2<sup>nd</sup> order polynomial
  * **phase_degree**: integer number of degree of coefficients used in fitting the phase of the cal solutions i.e. 2 = 2<sup>nd</sup> order polynomial
  * **bandpass**: integer 1/0 value of whether or not a bandpass was fit
  * **mode_fit**: integer 1/0 value of whether or not cable reflection mode-fitting was done
  * **mean_gain**: average amplitude of the calibration solutions for each polarization
  * **mean_gain_residual**: average amplitude of the residual calibration solutions, after the fitted solutions have been subtracted
  * **mean_gain_restrict**: average amplitude of the residual calibration solutions excluding outliers, after the fitted solutions have been subtracted
  * **stddev_gain_residual**: pointer which references a new source_list structure containing all of the extended components for extended sources, or a Null pointer for point sources
  * **cal_origin**: result of `git describe` run on the FHD repo during calibration. Effectively the git hash, does not include the branch name.

Pointers and large arrays in **cal** structure:

  * **cal.bin_offset**: array of values that specifies the index number of the start of the input visibilities given a time step index. This can be used in conjuncture with cal.uu, cal.vv, cal.tile_a, cal.tile_b, and the like to to find the visibilities associated with a specific time step. For example, to find visibility indices associated with the 20<sup>th</sup> time step, cal.bin_offset[19] specifies the start index and cal.bin_offset[20]-1 specifies the end index.

  * **cal.uu/cal.vv**: array of the light travel time in seconds for the U or V coordinate for each visibility. Visibilities are ordered by iterating through tile A for each tile B, and for each time step. This ordering is the same for cal.tile_a and cal.tile_b, and indices from cal.bin_offset can be used.

  * **cal.tile_a/cal.tile_b**: array of the tile index of the visibility, where tile A and tile B refer to the two separate tiles used in creating that visibility. This ordering is the same for cal.uu and cal.vv, and indices from cal.bin_offset can be used.

  * **cal.gain/cal.gain_residual**: a pointer array of dimension N<sub>pol</sub> which points to an complex array of dimension N<sub>freq</sub> by N<sub>tile</sub>. These arrays contain the complex gain or residual complex gains after all fits have been applied.

  * **cal.auto_params**: Npol pointer array, referencing 2 x Ntile arrays of the linear fit offset and slope parameters calculated between the amplitudes of the autocorrelations and cross-correlations for each tile.

  * **cal.convergence**: a pointer array of dimension Npol which points to an float array of dimension Nfreq by Ntile. These arrays contain a convergence measure for each pol/frequency/tile.

  * **cal.amp_params/cal.phase_params/cal.mode_params**: a pointer array of dimension N<sub>pol</sub> by N<sub>tile</sub> which points to an array of coefficients used in the polynomial or mode fitting. The amplitude and phase parameters are ordered by the least-degree to the highest-degree; a polynomial fit of A + Bx + Cx<sup>2</sup> has an array of three values A,B, and C. The mode parameters are ordered by mode in index units of the Fourier transform (can be a non-integer for the hyperfine DFT option), amplitude of the mode, and phase of the mode. The wave can be reconstructed by amp * e<sup>-i2pi*(mode*[0,1,2...n_freq-1]/n_freq)+i*phase</sup>. If mode parameters (or amplitude or phase parameters) are not calculated for that tile, there will be a Null pointer in that place.

  * **cal.skymodel**: a structure of the various skymodel parameters used in calibration
    * **include_calibration**: integer 1/0 value of whether or not the sources used for calibration are included in the source list
    * **n_sources**: number of sources included in the calibration skymodel
    * **catalog_name**: name of the catalog used in the calibration
    * **galaxy_model**: integer 1/0 value of whether or not a galaxy model was used
    * **galaxy_spectral_index**: the spectral index of the galaxy model used or NaN if no galaxy model was used
    * **diffuse_model**: name of the diffuse model used in the calibration
    * **diffuse_spectral_index**: the spectral index of the diffuse model used of NaN if no diffuse model was used

    * **cal.skymodel.source_list**: an array of structures of dimension N<sub>sources</sub>
      * **x**: centroided pixel column coordinate of the source
      * **y**: centroided pixel row coordinate of the source
      * **ra**: RA coordinate value of the source
      * **dec**: DEC coordinate value of the source
      * **ston**: not used in the calibration source list (see Deconvolution)
      * **freq**: frequency of the source model in MHz
      * **alpha**: spectral index of the source
      * **gain**: not used in the calibration source list (see Deconvolution)
      * **flag**: type codes where 0 is no flag, 1 is low confidence, and 2 is sidelobe contamination
      * **extend**: A pointer. For extended sources, the pointer references a new source_list structure containing all of the extended components, in the same format. For point sources, it is a Null pointer.
      * **flux**: structure of the fluxes for that source, where the dimension depends on what polarizations are reported for that source. The order of potential polarizations is xx, yy, xy, and yx in apparent brightness or I, Q, U, V in sky brightness.

### \<obsids\>\_bandpass.txt <br />

Text file of generated bandpass solutions. The first column is the frequency channels in Hz, and the following columns depend on the type of bandpass calibration used. For a global bandpass, the next columns are the average xx and yy instrumental polarization bandpass solutions. For a cable-dependent bandpass, the next columns are cable 90m xx, cable 90m yy, cable 150m xx, cable 150m yy, cable 230m xx, cable 230m yy, cable 320m xx, cable 320m yy, cable 400m xx, cable 400m yy, cable 524m xx, and cable 524m yy for MWA Phase I.

##  Deconvolution (deconvolution only)<br />

### \<obsids\>\_fhd.sav <br />

  * **astr**: structure containing the fits image header information.<br />

  * **beam_base**<br />

  * **beam_correction**<br />

  * **component_array**: an array of structures of dimension N<sub>components</sub>. Gives the deconvolution components before clustering
      * **id**: unique ID labeling each component
      * **x**: centroided pixel column coordinate of the component
      * **y**: centroided pixel row coordinate of the component
      * **ra**: RA coordinate value of the component
      * **dec**: DEC coordinate value of the component
      * **ston**: signal-to-noise of the component. Calculated as the beam-weighted component flux density divided by the standard deviation across the beam-weighted residual.
      * **freq**: frequency of the component in MHz
      * **alpha**: spectral index of the component. Deconvolution does not calculate a spectral index and defaults to -0.8.
      * **gain**: !Q
      * **flag**: type codes where 0 is no flag, 1 is low confidence, and 2 is sidelobe contamination
      * **extend**: a Null pointer. Not used for component_array.
      * **flux**: structure of the fluxes for the component. The order of potential polarizations is xx, yy, xy, and yx in apparent brightness or I, Q, U, V in sky brightness. Deconvolution only operates in Stokes I so Q=U=V=0.

  * **dirty_array**<br />

  * **image_uv_arr**<br />

  * **model_uv_full**<br />

  * **model_uv_holo**<br />

  * **residual_array**<br />

  * **source_array**: an array of structures of dimension N<sub>sources</sub>. Gives the clustered deconvolution sources. Use generate_calibration_catalog.pro to extract this source array.<br />
      * **id**: A source label.
      * **x**: centroided pixel column coordinate of the source.
      * **y**: centroided pixel row coordinate of the source.
      * **ra**: RA coordinate value of the source.
      * **dec**: DEC coordinate value of the source.
      * **ston**: signal-to-noise of the source. Calculated as the beam-weighted source flux density divided by the standard deviation across the beam-weighted residual.
      * **freq**: frequency of the source in MHz.
      * **alpha**: spectral index of the source. Deconvolution does not calculate a spectral index and defaults to -0.8.
      * **gain**: !Q
      * **flag**: type codes where 0 is no flag, 1 is low confidence, and 2 is sidelobe contamination.
      * **extend**: a pointer. For extended sources, the pointer references a new source_array structure containing all of the extended components, in the same format. For point sources, it is a Null pointer.
      * **flux**: structure of the fluxes for the source. The order of potential polarizations is xx, yy, xy, and yx in apparent brightness or I, Q, U, V in sky brightness. Deconvolution only operates in Stokes I so Q=U=V=0.

  * **source_mask**<br />

  * **weights_arr**<br />

### \<obsids\>\_fhd_params.sav <br />
  * **params**: Structure with arrays that contain various metadata in attributes listed below.
      * **antenna1**: Array of first antenna indices, 1 indexed.
      * **antenna2**: Array of second antenna indices, 1 indexed.
      * **baseline_arr**: Array of unique baseline indices. Obtained by a product of antenna1 and antenna2 plus a shift proportional to Nants. !Q
      * **time**: Array of times, where the time is the center of the integration period. Measured in Julian date, and is in reference to the specified Julian date from the uvfits header (i.e. the sum of the DATE parameter and this time array gives the true Julian date).
      * **UU**: Array of u-coordinate of baselines. Units: lightseconds.
      * **VV**: Array of v-coordinate of baselines. Units: lightseconds.
      * **WW**: Array of w-coordinate of baselines. Units: lightseconds.

## Grid Data<br />

### \<obsids\>\_uv_\<pol\>.sav <br />

  * **grid_uv**: a complex image of dimensions N<sub>dimension</sub> by N<sub>elements</sub> (where dimension and elements are FHD keywords). This is the gridded UV plane of the calibrated data visibilities where all frequencies have been gridded together.<br />

  * **pol_i**: the polarization index of the gridded UV plane. In 2 polarizations, 0 = xx and 1 = yy.<br />

### \<obsids\>\_uv_model_\<pol\>.sav <br />

  * **grid_uv_model**: a complex image of dimensions N<sub>dimension</sub> by N<sub>elements</sub> (where dimension and elements are FHD keywords). This is the gridded UV plane of the model visibilities where all frequencies have been gridded together.<br />

  * **pol_i**: the polarization index of the gridded UV plane. In 2 polarizations, 0 = xx and 1 = yy.<br />

### \<obsids\>\_uv_weights_\<pol\>.sav <br />

  * **weights_uv**: a complex image of dimensions N<sub>dimension</sub> by N<sub>elements</sub> (where dimension and elements are FHD keywords). This is a gridded UV plane where a value of 1 is gridded with the beam for each visibility and where all frequencies have been gridded together. The units are Jy/beam.<br />

  * **pol_i**: the polarization index of the gridded UV plane. In 2 polarizations, 0 = xx and 1 = yy.<br />

### \<obsids\>\_vis_count.sav <br />

  * **vis_count**: an image of dimensions N<sub>dimension</sub> by N<sub>elements</sub> (where dimension and elements are FHD keywords). Each pixel value is how many visibilities contributed to that pixel, which is the same as our in-house uniform weighting. (!Q right?)

### \<obsids\>\_\<even/odd\>\_gridded_uvf_\<pol\>.sav <br />
Gridded uvf cubes, alternative input to eppsilon. Saved only if keyword `save_uvf` is set.

##  HEALPix<br />

### \<obsids\>\_\<even/odd\>_cube\<pol\>.sav

  * **variance_cube/weights_cube**: an array of HEALPix pixels organized by hpx_inds for each output frequency. This is an image cube of 1's gridded with the beam for all visibilities (weights) or 1's gridded with the beam squared for all visibilities (variances).

  * **dirty_cube/model_cube**: an array of HEALPix pixels organized by hpx_inds for each output frequency. This is an image cube of the calibrated data (or model).

  * **beam_squared_cube**: an array of HEALPix pixels organized by hpx_inds for each output frequency. This is an image cube of the beam squared, where the square has taken place in image space. Used for general diagnostic or image-space weighting.

  * **hpx_inds**: the HEALPix indices of the output image cubes to be interpreted by HEALPix plotting code, **nside**: the HEALPix resolution factor that determines how many equal-area pixels are made, **n_avg**: the number of frequency channels that have been averaged over to generate these HEALPix cubes from the original data, **obs**: an obs structure to be handed around with the cubes for clarity.

##  Mapfn (deconvolution only)<br />

### \<obsids\>\_mapfn_\<pol\>.sav <br />

##  Metadata<br />

### \<obsids\>\_obs.sav <br />

**obs**: structure of the various observation parameters. Pointers and large arrays in this structure are defined below. Other variables in the structure include:
   * **code_version**: githash string of the observation run
   * **instrument**: string of the instrument name
   * **obsname**: string of the observation name identifier, which depends on the instrument
   * **dimension**: float number of the pixels on the u-axis
   * **elements**: float number of the pixels on the v-axis      
   * **nbaselines**: long number of of baselines in the observation
   * **dft_threshold**: if set, float threshold for the DFT image
   * **double_precision**: integer flag of 0/1 to indication if double precision (1) or float precision (1) was used in calculations
   * **kpix**: float of the uv (and thus k) resolution
   * **degpix**: float of the number of degrees per pixel in image space
   * **obsaz**: float of the observation's azimuth in degrees calculated from the equatorial coordinates
   * **obsalt**: float of the observation's altitude in degrees calculated from the equatorial coordinates
   * **obsra**: float of the observation's RA in degrees
   * **obsdec**: float of the observation's DEC in degrees
   * **zenra**: float of the zenith RA in degrees for the instrument and julian date specified
   * **zendec**: float of the zenith DEC in degrees for the instrument and julian date specified
   * **obsx**: float of the positive x-axis extent for the observation, set by dimension/2
   * **obsy**: float of the positive y-axis extent for the observation, set by elements/2
   * **zenx**: float of the positive x-axis extent for the zenith, set by dimension/2
   * **zeny**: float of the positive y-axis extent for the zenith, set by elements/2
   * **phasera**: float of the RA of the phase center for the observation
   * **phasedec**: float of the DEC of the phase center for the observation
   * **orig_phasera**: float of the RA of the phase center of the input uvfits file, which may not be the desired phase center
   * **orig_phasedec**: float of the DEC of the phase center of the input uvfits file, which may not be the desired phase center
   * **n_pol**: integer number of polarizations
   * **n_tile**: long number of tiles
   * **n_tile_flag**: long number of flagged tiles
   * **n_freq**: long number of frequency channels
   * **n_freq_flag**: long number of flagged frequency channels
   * **n_time**: long number of time channels
   * **n_time_flag**: long number of flagged time channels
   * **n_vis**: long number of visibilities for one polarization that have a weight greater than 0 after processing (i.e. after cuts in u,v space)
   * **n_vis_in**: long number of the visibilities for one polarization after basic time/frequency instrument flagging
   * **n_vis_raw**: long number of the total possible input visibilities from the uvfits file. This disregards any input flagging, and is theoretically largest possible amount of visibilities for one polarization.
   * **nf_vis**: long array of the number of gridded visibilities per frequency
   * **primary_beam_area**: a pointer array of dimension N<sub>pol</sub> which points to an array of dimension N<sub>freq</sub>. Each entry is the primary beam area for that frequnecy and polarization in degrees.
   * **primary_beam_sq_area**: a pointer array of dimension N<sub>pol</sub> which points to an array of dimension N<sub>freq</sub>. Each entry is the primary beam squared area for that frequnecy and polarization in degrees.
   * **pol_names**: string array of the names of the polarizations, starting with four instrumental polarizations and ending with the four stokes polarizations
   * **jd0**: double of the julian date of the observation (including a time offset if supplied)
   * **max_baseline**: float of the maximum baselines in wavelengths
   * **min_baseline**: float of the minimun baselines in wavelengths
   * **delays**: pointer to a float array of dimension N<sub>antennas</sub> that describes the individual antenna delays to point the instrument, particularly for the MWA
   * **lon**: longitude of the instrument
   * **lat**: latitude of the instrument                                                
   * **alt**: altitude of the instrument
   * **freq_center**: center frequency of the observation in Hz
   * **freq_res**: frequency resolution of the observation in Hz
   * **time_res**: time resolution of the observation in seconds
   * **astr**: structure of header keywords and values relating to astrometry following the fits standard
      * **naxis**: number of pixels per axis
      * **cd**: coordinate description matrix
      * **cdelt**: delta per pixel of the coordinate description matrix (degrees per pixel)
      * **crpix**: 2 element vector giving X and Y coordinates of reference pixel (def = NAXIS/2). Values must be in fits convention (first pixel is [1,1])
      * **crval**: 2 element double precision vector giving RA and DEC of reference pixel in degrees
      * **ctype**: 2 element string vector giving projection types, default ['RA---SIN','DEC--SIN'] (slant orthographic)
      * **longpole**: scalar longitude of north pole, default = 180. Note that the default value of 180 is valid only for zenithal projections; it should be set to PV2_1 for conic projections, and zero for other projections.
      * **latpole**: scalar latitude of the north pole, default = 0
      * **pv2**: vector of projection parameters associated with the latitude axis of the slant orthographic system, corresponding to Xi and eta. This is an advanced extension of the AIPS SIN convention.
      * **pv1**: vector of projection parameters associated with longitude axis
      * **axes**: axis numerical labels in fits convention, default [1,2]
      * **reverse**: byte, true if first astrometry axis is DEC/latitude
      * **coord_sys**: 1 or 2 character code giving coordinate system, including 'C' = RA/DEC, 'G' = Galactic, 'E' = Ecliptic, 'X' = unknown
      * **projection**: projection type, default 'SIN' (slant orthographic)
      * **known**: true if IDL WCS routines recognise this projection
      * **radecsys**: the system that describes RA/DEC coordinates, defaulted to IAU 1984 (FK5) standard
      * **equinox**: coordinate equinox in J2000 standard
      * **dateobs**: date and time string of the start of the observation, ordered as '2013-08-23T18:04:40.00'
      * **mjobs**: Modified Julian Date at start of observation
      * **x0y0**: Implied offset in intermediate world coordinates (x,y) if a non-standard fiducial point is set via PV1 and also PV1_0a =/ 0, indicating that an offset should be applied to place CRVAL at the (x,y) origin.
   * **alpha**: overall spectral index of the point-source catalog, generally -0.8
   * **residual**: flag option to create residual HEALPix cubes (rather than letting the power spectrum code find the residual in 3D PS space using the calibrated data and simulated model)
   * **vis_noise**: pointer to an array of dimension N<sub>pol</sub>, N<sub>freq</sub> with the calculated visibility noise in units of Jy via even--odd subtractions
   * **baseline_info**: pointer to a structure filled with information about each baseline    
      * **tile_a**: index of the first tile in each auto- and cross-correlation visibility vector
      * **tile_b**: index of the second tile in each auto- and cross-correlation visibility vector
      * **bin_offset**: the first index of each new time sample in the visibility vector
      * **JDate**: Julian Date (J2000) of each time step in an observation
      * **fbin_i**: index of the beam frequency for every frequency sample (i.e. if only one beam is used for all frequencies, then it will be an array of 0's, if a new beam is used per frequency, then it will be an ordered array)
      * **freq_use**: array of frequencies used in the observation (0 is flagged, 1 is used)tile_use: array of tiles used in the observation (0 is flagged, 1 is used)
      * **time_use**: array of times used in the observation (0 is flagged, 1 is used)
      * **tile_names**: name of each tile, which is stored as a string in case the naming system is not number-based
      * **tile_height**: height of each tile, taken directly from the metadata header 
      * **tile_flag**: flags for tiles taken directly from the metadata header (and thus the flags given by the uvfits/metadata rather than flags generated during analysis)
   * **META_DATA**:       POINTER   <PtrHeapVar7>
   * **META_HDR**:        POINTER   <PtrHeapVar8>
   * **DEGRID_SPECTRAL_TERMS**:
                   INT              0
   * **GRID_SPECTRAL_TERMS**:
                   INT              0
   * **GRID_INFO**:        POINTER   <NullPointer>
   * **HEALPIX**:          STRUCT    -> <Anonymous> Array[1]

### \<obsids\>\_params.sav <br />

### \<obsids\>\_settings.txt <br />

### \<obsids\>\_ps_settings.txt <br />

### \<obsids\>\_status.sav <br />

### \<obsids\>\_status.txt <br />

##  Output Data<br />

### \<obsids\>\_Beam_\<pol\>.sav <br />

### \<obsids\>\_\<filter\>\_Dirty\_\<pol\>.fits / \<obsids\>\_\<filter\>\_Model_\<pol\>.fits / \<obsids\>\_\<filter\>\_Residual_\<pol\>.fits <br />

Fits files containing the dirty, model, and residual (dirty minus model) images. These images are constrained to be real (via taking the real part of the uv FFT) for XX and YY instrumental polarizations. Units are Jy per sterradian.

### \<obsids\>\_\<filter\>\_Dirty_XY_real.fits / \<obsids\>\_\<filter\>\_Model_XY_real.fits / \<obsids\>\_\<filter\>\_Residual_XY_real.fits <br />

Fits files containing the real part of the dirty, model, and residual XY- and YX-polarized images (XY- and YX-polarized images are complex conjugates of one another). Units are Jy per sterradian. Produced only when `n_pol=4`.

### \<obsids\>\_\<filter\>\_Dirty_XY_imaginary.fits / \<obsids\>\_\<filter\>\_Model_XY_imaginary.fits / \<obsids\>\_\<filter\>\_Residual_XY_imaginary.fits <br />

Fits files containing the imaginary part of the dirty, model, and residual XY-polarized images. Equal to the opposite of the imaginary part of the YX-polarized images because XY- and YX-polarized images are complex conjugates of one another. Units are Jy per sterradian. Produced only when `n_pol=4`.

### \<obsids\>\_\<filter\>\_Restored_rings_\<pol\>.fits <br />

Fits file consisting of the residual image with the subtracted sources over-plotted. Each source is plotted with a ring around it; the ring size is set with the `ring_radius` keyword. Set `ring_radius=0` to turn off rings. Because this is a combined diffuse and point source image the units are not well-defined.

### \<obsids\>\_\<filter\>\_UV_weights_\<pol\>.fits <br />

### \<obsids\>\_source_array.sav (deconvolution only) <br />

See cal.skymodel.source_list in Calibration for structure description. This output is generated from the source clustering of deconvolution components, and can be input back into FHD calibration as a self-calibration loop.

### \<obsids\>\_source_array.txt (deconvolution only) <br />

### \<obsids\>\_source_list.txt <br />

##  Output Images<br />

### \<obsids\>\_cal_amp.png<br />

Plots the fitted gain amplitude solutions as a function of frequency. Each panel
belongs to an antenna. Blue is the X polarization and red is the Y polarization.<br />

### \<obsids\>\_cal_autocorr.png<br />

Plots the fitted gain solutions from `vis_cal_auto_fit.pro`, which uses the
autocorrelations as outlined in [Barry et al. 2019a](https://arxiv.org/abs/1901.02980).<br />

### \<obsids\>\_cal_phase.png<br />

Plots the fitted gain phase solutions as a function of frequency. Each panel
belongs to an antenna. Blue is the X polarization and red is the Y polarization.<br />

### \<obsids\>\_Beam_\<pol\>.png <br />

### \<obsids\>\_\<filter\>\_Dirty_\<pol\>.png / \<obsids\>\_\<filter\>\_Model_\<pol\>.png / \<obsids\>\_\<filter\>\_Residual_\<pol\>.png <br />

Dirty, model, and residual (dirty minus model) images. These images are constrained to be real (via taking the real part of the uv FFT) for XX and YY instrumental polarizations. Units are Jy per sterradian.

### \<obsids\>\_\<filter\>\_Dirty_XY_real.png / \<obsids\>\_\<filter\>\_Model_XY_real.png / \<obsids\>\_\<filter\>\_Residual_XY_real.png <br />

Real part of the dirty, model, and residual XY- and YX-polarized images (XY- and YX-polarized images are complex conjugates of one another). Units are Jy per sterradian. Produced only when `n_pol=4`.

### \<obsids\>\_\<filter\>\_Dirty_XY_imaginary.png / \<obsids\>\_\<filter\>\_Model_XY_imaginary.png / \<obsids\>\_\<filter\>\_Residual_XY_imaginary.png <br />

Imaginary part of the dirty, model, and residual XY-polarized images. Equal to the opposite of the imaginary part of the YX-polarized images because XY- and YX-polarized images are complex conjugates of one another. Units are Jy per sterradian. Produced only when `n_pol=4`.

### \<obsids\>\_\<filter\>\_Restored_rings_\<pol\>.png <br />

Residual image with the subtracted sources over-plotted. Each source is plotted with a ring around it; the ring size is set with the `ring_radius` keyword. Set `ring_radius=0` to turn off rings. Because this is a combined diffuse and point source image the units are not well-defined.

### \<obsids\>\_\<filter\>\_Sources_\<pol\>.png <br />

### \<obsids\>\_\<filter\>\_UV_weights_\<pol\>.png <br />


##  Vis Data<br />

### \<obsids\>\_autos.sav <br />

  * **auto_corr**: Pointer array. Each points to an array of autocorrelations of a single pol. Shape is [Nfreqs, Nants X Ntimes]. Unsure of pol order or packing order for second axis. !Q

  * **obs**: See <obsids>_obs.sav under metadata.

### \<obsids\>\_flags.sav <br />

### \<obsids\>\_vis_model_\<pol\>.sav / \<obsids\>\_vis_\<pol\>.sav <br />
