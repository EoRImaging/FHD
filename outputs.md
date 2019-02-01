# Outputs <br />
FHD outputs various data products. We outline and describe these below. Items marked with (!Q are currently not defined -- ask Ian if needed and update please!)<br />

## Beam <br />

### \<obsid\>\_beams.sav <br />

**beam_ptr**: a pointer to a pointer array of dimensions N<sub>polarizations</sub> x N<sub>frequencies</sub> x N<sub>baselines</sub>, which themselves are pointer arrays of dimension N<sub>UVresolution</sub> x N<sub>UVresolution</sub> to the appropriate hyperresolution uv complex beam model for that particular frequency, polarization, and baseline sampled at the uv resolution pixel. There is a one-pixel offset in the UV resolution for interpolation purposes. The final result is a vectorized image, where each slice has been concatenated into a vector that can be efficiently used in matrix multiplication algorithms. Image below shows the output of fully dereferencing the beam_ptr, and what it pictorially represents.

<p align="center">
  <img src="https://github.com/nicholebarry/MWA_data_analysis/blob/master/image_docs/beam_ptr2-crop.pdf" width="350"/>
</p>

### \<obsid\>\_jones.sav <br />

## Calibration <br />

### \<obsids\>\_cal.sav <br />

**cal**: a structure of the various calibration parameters. Pointers and large arrays in this structure are defined below. Other variables in the structure include:
  * **n_pol**: long number of polarizations
  * **n_freq**: long number of frequency channels
  * **n_tile**: long number of tiles
  * **n_time**: long number of time steps
  * **auto_initialize**: integer 1/0 value of whether window scaled autocorrelations were used/not used to set the initial calibration solution before iteration.
  * **max_iter**: long number of maximum iterations in the X<sup>2</sup> calibration subroutine
  * **phase_iter**: long number of maximum iterations in the X<sup>2</sup> calibration subroutine where only the phase was updated
  * **auto_scale**: The average amplitude of the calibration solutions fit from the auto-correlations.
  * **tile_names**: string array of the tile names specified in the metafits and if unspecified then indices of tiles starting at 1
  * **freq**: float array of frequency values in Hz
  * **min_cal_baseline**: float of the minimum baseline length included in calibration
  * **max_cal_baseline**: float of the maximum baseline included in calibration
  * **n_vis_cal**: long number of visibilities used in calibration
  * **time_avg**: integer 1/0 value of whether or not visibilities were averaged in time during the X<sup>2</sup> calibration subroutine
  * **min_sols**: integer minimum number of equations used in the X<sup>2</sup> calibration subroutine per frequency channel
  * **ref_antenna**: long number index of tile to reference phases from
  * **ref_antenna_name**: name of the antenna used as the phase reference.
  * **conv_thresh**: float number of the required convergence threshold
  * **polyfit**: integer 1/0 value of whether or not polynomials were fit
  * **amp_degree**: integer number of degree of coefficients used in fitting the amplitude of the cal solutions i.e. 2 = 2<sup>nd</sup> order polynomial
  * **phase_degree**: integer number of degree of coefficients used in fitting the phase of the cal solutions i.e. 2 = 2<sup>nd</sup> order polynomial
  * **bandpass**: integer 1/0 value of whether or not a bandpass was fit
  * **mode_fit**: integer 1/0 value of whether or not cable reflection mode-fitting was done,
  * **mean_gain**: The average amplitude of the calibration solutions for each polarization.
  * **mean_gain_residual**: The average amplitude of the residual calibration solutions, after the fitted solutions have been subtracted.
  * **mean_gain_restrict**: The average amplitude of the residual calibration solutions excluding outliers, after the fitted solutions have been subtracted.
  * **stddev_gain_residual**: A pointer. For extended sources, the pointer references a new source_list structure containing all of the extended components, in the same format. For point sources, it is a Null pointer.
  * **cal_origin**: result of `git describe` run on the FHD repo during calibration. Effectively the git hash, does not include the branch name.

Pointers and large arrays in **cal** structure:

  * **cal.bin_offset**: array of values that specifies the index number of the start of the input visibilities given a time step index. This can be used in conjuncture with cal.uu, cal.vv, cal.tile_a, cal.tile_b, and the like to to find the visibilities associated with a specific time step. For example, to find visibility indices associated with the 20<sup>th</sup> time step, cal.bin_offset[19] specifies the start index and cal.bin_offset[20]-1 specifies the end index.

  * **cal.uu/cal.vv**: array of the light travel time in seconds for the U or V coordinate for each visibility. Visibilities are ordered by iterating through tile A for each tile B, and for each time step. This ordering is the same for cal.tile_a and cal.tile_b, and indices from cal.bin_offset can be used.

  * **cal.tile_a/cal.tile_b**: array of the tile index of the visibility, where tile A and tile B refer to the two separate tiles used in creating that visibility. This ordering is the same for cal.uu and cal.vv, and indices from cal.bin_offset can be used.

  * **cal.gain/cal.gain_residual**: a pointer array of dimension N<sub>pol</sub> which points to an complex array of dimension N<sub>freq</sub> by N<sub>tile</sub>. These arrays contain the complex gain or residual complex gains after all fits have been applied.

  * **cal.auto_params**: Npol pointer array, referencing 2 x Ntile arrays of the linear fit offset and slope parameters calculated between the amplitudes of the autocorrelations and cross-correlations for each tile.

  * **cal.convergence**: a pointer array of dimension Npol which points to an float array of dimension Nfreq by Ntile. These arrays contain a convergence measure for each pol/frequency/tile.

  * **cal.amp_params/cal.phase_params/cal.mode_params**: a pointer array of dimension N<sub>pol</sub> by N<sub>tile</sub> which points to an array of coefficients used in the polynomial or mode fitting. The amplitude and phase parameters are ordered by the least-degree to the highest-degree; a polynomial fit of A + Bx + Cx<sup>2</sup> has an array of three values A,B, and C. The mode parameters are ordered by mode in index units of the Fourier transform (can be a non-integer for the hyperfine DFT option), amplitude of the mode, and phase of the mode. The wave can be reconstructed by amp * e<sup>-i2pi*(mode*[0,1,2...n_freq-1]/n_freq)+i*phase</sup>. If mode parameters (or amplitude or phase parameters) are not calculated for that tile, there will be a Null pointer in that place.

  * **cal.skymodel**: a structure of the various skymodel parameters used in calibration.
    * **include_calibration**: integer 1/0 value of whether or not the sources used for calibration are included in the source list.
    * **n_sources**: number of sources included in the calibration skymodel
    * **catalog_name**: name of the catalog used in the calibration
    * **galaxy_model**: integer 1/0 value of whether or not a galaxy model was used
    * **galaxy_spectral_index**: the spectral index of the galaxy model used or NaN if no galaxy model was used
    * **diffuse_model**: name of the diffuse model used in the calibration
    * **diffuse_spectral_index**: the spectral index of the diffuse model used of NaN if no diffuse model was used.

    * **cal.skymodel.source_list**: an array of structures of dimension N<sub>sources</sub>.
      * **x**: The centroided pixel column coordinate of the source.
      * **y**: The centroided pixel row coordinate of the source.
      * **ra**: the RA coordinate value of the source
      * **dec**: the DEC coordinate value of the source
      * **ston**: not used in the calibration source list (!Q right?)
      * **freq**: frequency of the source model
      * **alpha**: spectral index of the source
      * **gain**: not used in the calibration source list (!Q right?)
      * **flag**: type codes where 0 is no flag, 1 is low confidence, and 2 is sidelobe contamination
      * **extend**: A pointer. For extended sources, the pointer references a new source_list structure containing all of the extended components, in the same format. For point sources, it is a Null pointer.
      * **flux**: structure of the fluxes for that source, where the dimension depends on what polarizations are reported for that source. The order of potential polarizations is xx, yy, xy, and yx in apparent brightness or I, Q, U, V in sky brightness.

### \<obsids\>\_bandpass.txt <br />

Text file of generated bandpass solutions. The first column is the frequency channels in Hz, and the following columns depend on the type of bandpass calibration used. For a global bandpass, the next columns are the average xx and yy instrumental polarization bandpass solutions. For a cable-dependent bandpass, the next columns are cable 90m xx, cable 90m yy, cable 150m xx, cable 150m yy, cable 230m xx, cable 230m yy, cable 320m xx, cable 320m yy, cable 400m xx, cable 400m yy, cable 524m xx, and cable 524m yy.

##  Deconvolution (deconvolution only)<br />

### \<obsids\>\_fhd.sav <br />

### \<obsids\>\_fhd_params.sav <br />

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

##  HEALPix<br />

### \<obsids\>\_even_cube\<pol\>.sav / \<obsids\>_even_cube\<pol\>.sav <br />

  * **variance_cube/weights_cube**: an array of HEALPix pixels organized by hpx_inds for each output frequency. This is an image cube of 1's gridded with the beam for all visibilities (weights) or 1's gridded with the beam squared for all visibilities (variances).

  * **dirty_cube*/model_cube*: an array of HEALPix pixels organized by hpx_inds for each output frequency. This is an image cube of the calibrated data (or model).

  * **beam_squared_cube**: an array of HEALPix pixels organized by hpx_inds for each output frequency. This is an image cube of the beam squared, where the square has taken place in image space. Used for general diagnostic or image-space weighting.

  * **hpx_inds**: the HEALPix indices of the output image cubes to be interpreted by HEALPix plotting code, **nside**: the HEALPix resolution factor that determines how many equal-area pixels are made, **n_avg**: the number of frequency channels that have been averaged over to generate these HEALPix cubes from the original data, **obs**: an obs structure to be handed around with the cubes for clarity.

##  Mapfn (deconvolution only)<br />

### \<obsids\>\_mapfn_\<pol\>.sav <br />

##  Metadata<br />

### \<obsids\>\_obs.sav <br />

### \<obsids\>\_params.sav <br />

### \<obsids\>\_settings.txt <br />

### \<obsids\>\_ps_settings.txt <br />

### \<obsids\>\_status.sav <br />

### \<obsids\>\_status.txt <br />

##  Output Data<br />

### \<obsids\>\_Beam_\<pol\>.sav <br />

### \<obsids\>\_\<filter\>_Dirty_\<pol\>.fits / \<obsids\>\_\<filter\>\_Model_\<pol\>.fits / \<obsids\>\_\<filter\>\_Residual_\<pol\>.fits <br />

### \<obsids\>\_\<filter\>\_Restored_rings_\<pol\>.fits <br />

### \<obsids\>\_\<filter\>\_UV_weights_\<pol\>.fits <br />

### \<obsids\>\_source_array.sav (deconvolution only) <br />

See cal.skymodel.source_list in Calibration for structure description. This output is generated from the source clustering of deconvolution components, and can be input back into FHD calibration as a self-calibration loop.

### \<obsids\>\_source_array.txt (deconvolution only) <br />

### \<obsids\>\_source_list.txt <br />

##  Output Images<br />

### \<obsids\>\_cal_amp.png / \<obsids\>\_cal_autocorr.png / \<obsids\>\_cal_phase.png<br />

### \<obsids\>\_Beam_\<pol\>.png <br />

### \<obsids\>\_\<filter\>\_Dirty_\<pol\>.png / \<obsids\>\_\<filter\>\_Model_\<pol\>.png / \<obsids\>\_\<filter\>\_Residual_\<pol\>.png <br />

### \<obsids\>\_\<filter\>\_Restored_rings_\<pol\>.png <br />

### \<obsids\>\_\<filter\>\_Sources_\<pol\>.png <br />

### \<obsids\>\_\<filter\>\_UV_weights_\<pol\>.png <br />

##  Vis Data<br />

### \<obsids\>\_autos.sav <br />

### \<obsids\>\_flags.sav <br />

### \<obsids\>\_vis_model_\<pol\>.sav / \<obsids\>\_vis_\<pol\>.sav <br />
