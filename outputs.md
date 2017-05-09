# Outputs <br />
FHD outputs various data products. We outline and describe these below. <br />

## Beam <br />

### \<obsid\>_beams.sav <br />

**beam_ptr**: a pointer to a pointer array of dimensions N<sub>polarizations</sub> x N<sub>frequencies</sub> x N<sub>baselines</sub>, which themselves are pointer arrays of dimension N<sub>UVresolution</sub> x N<sub>UVresolution</sub> to the appropriate hyperresolution uv complex beam model for that particular frequency, polarization, and baseline sampled at the uv resolution pixel. There is a one-pixel offset in the UV resolution for interpolation purposes. The final result is a vectorized image, where each slice has been concatenated into a vector that can be efficiently used in matrix multiplication algorithms. Image below shows the output of fully dereferencing the beam_ptr, and what it pictorially represents.

<p align="center">
  <img src="https://github.com/nicholebarry/MWA_data_analysis/blob/master/image_docs/beam_ptr2-crop.pdf" width="350"/>
</p>

## Calibration <br />

### \<obsids\>_cal.sav <br />

**cal**: a structure of the various calibration parameters. Pointers and large arrays in this structure are defined below. Other variables in the structure include: 
  * **n_pol**: long number of polarizations, **n_freq**: long number of frequency channels, **n_tile**: long number of tiles, **n_time**: long number of time steps 
  * **auto_initialize**: integer 1/0 value of whether window scaled autocorrelations were used/not used, **max_iter**: long number of maximum iterations in the X<sup>2</sup> calibration subroutine, **phase_iter**: long number of maximum iterations in the X<sup>2</sup> calibration subroutine where only the phase was updated, **auto_scale**: !Q
  * **tile_names**: string array of the tile names specified in the metafits and if unspecified then indices of tiles starting at 1, **freq**: float array of frequency values in Hz
  * **min_cal_baseline**: float of the minimum baseline length included in calibration, **max_cal_baseline**: float of the maximum baseline included in calibration, **n_vis_cal**: long number of visibilities used in calibration 
  * **time_avg**: integer 1/0 value of whether or not visibilities were averaged in time during the X<sup>2</sup> calibration subroutine, **min_sols**: integer minimum number of equations used in the X<sup>2</sup> calibration subroutine per frequency channel, **ref_antenna**: long number index of tile to reference phases from, **ref_antenna_name**: !Q, **conv_thresh**: float number of the required convergence threshold 
  * **polyfit**: integer 1/0 value of whether or not polynomials were fit, **amp_degree**: integer number of degree of coefficients used in fitting the amplitude of the cal solutions i.e. 2 = 2<sup>nd</sup> order polynomial, **phase_degree**: integer number of degree of coefficients used in fitting the phase of the cal solutions i.e. 2 = 2<sup>nd</sup> order polynomial, **bandpass**: integer 1/0 value of whether or not a bandpass was fit, **mode_fit**: integer 1/0 value of whether or not cable reflection mode-fitting was done,
  * **mean_gain**: !Q, **mean_gain_residual**: !Q, **mean_gain_restrict**: !Q, **stddev_gain_residual**: !Q, **cal_origin**: !Q

**cal.bin_offset**: array of values that specifies the index number of the start of the input visibilities given a time step index. This can be used in conjuncture with cal.uu, cal.vv, cal.tile_a, cal.tile_b, and the like to to find the visibilities associated with a specific time step. For example, to find visibility indices associated with the 20<sup>th</sup> time step, cal.bin_offset[19] specifies the start index and cal.bin_offset[20]-1 specifies the end index.

**cal.uu/cal.vv**: array of the light travel time in seconds for the U or V coordinate for each visibility. Visibilities are ordered by iterating through tile A for each tile B, and for each time step. This ordering is the same for cal.tile_a and cal.tile_b, and indices from cal.bin_offset can be used.

**cal.tile_a/cal.tile_b**: array of the tile index of the visibility, where tile A and tile B refer to the two separate tiles used in creating that visibility. This ordering is the same for cal.uu and cal.vv, and indices from cal.bin_offset can be used.

**cal.gain/cal.gain_residual**: a pointer array of dimension N<sub>pol</sub> which points to an complex array of dimension N<sub>freq</sub> by N<sub>tile</sub>. These arrays contain the complex gain or residual complex gains after all fits have been applied.

**cal.auto_params**:

**cal.convergence**:

**cal.amp_params/cal.phase_params/cal.mode_params**: a pointer array of dimension N<sub>pol</sub> by N<sub>tile</sub> which points to an array of coefficients used in the polynomial or mode fitting. The amplitude and phase parameters are ordered by the least-degree to the highest-degree; a polynomial fit of A + Bx + Cx<sup>2</sup> has an array of three values A,B, and C. The mode parameters are ordered by mode in index units of the Fourier transform (can be a non-integer for the hyperfine DFT option), amplitude of the mode, and phase of the mode. The wave can be reconstructed by amp * e<sup>-i2pi*(mode*[0,1,2...n_freq-1]/n_freq)+i*phase</sup>. If mode parameters (or amplitude or phase parameters) are not calculated for that tile, there will be a Null pointer in that place.

**cal.skymodel**: a structure of the various skymodel parameters used in calibration.
  * **include_calibration**: !Q, **n_sources**: number of sources included in the calibration skymodel, **catalog_name**: name of the catalog used in the calibration, **galaxy_model**: integer 1/0 value of whether or not a galaxy model was used, **galaxy_spectral_index**: the spectral index of the galaxy model used or NaN if no galaxy model was used, **diffuse_model**: name of the diffuse model used in the calibration, **diffuse_spectral_index**: the spectral index of the diffuse model used of NaN if no diffuse model was used. Source_list defined below.

**cal.skymodel.source_list**: 

   SOURCE_LIST     STRUCT    -> <Anonymous> Array[11051]

### \<obsids\>_bandpass.txt <br />


## Grid Data<br />

### \<obsids\>_uv_\<pol\>.sav <br />

### \<obsids\>_uv_model_\<pol\>.sav <br />

### \<obsids\>_uv_weights_\<pol\>.sav <br />

### \<obsids\>_vis_count.sav <br />

##  Healpix<br />

### \<obsids\>_even_cube\<pol\>.sav / \<obsids\>_even_cube\<pol\>.sav <br />

##  Metadata<br />

### \<obsids\>_obs.sav <br />

### \<obsids\>_params.sav <br />

### \<obsids\>_settings.txt <br />

### \<obsids\>_ps_settings.txt <br />

### \<obsids\>_status.sav <br />

### \<obsids\>_status.txt <br />

##  Output Data<br />

### \<obsids\>_Beam_\<pol\>.sav <br />

### \<obsids\>_\<filter\>_Dirty_\<pol\>.fits / \<obsids\>_\<filter\>_Model_\<pol\>.fits / \<obsids\>_\<filter\>_Residual_\<pol\>.fits <br />

### \<obsids\>_\<filter\>_Restored_rings_\<pol\>.fits <br />

### \<obsids\>_\<filter\>_UV_weights_\<pol\>.fits <br />

##  Output Images<br />

### \<obsids\>_cal_amp.png / \<obsids\>_cal_autocorr.png / \<obsids\>_cal_phase.png<br />

### \<obsids\>_Beam_\<pol\>.png <br />

### \<obsids\>_\<filter\>_Dirty_\<pol\>.png / \<obsids\>_\<filter\>_Model_\<pol\>.png / \<obsids\>_\<filter\>_Residual_\<pol\>.png <br />

### \<obsids\>_\<filter\>_Restored_rings_\<pol\>.png <br />

### \<obsids\>_\<filter\>_Sources_\<pol\>.png <br />

### \<obsids\>_\<filter\>_UV_weights_\<pol\>.png <br />

##  Vis Data<br />

### \<obsids\>_autos.sav <br />

### \<obsids\>_flags.sav <br />

### \<obsids\>_vis_model_\<pol\>.sav / \<obsids\>_vis_\<pol\>.sav <br />
