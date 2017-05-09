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

**cal**: a structure of the various calibration parameters. Pointers and large arrays in this structure are defined below. Other variables in the structure include: n_pol (long number of polarizations), n_freq (long number of frequency channels), n_tile (long number of tiles), n_time (long number of time steps), auto_initialize (integer 1/0 value of whether window scaled autocorrelations were used/not used), max_iter (long number of maximum iterations in the \Chi<sup>2</sup> calibration subroutine), phase_iter (long number of maximum iterations in the \Chi<sup>2</sup> calibration subroutine where only the phase was updated), tile_names (string array of the tile names specified in the metafits, if unspecified then indices of tiles starting at 1), freq (float array of frequency values in Hz), auto_scale (!Q), min_cal_baseline (float of the minimum baseline length included in calibration), max_cal_baseline (float of the maximum baseline included in calibration), n_vis_cal (long number of visibilities used in calibration), time_avg (integer 1/0 value of whether or not visibilities were averaged in time during the \Chi<sup>2</sup> calibration subroutine), min_sols (integer minimum number of equations used in the \Chi<sup>2</sup> calibration subroutine per frequency channel), ref_antenna (long number index of tile to reference phases from), ref_antenna_name (!Q), conv_thresh (float number of the required convergence threshold), polyfit (!Q), amp_degree (integer number of degree of coefficients used in fitting the amplitude of the cal solutions, i.e. 2 = 2<sup>nd</sup> order polynomial), phase_degree (integer number of degree of coefficients used in fitting the phase of the cal solutions, i.e. 2 = 2<sup>nd</sup> order polynomial), mean_gain (!Q) <br />

**cal.bin_offset**:

**cal.uu/cal.vv**:

**cal.tile_a/cal.tile_b**:

**cal.gain/cal.gain_residual**:

**cal.auto_params**:

**cal.convergence**:

**cal.amp_params/cal.phase_params/cal.mode_params**:

**cal.skymodel**:

   MEAN_GAIN       FLOAT     Array[2]
   MEAN_GAIN_RESIDUAL
                   FLOAT     Array[2]
   MEAN_GAIN_RESTRICT FLOAT     Array[2]
   STDDEV_GAIN_RESIDUAL
                   FLOAT     Array[2]
   BANDPASS        INT              1
   MODE_FIT        FLOAT           1.00000
   MODE_PARAMS     POINTER   Array[2, 128]
   CAL_ORIGIN      STRING    '1061316296'
   SKYMODEL        STRUCT    -> <Anonymous> Array[1]

## Grid Data<br />

##  Healpix<br />

##  Metadata<br />

##  Output Data<br />

##  Output Images<br />

##  Vis Data<br />
