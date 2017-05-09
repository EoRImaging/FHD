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
  * **polyfit**: !Q, **amp_degree**: integer number of degree of coefficients used in fitting the amplitude of the cal solutions i.e. 2 = 2<sup>nd</sup> order polynomial, **phase_degree**: integer number of degree of coefficients used in fitting the phase of the cal solutions i.e. 2 = 2<sup>nd</sup> order polynomial
  * **mean_gain**: !Q <br />

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
   * Output files of grid engine or slurm.

##  Healpix<br />

### \<obsids\>_{even,odd}_cube\<pol\>.sav<br />
  * DIRTY_CUBE = Healpix cube of the dirty sky image, shape = (n_hpx_inds, n_freq)
  * MODEL_CUBE = Healpix cube of the model sky image, shape = (n_hpx_inds, n_freq)
  * WEIGHTS_CUBE = Healpix cube of weights, shape = (n_hpx_inds, n_freq)
  * VARIANCE_CUBE = Healpix cube of variance, shape = (n_hpx_inds, n_freq)
  * RES_CUBE = Healpix cube of residuals (dirty-model), shape = (n_hpx_inds, n_freq)
  * BEAM_SQUARED_CUBE = Healpix cube of weights, shape = (n_hpx_inds, n_freq)
  * OBS = obs object
  * NSIDE = Healpix NSIDE parameter
  * HPX_INDS. = Healpix indices corresponding with the saved cubes
  * N_AVG = !Q

##  Metadata<br />

### \<obsids\>_obs.sav <br />
  * obs = The obs structure, which holds observational parameters throughout analysis.  !Q When does it get saved here?

        CODE_VERSION    STRING    'v3.5-625-gc3a70e6-dirty'
        INSTRUMENT      STRING    'hera'
        OBSNAME         STRING    'ewbase_14m_plat'
        DIMENSION       FLOAT           1024.00
        ELEMENTS        FLOAT           1024.00
        NBASELINES      LONG                 3
        DFT_THRESHOLD   FLOAT           0.00000
        DOUBLE_PRECISION
                        INT              0
        KPIX            FLOAT         0.0830000
        DEGPIX          FLOAT          0.674131
        OBSAZ           FLOAT           190.130
        OBSALT          FLOAT           89.9929
        OBSRA           FLOAT           72.6386
        OBSDEC          FLOAT          -30.7483
        ZENRA           FLOAT           72.6400
        ZENDEC          FLOAT          -30.7414
        OBSX            FLOAT           512.000
        OBSY            FLOAT           512.000
        ZENX            FLOAT           512.002
        ZENY            FLOAT           512.010
        PHASERA         FLOAT           72.6386
        PHASEDEC        FLOAT          -30.7483
        ORIG_PHASERA    FLOAT           72.6386
        ORIG_PHASEDEC   FLOAT          -30.7483
        N_POL           INT              2
        N_TILE          LONG                 2
        N_TILE_FLAG     LONG                 0
        N_FREQ          LONG               203
        N_FREQ_FLAG     LONG                 0
        N_TIME          LONG                56
        N_TIME_FLAG     FLOAT           0.00000
        N_VIS           LONG             11366
        N_VIS_IN        LONG             34104
        N_VIS_RAW       LONG             34104
        NF_VIS          LONG      Array[203]
        BEAM_INTEGRAL   POINTER   Array[4]
        POL_NAMES       STRING    Array[8]
        JD0             DOUBLE           2457458.2
        MAX_BASELINE    FLOAT           9.31679
        MIN_BASELINE    FLOAT           4.66654
        DELAYS          POINTER   <NullPointer>
        LON             FLOAT           21.4283
        LAT             FLOAT          -30.7215
        ALT             FLOAT           1073.00
        FREQ_CENTER     FLOAT       1.49754e+08
        FREQ_RES        FLOAT           492611.
        TIME_RES        FLOAT           11.0001
        ASTR            STRUCT    -> <Anonymous> Array[1]
        ALPHA           FLOAT         -0.800000
        PFLAG           INT              0
        CAL             FLOAT     Array[4]
        RESIDUAL        INT              0
        VIS_NOISE       POINTER   <PtrHeapVar9>
        BASELINE_INFO   POINTER   <PtrHeapVar6>
        META_DATA       POINTER   <NullPointer>
        META_HDR        POINTER   <NullPointer>
        DEGRID_SPECTRAL_TERMS
                        INT              0
        GRID_SPECTRAL_TERMS
                        INT              0
        GRID_INFO       POINTER   <NullPointer>
        HEALPIX         STRUCT    -> <Anonymous> Array[1]


### \<obsids\>_params.sav <br />
  * params = Structure to do what? !Q

        UU              DOUBLE    Array[168]
        VV              DOUBLE    Array[168]
        WW              DOUBLE    Array[168]
        BASELINE_ARR    FLOAT     Array[168]
        TIME            FLOAT     Array[168]
        ANTENNA1        FLOAT     Array[168]
        ANTENNA2        FLOAT     Array[168]

##  Output Data<br />

##  Output Images<br />

##  Vis Data<br />
   
### \<obsids\>_vis_\<pol\>.sav <br />
  * vis_ptr = Pointer to a complex array of N_FREQ x NBlts entries, representing the visibilities of polarization \<pol\>
  * obs = Obs object
  * pol_i = Number representing polarization (0 = XX, 1 = YY)

### \<obsids\>_autos.sav <br />
  * auto_corr = Pointer array of length 2. 0=X pol, 1 = Y pol. Each entry points to the (N_FREQ x NBlts) complex array of autocorrelations for that polarization.
  * obs = Obs object

### \<obsids\>_flags.sav <br />
  * vis_weights = Pointer array of length 2. 0=X pol, 1 = Y pol. Each entry points to the (N_FREQ x NBlts) complex array of flags (1 or 0) for that polarization. Visibility is flagged if the flag is <=0 !Q
