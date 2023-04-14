# Examples of specific run cases    
FHD is very flexible, and has been designed to encompass a wide variety of instruments, simulations, and accuracy vs. speed runs. Documented below is typical examples with keywords and expected results. All keywords are additions to the [eor_wrapper_defaults.pro](https://github.com/EoRImaging/pipeline_scripts/blob/master/FHD_IDL_wrappers/eor_wrapper_defaults.pro) keywords.    

In order to run FHD, you will need to construct an IDL wrapper that calls the FHD procedures. A basic wrapper for a general run is here:    
[General](#general)  

Current MWA examples include:   
[Firstpass](#firstpass)   
[Deconvolution](#deconvolution)   
[Drift scan](#drift-scan)   
[MWA Phase II](#mwa-phase-ii)   
[Calibration only](#calibration-only)  
[Gaussian Decomposition Beams](#gaussian-decomposition-beams) <br />
[Modified gridding kernel](#modified-gridding-kernel)  

There are also examples for:    
[In situ simulation](#in-situ-simulation)

## General

This is a template for the most basic run of FHD:    
~~~
pro my_run_script

  ; Keywords
  obs_id = '<Observation ID string>'
  output_directory = '/path/to/output/dir/'
  version = '<unique version name>'
  vis_file_list = 'path/to/uvfits/' + string(obs_id) +'.uvfits'

  ; Directory setup
  fhd_file_list=fhd_path_setup(vis_file_list,version=version,output_directory=output_directory)
  healpix_path=fhd_path_setup(output_dir=output_directory,subdir='Healpix',output_filename='Combined_obs',version=version)

  ; Set global defaults and bundle all the variables into a structure.
  ; Any keywords set on the command line or in the top-level wrapper will supercede these defaults
  eor_wrapper_defaults,extra
  fhd_depreciation_test, _Extra=extra

  print,""
  print,"Keywords set in wrapper:"
  print,structure_to_text(extra)
  print,""
  general_obs,_Extra=extra
  
end
~~~     
Add keywords as necessary to change the analysis. All keywords are additions to the [eor_wrapper_defaults.pro](https://github.com/EoRImaging/pipeline_scripts/blob/master/FHD_IDL_wrappers/eor_wrapper_defaults.pro) keywords.     


## MWA    

### Firstpass
An analysis that performs calibration and subtraction using a sky-based model. No deconvolution is performed. This is a much faster method, however it will encode systematics due to the flux density differences between the sky model and the real sky due to various effects (beam differences, flux density changes, ionospheric effects, etc.).

Defaults for EoR MWA Phase I Firstpass runs are documented in [eor_wrapper_defaults.pro](https://github.com/EoRImaging/pipeline_scripts/blob/master/FHD_IDL_wrappers/eor_wrapper_defaults.pro)

### Deconvolution 
Please see [
Fast Holographic Deconvolution: A New Technique for Precision Radio Interferometry](http://adsabs.harvard.edu/cgi-bin/bib_query?arXiv:1209.1653) for more information.

Deconvolution settings   
~~~
deconvolve = 1 
max_deconvolution_components = 100000
return_decon_visibilities = 1
deconvolution_filter = 'filter_uv_uniform'
~~~

Gain factor of deconvolution (percent of flux density detected to remove during each iteration)
~~~
gain_factor = 0.1
~~~

Smooth background diffuse structure before deconvolving
~~~
smooth_width = 32
filter_background = 1
~~~

Export keywords for per-snapshot images
~~~
pad_uv_image = 1
ring_radius = 0
~~~

Optional DFT approximation
~~~
dft_threshold = 0
~~~

### Drift scan
Set the observation center to the pointing center, i.e. do not rephase.  
~~~
rephase_weights = 0 
restrict_hpx_inds = 0
hpx_radius = 10
~~~

### MWA Phase II
Set the instrument keyword
~~~
instrument = "mwa2"
~~~
The larger, default dimension is in general not necessary. 
~~~
dimension = 1024
~~~

### Calibration only
This keyword runs FHD up until the calibration outputs, and saves necessary files for efficiently running with transferred calibration (vis model files and metadata) in a directory called cal_prerun.
~~~
cal_stop = 1
~~~
From these runs, you can transfer the calibration solutions. For example, from each observation
~~~
transfer_calibration = '<path>/<to>/<FHDdir>/calibration/' + obs_id + '_cal.sav'
~~~

For model visibilities seen by an instrument, you can transfer from the cal_stop run by setting
~~~
model_transfer = '<path>/<to>/<FHDdir>/cal_prerun/vis_data'
~~~
or the non-instrumental model uv-plane by setting
~~~
model_uv_transfer = '<path>/<to>/<FHDdir>/cal_prerun/' + obs_id + '_model_uv_arr.sav'
~~~

### Gaussian Decomposition Beams
Avoid the effects of an FFT on the (de)gridding kernel by generating an analytic uv-beam kernel from a set of gaussians. The gaussians are fit using input parameters from a provided instrument file (searches for <instrument>_beam_gaussian_decomp.pro by default) on the hyper-resolved image space beam. Frequencies are interpolated between a maximum set in the instrument file to ensure smoothness and speed. See [Barry & Chokshi 2022](https://ui.adsabs.harvard.edu/abs/2022ApJ...929...64B/abstract) for more information.
  
Set this parameter to generate a gaussian decomposition beam that matches the instrumental response. This can be used for calibration. Gaussian fitting that requires multiple gaussians will take significant time.
~~~
beam_gaussian_decomp = 1
~~~
Optionally, read in parameters from a psf structure where the fitting as already been performed. For a specific psf structure, set
~~~
beam_gaussian_decomp = 1
beam_gauss_param_transfer = '<path>/<to>/<FHDdir>/beams/' + obs_id + '_beams.sav'
~~~  
or for the MWA instrumental default, set
~~~  
beam_gaussian_decomp = 1
beam_gauss_param_transfer = 1 (or 'decomp')
~~~  
A single gaussian which is FWHM-matched to the MWA beam can also be used. As seen in [Barry & Chokshi 2022](https://ui.adsabs.harvard.edu/abs/2022ApJ...929...64B/abstract), this reduces systematics. However, this is no longer an instrumental beam, and thus cannot be used for calibration (see [Calibration only](#calibration-only)).
~~~
beam_gaussian_decomp = 1
beam_gauss_param_transfer = 'gauss'  
~~~
Depending on the health of your machine's file system, it may be benificial to generate the psf (and obs) structure of the gaussian decomposition and use the `transfer_psf` option instead. 
  
  
### Modified Gridding Kernel
In order to produce EoR limit quality power spectra, a modified gridding kernel must be used to avoid model degridding errors and image aliasing (see [Barry et al 2019a](https://arxiv.org/abs/1901.02980) and [Barry et al 2019b](https://arxiv.org/abs/1909.00561)). These options cannot be used to generate calibration solutions, please use [Calibration only](calibration-only) using an instrumental beam. 

Default modified kernel window is a Blackman-Harris squared, can optionally set to any window present in [spectral_window.pro](https://github.com/EoRImaging/fhdps_utils/blob/master/spectral_window.pro).
~~~
kernel_window=1
~~~
Depending on the modified kernel window chosen, change these keywords (please see [dictionary.md](https://github.com/EoRImaging/FHD/blob/master/dictionary.md)) 
~~~
debug_dim=1
beam_mask_threshold=1e3
~~~


## In situ simulation      

FHD is also an instrument simulator. The in situ simulation harnesses the model generation aspect of FHD to run simulations purely within the same pipeline that analyzes data. 

The basic setup of the in situ simulation is    
1) generate or read in model control visibilities as "raw" visibilities,    
2) add various additional visibilities to the input (EoR, noise, etc),
3) calibrate and subtract given a new model specified by the user, and 
4) export data products for power spectrum packages.    
Please see [Calibration requirements for detecting the 21 cm epoch of reionization power spectrum and implications for the SKA
](http://adsabs.harvard.edu/cgi-bin/bib_query?arXiv:1603.00607) for more information.

The main code for the in situ simulation lives in [in_situ_sim_setup.pro](https://github.com/EoRImaging/FHD/blob/master/simulation/in_situ/in_situ_sim_setup.pro), called by [fhd_main.pro](https://github.com/EoRImaging/FHD/blob/master/fhd_core/fhd_main.pro). 

First, run a control run to generate input visibilities. Then, run an in situ simulation which uses the control run visibilities.

### Control run  
~~~
calibrate_visibilities=0
model_visibilities=1
unflag_all=1
~~~   

### In situ simulation run basics
~~~
in_situ_sim_input = '<path>/<to>/<dir>/fhd_control_run'
remove_sim_flags=1
~~~   

Add any of the various typical in situ simulation options below (or any other FHD keywords).

#### Perfect bandpass calibration
Option for a perfect bandpass. Set/unset polynomial fitting and cable reflection fitting as necessary.
~~~
sim_perf_calibrate=1
~~~

#### Per frequency bandpass calibration
Option for a per-frequency bandpass. Set/unset polynomial fitting and cable reflection fitting as necessary.
~~~
sim_over_calibrate=1
~~~

#### Calibrate on subset of sources
Typical keywords to select sources to calibrate from a catalog.
~~~
calibration_flux_threshhold = 0.1 ;; in Jy
~~~
or 
~~~
max_calibration_sources = 1000 ;; brightest in apparent brightness
~~~

#### Subtract a subset of sources
Typical keywords to select sources to subtract from a catalog independent of calibration
~~~
model_flux_threshhold = 0.1 ;; in Jy
~~~
or 
~~~
max_model_sources = 1000 ;; brightest in apparent brightness
~~~

#### Add EoR visibilities to the input control run
Point to the directory where EoR visibility sav files are located or the full filepath to a uvfits file, and add them to the control. 
~~~
eor_vis_filepath = <path>/<to>/<dir>
~~~
A multiplicative factor on the EoR visibilties to optionally scale them.
~~~
enhance_eor = 2.
~~~

#### Add thermal noise to input control run
Add white noise to the control visibilties. Need to specify either a filepath to an obs with vis_noise calculation or a noise simulation.
~~~
sim_noise = <path>/<to>/<dir>
~~~

#### Add any other general visibilities
Add any other visibility uvfits file.
~~~
extra_vis_filepath = <path>/<to>/<file>.uvfits
~~~


### EoR <br />

eor_sim=1
include_catalog_sources=0
