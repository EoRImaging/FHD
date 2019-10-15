# Examples of specific run cases <br />
FHD is very flexible, and has been designed to encompass a wide variety of instruments, simulations, and accuracy vs. speed runs. Documented below is typical examples with keywords and expected results. All keywords are additions to the [eor_wrapper_defaults.pro](https://github.com/EoRImaging/pipeline_scripts/blob/master/FHD_IDL_wrappers/eor_wrapper_defaults.pro) keywords. <br />

Current MWA examples include: 
[Firstpass](#firstpass) 
[Deconvolution](#deconvolution) 
[Drift scan](#drift-scan) 

There are also examples for:



## MWA <br />

### Firstpass
An analysis that performs calibration and subtraction using a sky-based model. No deconvolution is performed. This is a much faster method, however it will encode systematics due to the flux density differences between the sky model and the real sky due to various effects (beam differences, flux density changes, ionospheric effects, etc.).



### Deconvolution 
Please see [
Fast Holographic Deconvolution: A New Technique for Precision Radio Interferometry](http://adsabs.harvard.edu/cgi-bin/bib_query?arXiv:1209.1653) for more information.

Deconvolution settings   
~~~
deconvolve = 1 
max_sources = 200000
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

### MWA Phase 2 <br />

**In progress testing by Wenyang**

## Simulations <br />

### In situ <br />

FHD is an analysis package, but it is also an instrument simulator. The in situ simulation harnesses the model generation aspect of FHD to run simulations purely within the same pipeline that analyzes data. 

The basic setup of the in situ simulation is 1) to generate or read in model visibilities as "raw" visibilities, 2) calibrate and subtract given a new model specified by the user, and 2) export data products for power spectrum packages. Please see [Calibration requirements for detecting the 21 cm epoch of reionization power spectrum and implications for the SKA
](http://adsabs.harvard.edu/cgi-bin/bib_query?arXiv:1603.00607) for more information.

The main code for the in situ simulation lives in [in_situ_sim_setup.pro](https://github.com/EoRImaging/FHD/blob/master/simulation/in_situ/in_situ_sim_setup.pro), called by [fhd_main.pro](https://github.com/EoRImaging/FHD/blob/master/fhd_core/fhd_main.pro). To activate, set `in_situ_sim_input`. Setting to 1 forces the "raw" visibilities to be made within the current run, and setting to a sav file path inputs model visibilities from a previous run (which is the preferred method since that run is independently documented). In order to then change the calibration and subtraction model, use the proper keywords that are normally used for an analysis run. To run with the same catalog that made the input, but with fewer known sources, use such keywords as `max_calibration_sources`, `max_model_sources`, `calibration_flux_threshhold`, `model_flux_threshold`, or others. These are documented in the [dictionary](https://github.com/EoRImaging/FHD/blob/e9ef646817928e5658d8347dc150b9ffd4d7d3ee/dictionary.md) and utlized in [generate_source_cal_list.pro](https://github.com/EoRImaging/FHD/blob/e9ef646817928e5658d8347dc150b9ffd4d7d3ee/fhd_core/calibration/generate_source_cal_list.pro).

There are options to how the in situ simulation is run, besides all of the options that come with FHD. To turn off all flagging, including coarse band edges, tiles, and times, set the keyword `remove_sim_flags`. To add an EoR signal to the input visibilities, set `eor_savefile` to the full path of a sav file that has the same setup as the input visiblities and named after the observation ID. To enhance the EoR signal, set `enhance_eor` to a multiplicative factor. To add noise to the in situ sim, set `sim_noise`. To calibrate each fine frequency independently, set `sim_over_calibrate`. To calibrate perfectly (i.e. insert 1's), set `sim_perf_calibrate`.

An example of a run to generate input model visibilities <br />
'nb_model_goldenset': begin <br />
&nbsp;&nbsp;    calibrate_visibilities=0 <br />
&nbsp;&nbsp;    model_visibilities=1 <br />
&nbsp;&nbsp;    unflag_all=1 <br />
&nbsp;&nbsp;    return_cal_visibilities=0 <br />
&nbsp;&nbsp;    nfreq_avg=384 <br />
&nbsp;&nbsp;    remove_sim_flags=1 <br />
&nbsp;&nbsp;    model_catalog_file_path = filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data') <br />
end <br />

An example of a run to generate the in situ simulation <br />
'nb_sim_hash_overfit': begin <br />
&nbsp;&nbsp;    in_situ_sim_input = '/nfs/mwa-04/r1/EoRuvfits/analysis/fhd_nb_model_goldenset'  <br />
&nbsp;&nbsp;    remove_sim_flags=1 <br />
&nbsp;&nbsp;    max_calibration_sources=4000 <br />
&nbsp;&nbsp;    nfreq_avg=384 <br />
&nbsp;&nbsp;    sim_over_calibrate=1 <br />
&nbsp;&nbsp;    eor_savefile = '/nfs/mwa-04/r1/EoRuvfits/analysis/calibration_sim/fhd_nb_hash_removesimflags/vis_data/' <br />
&nbsp;&nbsp;    calibration_catalog_file_path=filepath('master_sgal_cat.sav',root=rootdir('FHD'),subdir='catalog_data') <br />
end <br />

### EoR <br />

eor_sim=1
include_catalog_sources=0

### Diffuse <br />

diffuse_model=filepath('gsm_150MHz.sav',root=rootdir('FHD'),subdir='catalog_data') <br />
  -*This is a full-sky map in galactic coordinates of the Global Sky Model at 150 MHz only. *

### Point sources <br />

sources_file_name='GLEAM_EGC_catalog'<br />
  -*Specifies to use the publicly-released GLEAM extragalactic point source catalog*<br />
max_model_sources=10000<br />

### Variable array layouts <br />
