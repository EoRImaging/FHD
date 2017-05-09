# Examples of specific run cases <br />
FHD is very flexible, and has been designed to encompass a wide variety of instruments, simulations, and accuracy vs. speed runs. Documented below is typical examples with keywords and expected results. <br />

## Accuracy vs. Speed <br />

### Fast Holographic Deconvolution <br />

### Firstpass -- New name forthcoming <br />

## Instruments <br />

### MWA <br />

**Put in standard keywords with explanations and outputs from NBarry** <br />

**Drift scan-like observations not from the EoR fields -- in-progress notes** <br />
Based on analysis of the "diffuse survey" observations. These are the in-progress notes by RByrne. <br />

-*Recalculate keywords* <br />
recalculate_all = 0 <br />
mapfn_recalculate = 0 <br />
-*Cotter versioning, bandpass handled by FHD calibration* <br />
uvfits_version = 5 <br />
uvfits_subversion = 1 <br />
saved_run_bp = 0 <br />
-*Calibrate to the GLEAM catalog (default only works for the EoR0 field)* <br />
calibration_catalog_file_path=filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data') <br />
-*Set the observation center to the pointing center, i.e. do not rephase (this is for drift-like scans)* <br />
rephase_weights = 0 <br />
restrict_hpx_inds = 0 <br />
hpx_radius = 10 <br />
-*No diffuse* <br />
undefine, diffuse_calibrate, diffuse_model <br />

**Deconvolving drift scan-like observations not from the EoR fields -- in-progress notes** <br />
Based on analysis of the "diffuse survey" observations. These are the in-progress notes by RByrne. <br />

-*Cotter versioning, bandpass handled by FHD calibration* <br />
uvfits_version = 5 <br />
uvfits_subversion = 1 <br />
saved_run_bp = 0 <br />
-*Calibrate to the GLEAM catalog* <br />
calibration_catalog_file_path = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data') <br />
return_cal_visibilities = 0 <br />
-*Deconvolution settings* <br />
deconvolve = 1 <br />
gain_factor = 0.1 <br />
max_sources = 200000 <br />
return_decon_visibilities = 1 <br />
deconvolution_filter = 'filter_uv_uniform' <br />
subtract_sidelobe_catalog = filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data') <br />
return_sidelobe_catalog = 1 <br />
-*Export keywords* <br />
pad_uv_image = 1 <br />
-*Recalculate run* <br />
recalculate_all = 1 <br />
snapshot_recalculate = 1 <br />
-*No diffuse* <br />
undefine, diffuse_calibrate, diffuse_model <br />
-*Set the observation center to the pointing center, i.e. do not rephase (this is for drift-like scans)* <br />
rephase_weights = 0 <br />
restrict_hpx_inds = 0 <br />
hpx_radius = 10 <br />
-*Not sure what these do* <br />
dft_threshold = 0   <br />
snapshot_healpix_export = 1 <br />
smooth_width = 32 <br />
filter_background = 1 <br />
dimension = 2048 <br />
FoV = 0 <br />


### MWA Phase 2 <br />

**In progress testing by Wenyang**

### PAPER imaging array<br />

- `instrument = 'paper'`<br />
Specify the paper array<br />
- `calibration_auto_initialize=1`<br />
The default set of 1's is not a good estimate to begin the calibration convergence loop. Scaled auto-correlations must be used<br />
- `ref_antenna = 1`<br />
Since many antennas are flagged in this data set, make sure an unflagged antenna is specified for the reference<br />
- `time_offset=5.*60.`<br />
Time offset of phase center from start time. PAPER data are phased to 5 minutes after the start time.<br />
- `hera_inds = [80,104,96,64,53,31,65,88,9,20,89,43,105,22,81,10,72,112,97]+1`<br />
Specify indices of HERA antennas for flagging array. Antennas are indexed from 1. (Not a FHD keyword)<br />
- `paper_inds = [1,3,4,13,15,16,23,26,37,38,41,42,46,47,49,50,56,54,58,59,61,63,66,67,70,71,73,74,82,83,87,90,98,99,103,106,124,123,122,121,120,119,118,117,0,14,44,113,126,127]+1`<br />
Specify indices of PAPER imaging antennas. Antennas are indexed from 1. (Not a FHD keyword)<br />
- `paper_hex = [2,21,45,17,68,62,116,125,84,100,85,57,69,40,101,102,114,115,86]+1`<br />
Specify indices of PAPER hex antennas. Antennas are indexed from 1. (Not a FHD keyword)<br />
- `paper_pol = [25,19,48,29,24,28,55,34,27,51,35,75,18,76,5,77,32,78,30,79,33,91,6,92,52,93,7,94,12,95,8,107,11,108,36,109,60,110,39,111]+1`<br />
Specify indices of PAPER polarization antennas. Antennas are indexed from 1. (Not a FHD keyword)<br />
- `tile_flag_list = [paper_hex,paper_pol,hera_inds]`<br />
Specify antennas to flag, leaving only the PAPER imaging antennas<br />
- `cal_time_average=0`<br />
Use all times in the calibration loop. Currently not default, and is technically more correct.<br />
- `nfreq_avg=1024`<br />
There are 1024 frequency channels. Only create one beam for the full set for testing purposes.<br />
- `calibration_catalog_file_path=filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data')`<br />
Use GLEAM as the calibration/model catalog. Needs updating so that the proper frequency band is used for the catalog (currently is set at 181MHz for the center of the band, future band will depend on the frequency subset chosen).<br />
- `cable_bandpass_fit=0`<br />
Cable averaging keyword is only for MWA at the moment.<br />
- `saved_run_bp=0`<br />
Saved bandpass keyword is only for MWA at the moment.<br />
- `cal_mode_fit=0`<br />
Cable reflection keyword is only for MWA at the moment.<br />
- `max_calibration_sources=500`<br />
Cut out dim sources from the calibration/model for quicker testing.<br />
- `undefine, diffuse_calibrate,diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct`<br />
Remove the default settings for the diffuse model and other cable reflection keywords since they are MWA specific.<br />
- `beam_offset_time=300`<br />
Move the beam calculation to be 300 seconds into the observation to be centered<br />
- `flag_calibration=0`<br />
Turn off extra flagging for testing purposes.<br />
- `min_cal_baseline = 10`<br />
Set the minimum calibration baseline to 10 wavelengths. This will definitely be in a region where a diffuse model is required for precision astrophysics, but there are very few baselines to calibrate on at the moment.<br />
- `calibration_polyfit=0`<br />
Remove typical calibration procedures; use per-freq calibration for testing purposes.<br />
- `bandpass_calibrate=0`<br />
Remove typical calibration procedures; use per-freq calibration for testing purposes.<br />
- `flag_visibilities=1`<br />
Unknown if needed. Was hoping to remove wild baselines.<br />
- `dimension=4096`<br />
Unknown if needed. Was concerned about long baselines being removed since there are so few to begin with.<br />
- `elements=4096`<br />
Unknown if needed. Was concerned about long baselines being removed since there are so few to begin with.<br />

###HERA <br />

###HERA x PAPER imaging array <br />

**Keyword testing with HERA19 x PAPER imag -- in-progress notes** <br />
HERA19 can be cross correlated with the PAPER imaging array. These are the in-progress notes by NBarry. <br />
	
instrument= 'hera' <br />
  -*There should be a new instrument keyword to indicate the hera x paper imag array, beam is currently created by combining two existing antenna sav files in a hacky way* <br />
nfreq_avg=1024 <br />
  -*Currently making a beam once for the whole band for quick testing* <br />
calibration_catalog_file_path=filepath('GLEAMIDR4_181_consistent.sav',root=rootdir('FHD'),subdir='catalog_data') <br />
  -*Needs to be recreated using the 150MHz catalog* <br />
max_calibration_sources=8000 <br />
  -*Limiting cal/model sources for quicker testing* <br />
cable_bandpass_fit=0 <br />
  -*Redundant* <br />
saved_run_bp=0 <br />
  -*Redundant* <br />
cal_mode_fit=0 <br />
  -*Redundant* <br />
undefine, diffuse_calibrate, diffuse_model,cal_cable_reflection_fit,cal_cable_reflection_mode_fit,cal_cable_reflection_correct <br />
  -*Take out the diffuse model and all cable reflection fits* <br />
hera_inds = [80,104,96,64,53,31,65,88,9,20,89,43,105,22,81,10,72,112,97] <br />
paper_inds = [1,3,4,13,15,16,23,26,37,38,41,42,46,47,49,50,56,57,58,59,61,63,66,67,70,71,73,74,82,83,87,90,98,99,103,106,114,115,116,117,118,119,120,121,122,123,124,125,126,127] <br />
paper_hex = [2,21,45,17,68,62,0,113,84,100,85,54,69,40,101,102,44,14,86] <br />
paper_pol = [25,19,48,29,24,28,55,34,27,51,35,75,18,76,5,77,32,78,30,79,33,91,6,92,52,93,7,94,12,95,8,107,11,108,36,109,60,110,39,111] <br />
tile_flag_list = [paper_hex,paper_pol] <br />
debug_antenna=1 <br />
debug_double_read=1 <br />
flag_calibration=0 <br />
beam_offset_time=300 <br />
  -*Create the beam halfway through time sampling* <br />
min_cal_baseline = 10 <br />
  -*Needs tweaked and testing <br />
calibration_polyfit=0 <br />
  -*Remove polyfit for cal solutions for now* <br />
bandpass_calibrate=0 <br />
  -*Remove bandpass calibration for now* <br />

## Simulations <br />

On Oscar (Brown University cluster), simulations may be run via one of two wrapper scripts. These are written to run on clusters managed by the Simple Linux Utility for Resource Management (SLURM) job scheduler.
  - 'sim_pipe_slurm.sh' is analogous to "pipe_dream", and allows a simple simulation to be run off a set of ObsIDs.
  - 'sim_variation.sh' must be passed a parameter "-p 'param=opt1,opt2,opt3,...'", and can only be run for one ObsID at a time. This allows for a single simulation parameter to be varied. All the results will be stored in the same output directory, but files corresponding with each value of param will have 'param=opt_i' appended to the filename (where opt_i is the i'th value).

The parameters for the simulation are specified by the version string, which matches an entry in the "case" statement in sim_versions_wrapper (analogous to eor_firstpass_versions). Each wrapper must be given a version string, obs_id list file, and other SLURM parameters (see examples in submit_sims.sh).

###EoR <br />

eor_sim=1
include_catalog_sources=0

**Needs updated by Bryna or Adam L** <br />

###Diffuse <br />

diffuse_model=filepath('gsm_150MHz.sav',root=rootdir('FHD'),subdir='catalog_data')
  -*This is a full-sky map in galactic coordinates of the Global Sky Model at 150 MHz only. *

###Point sources <br />

sources_file_name='GLEAM_EGC_catalog'
  -*Specifies to use the publicly-released GLEAM extragalactic point source catalog*
max_model_sources=10000
  -*Limits the number of sources under consideration. The number of included sources is also limitd by the beam threshold and whether or not sidelobe sources are included, but this is a hard cutoff.*

###MWA <br />

###PAPER <br />

instrument='paper'
nfreq_avg=203
  -*Calculate the psf at only one frequency bin.*

###In-situ <br />

**Nichole will fill in**  <br />

###HERA <br />

instrument='hera' <br />
beam_model_version=2 <br />
 -*For newest version of the beam model*



###Variable array layouts <br />
