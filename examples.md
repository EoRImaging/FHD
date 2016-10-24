# Examples of specific run cases <br />
FHD is very flexible, and has been designed to encompass a wide variety of instruments, simulations, and accuracy vs. speed runs. Documented below is typical examples with keywords and expected results. <br />

## Accuracy vs. Speed <br />

###Fast Holographic Deconvolution <br />

###Firstpass -- New name forthcoming <br />

## Instruments <br />

###MWA <br />

**Put in standard keywords with explanations and outputs from NBarry**

###MWA Phase 2 <br />

**In progress testing by Wenyang**

###PAPER <br />

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

###EoR <br />

**Needs updated by Bryna or Adam L** <br />

###MWA <br />

###PAPER <br />

###HERA <br />



###Variable array layouts <br />
