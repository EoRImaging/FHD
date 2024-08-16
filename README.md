# FHD
Fast Holographic Deconvolution

## Introduction

FHD is an open-source imaging algorithm for radio interferometers, specifically tested on MWA Phase I, MWA Phase II, PAPER, and HERA. There are three main use-cases for FHD: efficient image deconvolution for general radio astronomy, fast-mode Epoch of Reionization analysis, and simulation. A license for IDL 8.2 or above is required.

For further details, please see [Sullivan et al 2012](https://arxiv.org/abs/1209.1653) or [Barry et al 2019a](https://arxiv.org/abs/1901.02980).

## Quick Start Guide

1. Acquire an IDL 8.2 or above license and install.
2. [Install FHD](#installation).
3. Acquire a data file in the uvfits format. For an explanation of the format and instructions for acquiring public MWA data is available in the [Inputs resource](https://github.com/EoRImaging/FHD/blob/master/inputs.md).
4. Create a wrapper script with the appropriate keywords. For examples of FHD run wrappers see the [Examples resource](https://github.com/EoRImaging/FHD/blob/master/examples.md). For an explanation of the keywords see the [Dictionary resource](https://github.com/EoRImaging/FHD/blob/master/dictionary.md).
5. Read and abide by the [Community Guidelines](#community-guidelines).

## Useful Documentation Resources

* [Dictionary](https://github.com/EoRImaging/FHD/blob/master/dictionary.md): FHD is run by specifying keywords in a wrapper that is passed to the main code. These keywords change the functionality of FHD for fine-tuning and flexibility. Please visit the dictionary for keywords and their definitions.
* [Assumptions](https://github.com/EoRImaging/FHD/blob/master/assumptions.md): Some mathematical assumptions are built into the framework of FHD. Please visit the assumptions page to explore the parameter space.
* [Examples](https://github.com/EoRImaging/FHD/blob/master/examples.md): Please visit the examples page for brief tutorials on specific cases.
* [Outputs](https://github.com/EoRImaging/FHD/blob/master/outputs.md): For documentation of the various outputs, see the outputs page.
* [Inputs](https://github.com/EoRImaging/FHD/blob/master/inputs.md): FHD inputs beam models, calibration files, and sky model catalogs. Currently available inputs are described on this page.
* [Publications](https://github.com/EoRImaging/FHD/blob/master/publications.md): FHD has been used in a variety of publications, documented on this page.

## Installation
Download the latest versions of these libraries/repos.    
a. This repository   
b. The [fhdps_utils](https://github.com/EoRImaging/fhdps_utils) repository   
c. The NASA [astro](https://github.com/wlandsman/IDLAstro) IDL library    
d. The [coyote](https://github.com/idl-coyote/coyote) library   
e. The [HEALPix](https://healpix.sourceforge.io/) library   
f. Optional: the [eppsilon](https://github.com/EoRImaging/eppsilon/) power spectrum generation code    

Add these libraries to your IDL path, with examples per OS below.  
a. Windows: `!PATH = !PATH + ';' + Expand_Path('+path\to\library\')`   
b. Unix: `!PATH = Expand_Path('+path/to/library/') + ':' + !PATH`  
Be sure to use the correct separator (; or :) and include the ‘+’ sign at the start of `+path/to/library/`. Optionally, you can create an IDL startup file to set the IDL path. The [IDL startup file](https://www.nv5geospatialsoftware.com/docs/StartupFiles.html) is essentially a `bashrc` for IDL -- create a file `idl_startup.pro` with all of your IDL path commands, and set the global variable `IDL_STARTUP` to its location (e.g. `export IDL_STARTUP=/home/<user>/idl_startup.pro` in your `bashrc` or similar).

Install [HEALPix](http://healpix.jpl.nasa.gov/html/install.htm)    

Install [Imagemagick](http://www.imagemagick.org) if not already present.   

Suggested test commands:  
a. `idl`  
will open up an IDL terminal if IDL is installed correctly. Run the following commands in an IDL terminal.  
b. `print,cgHasImageMagick()`   
prints error if coyote library not installed, returns 0 if coyote library installed but Imagemagick not installed correctly, and returns 1 if both are installed correctly.   
c. `astrolib`   
prints error if astro IDL library not installed correctly, prints message “ASTROLIB: Astronomy Library system variables have been added” if installed correctly.   
d. `init_healpix`   
prints error if HEALPix not installed correctly   
e. `imagefast,randomN(5,256,256),file_path='some/output/path/testimage.png'`  
prints error if FHD not installed correctly, generates a 256x256 pixel image of poisson noise at path 'some/output/path/testimage.png' if installed correctly and read/write permissions are set correctly.

## Community Guidelines
We are an open-source community that interacts and discusses issues via GitHub. We encourage collaborative development. New users are encouraged to submit [issues](https://github.com/EoRImaging/FHD/issues) and [pull requests](https://github.com/EoRImaging/FHD/pulls) and to create branches for new development and exploration. Comments and suggestions are welcome.

Please cite [Sullivan et al 2012](https://arxiv.org/abs/1209.1653) and [Barry et al 2019a](https://arxiv.org/abs/1901.02980) when publishing data reduction from FHD.

## Maintainers
FHD was built by Ian Sullivan and the University of Washington radio astronomy team. Maintainance is a group effort split across University of Washington and Brown University, with contributions from University of Melbourne and Arizona State University. 
