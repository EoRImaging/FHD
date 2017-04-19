# Outputs <br />
FHD outputs various data products. We outline and describe these below. <br />

## Beam <br />

### \<obsid\>_beams.sav <br />

**beam_ptr**: A pointer to a pointer array of dimensions N<sub>polarizations</sub> x N<sub>frequencies</sub> x N<sub>baselines</sub>, which themselves are pointer arrays of dimension N<sub>UVresolution</sub> x N<sub>UVresolution</sub> to the appropriate hyperresolution uv complex beam model for that particular frequency, polarization, and baseline sampled at the uv resolution pixel. There is a one-pixel offset in the UV resolution for interpolation purposes. The final result is a vectorized image, where each slice has been concatenated into a vector that can be efficiently used in matrix multiplication algorithms. Image below shows the output of fully dereferencing the beam_ptr, and what it pictorially represents.

<p align="center">
  <img src="https://github.com/nicholebarry/MWA_data_analysis/blob/master/image_docs/beam_ptr2-crop.pdf" width="350"/>
</p>

## Calibration <br />

## Grid Data<br />

##  Healpix<br />

##  Metadata<br />

##  Output Data<br />

##  Output Images<br />

##  Vis Data<br />
