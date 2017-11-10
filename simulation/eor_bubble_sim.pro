PRO eor_bubble_sim ; , hdf5_fname, obs, jones, _Extra=extra

;; How to handle if the frequency structure is different? --- Import the frequency structure in the hdf5 file. Subselect if it's smaller than the range of obs, then interpolate after everything else is done. (loop over pol, interpolate in freq)

;Opening an HDF5 file and extract relevant data
file = '/users/alanman/data/alanman/BubbleCube/TiledHpxCubes/light_cone_surfaces.hdf5'   ; For now
f_id = H5F_OPEN(file)
dset_id_eor  = H5D_OPEN(f_id, '/spectral_info/spectrum')
dspace_id_eor = H5D_GET_SPACE(dset_id_eor)

freq_hpx = H5_GETDATA(file, '/spectral_info/freq')
ra_hpx = H5_GETDATA(file, '/object/RA') / !RaDeg
dec_hpx = H5_GETDATA(file, '/object/Dec') /!RaDeg

dims = REVERSE(H5S_GET_SIMPLE_EXTENT_DIMS(dspace_id_eor))
nside=NPIX2NSIDE(dims[0])
nfreq_hpx = dims[1]

restore, '/gpfs/data/jpober/alanman/FHD_out/fhd_sim_semicircle_point/metadata/semicircle_14m_comp_0_obs.sav'  ; For now.
restore, '/gpfs/data/jpober/alanman/FHD_out/fhd_sim_semicircle_point/beams/semicircle_14m_comp_0_jones.sav'  ; For now.

phase_ra = obs.phasera / !RaDeg
phase_dec = obs.phasedec / !RaDeg
; Identify the healpix pixels within 90\deg of the phase center.
gcirc, 0, phase_ra, phase_dec, ra_hpx, dec_hpx, dists
inds_select = where(dists LT !Pi/2.)
npix_sel =  n_elements(inds_select)

; Limit the range of frequencies in the uvf cube to the range of the obs
freq_arr = (*obs.baseline_info).freq
lim = minmax(freq_arr)
freq_inds = where((freq_hpx GT lim[0]) and (freq_hpx LT lim[1]) )
freq_hpx = freq_hpx[freq_inds]
nfreq_hpx = n_elements(freq_hpx)

;; Extract only these healpix indices from the file.
;; !! Unclear at this time how exactly to get the selection working. I'll need to experiment more. For now, just read the whole dataset in and select from it.
;H5S_SELECT_ELEMENTS, dspace_id_eor, hpx_inds, /RESET

print, "Reading HDF5 file with EoR Healpix Cube"
t0 = systime(/seconds)
dat = H5D_READ(dset_id_eor, FILE_SPACE=dpsace_id_eor)
print, 'HDF5 reading time = ', systime(/seconds) - t0, ' seconds'

; Interpolate in frequency:
dat_interp = Fltarr(obs.n_freq,npix_sel)
for hpx_i=0,npix_sel-1 DO dat_interp[*,hpx_i] = Interpol(dat[freq_inds,hpx_i],freq_hpx,freq_arr, /spline)

hpx_arr = Ptrarr(obs.n_freq)
for fi=0, obs.n_freq-1 DO hpx_arr[fi] = ptr_new(reform(dat_interp[fi,*]))


H5S_CLOSE, dspace_id_eor
H5D_CLOSE, dset_id_eor
H5F_CLOSE, f_id

n_pol=4

model_uv_arr=Ptrarr(n_pol,obs.n_freq, /allocate)
t0 = systime(/seconds)
model_stokes_arr = healpix_interpolate(hpx_arr,obs,nside=nside,hpx_inds=inds_select,/from_kelvin)
print, 'Hpx_interpolate timing: ', systime(/seconds) - t0

FOR fi=0, obs.n_freq-1 do begin    ; 30 seconds for 203 channels
   model_tmp=*model_stokes_arr[fi]
   model_tmp=Ptrarr(n_pol,/allocate)
   *model_tmp[0] = *model_stokes_arr[fi]
   FOR pol_i=1,n_pol-1 DO *model_tmp[pol_i]=Fltarr(obs.dimension,obs.elements)
   model_arr = stokes_cnv(model_tmp, jones, /inverse)   ; In vis_simulate, the I to X/Y conversion is done by simply splitting. Would that be better?
   Ptr_free, model_tmp

   FOR pol_i=0,n_pol-1 DO BEGIN
       model_uv=fft_shift(FFT(fft_shift(*model_arr[pol_i]),/inverse))
       *model_uv_arr[pol_i,fi]=model_uv
   ENDFOR
   Ptr_free,model_arr

ENDFOR

print, size(model_uv_arr)

END
