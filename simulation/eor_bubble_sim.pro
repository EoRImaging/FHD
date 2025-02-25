FUNCTION eor_bubble_sim, obs, jones, ltaper=ltaper, select_radius=select_radius, bubble_fname=bubble_fname, dat=dat, file_path_fhd=file_path_fhd,orthomap_var=orthomap_var, shellreplace=shellreplace

;Opening an HDF5 file and extract relevant data
if keyword_set(bubble_fname) THEN hdf5_fname = bubble_fname ELSE message, "Missing bubble file path"
if not keyword_set(select_radius) THEN  select_radius = 20     ; Degrees

dimension=obs.dimension
elements= obs.elements

f_id = H5F_OPEN(hdf5_fname)
dset_id_eor  = H5D_OPEN(f_id, '/spectral_info/spectrum')
dspace_id_eor = H5D_GET_SPACE(dset_id_eor)

freq_hpx = H5_GETDATA(hdf5_fname, '/spectral_info/freq')

dims = REVERSE(H5S_GET_SIMPLE_EXTENT_DIMS(dspace_id_eor))
nside=NPIX2NSIDE(dims[0])
nfreq_hpx = dims[1]

n_pol=obs.n_pol

; Identify the healpix pixels within select_radius of the primary beam
ang2vec,obs.obsdec,obs.obsra,cen_coords,/astro
Query_disc,nside,cen_coords,select_radius,inds_select,npix_sel,/deg
print, 'selection radius (degrees) ', select_radius
print, "Npix_selected: ", npix_sel

; Limit the range of frequencies in the uvf cube to the range of the obs
freq_arr = (*obs.baseline_info).freq
nfreq_hpx = n_elements(freq_hpx)

print, "Reading HDF5 file with EoR Healpix Cube"
t0 = systime(/seconds)
if n_elements(dat) eq 0 then dat = H5D_READ(dset_id_eor, FILE_SPACE=dpsace_id_eor)
print, 'HDF5 reading time = ', systime(/seconds) - t0, ' seconds'

if keyword_set(shellreplace) THEN BEGIN
; Replace dat with a new array of the same variance and mean.
    print, 'Replacing the shell'
    sig = sqrt(variance(dat))
    mu = mean(dat)
    print, 'sig=', sig, 'mu=', mu
    dat = randomn(seed, dims[1], dims[0])*sig + mu
ENDIF

;; This is commented out to avoid adding a Python dependency to FHD. Uncomment to enable ltaper.
;; If l taper is set --- apply weights to l modes, up to the maximum l mode of the orthoslant map.
;; There isn't a standard way to do this strictly within IDL, so call healpy functions using IDL to Python bridge.
;if keyword_set(ltaper) THEN BEGIN
;    print, 'Applying l taper'
;    lmax_fhd = floor(180./obs.degpix)
;    lmax_hpx = 3*nside-1
;    if lmax_fhd GT lmax_hpx THEN BEGIN
;        print, "Input map resolution below FHD settings. lmax_fhd="+string(lmax_fhd)
;        lmax_fhd = lmax_hpx
;    ENDIF
;    hp = python.import('healpy')
;    mmax_hpx= lmax_hpx
;    almsize = floor(mmax_hpx * (2 * lmax_hpx + 1 - mmax_hpx) / 2.) + lmax_hpx + 1
;    almidx = lindgen(almsize)
;    lm = hp.Alm.getlm(lmax_hpx,almidx)
;    lmat = lm[0]
;    mmat = lm[1]
;    win_mat = 0.5*(1-tanh(ltaper*(lmat-lmax_fhd)))
;    ; In each frequency channel, convert map to alm and apply l-taper.
;    for fi=0, nfreq_hpx-1 DO BEGIN
;        map = Temporary(reform(dat[fi,*]))
;        alm = hp.map2alm(map,lmax=lmax_hpx)
;        alm *= win_mat
;        dat[fi,*] = hp.alm2map(alm,nside,lmax=lmax_hpx)
;        dat[fi,*] *= sqrt(variance(map)/variance(dat[fi,*]))    ; Preserve variance for gaussian maps
;    ENDFOR
;ENDIF
;; End of ltaper block.


; Select pixels.
dat_interp = Fltarr(obs.n_freq, npix_sel)
dat_interp = dat[*, inds_select]
 
IF keyword_set(shellreplace) THEN BEGIN
    save, dat_interp, inds_select, filename=file_path_fhd+'_shellreplace.sav'
ENDIF

hpx_arr = Ptrarr(obs.n_freq)
for fi=0, obs.n_freq-1 DO hpx_arr[fi] = ptr_new(reform(dat_interp[fi,*]))


H5S_CLOSE, dspace_id_eor
H5D_CLOSE, dset_id_eor
H5F_CLOSE, f_id


model_uv_arr=Ptrarr(n_pol,/allocate)
t0 = systime(/seconds)
model_stokes_arr = healpix_bin(hpx_arr,obs,nside=nside,hpx_inds=inds_select,/from_kelvin)
print, 'Healpix_interpolate timing: ', systime(/seconds) - t0

; Replace the interpolated orthoslant with gaussian noise
IF keyword_set(orthomap_var) THEN BEGIN
    model_stokes_arr = ptrarr(obs.n_freq)
    convert_factor = convert_kelvin_jansky(1.,degpix=obs.degpix,freq=obs.freq_center)
    var = orthomap_var
    for fi=0, obs.n_freq-1 do begin
        model_stokes_arr[fi] = ptr_new(randomn(seed,obs.dimension,obs.elements) * sqrt(var) * convert_factor)  ; K -> Jy/pix (ortho)
    ENDFOR
ENDIF

FOR pol_i=0, n_pol-1 DO *model_uv_arr[pol_i]=Complexarr(obs.dimension,obs.elements,obs.n_freq)
FOR fi=0, obs.n_freq-1 do begin
   model_tmp=Ptrarr(n_pol,/allocate)
   FOR pol_i=1,n_pol-1 DO *model_tmp[pol_i]=Fltarr(obs.dimension,obs.elements)
   *model_tmp[0] = *model_stokes_arr[fi]      ; model_stokes_arr is Stokes I
   model_arr = stokes_cnv(model_tmp, jones, /inverse, _extra=extra)
   Ptr_free, model_tmp

   FOR pol_i=0,n_pol-1 DO BEGIN
       model_uv=fft_shift(FFT(fft_shift(*model_arr[pol_i]),/inverse))
       (*model_uv_arr[pol_i])[*,*,fi]=model_uv
   ENDFOR
   Ptr_free,model_arr
ENDFOR

return, model_uv_arr
END
