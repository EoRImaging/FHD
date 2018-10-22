function l_m_n, obs, psf, obsdec=obsdec, obsra=obsra, dec_arr=dec_arr, ra_arr=ra_arr,$
    l_mode=l_mode,m_mode=m_mode

if ~keyword_set(obsdec) then obsdec = obs.obsdec
if ~keyword_set(obsra) then obsra = obs.obsra
if ~keyword_set(dec_arr) then dec_arr = (*psf.image_info).dec_arr
if ~keyword_set(ra_arr) then ra_arr = (*psf.image_info).ra_arr

;Calculate l mode, m mode, and phase-tracked n mode of pixel centers
cdec0 = cos(obsdec*!dtor)
sdec0 = sin(obsdec*!dtor)
cdec = cos(dec_arr*!dtor)
sdec = sin(dec_arr*!dtor)
cdra = cos((ra_arr-obsra)*!dtor)
sdra = sin((ra_arr-obsra)*!dtor)
l_mode = cdec*sdra
m_mode = sdec*cdec0 - cdec*sdec0*cdra
;n=1 at phase center, so reference from there for phase tracking
n_tracked = (sdec*sdec0 + cdec*cdec0*cdra) - 1.

nan_vals=where(finite(n_tracked, /nan),n_count)
if n_count GT 0 then begin
    n_tracked[nan_vals]=0
    l_mode[nan_vals]=0
    m_mode[nan_vals]=0
endif

return, n_tracked

end
