FUNCTION fhd_struct_update_obs,obs, dimension=dimension, elements=elements, degpix=degpix, kbinsize=kbinsize, $
    n_pol=n_pol,max_baseline=max_baseline,min_baseline=min_baseline,FoV=FoV,$
    obsx=obsx,obsy=obsy,nfreq_avg=nfreq_avg,spectral_index=spectral_index,nside=nside,$
    restrict_hpx_inds=restrict_hpx_inds,n_hpx=n_hpx,n_zero_hpx=n_zero_hpx,_Extra=extra

;updates the structure containing frequently needed parameters relating to the observation
IF N_Elements(spectral_index) EQ 0 THEN spectral_index=obs.alpha
IF N_Elements(n_pol) EQ 0 THEN n_pol=obs.n_pol
IF N_Elements(max_baseline) EQ 0 THEN max_baseline=obs.max_baseline
IF N_Elements(min_baseline) EQ 0 THEN min_baseline=obs.min_baseline
git,'describe',result=code_version,repo_path=rootdir('fhd'),args='--long --dirty'
IF N_Elements(code_version) GT 0 THEN code_version=code_version[0] ELSE code_version=''

IF Tag_exist(obs,'healpix') THEN BEGIN
    IF Keyword_Set(nside) THEN obs.healpix.nside=Long(nside)
    IF Keyword_Set(restrict_hpx_inds) THEN obs.healpix.ind_list=String(restrict_hpx_inds)
    IF Keyword_Set(n_hpx) THEN obs.healpix.n_pix=Long(n_hpx)
    IF Keyword_Set(n_zero_hpx) THEN obs.healpix.n_zero=Long(n_zero_hpx)
ENDIF

b_info=*(obs.baseline_info)
n_freq=obs.n_freq
IF N_Elements(nfreq_avg) EQ 0 THEN nfreq_avg=Round(n_freq/(Max(b_info.fbin_i)+1.))
IF tag_exist(obs,'freq_res') THEN freq_bin=nfreq_avg*obs.freq_res ELSE freq_bin=nfreq_avg*(Max(frequency_array)-Min(frequency_array))/(n_freq-1.)
frequency_array=b_info.freq
freq_hist=histogram(frequency_array,locations=freq_bin_val,binsize=freq_bin,reverse_ind=freq_ri)
nfreq_bin=N_Elements(freq_hist)
freq_bin_i=fltarr(n_freq)
FOR bin=0L,nfreq_bin-1 DO IF freq_ri[bin] LT freq_ri[bin+1] THEN freq_bin_i[freq_ri[freq_ri[bin]:freq_ri[bin+1]-1]]=bin
freq_center=Median(frequency_array)

IF N_Elements(dimension) EQ 0 THEN BEGIN
    dimension = obs.dimension
    IF N_Elements(elements) EQ 0 THEN elements = obs.elements
ENDIF ELSE IF N_Elements(elements) EQ 0 THEN elements = dimension
IF Keyword_Set(FoV) THEN kbinsize=!RaDeg/FoV
IF Keyword_Set(kbinsize) THEN BEGIN
    degpix=!RaDeg/(kbinsize*dimension) ;image space resolution, in degrees per pixel
ENDIF ELSE BEGIN
    IF ~Keyword_Set(degpix) THEN BEGIN
        degpix = obs.degpix
    ENDIF
    kbinsize=!RaDeg/(degpix*dimension) ;k-space resolution, in wavelengths per pixel
ENDELSE
k_span = (dimension > elements)*kbinsize
max_baseline=max_baseline<(k_span/sqrt(2.))

struct=obs
struct.dimension=Float(dimension)
struct.elements=Float(elements)
struct.kpix=Float(kbinsize)
struct.degpix=Float(degpix)
struct.max_baseline=Float(max_baseline)
struct.min_baseline=Float(min_baseline)

struct.n_pol=Fix(n_pol,type=2)
struct.alpha=Float(spectral_index)

projection_slant_orthographic,astr=astr,degpix=degpix,obsra=obs.obsra,obsdec=obs.obsdec,zenra=obs.zenra,zendec=obs.zendec,$
    dimension=dimension,elements=elements,phasera=obs.phasera,phasedec=obs.phasedec,obsx=obsx,obsy=obsy,zenx=zenx,zeny=zeny,$
    epoch=2000.,JDate=obs.JD0


struct.obsx=Float(obsx)
struct.obsy=Float(obsy)
struct.zenx=Float(zenx)
struct.zeny=Float(zeny)
struct.astr=astr
struct.code_version=String(code_version)


b_info=*obs.baseline_info
;freq_use:Fix(freq_use)
;tile_use:Fix(tile_use)
;time_use:Fix(time_use)
;tile_flag:meta.tile_flag
b_info.fbin_i=Long(freq_bin_i)
b_info.freq=Float(frequency_array)
struct.baseline_info=Ptr_new(b_info)

RETURN,struct
END
